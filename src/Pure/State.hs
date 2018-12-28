{-# LANGUAGE GeneralizedNewtypeDeriving, ConstraintKinds, ImplicitParams, MagicHash, FlexibleInstances, MultiParamTypeClasses, RankNTypes #-}
module Pure.State
  ( Ref, ref
  , getWith, putWith, modifyWith
  , runPureWith, runPure, runPureIO, runPureWithIO
  , (=<|), (|>=), (=<||>), (=<||>=)
  , inject
  , module State
  , module Export
  ) where

import Pure as Export hiding (inject,modify,get,Ref)
import qualified Pure

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.IO.Class
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.State.Class as State

import Data.IORef
import Data.Kind
import Data.Proxy
import Data.Typeable

import GHC.Exts

data Ref s = Ref
  { reader :: IO s
  , writer :: s -> IO ()
  }

newtype PureM s m a = PureM { unPureM :: Reader.ReaderT (Ref s) m a }
  deriving (Functor,Applicative,Monad,Alternative,MonadPlus)

instance MonadIO m => MonadIO (PureM s m) where
  {-# INLINE liftIO #-}
  liftIO = PureM . liftIO

instance MonadTrans (PureM s) where
  {-# INLINE lift #-}
  lift = PureM . lift

instance MonadIO m => State.MonadState s (PureM s m) where
  {-# INLINE get #-}
  get = get
  {-# INLINE put #-}
  put = put

instance MonadFix m => MonadFix (PureM s m) where
  {-# INLINE mfix #-}
  mfix = PureM . mfix . (unPureM .)

{-# INLINE ref #-}
ref :: Monad m => PureM s m (Ref s)
ref = PureM Reader.ask

{-# INLINE get #-}
get :: MonadIO m => PureM s m s
get = ref >>= getWith

{-# INLINE put #-}
put :: MonadIO m => s -> PureM s m ()
put s = ref >>= flip putWith s

{-# INLINE modify #-}
modify :: MonadIO m => (s -> s) -> PureM s m ()
modify f = ref >>= flip modifyWith f

{-# INLINE getWith #-}
getWith :: MonadIO m => Ref s -> m s
getWith ref = liftIO (reader ref)

{-# INLINE putWith #-}
putWith :: MonadIO m => Ref s -> s -> m ()
putWith ref s = liftIO (writer ref s)

{-# INLINE modifyWith #-}
modifyWith :: MonadIO m => Ref s -> (s -> s) -> m ()
modifyWith ref f = liftIO (reader ref >>= \s -> writer ref (f s))

{-# INLINE runPureWith #-}
runPureWith :: (Typeable s, Typeable m, Monad m) => (forall a. m a -> IO a) -> s -> (Ref s -> PureM s m View) -> View
runPureWith f s st = runPure f s (ref >>= st)

{-# INLINE runPure #-}
runPure :: (Typeable s, Typeable m) => (forall a. m a -> IO a) -> s -> PureM s m View -> View
runPure f s st = flip LibraryComponentIO (f,s,st) $ \self ->
  let updateView = do
        (_,s_) <- Pure.get self
        let
          reader = readIORef s_
          writer s = writeIORef s_ s >> updateView
          stateRef = Ref reader writer
        Pure.modifyM_ self $ \(f,_,PureM st) _ -> do
          v <- f (Reader.runReaderT st stateRef)
          return ((v,s_),return ())
  in
    def
      { construct = do
          s_ <- newIORef s
          return (NullView Nothing,s_)
      , executing = updateView
      , receive = \(_,new_s,_) (v,s_) -> do
          (_,old_s,_) <- Pure.ask self

          -- Yeah, I don't like it either, but what can you do?
          unless (isTrue# (reallyUnsafePtrEquality# new_s old_s)) $
            writeIORef s_ new_s

          updateView
          return (v,s_)
      , render = \_ (v,_) -> v
      }

{-# INLINE runPureIO #-}
runPureIO :: (Typeable s) => s -> PureM s IO View -> View
runPureIO = runPure id

{-# INLINE runPureWithIO #-}
runPureWithIO :: (Typeable s) => s -> (Ref s -> PureM s IO View) -> View
runPureWithIO = runPureWith id

infixr 9 |>=
{-# INLINE (|>=) #-}
(|>=) :: (Applicative m, HasChildren a) => (a -> a) -> [m View] -> a -> m a
(|>=) f mvs a = (f a) =<||>= mvs

{-# INLINE (=<||>=) #-}
(=<||>=) :: (Applicative m, HasChildren a) => a -> [m View] -> m a
(=<||>=) a mvs = flip setChildren a <$> sequenceA mvs

{-# INLINE (=<||>) #-}
(=<||>) :: (Applicative m, HasChildren a) => a -> [View] -> m a
(=<||>) a cs = pure (setChildren cs a)

infixl 8 =<|
{-# INLINE (=<|) #-}
(=<|) :: (ToView b, Functor f) => a -> (a -> f b) -> f View
(=<|) a g = fmap toView (g a)

{-# INLINE inject #-}
inject :: Typeable s => Element -> s -> PureM s IO View -> IO ()
inject e s p = Pure.inject e (runPureIO s p)

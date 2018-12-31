{-# LANGUAGE GeneralizedNewtypeDeriving, ImplicitParams, MagicHash, FlexibleInstances, MultiParamTypeClasses, RankNTypes, PatternSynonyms #-}
module Pure.State
  ( -- * Stateful View Monad
    PureM, runPure, runPureIO
  , runPureWith, runPureWithIO
  -- * State Reference Utilities
  , SRef(..), ref, getWith, putWith, modifyWith
  -- * Stateful View Construction Combinators
  , (<$|), (=<|), (|>=), (=<||>), (=<||>=), (|#>=), (=<||#>=), (=<||#>)
  -- * Stateful Application Initlizer
  , injectPure
  -- * Re-exports
  ) where

import Pure.Data.Default (Default(..))
import Pure.Data.Lifted (IsNode)
import Pure.Data.View (Comp(..),View(..),ToView(..))
import qualified Pure.Data.View as Pure
import Pure.Data.View.Patterns (pattern LibraryComponentIO,HasChildren(..),HasKeyedChildren(..))
import Pure.DOM

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.IO.Class
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.State.Class as State

import Data.IORef
import Data.Typeable

import GHC.Exts

-- These prevent GC if used outside of the View context. I should
-- use System.Mem.Weak and the stateful component's ThreadId.
data SRef s = SRef
  { reader :: IO s
  , writer :: s -> IO ()
  }

newtype PureM s m a = PureM { unPureM :: Reader.ReaderT (SRef s) m a }
  deriving (Functor,Applicative,Monad,Alternative,MonadPlus)

instance MonadIO m => MonadIO (PureM s m) where
  {-# INLINE liftIO #-}
  liftIO = PureM . liftIO

instance MonadTrans (PureM s) where
  {-# INLINE lift #-}
  lift = PureM . lift

instance MonadIO m => State.MonadState s (PureM s m) where
  {-# INLINE get #-}
  get = ref >>= getWith
  {-# INLINE put #-}
  put s = ref >>= flip putWith s

instance MonadFix m => MonadFix (PureM s m) where
  {-# INLINE mfix #-}
  mfix = PureM . mfix . (unPureM .)

{-# INLINE ref #-}
ref :: Monad m => PureM s m (SRef s)
ref = PureM Reader.ask

{-# INLINE getWith #-}
getWith :: MonadIO m => SRef s -> m s
getWith ref = liftIO (reader ref)

{-# INLINE putWith #-}
putWith :: MonadIO m => SRef s -> s -> m ()
putWith ref s = liftIO (writer ref s)

{-# INLINE modifyWith #-}
modifyWith :: MonadIO m => SRef s -> (s -> s) -> m ()
modifyWith ref f = liftIO (reader ref >>= \s -> writer ref (f s))

{-# INLINE runPureWith #-}
runPureWith :: (Typeable s, Typeable m, Monad m) => (forall a. m a -> IO a) -> s -> (SRef s -> PureM s m View) -> View
runPureWith f s st = runPure f s (ref >>= st)

{-# INLINE runPure #-}
runPure :: (Typeable s, Typeable m) => (forall a. m a -> IO a) -> s -> PureM s m View -> View
runPure f s st = flip LibraryComponentIO (f,s,st) $ \self ->
  let updateView = do
        (_,s_) <- Pure.get self
        let
          reader = readIORef s_
          writer s = writeIORef s_ s >> updateView
          stateRef = SRef reader writer
        Pure.modifyM_ self $ \(f,_,PureM st) _ -> do
          v <- f (Reader.runReaderT st stateRef)
          return ((v,s_),return ())
  in
    def
      { construct = do
          s_ <- newIORef s
          return (NullView Nothing,s_)
      , mounted = updateView
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
runPureWithIO :: (Typeable s) => s -> (SRef s -> PureM s IO View) -> View
runPureWithIO = runPureWith id

infixr 9 |>=
{-# INLINE (|>=) #-}
(|>=) :: (Applicative m, HasChildren a) => (a -> a) -> [m View] -> a -> m a
(|>=) f mvs a = flip setChildren (f a) <$> sequenceA mvs

infixr 9 |#>=
{-# INLINE (|#>=) #-}
(|#>=) :: (Applicative m, HasKeyedChildren a) => (a -> a) -> [m (Int,View)] -> a -> m a
(|#>=) f mvs a = flip setKeyedChildren (f a) <$> sequenceA mvs

{-# INLINE (=<||>=) #-}
(=<||>=) :: (Applicative m, ToView a, HasChildren a) => a -> [m View] -> m View
(=<||>=) a mvs = (toView . flip setChildren a) <$> sequenceA mvs

{-# INLINE (=<||#>=) #-}
(=<||#>=) :: (Applicative m, ToView a, HasKeyedChildren a) => a -> [m (Int,View)] -> m View
(=<||#>=) a mvs = (toView . flip setKeyedChildren a) <$> sequenceA mvs

{-# INLINE (=<||>) #-}
(=<||>) :: (Applicative m, ToView a, HasChildren a) => a -> [View] -> m View
(=<||>) a cs = pure (toView $ setChildren cs a)

{-# INLINE (=<||#>) #-}
(=<||#>) :: (Applicative m, ToView a, HasKeyedChildren a) => a -> [(Int,View)] -> m View
(=<||#>) a cs = pure (toView $ setKeyedChildren cs a)

infixl 8 =<|
{-# INLINE (=<|) #-}
(=<|) :: (ToView b, Applicative f) => a -> (a -> b) -> f View
(=<|) a g = pure (toView (g a))

infixl 8 <$|
{-# INLINE (<$|) #-}
(<$|) :: (ToView b, Functor f) => f a -> (a -> b) -> f View
(<$|) a f = fmap (toView . f) a

{-# INLINE injectPure #-}
injectPure :: (IsNode e, Typeable s) => e -> s -> PureM s IO View -> IO ()
injectPure e s p = inject e (runPureIO s p)

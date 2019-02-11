{-# LANGUAGE GeneralizedNewtypeDeriving, ImplicitParams, MagicHash, FlexibleInstances, MultiParamTypeClasses, RankNTypes, PatternSynonyms #-}
module Pure.State
  ( -- * Stateful View Monad
    PureM, PureRef, Reactive(..)
  , runPureWith, runPure, runPureDyn
  -- * Lift Pure computations
  , liftPure
  -- * Convenience utilities
  , withSRef
  -- * State Reference Utilities
  , SRef(..), getSRef, readSRef, writeSRef, modifySRef
  -- * Stateful View Construction Combinators
  , (<$|), (=<|), (|>=), (=<||>), (=<||>=), (|#>=), (=<||#>=), (=<||#>)
  ) where

import Pure.Data.Default (Default(..))
import Pure.Data.Lifted (IsNode)
import Pure.Data.View (Comp(..),View(..),ToView(..))
import qualified Pure.Data.View as Pure
import Pure.Data.View.Patterns (pattern Component,HasChildren(..),HasKeyedChildren(..))
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

data SRef s = SRef
  { sref   :: IORef s
  , reader :: IO s
  , writer :: s -> IO ()
  }

newtype PureM s a = PureM { unPureM :: Reader.ReaderT (SRef s) IO a }
  deriving (Functor,Applicative,Monad,Alternative,MonadPlus)

instance MonadIO (PureM s) where
  {-# INLINE liftIO #-}
  liftIO = PureM . liftIO

instance State.MonadState s (PureM s) where
  {-# INLINE get #-}
  get = getSRef >>= readSRef
  {-# INLINE put #-}
  -- Note: this causes a new state to be seen in the rest of the computation, 
  -- but does not force a new component update! To force a component update, 
  -- you must call:
  --
  -- > `writer :: SRef s -> s -> IO ()` 
  --
  -- which is also possible within any `PureM`. I believe this is the more 
  -- intuitive behavior. If we called `writer` for every `put`, it could loop!
  put s = getSRef >>= \sr -> liftIO ( writeIORef (sref sr) s )

instance MonadFix (PureM s) where
  {-# INLINE mfix #-}
  mfix = PureM . mfix . (unPureM .)

{-# INLINE getSRef #-}
getSRef :: PureM s (SRef s)
getSRef = PureM Reader.ask

{-# INLINE readSRef #-}
readSRef :: MonadIO m => SRef s -> m s
readSRef ref = liftIO (reader ref)

{-# INLINE writeSRef #-}
-- NOTE: Using `writeSRef` within a `PureM` could cause a loop. Use
-- `put` where possible.
writeSRef :: MonadIO m => SRef s -> s -> m ()
writeSRef ref s = liftIO (writer ref s)

{-# INLINE modifySRef #-}
-- NOTE: Using `modifySRef` within a `PureM` could cause a loop. Use
-- `modify` where possible.
modifySRef :: MonadIO m => SRef s -> (s -> s) -> m ()
modifySRef ref f = liftIO (reader ref >>= \s -> writer ref (f s))

{-# INLINE withSRef #-}
withSRef :: (SRef s -> IO a) -> PureM s a
withSRef f = getSRef >>= liftIO . f

data Reactive = Reactive
  { onState :: Bool
  , onPure  :: Bool
  }

instance Default Reactive where
  def = Reactive False False

type PureRef s = Pure.Ref (Reactive,s,PureM s View) (s,PureM s View,View,SRef s)

{-# INLINE runPureWith #-}
runPureWith :: (Typeable s) => Reactive -> s -> PureM s View -> View
runPureWith dyn s p = flip Component (dyn,s,p) $ \self ->
  let eval p stateRef = Reader.runReaderT (unPureM p) stateRef
      updateView = Pure.modifyM_ self $ \_ (s,p,_,stateRef) -> do
        view <- eval p stateRef
        return ((s,p,view,stateRef),return ())
  in
    def
      { construct = do
          (dyn,s,p) <- Pure.ask self
          s_ <- newIORef s
          let stateRef = SRef s_ (readIORef s_) (\s -> writeIORef s_ s >> updateView)
          view <- eval p stateRef
          return (s,p,view,stateRef)
      , receive = \(new_dyn,new_s,new_p) (s,p,view,stateRef) -> do
          (old_dyn,_,_) <- Pure.ask self

          let
            newstate    = not ( isTrue# ( reallyUnsafePtrEquality# new_s s ))
            newview     = not ( isTrue# ( reallyUnsafePtrEquality# new_p p ))

            injectstate = newstate && onState new_dyn
            injectpure  = newview && onPure new_dyn

            s' = if injectstate then new_s else s
            p' = if injectpure  then new_p else p

          when injectstate 
            ( writeIORef ( sref stateRef ) new_s )

          if injectstate || injectpure then do
            view <- eval p' stateRef
            return (s',p',view,stateRef)
          else
            return (s',p',view,stateRef)

      , render = \_ (_,_,v,_) -> v
      }

{-# INLINE liftPure #-}
liftPure :: MonadIO m => PureRef s -> PureM s a -> m a
liftPure ref pm = liftIO $ do
  (_,_,_,sref) <- Pure.get ref
  Reader.runReaderT (unPureM pm) sref

{-# INLINE runPure #-}
runPure :: Typeable s => s -> PureM s View -> View
runPure = runPureWith def

{-# INLINE runPureDyn #-}
runPureDyn :: Typeable s => s -> PureM s View -> View
runPureDyn = runPureWith (Reactive True True)

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
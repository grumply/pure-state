{-# LANGUAGE GeneralizedNewtypeDeriving, ImplicitParams, MagicHash, FlexibleInstances, MultiParamTypeClasses, RankNTypes, PatternSynonyms, FunctionalDependencies, UndecidableInstances #-}
module Pure.State
  ( -- * Stateful View Monad
    PureM(..), PureRef, Reactive(..)
  , MonadSRef(..)
  , runPureWith, runPure, runPureDyn
  -- * Lift Pure computations
  , liftPure, embedPure
  -- * State Reference Utilities
  , SRef(..), readSRef, writeSRef, modifySRef
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
import Control.Monad.Reader as Reader
import Control.Monad.State as State

import Data.IORef
import Data.Typeable

import GHC.Exts

-- | A reference to some mutable state `s` with an extensible write effect that
-- enables implicitly reactive views produced in monadic contexts.
--
-- The `SRef` serves a dual purpose: 
--
--  1. Code that uses the state to produce a view must be able to modify the 
--     state without causing re-evaluation of the view-producing code which
--     could cause possible unintended looping of the view-producing code.
--
--  2. Produced views must be able to inject changes back into the reference 
--     and force re-evaluation of the view-producing code.
--
data SRef s = SRef
  { ref   :: IORef s
  , write :: s -> IO ()
  }

{-# INLINE readSRef #-}
-- | Read the current state of an SRef in a MonadIO context.
readSRef :: MonadIO m => SRef s -> m s
readSRef r = liftIO (readIORef (ref r))

{-# INLINE writeSRef #-}
-- | Write the current state of an SRef in a MonadIO context and force 
-- re-evaluation of the view managing the SRef. 
--
-- Choose `writeSRef` for effect handlers within `View`s and `writeState`
-- within view-producing code, like `PureM`.
--
-- NOTE: Using `writeSRef` within view-producing code is likely to cause 
--       unintended looping.
--
writeSRef :: MonadIO m => SRef s -> s -> m ()
writeSRef ref s = liftIO (write ref s)

{-# INLINE modifySRef #-}
-- | Modify the current state of an SRef in a MonadIO context and force 
-- re-evaluation of the view managing the SRef. 
--
-- Choose `modifySRef` for effect handlers within `View`s and `modifyState`
-- within view-producing code, like `PureM`.
--
-- NOTE: Using `modifySRef` within view-producing code is likely to cause 
--       unintended looping.
modifySRef :: MonadIO m => SRef s -> (s -> s) -> m ()
modifySRef r f = liftIO (readIORef (ref r) >>= \s -> write r (f s))

newtype PureM s a = PureM { unPureM :: Reader.ReaderT (SRef s) IO a }
  deriving (Functor,Applicative,Monad,Alternative,MonadPlus)

instance MonadIO (PureM s) where
  {-# INLINE liftIO #-}
  liftIO = PureM . liftIO

instance MonadFix (PureM s) where
  {-# INLINE mfix #-}
  mfix = PureM . mfix . (unPureM .)

class MonadIO m => MonadSRef s m | m -> s where
  -- | Access an SRef in an `m` context.
  sref :: m (SRef s)

  -- | Get the current state of an SRef.
  getState :: m s
  getState = sref >>= readSRef

  -- | Set the state of an SRef in a context containing access to the SRef.
  -- This will generally be in view-producing code and the update will be 
  -- available to any code executed after this modification, but will not force
  -- any previously executed code to re-evaluate, unlike `writeSRef` which 
  -- queues an update for re-evaluation.
  setState :: s -> m ()
  setState s = sref >>= \sr -> liftIO $ writeIORef (ref sr) s

  -- | Modify the state of an SRef in a context containing access to the SRef.
  -- This will generally be in view-producing code and the update will be 
  -- available to any code executed after this modification, but will not force
  -- any previously executed code to re-evaluate, unlike `modifySRef` which 
  -- queues an update for re-evaluation.
  modifyState :: (s -> s) -> m ()
  modifyState f = sref >>= \sr -> liftIO $ modifyIORef (ref sr) f

  {-# MINIMAL sref #-}

instance MonadSRef s (PureM s) where
  sref = PureM Reader.ask

instance (MonadTrans t, MonadIO (t m), MonadSRef s m) => MonadSRef s (t m) where
  sref = lift sref

instance (MonadIO m, MonadSRef s m) => MonadState s m where
  get = getState
  put = setState

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
          let stateRef = SRef s_ (\s -> writeIORef s_ s >> updateView)
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
            ( writeIORef ( ref stateRef ) new_s )

          if injectstate || injectpure then do
            view <- eval p' stateRef
            return (s',p',view,stateRef)
          else
            return (s',p',view,stateRef)

      , render = \_ (_,_,v,_) -> v
      }

{-# INLINE liftPure #-}
liftPure :: MonadIO m => SRef s -> PureM s a -> m a
liftPure s pm = liftIO $ do
  let wrapped = do
        st0 <- State.get
        a <- pm
        st1 <- State.get
        unless ( isTrue# ( reallyUnsafePtrEquality# st0 st1 ) )
          ( writeSRef s st1 )
        return a
  Reader.runReaderT ( unPureM wrapped ) s

{-# INLINE embedPure #-}
embedPure :: (MonadSRef s m, MonadIO m, MonadIO n) => PureM s a -> m (n a)
embedPure m = do
  s <- sref
  pure ( liftPure s m )

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

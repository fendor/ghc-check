{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP, TemplateHaskell #-}
module GHC.Check.Util (MyVersion(..), liftTyped, gcatchSafe) where

import           Control.Monad ((>=>))
import           Control.Exception.Safe
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Data.Maybe
import           Data.Version
import           GHC (Ghc)
import           GHC.Exts                   (IsList (fromList), toList)
import qualified GHC.Paths
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax as TH
import           System.Environment (lookupEnv)

-- | A wrapper around 'Version' with TH lifting
newtype MyVersion = MyVersion Version
  deriving (Eq, IsList, Show)

instance Lift MyVersion where
#if MIN_VERSION_template_haskell(2,16,0)
    liftTyped = liftMyVersion
#endif
    lift = unTypeCode . liftMyVersion

liftMyVersion :: Quote m => MyVersion -> Code m MyVersion
liftMyVersion ver = TH.liftTyped ver

#if !MIN_VERSION_template_haskell(2,16,0)
liftTyped :: Lift a => a -> Code m a
liftTyped = unsafeTExpCoerce . lift
#endif

gcatchSafe :: forall e a . Exception e => Ghc a -> (e -> Ghc a) -> Ghc a
gcatchSafe act h = act `catch` rethrowAsyncExceptions
  where
      rethrowAsyncExceptions :: e -> Ghc a
      rethrowAsyncExceptions e
        | isAsyncException e = liftIO . throwIO $ e
        | otherwise = h e

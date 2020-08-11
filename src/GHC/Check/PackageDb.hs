{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE CPP #-}
-- | Discover the GHC version via the package database. Requirements:
--
--     * the package database must be compatible, which is usually not the case
--       across major ghc versions.
--
--     * the 'ghc' package is registered, which is not always the case.
module GHC.Check.PackageDb
  ( PackageVersion(abi), version,
    getPackageVersion,
  )
where

import Control.Monad.Trans.Class as Monad (MonadTrans (lift))
import Data.Maybe (fromMaybe)
import Data.String (IsString (fromString))
import Data.Version (Version)
import GHC
  (Ghc,
    getSessionDynFlags,
    runGhc,
    setSessionDynFlags,
  )
#if __GLASGOW_HASKELL__ >= 811
import GHC
  (unitState)
#else
import GHC
  (pkgState)
#endif
import GHC.Check.Util
import GHC.Exts (IsList (fromList), toList)

import Maybes (MaybeT (MaybeT), runMaybeT)

#if __GLASGOW_HASKELL__ >= 811
import qualified Data.Map as Map
import Packages
  (PackageName (PackageName), lookupUnit, explicitUnits, lookupPackageName,
    UnitInfo, GenericUnitInfo(..), GenUnitInfo, unitPackageNameString
  )
#else
import PackageConfig (PackageName (PackageName))
import Packages
  (lookupPackage, getPackageDetails, explicitPackages,  lookupInstalledPackage,
    lookupPackageName, packageNameString, InstalledPackageInfo (..), PackageConfig
  )
import Module (componentIdToInstalledUnitId)
#endif
import Language.Haskell.TH.Syntax (Lift)
import Language.Haskell.TH (TExpQ)
import Data.Foldable (find)
import Control.Applicative (Alternative((<|>)))

data PackageVersion
  = PackageVersion
      { myVersion :: !MyVersion,
        abi :: !String
      }
  deriving (Eq, Lift, Show)

version :: PackageVersion -> Version
version PackageVersion{ myVersion = MyVersion v} = v

-- | @getPackageVersion p@ returns the version of package @p@ that will be used in the Ghc session.
getPackageVersion :: String -> Ghc (Maybe PackageVersion)
#if __GLASGOW_HASKELL__ >= 811
getPackageVersion pName = runMaybeT $ do
  dflags <- Monad.lift getSessionDynFlags
  let pkgst   = unitState dflags
      depends = explicitUnits pkgst

  let explicit = do
        pkgs <- traverse (MaybeT . return . lookupUnit pkgst) depends
        MaybeT $ return $ find (\p -> unitPackageNameString p == pName ) pkgs

  p <- explicit

  return $ mkPackageVersion p
#else
getPackageVersion pName = runMaybeT $ do
  dflags <- Monad.lift getSessionDynFlags
  let pkgst   = pkgState dflags
      depends = explicitPackages pkgst

  let explicit = do
        pkgs <- traverse (MaybeT . return . lookupPackage dflags) depends
        MaybeT $ return $ find (\p -> packageNameString p == pName ) pkgs

      notExplicit = do
        component <- MaybeT $ return $ lookupPackageName dflags $ PackageName $ fromString pName
        MaybeT $ return $ lookupInstalledPackage dflags (componentIdToInstalledUnitId component)

  p <- explicit <|> notExplicit

  return $ mkPackageVersion p
#endif

#if __GLASGOW_HASKELL__ >= 811
mkPackageVersion :: UnitInfo -> PackageVersion
mkPackageVersion p = PackageVersion (MyVersion $ unitPackageVersion p) (unitAbiHash p)
#else
mkPackageVersion :: PackageConfig -> PackageVersion
mkPackageVersion p = PackageVersion (MyVersion $ packageVersion p) (abiHash p)
#endif
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_DinoGame (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/avask/Documents/LearningHaskell/DinoGame/.cabal-sandbox/bin"
libdir     = "/Users/avask/Documents/LearningHaskell/DinoGame/.cabal-sandbox/lib/x86_64-osx-ghc-8.2.2/DinoGame-0.1.0.0-23JftAEoI8mDWcCHjOP016-DinoGame"
dynlibdir  = "/Users/avask/Documents/LearningHaskell/DinoGame/.cabal-sandbox/lib/x86_64-osx-ghc-8.2.2"
datadir    = "/Users/avask/Documents/LearningHaskell/DinoGame/.cabal-sandbox/share/x86_64-osx-ghc-8.2.2/DinoGame-0.1.0.0"
libexecdir = "/Users/avask/Documents/LearningHaskell/DinoGame/.cabal-sandbox/libexec/x86_64-osx-ghc-8.2.2/DinoGame-0.1.0.0"
sysconfdir = "/Users/avask/Documents/LearningHaskell/DinoGame/.cabal-sandbox/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "DinoGame_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "DinoGame_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "DinoGame_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "DinoGame_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "DinoGame_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "DinoGame_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

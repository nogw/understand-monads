{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_monads (
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

bindir     = "/mnt/c/Users/nogueira/Desktop/haskell/monads/.cabal-sandbox/bin"
libdir     = "/mnt/c/Users/nogueira/Desktop/haskell/monads/.cabal-sandbox/lib/x86_64-linux-ghc-8.6.5/monads-0.1.0.0-HX9sIjgphS7K9BAeK1ZNsO"
dynlibdir  = "/mnt/c/Users/nogueira/Desktop/haskell/monads/.cabal-sandbox/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/mnt/c/Users/nogueira/Desktop/haskell/monads/.cabal-sandbox/share/x86_64-linux-ghc-8.6.5/monads-0.1.0.0"
libexecdir = "/mnt/c/Users/nogueira/Desktop/haskell/monads/.cabal-sandbox/libexec/x86_64-linux-ghc-8.6.5/monads-0.1.0.0"
sysconfdir = "/mnt/c/Users/nogueira/Desktop/haskell/monads/.cabal-sandbox/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "monads_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "monads_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "monads_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "monads_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "monads_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "monads_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

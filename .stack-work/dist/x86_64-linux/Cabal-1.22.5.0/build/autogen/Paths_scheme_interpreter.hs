module Paths_scheme_interpreter (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/lhahn/dev/haskell/scheme-interpreter/.stack-work/install/x86_64-linux/lts-4.2/7.10.3/bin"
libdir     = "/home/lhahn/dev/haskell/scheme-interpreter/.stack-work/install/x86_64-linux/lts-4.2/7.10.3/lib/x86_64-linux-ghc-7.10.3/scheme-interpreter-0.1.0.0-JKJnTqsSGsY8xQJBR5BvwX"
datadir    = "/home/lhahn/dev/haskell/scheme-interpreter/.stack-work/install/x86_64-linux/lts-4.2/7.10.3/share/x86_64-linux-ghc-7.10.3/scheme-interpreter-0.1.0.0"
libexecdir = "/home/lhahn/dev/haskell/scheme-interpreter/.stack-work/install/x86_64-linux/lts-4.2/7.10.3/libexec"
sysconfdir = "/home/lhahn/dev/haskell/scheme-interpreter/.stack-work/install/x86_64-linux/lts-4.2/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "scheme_interpreter_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "scheme_interpreter_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "scheme_interpreter_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "scheme_interpreter_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "scheme_interpreter_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)

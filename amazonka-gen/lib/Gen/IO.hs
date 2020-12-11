-- |
-- Module      : Gen.IO
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Gen.IO where

import qualified Control.Exception as Exception
import qualified Control.Monad.Except as Except
import qualified Control.Monad.State.Strict as State
import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text as Text.Lazy
import qualified Data.Text.Lazy.Builder as Text.Builder
import qualified Data.Text.Lazy.IO as Text.Lazy.IO
import Gen.Prelude
import Gen.Types
import qualified System.Directory as Directory
import System.FilePath ((</>))
import qualified System.FilePath as FilePath
import qualified System.IO as IO
import qualified Text.EDE as EDE

title :: MonadIO m => String -> m ()
title = liftIO . IO.hPutStrLn IO.stderr

say :: MonadIO m => String -> m ()
say = title . mappend " -> "

done :: MonadIO m => m ()
done = title ""

readBSFile :: MonadIO m => FilePath -> m ByteString
readBSFile path =
  liftIO $ do
    say ("Reading " ++ path)
    ByteString.readFile path

writeLTFile :: MonadIO m => FilePath -> LazyText -> m ()
writeLTFile path text =
  liftIO $ do
    say ("Writing " ++ path)

    IO.withFile path IO.WriteMode $ \handle -> do
      IO.hSetEncoding handle IO.utf8
      Text.Lazy.IO.hPutStr handle text

touchFile :: MonadIO m => FilePath -> LazyText -> m ()
touchFile path text =
  liftIO $ do
    exists <- Directory.doesFileExist path

    unless exists $
      writeLTFile path text

copyFileIfExists :: MonadIO m => FilePath -> FilePath -> m ()
copyFileIfExists source target =
  liftIO $ do
    exists <- Directory.doesFileExist source

    when exists $ do
      say ("Copying " ++ source ++ " to " ++ target)
      Directory.copyFile source target

createDir :: MonadIO m => FilePath -> m ()
createDir path =
  liftIO $ do
    exists <- Directory.doesDirectoryExist path

    unless exists $ do
      say ("Creating " ++ path)
      Directory.createDirectoryIfMissing True path

copyDir :: MonadIO m => FilePath -> FilePath -> m ()
copyDir source target =
  liftIO $ do
    contents <- Directory.listDirectory source

    flip mapM_ contents $ \name -> do
      let a = source </> name
          b = target </> name

      say ("Copying " ++ a ++ " to " ++ b)

      Directory.copyFile a b

readTemplate ::
  FilePath ->
  FilePath ->
  StateT (HashMap Text (EDE.Result EDE.Template)) IO EDE.Template
readTemplate dir file = do
  let name = Text.pack path
      path = dir </> file

  liftIO (readBSFile path)
    >>= EDE.parseWith EDE.defaultSyntax (load dir) name
    >>= EDE.result (Except.throwError . userError . show) pure
  where
    load parent config key _resolver = do
      let source
            | Text.null key = Text.unpack key
            | otherwise = parent </> Text.unpack key

      bytes <- liftIO (readBSFile source)
      EDE.parseWith config (load (FilePath.takeDirectory source)) key bytes

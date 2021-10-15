-- Module      : Gen.IO
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Gen.IO where

import Control.Monad.Except
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.String (fromString)
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.IO as LText
import System.FilePath ((</>))
import qualified System.FilePath as FilePath
import System.IO
import Text.EDE (Template)
import qualified Text.EDE as EDE
import qualified UnliftIO
import qualified UnliftIO.Directory as UnliftIO

title :: MonadIO m => String -> m ()
title = liftIO . putStrLn

say :: MonadIO m => String -> m ()
say = title . mappend " -> "

done :: MonadIO m => m ()
done = liftIO (putStrLn "")

readBSFile :: MonadIO m => FilePath -> m ByteString
readBSFile path =
  say ("Reading " ++ path)
    >> liftIO (ByteString.readFile path)

writeLTFile :: UnliftIO.MonadUnliftIO m => FilePath -> LText.Text -> m ()
writeLTFile path text = do
  say ("Writing " ++ path)
  UnliftIO.withFile path WriteMode $ \handle ->
    liftIO $ do
      hSetEncoding handle utf8
      LText.hPutStr handle text

touchFile :: UnliftIO.MonadUnliftIO m => FilePath -> LText.Text -> m ()
touchFile path text = do
  exists <- UnliftIO.doesFileExist path
  unless exists $
    writeLTFile path text

createDir :: MonadIO m => FilePath -> m ()
createDir dir = do
  exists <- UnliftIO.doesDirectoryExist dir
  unless exists $ do
    say ("Creating " ++ dir)
    UnliftIO.createDirectoryIfMissing True dir

copyDir :: MonadIO m => FilePath -> FilePath -> m ()
copyDir src dst =
  UnliftIO.listDirectory src >>= mapM_ copy
  where
    copy fsrc = do
      let fdst = dst </> FilePath.takeFileName fsrc

      say $
        "Copying "
          ++ fsrc
          ++ " to "
          ++ FilePath.takeDirectory fdst

      UnliftIO.copyFile fsrc fdst

readTemplate ::
  MonadIO m =>
  FilePath ->
  FilePath ->
  m Template
readTemplate dir name =
  liftIO $
    readBSFile (dir </> name)
      >>= EDE.parseWith EDE.defaultSyntax (EDE.includeFile dir) (fromString name)
      >>= EDE.result (UnliftIO.throwString . show) pure

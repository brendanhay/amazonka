{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

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
import qualified Data.ByteString as ByteString
import Control.Error
import qualified Control.Monad.Except as Except
import Control.Monad.State
import qualified Data.Bifunctor as Bifunctor
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Text.Lazy.IO as LText
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import System.FilePath ((</>))
import Gen.Types
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
  exists <- Directory.doesFileExist path

  unless exists $
    error ("Missing " ++ path)
  
  say ("Reading " ++ path)
  ByteString.readFile path

writeLTFile :: MonadIO m => FilePath -> LText.Text -> m ()
writeLTFile path text =
 liftIO $ do
  say ("Writing " ++ path)
  
  IO.withFile path IO.WriteMode $ \handle -> do
    IO.hSetEncoding handle IO.utf8
    LText.hPutStr handle text

touchFile :: MonadIO m => FilePath -> LText.Text -> m ()
touchFile path text =
 liftIO $ do
  exists <- Directory.doesFileExist path
  
  unless exists $
    writeLTFile path text

createDir :: MonadIO m => FilePath -> m ()
createDir path =
 liftIO $ do
  exists <- Directory.doesDirectoryExist path
  
  unless exists $ do
    say ("Creating " ++ path)
    Directory.createDirectoryIfMissing True path

copyDir :: MonadIO m => FilePath -> FilePath -> m ()
copyDir source target =
  liftIO (Directory.listDirectory source >>= mapM_ copy)
  where
    copy file = do
      let path = target </> FilePath.takeFileName file
      say ("Copying " ++ path ++ " to " ++ target)
      Directory.copyFile file path

readTemplate ::
  FilePath ->
  FilePath ->
  StateT (Map Text (EDE.Result EDE.Template)) IO EDE.Template
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

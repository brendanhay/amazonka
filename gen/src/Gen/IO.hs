{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Gen.IO
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.IO where

import           Control.Applicative
import           Control.Error
import           Control.Monad.IO.Class
import qualified Data.Aeson               as A
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy     as LBS
import           Data.Jason               (eitherDecode')
import           Data.Jason.Types
import           Data.Monoid
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Text.IO             as Text
import qualified Data.Text.Lazy.IO        as LText
import           Gen.Filters
import           Gen.Types
import           System.Directory
import           System.FilePath
import           Text.EDE                 (Template)
import qualified Text.EDE                 as EDE

dots :: FilePath -> Bool
dots "."  = False
dots ".." = False
dots _    = True

createDir :: MonadIO m => FilePath -> EitherT String m ()
createDir = scriptIO . createDirectoryIfMissing True

copyContents :: FilePath -> FilePath -> Script ()
copyContents s d = do
    fs <- map (combine s) . filter dots <$> scriptIO (getDirectoryContents s)
    scriptIO (mapM_ copy fs)
  where
    copy f@(dest -> p) = say "Copy" p >> copyFile f p

    dest f = d </> takeFileName f

renderFile :: ToFilePath a
           => Text
           -> Template
           -> FilePath
           -> a
           -> A.Object
           -> Script ()
renderFile lbl t d p env = do
    createDir (dropFileName f)
    say lbl f
    txt <- hoistEither (EDE.eitherRenderWith genFilters t env)
    scriptIO (LText.writeFile f txt)
  where
    f = d </> toFilePath p

readFromFile :: (LBS.ByteString -> Either String a)
             -> FilePath
             -> Script (Either String a)
readFromFile f p = do
    b <- scriptIO (doesFileExist p)
    if not b
        then return . Left $ "Failed to find " ++ p
        else f <$> scriptIO (LBS.readFile p)

writeJSON :: A.ToJSON a => FilePath -> a -> Script ()
writeJSON p x = do
    say "Write JSON" p
    scriptIO (LBS.writeFile p (encodePretty x))

requireObject :: FromJSON a => FilePath -> Script a
requireObject f = loadObject f >>= hoistEither

optionalObject :: Text -> FilePath -> Script Object
optionalObject k = fmap (fromMaybe (mkObject [(k, Object mempty)]) . hush)
    . loadObject

loadObject :: FromJSON a => FilePath -> Script (Either String a)
loadObject = readFromFile eitherDecode'

say :: MonadIO m => Text -> String -> m ()
say x msg = liftIO . Text.putStrLn $ "[ " <> y <> "] " <> Text.pack msg
  where
    y | n > 0     = x <> Text.replicate n " "
      | otherwise = x

    n = 17 - Text.length x

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

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
import           Control.Monad
import           Control.Monad.Trans.Except
import Control.Monad.Error
import qualified Data.ByteString.Lazy      as LBS
import           Data.List                 (intercalate)
import           Data.Maybe
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.IO              as Text
import qualified Data.Text.Lazy            as LText
import qualified Data.Text.Lazy.IO         as LText
import qualified Filesystem                as FS
import           Filesystem.Path.CurrentOS hiding (encode)
import           Gen.Types
import           Prelude                   hiding (FilePath)
import           System.Directory.Tree

writeTree :: Text -> AnchoredDirTree LText.Text -> Script (AnchoredDirTree ())
writeTree k t = do
    d <- scriptIO (writeDirectoryWith write t)
    verify d
    return d
  where
    write p x = say k (Text.pack p) >> LText.writeFile p x

    verify (_ :/ d)
        | [] <- xs  = return ()
        | otherwise = throwError (intercalate "\n" xs)
      where
        xs = mapMaybe f (failures d)

        f (Failed _ e) = Just (show e)
        f _            = Nothing

fileContents :: FilePath -> Script LBS.ByteString
fileContents p = do
    say "Read File" (encode p)
    b <- scriptIO (FS.isFile p)
    if b
        then LBS.fromStrict <$> scriptIO (FS.readFile p)
        else throwError $ "Unable to find " ++ show p

copyDirectory :: FilePath -> FilePath -> Script ()
copyDirectory src dst = scriptIO (FS.listDirectory src >>= mapM_ copy)
  where
    copy f = say "Copy File" (encode p) >> FS.copyFile f p
      where
        p = dst </> filename f

say :: MonadIO m => Text -> Text -> m ()
say x msg = liftIO . Text.putStrLn $ "[ " <> y <> "] " <> msg
  where
    y | n > 0     = x <> Text.replicate n " "
      | otherwise = x

    n = 17 - Text.length x

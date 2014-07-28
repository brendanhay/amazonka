{-# LANGUAGE OverloadedStrings #-}

-- Module      : Generator.Models
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Generator.Models
    ( Model (..)
    , models
    ) where

import Control.Applicative
import Control.Error
import Control.Monad.IO.Class
import Data.List
import Data.Ord
import Generator.Log
import System.Directory
import System.FilePath

data Model = Model
    { modPath    :: FilePath
    , modVersion :: String
    , modGlobal  :: Maybe FilePath
    , modLocal   :: Maybe FilePath
    } deriving (Show, Eq)

instance Ord Model where
    compare = comparing modVersion

models :: Int -> FilePath -> [FilePath] -> Script [Model]
models n o xs = concat . fmap (take n . reverse . sort) <$> mapM model xs
  where
    model d = (json <$> scriptIO (getDirectoryContents d))
        >>= mapM (fromPath o d)

    json = filter (isSuffixOf ".json")

fromPath :: FilePath -> FilePath -> String -> Script Model
fromPath o d f = do
    (p, g, l) <- scriptIO $ (,,)
        <$> doesFileExist file
        <*> doesFileExist global
        <*> doesFileExist local

    bool (left ("Unable to locate model: " ++ file))
         (do say "Located Model" file
             override g global
             override l local
             right $ Model file (dropExtension f) (may g global) (may l local))
         p
  where
    file   = d </> f
    global = o </> takeBaseName d <.> ".override.json"
    local  = o </> takeBaseName d </> replaceExtension f ".override.json"

    may = bool (const Nothing) Just

    override :: MonadIO m => Bool -> (String -> m ())
    override = bool (say "Skipping Override") (say "Located Override")




{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

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
import Data.List
import Data.Ord
import System.Directory
import System.FilePath

data Model = Model
    { modPath     :: FilePath
    , modVersion  :: String
    , modOverride :: FilePath
    } deriving (Show, Eq)

instance Ord Model where
    compare = comparing modVersion

models :: FilePath -> [FilePath] -> Script [Model]
models o xs = concat . fmap (take 1 . sortBy (flip compare)) <$> mapM model xs
  where
    model d = (json <$> scriptIO (getDirectoryContents d))
        >>= mapM (fromPath o d)

    json = filter (isSuffixOf ".json")

fromPath :: FilePath -> FilePath -> String -> Script Model
fromPath o d f = do
    (p, g) <- scriptIO $ (,)
        <$> check file
        <*> check override

    Model file
        <$> (dropExtension <$> must p)
        <*> must g
  where
    file     = d </> f
    override = o </> takeBaseName d <.> ".override.json"

    check p = (p,) <$> doesFileExist p

    must :: (FilePath, Bool) -> Script FilePath
    must (p, e) =
        bool (left $ "Unable to locate: " ++ p)
             (right p)
             e

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

module Generator.Models where

import Control.Applicative
import Control.Error
import Control.Monad
import Data.List
import Data.Ord
import Generator.Log
import System.Directory
import System.FilePath

data Model = Model
    { modPath    :: FilePath
    , modVersion :: String
    } deriving (Show, Eq)

instance Ord Model where
    compare = comparing modVersion

modelFromPath :: FilePath -> String -> Model
modelFromPath d f = Model (d </> f) (fst $ break (== '.') f)

models :: Int -> [FilePath] -> Script [Model]
models n xs = concat . fmap (take n . reverse . sort) <$> mapM model xs
  where
    model d = scriptIO $ do
        xs <- json <$> getDirectoryContents d
        forM xs $ \f ->
            say "Locate Model" (d </> f)
                >> return (modelFromPath d f)

    json = filter (isSuffixOf ".json")

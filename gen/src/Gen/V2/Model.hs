{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Gen.V2.Model
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.V2.Model
    ( Model (..)
    , loadModel
    ) where

import           Control.Applicative  ((<$>))
import           Control.Error
import           Control.Monad
import           Data.Aeson           hiding (object)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict  as Map
import           Data.List
import           Data.Monoid
import           Data.Ord
import           System.Directory
import           System.FilePath

data Model = Model
    { _mVersion :: String
    , _mModel   :: Object
    } deriving (Show, Eq)

instance Ord Model where
    compare = comparing _mVersion

loadModel :: FilePath -> FilePath -> Script Model
loadModel o d = do
    v  <- version
    Model v . Map.unions <$> sequence
        [ required override
        , required (api     v)
        , optional (waiters v)
        , optional (pagers  v)
        ]
  where
    version = do
        fs <- scriptIO (getDirectoryContents d)
        f  <- tryHead ("Failed to get model version from " ++ d) fs
        return (takeWhile (/= '.') f)

    required f = object f !? ("Failed to load " ++ f)
    optional   = scriptIO . fmap (fromMaybe mempty) . object

    object f = do
        p <- doesFileExist f
        bool (return Nothing)
             (decode' <$> LBS.readFile f)
             p

    override = o
        </> takeBaseName (dropTrailingPathSeparator d)
        <.> "overrides.json"

    api     = path "api.json"
    waiters = path "waiters.json"
    pagers  = path "paginators.json"

    path e v = d </> v <.> e

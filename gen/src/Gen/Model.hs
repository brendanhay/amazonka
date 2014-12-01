{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE BangPatterns            #-}
{-# LANGUAGE RecordWildCards            #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- Module      : Gen.Model
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.Model where

import           Control.Applicative
import           Control.Error
import           Control.Monad
import qualified Data.HashMap.Strict as Map
import qualified Data.Text           as Text
import           Gen.IO
import           Gen.JSON
import           Gen.Types
import           System.Directory
import           System.FilePath

loadRetries :: FilePath -> Script Retries
loadRetries = requireObject >=> parse

loadModel :: FilePath -> FilePath -> Retries -> Script Model
loadModel d o Retries{..} = do
    v  <- version
    m1 <- requireObject override
    m2 <- merge <$> sequence
        [ return m1
        , requireObject (api v)
        , optionalObject "waiters"    (waiters v)
        , optionalObject "pagination" (pagers  v)
        ]

    let !r = retry

    scriptIO (print r)

    Model name v d m2 r <$> parse m1
  where
    version = do
        fs <- scriptIO (getDirectoryContents d)
        f  <- tryHead ("Failed to get model version from " ++ d) (filter dots fs)
        return (takeWhile (/= '.') f)

    retry   = Map.lookupDefault _rDefaultRetry (Text.pack name) _rRetry

    api     = path "api.json"
    waiters = path "waiters.json"
    pagers  = path "paginators.json"

    path e v = d </> v <.> e

    override = o </> name <.> "json"

    name = takeBaseName (dropTrailingPathSeparator d)

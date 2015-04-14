{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- Module      : Gen.Model
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
import           Data.List           (sort)
import           Gen.IO
import           Gen.JSON
import           Gen.Types
import           System.Directory
import           System.FilePath

loadRetries :: FilePath -> Script Retries
loadRetries = requireObject >=> parse

loadModel :: FilePath -> FilePath -> Script Model
loadModel d o = do
    v  <- version
    m1 <- requireObject override
    m2 <- merge <$> sequence
        [ return m1
        , requireObject (normal v)
        , optionalObject "waiters"    (waiters v)
        , optionalObject "pagination" (pagers  v)
        ]
    Model name v d m2 <$> parse m1
  where
    version = do
        fs <- reverse . sort . filter dots <$> scriptIO (getDirectoryContents d)
        f  <- tryHead ("Failed to get model version from " ++ d) fs
        return (takeWhile (/= '.') f)

    normal  = path "normal.json"
    waiters = path "waiters.json"
    pagers  = path "paginators.json"

    path e v = d </> v <.> e

    override = o </> name <.> "json"

    name = takeBaseName (dropTrailingPathSeparator d)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Main
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Main (main) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.List
import Data.Monoid
import Data.Traversable    (for)
import Options.Applicative
import System.Directory
import System.Environment
import System.Exit
import System.FilePath

data Options = Options
    { optDir    :: FilePath
    , optModels :: [FilePath]
    } deriving (Show)

options :: Parser Options
options = Options
    <$> strOption
         ( long "dir"
        <> metavar "DIR"
        <> help "Directory to place the generated library. [required]"
         )
    <*> some (strOption
         ( long "model"
        <> metavar "DIR"
        <> help "Path to a botocore JSON model directory. [required]"
         ))

data Service = Service
    { svcPath :: FilePath
    , svcVers :: String
    } deriving (Show)

services :: [FilePath] -> IO [Service]
services = fmap concat . mapM svc
  where
    svc d = do
        p  <- doesDirectoryExist d
        unless p $ do
            putStrLn ("Directory: " ++ d ++ " does not exist.")
            exitFailure
        fs <- getDirectoryContents d
        return $ map (mk d) (json fs)

    json = filter (isSuffixOf ".json")

    mk d f = Service (d </> f) (fst $ break (== '.') f)

main :: IO ()
main = do
    Options{..} <- customExecParser pPrefs pInfo

    createDirectoryIfMissing True optDir

    ss <- services optModels

    mapM_ print ss

  where
    pPrefs = prefs showHelpOnError
    pInfo  = info (helper <*> options) fullDesc


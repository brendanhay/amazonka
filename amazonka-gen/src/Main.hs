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
import Control.Error
import Data.Monoid
import Generator.AST
import Generator.FromJSON
import Generator.Log
import Generator.Models
import Generator.Templates
import Generator.ToJSON
import Options.Applicative
import System.Directory

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

main :: IO ()
main = do
    Options{..} <- customExecParser pPrefs pInfo

    createDirectoryIfMissing True optDir

    runScript $ do
        ts <- getTemplates
        ms <- models optModels
        ss <- mapM parseModel ms

        mapM_ (scriptIO . print . svcName) ss

--         Stage3.render optDir (map Stage2.transform ss) ts

  where
    pPrefs = prefs showHelpOnError
    pInfo  = info (helper <*> options) fullDesc

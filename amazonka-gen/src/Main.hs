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

import           Control.Applicative
import           Control.Error
import           Control.Monad
import           Data.List
import           Data.Monoid
import qualified Data.Text.Lazy.IO   as LText
import           Generator.AST
import           Generator.FromJSON
import           Generator.Models
import           Generator.Templates
import           Generator.ToJSON
import           Options.Applicative
import           System.Directory
import           System.Exit
import           System.FilePath

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
        ms <- models optModels
        ts <- templates

        return ()

--         ss <- mapM (Stage1.parse) ms

--         Stage3.render optDir (map Stage2.transform ss) ts

    return ()

  where
    pPrefs = prefs showHelpOnError
    pInfo  = info (helper <*> options) fullDesc

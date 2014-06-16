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
import qualified Data.Text.Lazy.IO            as LText
import qualified Network.AWS.Generator.Stage1 as Stage1
import qualified Network.AWS.Generator.Stage2 as Stage2
import qualified Network.AWS.Generator.Stage3 as Stage3
import           Network.AWS.Generator.Types
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

models :: [FilePath] -> IO [Model]
models = fmap concat . mapM model
  where
    model d = do
        p  <- doesDirectoryExist d
        unless p $ do
            putStrLn ("Directory: " ++ d ++ " does not exist.")
            exitFailure
        fs <- getDirectoryContents d
        return $ map (modelFromPath d) (json fs)

    json = filter (isSuffixOf ".json")

main :: IO ()
main = do
    Options{..} <- customExecParser pPrefs pInfo

    createDirectoryIfMissing True optDir

    ms <- models optModels

    runScript $ do
        ts <- Stage3.templates
        ss <- mapM (Stage1.parse) ms

        Stage3.render optDir (map Stage2.transform ss) ts

  where
    pPrefs = prefs showHelpOnError
    pInfo  = info (helper <*> options) fullDesc

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
-- import Gen.FromJSON
import Gen.V2.Model
-- import Gen.Render
import Gen.V2.Decode
import Options.Applicative
import System.Directory

main = return ()

data Options = Options
    { optDir      :: FilePath
    , optOverride :: FilePath
    , optModels   :: [FilePath]
    , optAssets   :: FilePath
    } deriving (Show)

options :: Parser Options
options = Options
    <$> strOption
         ( long "output"
        <> metavar "DIR"
        <> help "Directory to place the generated library. [required]"
         )

    <*> strOption
         ( long "override"
        <> metavar "DIR"
        <> help "Directory containing model overrides. [required]"
         )

    <*> some (strOption
         ( long "model"
        <> metavar "PATH"
        <> help "Directory containing a service's models. [required]"
         ))

    <*> strOption
         ( long "assets"
        <> metavar "PATH"
        <> help "Directory containing service assets. [required]"
         )

main :: IO ()
main = do
    Options{..} <- customExecParser
        (prefs showHelpOnError)
        (info (helper <*> options) fullDesc)

    createDirectoryIfMissing True optDir

    runScript $ do
        ms <- models optOverride optModels
        ss <- mapM parseModel ms

        render optDir optAssets (transform ss)

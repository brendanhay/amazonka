{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

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
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Monoid
import Gen.V2.Asset
import Gen.V2.Model
import Gen.V2.Stage1
import Gen.V2.Stage2
import Gen.V2.Template
import Options.Applicative
import System.Directory

data Options = Options
    { _out       :: FilePath
    , _models    :: [FilePath]
    , _overrides :: FilePath
    , _templates :: FilePath
    , _assets    :: FilePath
    } deriving (Show)

makeLenses ''Options

options :: ParserInfo Options
options = info (helper <*> parser) fullDesc

parser :: Parser Options
parser = Options
    <$> strOption
         ( long "out"
        <> metavar "DIR"
        <> help "Directory to place the generated library. [required]"
         )

    <*> some (strOption
         ( long "model"
        <> metavar "PATH"
        <> help "Directory containing a service's models. [required]"
         ))

    <*> strOption
         ( long "overrides"
        <> metavar "DIR"
        <> help "Directory containing model overrides. [required]"
         )

    <*> strOption
         ( long "templates"
        <> metavar "DIR"
        <> help "Directory containing ED-E templates. [required]"
         )

    <*> strOption
         ( long "assets"
        <> metavar "PATH"
        <> help "Directory containing service assets. [required]"
         )

validate :: MonadIO m => Options -> m Options
validate o = flip execStateT o $ do
    sequence_
        [ check out
        , check overrides
        , check templates
        , check assets
        ]
    mapM canon (o ^. models)
        >>= assign models

check :: (MonadIO m, MonadState s m) => Lens' s FilePath -> m ()
check l = gets (view l) >>= canon >>= assign l

canon :: MonadIO m => FilePath -> m FilePath
canon = liftIO . canonicalizePath

main :: IO ()
main = do
    Options{..} <- customExecParser (prefs showHelpOnError) options
        >>= validate

    mapM_ (createDirectoryIfMissing True)
        [_out, _overrides, _templates, _assets]

    runScript $ do
        !ts <- loadTemplates _templates

        -- Load a single model, then process it
        forM_ _models $ \d -> do
            !m  <- loadModel _overrides d

            -- Decode the JSON to Stage1 AST
            !s1 <- stage1 m

            -- Transformation from Stage1 -> Stage2 AST

            -- Decode the overrides from the JSON

            -- Apply the overrides to Stage2 AST

            -- Render the templates

            return ()

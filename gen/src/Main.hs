{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
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
import Control.Lens           (Lens', (^.), view, assign, makeLenses)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Monoid
import Gen.AST
import Gen.IO
import Gen.JSON
import Gen.Library
import Gen.Model
import Gen.Templates
import Gen.Types
import Options.Applicative
import System.Directory
import System.FilePath
import System.IO

data Options = Options
    { _output    :: FilePath
    , _models    :: [FilePath]
    , _services  :: FilePath
    , _overrides :: FilePath
    , _templates :: FilePath
    , _assets    :: FilePath
    , _retry     :: FilePath
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
        <> help "Directory for a service's botocore models. [required]"
         ))

    <*> strOption
         ( long "services"
        <> metavar "PATH"
        <> help "Directory for amazonka models. [required]"
         )

    <*> strOption
         ( long "overrides"
        <> metavar "DIR"
        <> help "Directory containing amazonka overrides. [required]"
         )

    <*> strOption
         ( long "templates"
        <> metavar "DIR"
        <> help "Directory containing ED-E templates. [required]"
         )

    <*> strOption
         ( long "assets"
        <> metavar "PATH"
        <> help "Directory containing assets for generated libraries. [required]"
         )

    <*> strOption
         ( long "retry"
        <> metavar "PATH"
        <> help "Path to the file containing retry definitions. [required]"
         )

validate :: MonadIO m => Options -> m Options
validate o = flip execStateT o $ do
    sequence_
        [ check output
        , check services
        , check overrides
        , check templates
        , check assets
        , check retry
        ]
    mapM canon (o ^. models)
        >>= assign models

check :: (MonadIO m, MonadState s m) => Lens' s FilePath -> m ()
check l = gets (view l) >>= canon >>= assign l

canon :: MonadIO m => FilePath -> m FilePath
canon = liftIO . canonicalizePath

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    o <- customExecParser (prefs showHelpOnError) options >>= validate

    runScript $ do
        !ts <- loadTemplates (o ^. templates)
        !rs <- loadRetries   (o ^. retry)

        -- Process a Input AST from the corresponding botocore model.
        forM_ (o ^. models) $ \d -> do
            -- Load the Input raw JSON.
            !m   <- loadModel d (o ^. overrides) rs

            -- Decode the Input JSON to AST.
            !inp <- parse (_mModel m)

            -- Transformation from Input -> Output AST.
            let !out = transformAST m inp

            -- Store the intemediary Output AST as JSON.
            -- Note: This is primarily done for debugging purposes.
            writeJSON (o ^. services </> _mName m <.> "json") out

            -- Render the templates, creating or overriding the target library.
            lib <- renderLibrary (o ^. output) ts out

            -- Copy static assets to the library root.
            copyContents (o ^. assets) lib

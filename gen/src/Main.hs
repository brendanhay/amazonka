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

import           Control.Applicative
import           Control.Error
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Monoid
import           Gen.IO
import qualified Gen.Stage1             as S1
import qualified Gen.Stage2             as S2
import           Gen.Transform
import           Options.Applicative
import           System.Directory
import           System.IO

data Options = Options
    { _out       :: FilePath
    , _models    :: [FilePath]
    , _services  :: FilePath
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

validate :: MonadIO m => Options -> m Options
validate o = flip execStateT o $ do
    sequence_
        [ check out
        , check services
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
    hSetBuffering stdout LineBuffering

    o <- customExecParser (prefs showHelpOnError) options
        >>= validate

    runScript $ do
        !ts <- loadTemplates (o ^. templates)

        -- Process a Stage1 AST from the corresponding botocore model.
        forM_ (o ^. models) $ \d -> do
            -- Load the Stage1 raw JSON.
            !m  <- S1.model d (o ^. overrides)

            -- Decode the Stage1 JSON to AST.
            !s1 <- S1.decode m

            -- Transformation from Stage1 -> Stage2 AST.
            let !s2 = transformS1ToS2 m s1

            -- Store the intemediary Stage2 AST as JSON.
            -- Note: This is primarily done for debugging purposes.
            S2.store (o ^. services) m s2

            -- Render the templates, creating or overriding the target library.
            lib <- S2.render (o ^. out) ts s2

            -- Copy static assets to the library root.
            copyAssets (o ^. assets) lib

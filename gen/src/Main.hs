{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}

{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- Module      : Main
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Main (main) where

import Control.Error
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.State

import Data.Monoid
import Data.String

import Filesystem.Path.CurrentOS

import           Gen.Formatting
import           Gen.IO
import qualified Gen.JSON       as JS
import           Gen.Types      hiding (info)

import Options.Applicative

import qualified Data.Text  as Text
import qualified Filesystem as FS
import qualified Gen.AST    as AST
import qualified Gen.Tree   as Tree

data Opt = Opt
    { _optOutput    :: Path
    , _optModels    :: [Path]
    , _optAnnexes   :: Path
    , _optConfigs   :: Path
    , _optTemplates :: Path
    , _optStatic    :: Path
    , _optRetry     :: Path
    , _optVersions  :: Versions
    } deriving (Show)

makeLenses ''Opt

parser :: Parser Opt
parser = Opt
    <$> option isPath
         ( long "out"
        <> metavar "DIR"
        <> help "Directory to place the generated library."
         )

    <*> some (option isPath
         ( long "model"
        <> metavar "DIR"
        <> help "Directory for a service's botocore models."
         ))

    <*> option isPath
         ( long "annexes"
        <> metavar "DIR"
        <> help "Directory containing botocore model annexes."
         )

    <*> option isPath
         ( long "configs"
        <> metavar "DIR"
        <> help "Directory containing service configuration."
         )

    <*> option isPath
         ( long "templates"
        <> metavar "DIR"
        <> help "Directory containing ED-E templates."
         )

    <*> option isPath
         ( long "static"
        <> metavar "DIR"
        <> help "Directory containing static files for generated libraries."
         )

    <*> option isPath
         ( long "retry"
        <> metavar "PATH"
        <> help "Path to the file containing retry definitions."
         )

    <*> (Versions
        <$> option version
             ( long "library-version"
            <> metavar "VER"
            <> help "Version of the library to generate."
             )

        <*> option version
             ( long "client-version"
            <> metavar "VER"
            <> help "Client library version dependecy for examples."
             )

        <*> option version
             ( long "core-version"
            <> metavar "VER"
            <> help "Core library version dependency."
             ))

isPath :: ReadM Path
isPath = eitherReader (Right . fromText . Text.dropWhileEnd (== '/') . fromString)

version :: ReadM (Version v)
version = eitherReader (Right . Version . Text.pack)

options :: ParserInfo Opt
options = info (helper <*> parser) fullDesc

validate :: MonadIO m => Opt -> m Opt
validate o = flip execStateT o $ do
    sequence_
        [ check optOutput
        , check optAnnexes
        , check optConfigs
        , check optTemplates
        , check optStatic
        , check optRetry
        ]
    mapM canon (o ^. optModels) >>= assign optModels
  where
    check :: (MonadIO m, MonadState s m) => Lens' s Path -> m ()
    check l = gets (view l) >>= canon >>= assign l

    canon :: MonadIO m => Path -> m Path
    canon = liftIO . FS.canonicalizePath

main :: IO ()
main = do
    Opt{..} <- customExecParser (prefs showHelpOnError) options
        >>= validate

    let i = length _optModels

    run $ do
        title "Initialising..." <* done

        let load = readTemplate _optTemplates

        tmpl <- flip evalStateT mempty $ do
            lift (title ("Loading templates from " % path) _optTemplates)

            cabalTemplate          <- load "cabal.ede"
            tocTemplate            <- load "toc.ede"
            waitersTemplate        <- load "waiters.ede"
            readmeTemplate         <- load "readme.ede"
            operationTemplate      <- load "operation.ede"
            typesTemplate          <- load "types.ede"
            sumTemplate            <- load "types/sum.ede"
            productTemplate        <- load "types/product.ede"
            testMainTemplate       <- load "test/main.ede"
            testNamespaceTemplate  <- load "test/namespace.ede"
            testInternalTemplate   <- load "test/internal.ede"
            fixturesTemplate       <- load "test/fixtures.ede"
            fixtureRequestTemplate <- load "test/fixtures/request.ede"
            blankTemplate          <- load "blank.ede"

            lift done

            pure Templates{..}

        r  <- JS.required _optRetry

        forM_ (zip [1..] _optModels) $ \(j, f) -> do
            title ("[" % int % "/" % int % "] model:" % path)
                  (j :: Int)
                  (i :: Int)
                  (filename f)

            m <- listDir f >>= hoistEither . loadModel f

            say ("Using version " % dateDash) (m ^. modelVersion)

            cfg <- JS.required (_optConfigs </> (m ^. configFile))
                >>= hoistEither . JS.parse

            api <- sequence
                [ JS.optional (_optAnnexes </> (m ^. annexFile))
                , JS.required (m ^. serviceFile)
                , JS.optional (m ^. waitersFile)
                , JS.optional (m ^. pagersFile)
                , pure r
                ] >>= hoistEither . JS.parse . JS.merge

            say ("Successfully parsed '" % stext % "' API definition")
                (api ^. serviceFullName)

            lib <- hoistEither (AST.rewrite _optVersions cfg api)

            dir <- hoistEither (Tree.populate _optOutput tmpl lib)
                >>= Tree.fold createDir (\x -> either (touchFile x) (writeLTFile x))

            say ("Successfully rendered " % stext % "-" % semver % " package")
                (lib ^. libraryName)
                (lib ^. libraryVersion)

            copyDir _optStatic (Tree.root dir)

            done

        title ("Successfully processed " % int % " models.") i

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- Module      : Main
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Main (main) where

import Control.Lens
import Control.Monad
import Control.Monad.State
import qualified Data.Text as Text
import qualified Gen.AST as AST
import Gen.IO
import qualified Gen.JSON as JS
import qualified Gen.Tree as Tree
import Gen.Types hiding (info)
import Options.Applicative
import qualified System.Directory as Directory
import System.FilePath ((</>))
import qualified System.FilePath as FilePath

data Opt = Opt
  { _optOutput :: FilePath,
    _optModels :: [FilePath],
    _optAnnexes :: FilePath,
    _optConfigs :: FilePath,
    _optTemplates :: FilePath,
    _optStatic :: FilePath,
    _optRetry :: FilePath,
    _optVersions :: Versions
  }
  deriving (Show)

makeLenses ''Opt

parser :: Parser Opt
parser =
  Opt
    <$> strOption
      ( long "out"
          <> metavar "DIR"
          <> help "Directory to place the generated library."
      )
    <*> some
      ( strOption
          ( long "model"
              <> metavar "DIR"
              <> help "Directory for a service's botocore models."
          )
      )
    <*> strOption
      ( long "annexes"
          <> metavar "DIR"
          <> help "Directory containing botocore model annexes."
      )
    <*> strOption
      ( long "configs"
          <> metavar "DIR"
          <> help "Directory containing service configuration."
      )
    <*> strOption
      ( long "templates"
          <> metavar "DIR"
          <> help "Directory containing ED-E templates."
      )
    <*> strOption
      ( long "static"
          <> metavar "DIR"
          <> help "Directory containing static files for generated libraries."
      )
    <*> strOption
      ( long "retry"
          <> metavar "PATH"
          <> help "Path to the file containing retry definitions."
      )
    <*> ( Versions
            <$> option
              version
              ( long "library-version"
                  <> metavar "VER"
                  <> help "Version of the library to generate."
              )
            <*> option
              version
              ( long "client-version"
                  <> metavar "VER"
                  <> help "Client library version dependecy for examples."
              )
            <*> option
              version
              ( long "core-version"
                  <> metavar "VER"
                  <> help "Core library version dependency."
              )
        )

version :: ReadM (Version v)
version = eitherReader (Right . Version . Text.pack)

options :: ParserInfo Opt
options = info (helper <*> parser) fullDesc

validate :: MonadIO m => Opt -> m Opt
validate o = flip execStateT o $ do
  sequence_
    [ check optOutput,
      check optAnnexes,
      check optConfigs,
      check optTemplates,
      check optStatic,
      check optRetry
    ]
  mapM canon (o ^. optModels) >>= assign optModels
  where
    check :: (MonadIO m, MonadState s m) => Lens' s FilePath -> m ()
    check l = gets (view l) >>= canon >>= assign l

    canon :: MonadIO m => FilePath -> m FilePath
    canon = liftIO . Directory.canonicalizePath

main :: IO ()
main = do
  Opt {..} <-
    customExecParser (prefs showHelpOnError) options
      >>= validate

  let count = length _optModels
      load = readTemplate _optTemplates
      hoistEither = either fail pure

  title "Initialising..." <* done

  tmpl <-
   flip evalStateT mempty $ do
    title ("Loading templates from " ++ _optTemplates)

    cabalTemplate <- load "cabal.ede"
    tocTemplate <- load "toc.ede"
    waitersTemplate <- load "waiters.ede"
    readmeTemplate <- load "readme.ede"
    operationTemplate <- load "operation.ede"
    typesTemplate <- load "types.ede"
    sumTemplate <- load "types/sum.ede"
    productTemplate <- load "types/product.ede"
    testMainTemplate <- load "test/main.ede"
    testNamespaceTemplate <- load "test/namespace.ede"
    testInternalTemplate <- load "test/internal.ede"
    fixturesTemplate <- load "test/fixtures.ede"
    fixtureRequestTemplate <- load "test/fixtures/request.ede"
    blankTemplate <- load "blank.ede"

    done

    pure Templates {..}

  r <- JS.required _optRetry

  forM_ (zip [1 :: Int ..] _optModels) $ \(j, f) -> do
    title $
      "[" ++ show j ++ "/" ++ show count ++ "] model:"
        ++ FilePath.takeFileName f

    m <- Directory.listDirectory f >>= hoistEither . loadModel f

    say ("Using version " ++ show (m ^. modelVersion))

    cfg <-
      JS.required (_optConfigs </> (m ^. configFile))
        >>= hoistEither . JS.parse

    api <-
      sequence
        [ JS.optional (_optAnnexes </> (m ^. annexFile)),
          JS.required (m ^. serviceFile),
          JS.optional (m ^. waitersFile),
          JS.optional (m ^. pagersFile),
          pure r
        ]
        >>= hoistEither . JS.parse . JS.merge

    say $
      "Successfully parsed '"
        ++ Text.unpack (api ^. serviceFullName)
        ++ "' API definition"

    lib <- hoistEither (AST.rewrite _optVersions cfg api)
    tree <- hoistEither (Tree.populate _optOutput tmpl lib)
    dir <- Tree.fold createDir (\x -> either (touchFile x) (writeLTFile x)) tree

    say $
      "Successfully rendered "
        ++ Text.unpack (lib ^. libraryName)
        ++ "-"
        ++ semver (lib ^. libraryVersion)
        ++ " package"

    copyDir _optStatic (Tree.root dir)

    done

  title ("Successfully processed " ++ show count ++ " models.")

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- Module      : Main
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Main (main) where

import Control.Lens (Lens', (^.))
import qualified Control.Lens as Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Text as Text
import qualified Gen.AST as AST
import Gen.IO
import qualified Gen.JSON as JSON
import qualified Gen.Tree as Tree
import Gen.Types hiding (info, service, config, retry)
import Options.Applicative
import System.FilePath ((</>))
import qualified System.FilePath as FilePath
import qualified UnliftIO
import qualified UnliftIO.Directory as UnliftIO

data Options = Options
  { _optionOutput :: FilePath,
    _optionAnnexes :: FilePath,
    _optionServices :: FilePath,
    _optionTemplates :: FilePath,
    _optionAssets :: FilePath,
    _optionRetry :: FilePath,
    _optionVersions :: Versions,
    _optionModels :: [FilePath]
  }
  deriving (Show)

$(Lens.makeLenses ''Options)

parser :: Parser Options
parser =
  Options
    <$> strOption
      ( long "out"
          <> metavar "OUT-PATH"
          <> help "Directory to place the generated library."
      )
    <*> strOption
      ( long "annexes"
          <> metavar "PATH"
          <> help "Directory containing botocore model annexes."
          <> value "config/annexes"
      )
    <*> strOption
      ( long "services"
          <> metavar "PATH"
          <> help "Directory containing service configuration."
          <> value "config/services"
      )
    <*> strOption
      ( long "templates"
          <> metavar "PATH"
          <> help "Directory containing ED-E templates."
          <> value "config/templates"
      )
    <*> strOption
      ( long "assets"
          <> metavar "PATH"
          <> help "Directory containing static files for generated libraries."
          <> value "config/assets"
      )
    <*> strOption
      ( long "retry"
          <> metavar "PATH"
          <> help "Path to the file containing retry definitions."
      )
    <*> ( Versions
            <$> option
              versionReader
              ( long "library-version"
                  <> metavar "VERSION"
                  <> help "Version of the library to generate."
              )
            <*> option
              versionReader
              ( long "client-version"
                  <> metavar "VERSION"
                  <> help "Client library version dependecy for examples."
              )
        )
    <*> some
      ( strArgument
          ( metavar "MODEL-PATH"
              <> help "Directory for a service's botocore models."
          )
      )

versionReader :: ReadM (Version v)
versionReader = eitherReader (Right . Version . Text.pack)

options :: ParserInfo Options
options = info (helper <*> parser) fullDesc

validate :: MonadIO m => Options -> m Options
validate o = flip execStateT o $ do
  sequence_
    [ check optionOutput,
      check optionAnnexes,
      check optionServices,
      check optionTemplates,
      check optionAssets,
      check optionRetry
    ]
  mapM canon (o ^. optionModels) >>= Lens.assign optionModels
  where
    check :: (MonadIO m, MonadState s m) => Lens' s FilePath -> m ()
    check l = gets (Lens.view l) >>= canon >>= Lens.assign l

    canon :: MonadIO m => FilePath -> m FilePath
    canon = UnliftIO.canonicalizePath

main :: IO ()
main = do
  Options {..} <-
    customExecParser (prefs showHelpOnError) options
      >>= validate

  title "Initialising..." <* done

  let total = show (length _optionModels)
      load = readTemplate _optionTemplates

  templates <- flip evalStateT mempty $ do
    title ("Loading templates from " ++ _optionTemplates)

    cabalTemplate <- load "cabal.ede"
    tocTemplate <- load "toc.ede"
    waitersTemplate <- load "waiters.ede"
    readmeTemplate <- load "readme.ede"
    operationTemplate <- load "operation.ede"
    typesTemplate <- load "types.ede"
    lensTemplate <- load "lens.ede"
    sumTemplate <- load "sum.ede"
    productTemplate <- load "product.ede"
    testMainTemplate <- load "test/main.ede"
    testNamespaceTemplate <- load "test/namespace.ede"
    testInternalTemplate <- load "test/internal.ede"
    fixturesTemplate <- load "test/fixtures.ede"
    fixtureRequestTemplate <- load "test/fixtures/request.ede"
    blankTemplate <- load "blank.ede"

    lift done

    pure Templates {..}

  let hoistEither = either UnliftIO.throwString pure

  retry <- JSON.required _optionRetry

  forM_ (zip [1 ..] _optionModels) $ \(index, path) -> do
    title $
      "[" ++ show (index :: Int)
        ++ "/"
        ++ total
        ++ "] model:"
        ++ FilePath.takeFileName path

    model@Model {..} <-
      UnliftIO.listDirectory path
        >>= hoistEither . loadModel path

    say $
      "Using version "
        ++ show _modelVersion
        ++ " out of "
        ++ show _modelVersions

    config@Config {..} <-
      JSON.required (_optionServices </> configFile model)
        >>= hoistEither . JSON.parse

    service <-
      sequence
        [ JSON.optional (_optionAnnexes </> annexFile model),
          JSON.required (serviceFile model),
          JSON.optional (waitersFile model),
          JSON.optional (pagersFile model),
          pure retry
        ]
        >>= hoistEither . JSON.parse . JSON.merge

    say $
      "Successfully parsed '"
        ++ Text.unpack (service ^. serviceFullName)
        ++ "' API definition"

    library <- hoistEither (AST.rewrite _optionVersions config service)

    say $
      "Successfully synthesised '"
        ++ Text.unpack (library ^. libraryName)
        ++ "' Haskell package"

    directoryTree <-
      hoistEither (Tree.populate _optionOutput templates library)
        >>= Tree.fold createDir (\x -> either (touchFile x) (writeLTFile x))

    say $
      "Successfully rendered "
        ++ Text.unpack _libraryName
        ++ "-"
        ++ Text.unpack (semver (library ^. libraryVersion))
        ++ " package"

    copyDir _optionAssets (Tree.root directoryTree)

    done

  title $
    "Successfully processed "
      ++ total
      ++ " models."

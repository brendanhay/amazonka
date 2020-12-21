-- |
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

import qualified Control.Monad.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Data.Text as Text
import Gen.IO
import qualified Gen.JSON as JSON
import Gen.Prelude
import qualified Gen.Transform as AST
import qualified Gen.Tree as Tree
import Gen.Types hiding (info)
import qualified Options.Applicative as Options
import qualified System.Directory as Directory
import System.FilePath ((</>))
import qualified System.FilePath as FilePath

data Options = Options
  { optionOutput :: FilePath,
    optionModels :: [FilePath],
    optionAnnexes :: FilePath,
    optionServices :: FilePath,
    optionTemplates :: FilePath,
    optionAssets :: FilePath,
    optionRetry :: FilePath,
    optionVersions :: Versions
  }
  deriving (Show)

parser :: Options.Parser Options
parser =
  Options
    <$> Options.strOption
      ( Options.long "out"
          <> Options.metavar "DIR"
          <> Options.help "Directory to place the generated library."
      )
    <*> Options.some
      ( Options.strOption
          ( Options.long "model"
              <> Options.metavar "DIR"
              <> Options.help "Directory for a service's botocore models."
          )
      )
    <*> Options.strOption
      ( Options.long "annexes"
          <> Options.metavar "DIR"
          <> Options.help "Directory containing botocore model annexes."
      )
    <*> Options.strOption
      ( Options.long "services"
          <> Options.metavar "DIR"
          <> Options.help "Directory containing service configuration."
      )
    <*> Options.strOption
      ( Options.long "templates"
          <> Options.metavar "DIR"
          <> Options.help "Directory containing ED-E templates."
      )
    <*> Options.strOption
      ( Options.long "assets"
          <> Options.metavar "DIR"
          <> Options.help "Directory containing static files for generated libraries."
      )
    <*> Options.strOption
      ( Options.long "retry"
          <> Options.metavar "PATH"
          <> Options.help "Path to the file containing retry definitions."
      )
    <*> ( Versions
            <$> Options.option
              version
              ( Options.long "library-version"
                  <> Options.metavar "VER"
                  <> Options.help "Version of the library to generate."
              )
            <*> Options.option
              version
              ( Options.long "client-version"
                  <> Options.metavar "VER"
                  <> Options.help "Client library version dependecy for examples."
              )
            <*> Options.option
              version
              ( Options.long "core-version"
                  <> Options.metavar "VER"
                  <> Options.help "Core library version dependency."
              )
        )

version :: Options.ReadM (Version v)
version = Options.eitherReader (Right . Version . Text.pack)

info :: Options.ParserInfo Options
info = Options.info (Options.helper <*> parser) Options.fullDesc

main :: IO ()
main = do
  Options {..} <-
    Options.customExecParser (Options.prefs Options.showHelpOnError) info

  let count = length optionModels
      load = readTemplate optionTemplates
      hoistEither = either fail pure

  title "Initialising..." <* done

  templates <-
    flip State.evalStateT mempty $ do
      title ("Loading templates from " ++ optionTemplates)

      cabalTemplate <- load "cabal.ede"
      tocTemplate <- load "toc.ede"
      waitersTemplate <- load "waiters.ede"
      readmeTemplate <- load "readme.ede"
      operationTemplate <- load "operation.ede"
      typesTemplate <- load "types.ede"
      sumTemplate <- load "sum.ede"
      productTemplate <- load "product.ede"
      testMainTemplate <- load "test/main.ede"
      testNamespaceTemplate <- load "test/namespace.ede"
      testInternalTemplate <- load "test/internal.ede"
      fixturesTemplate <- load "test/fixtures.ede"
      fixtureRequestTemplate <- load "test/request.ede"
      blankTemplate <- load "blank.ede"

      done

      pure Templates {..}

  retry <- JSON.required optionRetry

  Foldable.forM_ (zip [1 :: Int ..] optionModels) $ \(index, path) -> do
    title $
      "["
        ++ show index
        ++ "/"
        ++ show count
        ++ "] model:"
        ++ FilePath.takeFileName path

    model <- Directory.listDirectory path >>= hoistEither . loadModel path

    say $
      "Using version "
        ++ show (model ^. modelVersion)

    config <-
      JSON.required (optionServices </> configFile model)
        >>= hoistEither . JSON.parse

    api <-
      sequence
        [ JSON.optional (optionAnnexes </> annexFile model),
          JSON.required (serviceFile model),
          JSON.optional (waitersFile model),
          JSON.optional (pagersFile model),
          pure retry
        ]
        >>= hoistEither . JSON.parse . JSON.merge

    say $
      "Successfully parsed '"
        ++ Text.unpack (api ^. serviceFullName)
        ++ "' API definition"

    lib <- hoistEither (AST.rewrite optionVersions config api)
    tree <- hoistEither (Tree.populate optionOutput templates lib)
    dir <- Tree.fold createDir (\x -> either (touchFile x) (writeLTFile x)) tree

    let modelOutput = Tree.root dir </> "model"

    Directory.createDirectoryIfMissing True modelOutput

    copyFileIfExists (optionAnnexes </> annexFile model) (modelOutput </> "annex.json")
    copyFileIfExists (serviceFile model) (modelOutput </> "service.json")
    copyFileIfExists (waitersFile model) (modelOutput </> "waiters.json")
    copyFileIfExists (pagersFile model) (modelOutput </> "pagers.json")

    say $
      "Successfully rendered "
        ++ Text.unpack (lib ^. libraryName)
        ++ "-"
        ++ semver (lib ^. libraryVersion)
        ++ " package"

    copyDir optionAssets (Tree.root dir)

    done

  title $
    "Successfully processed " ++ show count
      ++ " models."

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

import Control.Error
import Control.Lens (Lens', (^.))
import Control.Lens qualified as Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.String
import Data.Text qualified as Text
import Filesystem qualified as FS
import Filesystem.Path.CurrentOS
import Gen.AST qualified as AST
import Gen.Formatting
import Gen.IO
import Gen.JSON qualified as JS
import Gen.Tree qualified as Tree
import Gen.Types hiding (info)
import Options.Applicative

data Options = Options
  { _optionOutput :: Path,
    _optionAnnexes :: Path,
    _optionServices :: Path,
    _optionTemplates :: Path,
    _optionAssets :: Path,
    _optionRetry :: Path,
    _optionVersions :: Versions,
    _optionModels :: [Path]
  }
  deriving (Show)

$(Lens.makeLenses ''Options)

parser :: Parser Options
parser =
  Options
    <$> option
      isPath
      ( long "out"
          <> metavar "DIR"
          <> help "Directory to place the generated library."
      )
    <*> option
      isPath
      ( long "annexes"
          <> metavar "DIR"
          <> help "Directory containing botocore model annexes."
          <> value "config/annexes"
      )
    <*> option
      isPath
      ( long "services"
          <> metavar "DIR"
          <> help "Directory containing service configuration."
          <> value "config/services"
      )
    <*> option
      isPath
      ( long "templates"
          <> metavar "DIR"
          <> help "Directory containing ED-E templates."
          <> value "config/templates"
      )
    <*> option
      isPath
      ( long "assets"
          <> metavar "DIR"
          <> help "Directory containing static files for generated libraries."
          <> value "config/assets"
      )
    <*> option
      isPath
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
    <*> some
      ( argument
          isPath
          ( metavar "DIR"
              <> help "Directory for a service's botocore models."
          )
      )

isPath :: ReadM Path
isPath = eitherReader (Right . fromText . Text.dropWhileEnd (== '/') . fromString)

version :: ReadM (Version v)
version = eitherReader (Right . Version . Text.pack)

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
    check :: (MonadIO m, MonadState s m) => Lens' s Path -> m ()
    check l = gets (Lens.view l) >>= canon >>= Lens.assign l

    canon :: MonadIO m => Path -> m Path
    canon = liftIO . FS.canonicalizePath

main :: IO ()
main = do
  Options {..} <-
    customExecParser (prefs showHelpOnError) options
      >>= validate

  let i = length _optionModels

  run $ do
    title "Initialising..." <* done

    let load = readTemplate _optionTemplates

    tmpl <- flip evalStateT mempty $ do
      lift (title ("Loading templates from " % path) _optionTemplates)

      cabalTemplate <- load "cabal.ede"
      tocTemplate <- load "toc.ede"
      waitersTemplate <- load "waiters.ede"
      readmeTemplate <- load "readme.ede"
      operationTemplate <- load "operation.ede"
      typesTemplate <- load "types.ede"
      lensTemplate <- load "lens.ede"
      sumTemplate <- load "types/sum.ede"
      productTemplate <- load "types/product.ede"
      testMainTemplate <- load "test/main.ede"
      testNamespaceTemplate <- load "test/namespace.ede"
      testInternalTemplate <- load "test/internal.ede"
      fixturesTemplate <- load "test/fixtures.ede"
      fixtureRequestTemplate <- load "test/fixtures/request.ede"
      blankTemplate <- load "blank.ede"

      lift done

      pure Templates {..}

    r <- JS.required _optionRetry

    forM_ (zip [1 ..] _optionModels) $ \(j, f) -> do
      title
        ("[" % int % "/" % int % "] model:" % path)
        (j :: Int)
        (i :: Int)
        (filename f)

      m <- listDir f >>= hoistEither . loadModel f

      say ("Using version " % dateDash) (m ^. modelVersion)

      cfg <-
        JS.required (_optionServices </> (m ^. configFile))
          >>= hoistEither . JS.parse

      api <-
        sequence
          [ JS.optional (_optionAnnexes </> (m ^. annexFile)),
            JS.required (m ^. serviceFile),
            JS.optional (m ^. waitersFile),
            JS.optional (m ^. pagersFile),
            pure r
          ]
          >>= hoistEither . JS.parse . JS.merge

      say
        ("Successfully parsed '" % stext % "' API definition")
        (api ^. serviceFullName)

      lib <- hoistEither (AST.rewrite _optionVersions cfg api)

      dir <-
        hoistEither (Tree.populate _optionOutput tmpl lib)
          >>= Tree.fold createDir (\x -> either (touchFile x) (writeLTFile x))

      say
        ("Successfully rendered " % stext % "-" % semver % " package")
        (lib ^. libraryName)
        (lib ^. libraryVersion)

      copyDir _optionAssets (Tree.root dir)

      done

    title ("Successfully processed " % int % " models.") i

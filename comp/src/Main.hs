{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}

-- Module      : Main
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Main (main) where

import           Compiler.AST              hiding (info)
import           Compiler.IO
import           Compiler.JSON
import           Compiler.Model
import           Compiler.Rewrite
import           Compiler.Tree
import           Compiler.Types
import           Control.Error
import           Control.Lens              hiding (rewrite, (<.>), (??))
import           Control.Monad
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Jason
import           Data.Monoid
import qualified Data.SemVer               as SemVer
import           Data.String
import qualified Data.Text                 as Text
import qualified Filesystem                as FS
import           Filesystem.Path.CurrentOS
import           Formatting
import           Formatting.Time
import           Options.Applicative

data Opt = Opt
    { _optOutput    :: Path
    , _optModels    :: [Path]
    , _optAnnexes   :: Path
    , _optConfigs   :: Path
    , _optTemplates :: Path
    , _optAssets    :: Path
    , _optRetry     :: Path
    , _optVersions  :: Versions
    } deriving (Show)

makeLenses ''Opt

parser :: Parser Opt
parser = Opt
    <$> option isString
         ( long "out"
        <> metavar "DIR"
        <> help "Directory to place the generated library."
         )

    <*> some (option isString
         ( long "model"
        <> metavar "DIR"
        <> help "Directory for a service's botocore models."
         ))

    <*> option isString
         ( long "annexes"
        <> metavar "DIR"
        <> help "Directory containing botocore model annexes."
         )

    <*> option isString
         ( long "configs"
        <> metavar "DIR"
        <> help "Directory containing service configuration."
         )

    <*> option isString
         ( long "templates"
        <> metavar "DIR"
        <> help "Directory containing ED-E templates."
         )

    <*> option isString
         ( long "assets"
        <> metavar "DIR"
        <> help "Directory containing assets for generated libraries."
         )

    <*> option isString
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

isString :: IsString a => ReadM a
isString = eitherReader (Right . fromString)

version :: ReadM (Version v)
version = eitherReader (fmap Version . SemVer.fromText . Text.pack)

options :: ParserInfo Opt
options = info (helper <*> parser) fullDesc

validate :: MonadIO m => Opt -> m Opt
validate o = flip execStateT o $ do
    sequence_
        [ check optOutput
        , check optAnnexes
        , check optConfigs
        , check optTemplates
        , check optAssets
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
        title ("Loading templates from " % path) _optTemplates

        let load = readTemplate _optTemplates

        tmpl <- Templates
            <$> load "cabal.ede"
            <*> load "service.ede"
            <*> load "waiters.ede"
            <*> load "readme.ede"
            <*> load "example/cabal.ede"
            <*> load "example/makefile.ede"
            <*> load "operation.ede"
            <*> load "types.ede"
            <*  done

        forM_ (zip [1..] _optModels) $ \(j, f) -> do
            title ("[" % int % "/" % int % "] model:" % path)
                  (j :: Int)
                  (i :: Int)
                  (filename f)

            m@Model{..} <- listDir f >>= modelFromDir f

            let Ver{..} = m ^. latest
                configf = _optConfigs </> (m ^. configFile)
                annexf  = _optAnnexes </> (m ^. annexFile)

            say ("Using version " % dateDash % ", ignoring " % dateDashes)
                _verDate
                (m ^.. unused . verDate)

            cfg <- parseObject =<< requiredObject configf
            api <- parseObject . mergeObjects =<< sequence
                [ optionalObject annexf
                , requiredObject _verNormal
                , optionalObject _verWaiters
                , optionalObject _verPagers
                ]

            say ("Successfully parsed '" % stext % "' API definition")
                (api ^. serviceFullName)

            lib <- createLibrary _optVersions cfg api

            dir <- foldTree (failure string . show) createDir writeLTFile
                (populateTree _optOutput tmpl lib)

            say ("Successfully rendered " % stext % "-" % semver % " package")
                (lib ^. libraryName)
                (lib ^. libraryVersion)

            copyDir _optAssets (rootTree dir)

            done

        title ("Successfully processed " % int % " models.") i

requiredObject :: MonadIO m => Path -> Compiler m Object
requiredObject = readBSFile >=> decodeObject

optionalObject :: MonadIO m => Path -> Compiler m Object
optionalObject f = readBSFile f `catchError` const (return mempty)
    >>= decodeObject

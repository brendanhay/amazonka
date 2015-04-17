{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
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

import           Control.Applicative
import           Control.Error
import           Control.Lens               hiding ((<.>), (??))
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.State
import           Control.Monad.Trans.Except
import           Data.Either
import           Data.Jason                 (eitherDecode)
import           Data.List                  (nub, sortBy)
import           Data.List.NonEmpty         (NonEmpty (..))
import qualified Data.List.NonEmpty         as NonEmpty
import           Data.Monoid
import qualified Data.SemVer                as SemVer
import           Data.String
import qualified Data.Text                  as Text
import qualified Filesystem                 as FS
import           Filesystem.Path.CurrentOS  hiding (encode)
import           Gen.IO
import qualified Gen.JSON                   as JSON
import qualified Gen.Library                as Library
import           Gen.Model
import qualified Gen.Override               as Override
import           Gen.Types
import           Options.Applicative
import           Prelude                    hiding (FilePath)
import qualified Text.EDE                   as EDE

data Options = Options
    { _optOutput    :: FilePath
    , _optModels    :: [FilePath]
    , _optOverrides :: FilePath
    , _optTemplates :: FilePath
    , _optAssets    :: FilePath
    , _optRetry     :: FilePath
    , _optVersion   :: SemVer.Version
    } deriving (Show)

makeLenses ''Options

options :: ParserInfo Options
options = info (helper <*> parser) fullDesc

parser :: Parser Options
parser = Options
    <$> option string
         ( long "out"
        <> metavar "DIR"
        <> help "Directory to place the generated library. [required]"
         )

    <*> some (option string
         ( long "model"
        <> metavar "PATH"
        <> help "Directory for a service's botocore models. [required]"
         ))

    <*> option string
         ( long "overrides"
        <> metavar "DIR"
        <> help "Directory containing amazonka overrides. [required]"
         )

    <*> option string
         ( long "templates"
        <> metavar "DIR"
        <> help "Directory containing ED-E templates. [required]"
         )

    <*> option string
         ( long "assets"
        <> metavar "PATH"
        <> help "Directory containing assets for generated libraries. [required]"
         )

    <*> option string
         ( long "retry"
        <> metavar "PATH"
        <> help "Path to the file containing retry definitions. [required]"
         )

    <*> option (eitherReader (SemVer.fromText . Text.pack))
         ( long "version"
        <> metavar "VER"
        <> help "Version of the library to generate. [required]"
         )

string :: IsString a => ReadM a
string = eitherReader (Right . fromString)

validate :: MonadIO m => Options -> m Options
validate o = flip execStateT o $ do
    sequence_
        [ check optOutput
        , check optOverrides
        , check optTemplates
        , check optAssets
        , check optRetry
        ]
    mapM canon (o ^. optModels) >>= assign optModels
  where
    check :: (MonadIO m, MonadState s m) => Lens' s FilePath -> m ()
    check l = gets (view l) >>= canon >>= assign l

    canon :: MonadIO m => FilePath -> m FilePath
    canon = liftIO . FS.canonicalizePath

-- main :: IO ()
-- main = runScript $ do
--     o <- scriptIO $ customExecParser (prefs showHelpOnError) options
--         >>= validate

--     t <- templates (o ^. optTemplates)

--     forM_ (o ^. optModels) $ \d -> do
--         s <- model d (o ^. optOverrides)

--         -- mapM_ (\p -> AST.pretty p >>= scriptIO . LText.putStrLn . (<> "\n"))
--         --     . mapMaybe (uncurry AST.transform)
--         --     $ Map.toList (s ^. svcShapes)

--         r <- Library.tree (o ^. optOutput) t (o ^. optVersion) s
--             >>= writeTree "Write Library"

--         copyDirectory (o ^. optAssets) (Library.root r)

--         say "Completed" (s ^. svcName)

--     say "Completed" (Text.pack $ show (length (o ^. optModels)) ++ " models.")

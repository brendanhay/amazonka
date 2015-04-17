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

main :: IO ()
main = runScript $ do
    o <- scriptIO $ customExecParser (prefs showHelpOnError) options
        >>= validate

    t <- templates (o ^. optTemplates)

    forM_ (o ^. optModels) $ \d -> do
        s <- model d (o ^. optOverrides)

        -- mapM_ (\p -> AST.pretty p >>= scriptIO . LText.putStrLn . (<> "\n"))
        --     . mapMaybe (uncurry AST.transform)
        --     $ Map.toList (s ^. svcShapes)

        r <- Library.tree (o ^. optOutput) t (o ^. optVersion) s
            >>= writeTree "Write Library"

        copyDirectory (o ^. optAssets) (Library.root r)

        say "Completed" (s ^. svcName)

    say "Completed" (Text.pack $ show (length (o ^. optModels)) ++ " models.")

model :: FilePath -> FilePath -> Script (Service (Derived Shape) (Untyped Ref))
model d o = do
    say "Load Model" (encode d)

    v <- version d
    x <- dec override'
    y <- JSON.merge <$> sequence
        [ pure x
        , dec (normal v)
        , JSON.def "waiters"    $ dec (waiters v)
        , JSON.def "pagination" $ dec (pagers  v)
        ]
    either (throwError . msg) Override.service (JSON.parse y)
  where
    normal  = path "normal.json"
    waiters = path "waiters.json"
    pagers  = path "paginators.json"

    override' = o </> basename d <.> "json"
    path e v  = d </> v <.> e

    dec = fileContents >=> hoistEither . eitherDecode

    msg = mappend ("Error in " ++ show d ++ ": ")

version :: FilePath -> Script FilePath
version d = do
    fs <- scriptIO (FS.listDirectory d)
    vs <- NonEmpty.nonEmpty (basename `map` sortBy (flip compare) fs)
        ?? ("Failed to find versioned model in " ++ show d)
    say "Using Version" (msg vs)
    return (NonEmpty.head vs)
  where
    msg (v :| vs) = mconcat
        [ encode (d </> v)
        , " from ["
        , Text.intercalate ", " . nub $ map encode vs
        , "]"
        ]

templates :: FilePath -> Script (Templates Protocol)
templates d = do
    f <- Templates
        <$> load "cabal.ede"
        <*> pure (error "service.ede")
        <*> pure (error "waiters.ede")
        <*> load "readme.ede"
        <*> pure (error "example-cabal.ede")
        <*> pure (error "example-makefile.ede")

        -- <*> load "service.ede"
        -- <*> load "waiters.ede"
        -- <*> load "readme.ede"
        -- <*> load "example-cabal.ede"
        -- <*> load "example-makefile.ede"

    -- x <- (,)
    --     <$> load "types-xml.ede"
    --     <*> load "operation-xml.ede"

    j <- (,)
        <$> load "types.ede"
        <*> load "operation.ede"

    -- q <- (,)
    --     <$> load "types-query.ede"
    --     <*> load "operation-query.ede"

    let x = j
        q = j

    return $! f $ \case
        JSON     -> j
        RestJSON -> j
        XML      -> x
        RestXML  -> x
        Query    -> q
        EC2      -> q
  where
    load p = do
        let f = encode (d </> p)
        say "Parse Template" f
        scriptIO (EDE.eitherParseFile (Text.unpack f)) >>= hoistEither

 -- * Calculate a stable 'amazonka-*' library name per service
 -- * Ensure every shape has documentation (even if 'documentation is missing' string)
 -- * Ensure every ref for a shape has documentation, same as above
 -- * Ensure timestamp format for metadata is set
 -- * Parse/create the Endpoints model
 -- * Use a post-processing step (similar to stylish-haskell) which wraps all comment lines to 80 chars

-- * Tag the different 'Doc' usages with their parent type and use Data.Default instances
--   to select the default documentation for that type using 'fromMaybe def'.

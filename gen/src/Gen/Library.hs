{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- Module      : Gen.Library
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.Library (renderLibrary) where

import           Control.Applicative
import           Control.Error
import           Control.Monad       (forM_)
import           Data.Aeson
import qualified Data.HashMap.Strict as Map
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Gen.IO
import           Gen.JSON
import           Gen.Output
import           Gen.Types
import           System.FilePath

default (Text, FilePath)

renderLibrary :: FilePath -> Templates -> Output -> Script FilePath
renderLibrary d Templates{..} Output{..} = do
    svc <- toEnv _outService

    createDir src

    renderFile "Render Service" _tService gen _outService svc

    renderFile "Render Types" typ gen _outTypes
        =<< Map.insert "service" (Object svc) <$> toEnv _outTypes

    forM_ _outOperations $ \o -> renderFile "Render Operation" op gen o
        =<< toEnv o

    renderFile "Render Cabal" _tCabal lib _outCabal
        =<< cabal

    renderFile "Render README" _tReadme lib "README.md"
        =<< cabal

    createDir (ex </> "src")

    renderFile "Render Cabal" _tExCabal ex
        ("amazonka-" <> name <> "-examples.cabal")
            =<< cabal

    renderFile "Render Makefile" _tExMakefile ex "Makefile"
        =<< cabal

    return lib
  where
    (typ, op) = _tProtocol (_svProtocol _outService)

    name = toFilePath . Text.toLower . unAbbrev $ _svAbbrev _outService

    cabal = toEnv _outCabal

    ex, src, gen, lib :: FilePath
    ex  = rel "examples"
    src = rel "src"
    gen = rel "gen"
    lib = rel ""

    rel :: ToFilePath a => a -> FilePath
    rel = combine d . combine (toFilePath (_cLibrary _outCabal)) . toFilePath

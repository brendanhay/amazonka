{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

-- Module      : Gen.V2.Template
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.V2.Template where

import           Control.Applicative
import           Control.Error
import           Control.Lens        ((^.))
import           Control.Monad
import           Data.Aeson
import           Data.Char
import qualified Data.Foldable       as Fold
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.List           (intersperse)
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as Text
import qualified Data.Text.Lazy.IO   as LText
import qualified Data.Vector         as Vector
import           Gen.V2.Decode       (Protocol(..))
import           Gen.V2.Log
import           System.Directory
import           System.FilePath     hiding (normalise)
import           Text.EDE            (Template)
import qualified Text.EDE            as EDE
import           Text.EDE.Filters

data Templates = Templates
    { _tCabal     :: Template
    , _tInterface :: Template
    , _tService   :: Protocol -> (Template, Template)
    }

loadTemplates :: FilePath -> Script Templates
loadTemplates d = do
    f  <- Templates
        <$> load "cabal"
        <*> load "interface"

    !x <- (,)
        <$> load "xml/types"
        <*> load "xml/operation"

    !j <- (,)
        <$> load "json/types"
        <*> load "json/operation"

    !q <- (,)
        <$> load "query/types"
        <*> load "query/operation"

    return $! f $ \t ->
        case t of
            JSON     -> j
            RestJSON -> j
            RestXML  -> x
            Query    -> q
  where
    load (path -> f) =
           say "Parse Template" f
        *> scriptIO (EDE.eitherParseFile f)
       >>= hoistEither

    path f = d </> f <.> "ede"

{-# LANGUAGE OverloadedStrings #-}

-- Module      : Gen.Filters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.Filters where

import           Data.Char
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Text.Manipulate
import           Gen.Documentation
import           Text.EDE
import           Text.EDE.Filters

genFilters :: HashMap Text Term
genFilters = Map.fromList
    [ "above"       @: wrapHaddock "| "
    , "below"       @: wrapHaddock "^ "
    , "description" @: wrapDescription
    , "highlight"   @: highlightType
    , "wrapped"     @: wrapped
    , "concat"      @: (mappend :: Text -> Text -> Text)
    ]

wrapped :: Text -> Text
wrapped t = parens (Text.any isSpace t)
  where
    parens True  = "(" <> t <> ")"
    parens False = t

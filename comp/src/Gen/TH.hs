{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Gen.TH
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.TH
    ( deriveFromJSON

    , Options(..)
    , defaults
    , upper
    , lower
    , spinal
    , camel
    ) where

import           Data.Jason.TH
import qualified Data.Text            as Text
import           Data.Text.Manipulate
import           Gen.Text

upper, lower, spinal, camel :: Options
upper  = defaults { constructorTagModifier = asText Text.toUpper }
lower  = defaults { constructorTagModifier = asText Text.toLower }
spinal = defaults { constructorTagModifier = asText toSpinal }
camel  = defaults

defaults :: Options
defaults = defaultOptions
    { constructorTagModifier = asText toCamel
    , fieldLabelModifier     = asText stripLens
    , allNullaryToStringTag  = True
    , sumEncoding            =
        defaultTaggedObject
            { tagFieldName      = "type"
            , contentsFieldName = "contents"
            }
    }

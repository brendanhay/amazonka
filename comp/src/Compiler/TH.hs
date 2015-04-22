{-# LANGUAGE OverloadedStrings #-}

-- Module      : Compiler.TH
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.TH
    ( Compiler.TH.deriveJSON
    , deriveFromJSON
    , A.deriveToJSON

    , Options(..)
    , upper
    , lower
    , spinal
    , camel
    , aeson
    ) where

import           Compiler.Text
import qualified Data.Aeson.TH        as A
import           Data.Char
import           Data.Jason.TH
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Text.Manipulate

deriveJSON o n = concat <$> sequence
    [ deriveFromJSON o         n
    , A.deriveToJSON (aeson o) n
    ]

upper, lower, spinal :: Options
upper  = camel { constructorTagModifier = asText Text.toUpper      }
lower  = camel { constructorTagModifier = asText Text.toLower      }
spinal = camel { constructorTagModifier = asText (toSpinal . safe) }

camel :: Options
camel = defaultOptions
    { constructorTagModifier = asText (toCamel . safe)
    , fieldLabelModifier     = asText (stripPrefix "_" . stripSuffix "'")
    , allNullaryToStringTag  = True
    , sumEncoding            =
        defaultTaggedObject
            { tagFieldName      = "type"
            , contentsFieldName = "contents"
            }
    }

aeson :: Options -> A.Options
aeson o = A.defaultOptions
    { A.constructorTagModifier = constructorTagModifier o
    , A.fieldLabelModifier     = fieldLabelModifier     o
    , A.allNullaryToStringTag  = allNullaryToStringTag  o
    , A.sumEncoding            =
        A.defaultTaggedObject
            { A.tagFieldName      = tagFieldName      tag
            , A.contentsFieldName = contentsFieldName tag
            }
    }
  where
    tag = sumEncoding o

safe :: Text -> Text
safe x
    | Text.all isUpper x = Text.toLower x
    | otherwise          = x

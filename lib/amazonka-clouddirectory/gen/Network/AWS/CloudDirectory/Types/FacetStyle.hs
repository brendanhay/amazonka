{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.FacetStyle
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.FacetStyle where

import Network.AWS.Prelude

data FacetStyle
  = Dynamic
  | Static
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText FacetStyle where
  parser =
    takeLowerText >>= \case
      "dynamic" -> pure Dynamic
      "static" -> pure Static
      e ->
        fromTextError $
          "Failure parsing FacetStyle from value: '" <> e
            <> "'. Accepted values: dynamic, static"

instance ToText FacetStyle where
  toText = \case
    Dynamic -> "DYNAMIC"
    Static -> "STATIC"

instance Hashable FacetStyle

instance NFData FacetStyle

instance ToByteString FacetStyle

instance ToQuery FacetStyle

instance ToHeader FacetStyle

instance ToJSON FacetStyle where
  toJSON = toJSONText

instance FromJSON FacetStyle where
  parseJSON = parseJSONText "FacetStyle"

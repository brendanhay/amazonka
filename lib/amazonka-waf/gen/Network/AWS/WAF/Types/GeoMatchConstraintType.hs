{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.GeoMatchConstraintType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.GeoMatchConstraintType where

import Network.AWS.Prelude

data GeoMatchConstraintType = Country
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

instance FromText GeoMatchConstraintType where
  parser =
    takeLowerText >>= \case
      "country" -> pure Country
      e ->
        fromTextError $
          "Failure parsing GeoMatchConstraintType from value: '" <> e
            <> "'. Accepted values: country"

instance ToText GeoMatchConstraintType where
  toText = \case
    Country -> "Country"

instance Hashable GeoMatchConstraintType

instance NFData GeoMatchConstraintType

instance ToByteString GeoMatchConstraintType

instance ToQuery GeoMatchConstraintType

instance ToHeader GeoMatchConstraintType

instance ToJSON GeoMatchConstraintType where
  toJSON = toJSONText

instance FromJSON GeoMatchConstraintType where
  parseJSON = parseJSONText "GeoMatchConstraintType"

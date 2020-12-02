{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.Unit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.Unit where

import Network.AWS.Prelude

data Unit
  = Count
  | Percent
  | Seconds
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

instance FromText Unit where
  parser =
    takeLowerText >>= \case
      "count" -> pure Count
      "percent" -> pure Percent
      "seconds" -> pure Seconds
      e ->
        fromTextError $
          "Failure parsing Unit from value: '" <> e
            <> "'. Accepted values: count, percent, seconds"

instance ToText Unit where
  toText = \case
    Count -> "COUNT"
    Percent -> "PERCENT"
    Seconds -> "SECONDS"

instance Hashable Unit

instance NFData Unit

instance ToByteString Unit

instance ToQuery Unit

instance ToHeader Unit

instance ToJSON Unit where
  toJSON = toJSONText

instance FromJSON Unit where
  parseJSON = parseJSONText "Unit"

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.DistanceUnit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.DistanceUnit where

import Network.AWS.Prelude

data DistanceUnit
  = Imperial
  | Metric
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

instance FromText DistanceUnit where
  parser =
    takeLowerText >>= \case
      "imperial" -> pure Imperial
      "metric" -> pure Metric
      e ->
        fromTextError $
          "Failure parsing DistanceUnit from value: '" <> e
            <> "'. Accepted values: imperial, metric"

instance ToText DistanceUnit where
  toText = \case
    Imperial -> "IMPERIAL"
    Metric -> "METRIC"

instance Hashable DistanceUnit

instance NFData DistanceUnit

instance ToByteString DistanceUnit

instance ToQuery DistanceUnit

instance ToHeader DistanceUnit

instance ToJSON DistanceUnit where
  toJSON = toJSONText

instance FromJSON DistanceUnit where
  parseJSON = parseJSONText "DistanceUnit"

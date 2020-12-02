{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.TemporalFilterStrength
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.TemporalFilterStrength where

import Network.AWS.Prelude

-- | Temporal Filter Strength
data TemporalFilterStrength
  = Auto
  | Strength1
  | Strength10
  | Strength11
  | Strength12
  | Strength13
  | Strength14
  | Strength15
  | Strength16
  | Strength2
  | Strength3
  | Strength4
  | Strength5
  | Strength6
  | Strength7
  | Strength8
  | Strength9
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

instance FromText TemporalFilterStrength where
  parser =
    takeLowerText >>= \case
      "auto" -> pure Auto
      "strength_1" -> pure Strength1
      "strength_10" -> pure Strength10
      "strength_11" -> pure Strength11
      "strength_12" -> pure Strength12
      "strength_13" -> pure Strength13
      "strength_14" -> pure Strength14
      "strength_15" -> pure Strength15
      "strength_16" -> pure Strength16
      "strength_2" -> pure Strength2
      "strength_3" -> pure Strength3
      "strength_4" -> pure Strength4
      "strength_5" -> pure Strength5
      "strength_6" -> pure Strength6
      "strength_7" -> pure Strength7
      "strength_8" -> pure Strength8
      "strength_9" -> pure Strength9
      e ->
        fromTextError $
          "Failure parsing TemporalFilterStrength from value: '" <> e
            <> "'. Accepted values: auto, strength_1, strength_10, strength_11, strength_12, strength_13, strength_14, strength_15, strength_16, strength_2, strength_3, strength_4, strength_5, strength_6, strength_7, strength_8, strength_9"

instance ToText TemporalFilterStrength where
  toText = \case
    Auto -> "AUTO"
    Strength1 -> "STRENGTH_1"
    Strength10 -> "STRENGTH_10"
    Strength11 -> "STRENGTH_11"
    Strength12 -> "STRENGTH_12"
    Strength13 -> "STRENGTH_13"
    Strength14 -> "STRENGTH_14"
    Strength15 -> "STRENGTH_15"
    Strength16 -> "STRENGTH_16"
    Strength2 -> "STRENGTH_2"
    Strength3 -> "STRENGTH_3"
    Strength4 -> "STRENGTH_4"
    Strength5 -> "STRENGTH_5"
    Strength6 -> "STRENGTH_6"
    Strength7 -> "STRENGTH_7"
    Strength8 -> "STRENGTH_8"
    Strength9 -> "STRENGTH_9"

instance Hashable TemporalFilterStrength

instance NFData TemporalFilterStrength

instance ToByteString TemporalFilterStrength

instance ToQuery TemporalFilterStrength

instance ToHeader TemporalFilterStrength

instance ToJSON TemporalFilterStrength where
  toJSON = toJSONText

instance FromJSON TemporalFilterStrength where
  parseJSON = parseJSONText "TemporalFilterStrength"

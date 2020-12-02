{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.WatermarkingStrength
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.WatermarkingStrength where

import Network.AWS.Prelude

-- | Optional. Ignore this setting unless Nagra support directs you to specify a value. When you don't specify a value here, the Nagra NexGuard library uses its default value.
data WatermarkingStrength
  = WSDefault
  | WSLighter
  | WSLightest
  | WSStronger
  | WSStrongest
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

instance FromText WatermarkingStrength where
  parser =
    takeLowerText >>= \case
      "default" -> pure WSDefault
      "lighter" -> pure WSLighter
      "lightest" -> pure WSLightest
      "stronger" -> pure WSStronger
      "strongest" -> pure WSStrongest
      e ->
        fromTextError $
          "Failure parsing WatermarkingStrength from value: '" <> e
            <> "'. Accepted values: default, lighter, lightest, stronger, strongest"

instance ToText WatermarkingStrength where
  toText = \case
    WSDefault -> "DEFAULT"
    WSLighter -> "LIGHTER"
    WSLightest -> "LIGHTEST"
    WSStronger -> "STRONGER"
    WSStrongest -> "STRONGEST"

instance Hashable WatermarkingStrength

instance NFData WatermarkingStrength

instance ToByteString WatermarkingStrength

instance ToQuery WatermarkingStrength

instance ToHeader WatermarkingStrength

instance ToJSON WatermarkingStrength where
  toJSON = toJSONText

instance FromJSON WatermarkingStrength where
  parseJSON = parseJSONText "WatermarkingStrength"

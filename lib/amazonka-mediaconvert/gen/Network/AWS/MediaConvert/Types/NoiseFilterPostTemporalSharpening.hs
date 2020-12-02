{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.NoiseFilterPostTemporalSharpening
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.NoiseFilterPostTemporalSharpening where

import Network.AWS.Prelude

-- | Optional. When you set Noise reducer (noiseReducer) to Temporal (TEMPORAL), you can use this setting to apply sharpening. The default behavior, Auto (AUTO), allows the transcoder to determine whether to apply filtering, depending on input type and quality. When you set Noise reducer to Temporal, your output bandwidth is reduced. When Post temporal sharpening is also enabled, that bandwidth reduction is smaller.
data NoiseFilterPostTemporalSharpening
  = NFPTSAuto
  | NFPTSDisabled
  | NFPTSEnabled
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

instance FromText NoiseFilterPostTemporalSharpening where
  parser =
    takeLowerText >>= \case
      "auto" -> pure NFPTSAuto
      "disabled" -> pure NFPTSDisabled
      "enabled" -> pure NFPTSEnabled
      e ->
        fromTextError $
          "Failure parsing NoiseFilterPostTemporalSharpening from value: '" <> e
            <> "'. Accepted values: auto, disabled, enabled"

instance ToText NoiseFilterPostTemporalSharpening where
  toText = \case
    NFPTSAuto -> "AUTO"
    NFPTSDisabled -> "DISABLED"
    NFPTSEnabled -> "ENABLED"

instance Hashable NoiseFilterPostTemporalSharpening

instance NFData NoiseFilterPostTemporalSharpening

instance ToByteString NoiseFilterPostTemporalSharpening

instance ToQuery NoiseFilterPostTemporalSharpening

instance ToHeader NoiseFilterPostTemporalSharpening

instance ToJSON NoiseFilterPostTemporalSharpening where
  toJSON = toJSONText

instance FromJSON NoiseFilterPostTemporalSharpening where
  parseJSON = parseJSONText "NoiseFilterPostTemporalSharpening"

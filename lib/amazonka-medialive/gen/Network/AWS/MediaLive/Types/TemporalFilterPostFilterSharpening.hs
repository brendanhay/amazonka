{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.TemporalFilterPostFilterSharpening
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.TemporalFilterPostFilterSharpening where

import Network.AWS.Prelude

-- | Temporal Filter Post Filter Sharpening
data TemporalFilterPostFilterSharpening
  = TFPFSAuto
  | TFPFSDisabled
  | TFPFSEnabled
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

instance FromText TemporalFilterPostFilterSharpening where
  parser =
    takeLowerText >>= \case
      "auto" -> pure TFPFSAuto
      "disabled" -> pure TFPFSDisabled
      "enabled" -> pure TFPFSEnabled
      e ->
        fromTextError $
          "Failure parsing TemporalFilterPostFilterSharpening from value: '" <> e
            <> "'. Accepted values: auto, disabled, enabled"

instance ToText TemporalFilterPostFilterSharpening where
  toText = \case
    TFPFSAuto -> "AUTO"
    TFPFSDisabled -> "DISABLED"
    TFPFSEnabled -> "ENABLED"

instance Hashable TemporalFilterPostFilterSharpening

instance NFData TemporalFilterPostFilterSharpening

instance ToByteString TemporalFilterPostFilterSharpening

instance ToQuery TemporalFilterPostFilterSharpening

instance ToHeader TemporalFilterPostFilterSharpening

instance ToJSON TemporalFilterPostFilterSharpening where
  toJSON = toJSONText

instance FromJSON TemporalFilterPostFilterSharpening where
  parseJSON = parseJSONText "TemporalFilterPostFilterSharpening"

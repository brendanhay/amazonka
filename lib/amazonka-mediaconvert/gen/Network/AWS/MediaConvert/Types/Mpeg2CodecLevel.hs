{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2CodecLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mpeg2CodecLevel where

import Network.AWS.Prelude

-- | Use Level (Mpeg2CodecLevel) to set the MPEG-2 level for the video output.
data Mpeg2CodecLevel
  = MCLAuto
  | MCLHIGH1440
  | MCLHigh
  | MCLLow
  | MCLMain
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

instance FromText Mpeg2CodecLevel where
  parser =
    takeLowerText >>= \case
      "auto" -> pure MCLAuto
      "high1440" -> pure MCLHIGH1440
      "high" -> pure MCLHigh
      "low" -> pure MCLLow
      "main" -> pure MCLMain
      e ->
        fromTextError $
          "Failure parsing Mpeg2CodecLevel from value: '" <> e
            <> "'. Accepted values: auto, high1440, high, low, main"

instance ToText Mpeg2CodecLevel where
  toText = \case
    MCLAuto -> "AUTO"
    MCLHIGH1440 -> "HIGH1440"
    MCLHigh -> "HIGH"
    MCLLow -> "LOW"
    MCLMain -> "MAIN"

instance Hashable Mpeg2CodecLevel

instance NFData Mpeg2CodecLevel

instance ToByteString Mpeg2CodecLevel

instance ToQuery Mpeg2CodecLevel

instance ToHeader Mpeg2CodecLevel

instance ToJSON Mpeg2CodecLevel where
  toJSON = toJSONText

instance FromJSON Mpeg2CodecLevel where
  parseJSON = parseJSONText "Mpeg2CodecLevel"

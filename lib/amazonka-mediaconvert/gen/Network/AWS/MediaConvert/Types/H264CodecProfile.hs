{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264CodecProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264CodecProfile where

import Network.AWS.Prelude

-- | H.264 Profile. High 4:2:2 and 10-bit profiles are only available with the AVC-I License.
data H264CodecProfile
  = HCPBaseline
  | HCPHigh
  | HCPHigh10BIT
  | HCPHigh422
  | HCPHigh42210BIT
  | HCPMain
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

instance FromText H264CodecProfile where
  parser =
    takeLowerText >>= \case
      "baseline" -> pure HCPBaseline
      "high" -> pure HCPHigh
      "high_10bit" -> pure HCPHigh10BIT
      "high_422" -> pure HCPHigh422
      "high_422_10bit" -> pure HCPHigh42210BIT
      "main" -> pure HCPMain
      e ->
        fromTextError $
          "Failure parsing H264CodecProfile from value: '" <> e
            <> "'. Accepted values: baseline, high, high_10bit, high_422, high_422_10bit, main"

instance ToText H264CodecProfile where
  toText = \case
    HCPBaseline -> "BASELINE"
    HCPHigh -> "HIGH"
    HCPHigh10BIT -> "HIGH_10BIT"
    HCPHigh422 -> "HIGH_422"
    HCPHigh42210BIT -> "HIGH_422_10BIT"
    HCPMain -> "MAIN"

instance Hashable H264CodecProfile

instance NFData H264CodecProfile

instance ToByteString H264CodecProfile

instance ToQuery H264CodecProfile

instance ToHeader H264CodecProfile

instance ToJSON H264CodecProfile where
  toJSON = toJSONText

instance FromJSON H264CodecProfile where
  parseJSON = parseJSONText "H264CodecProfile"

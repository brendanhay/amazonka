{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264Profile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264Profile where

import Network.AWS.Prelude

-- | H264 Profile
data H264Profile
  = HPBaseline
  | HPHigh
  | HPHigh10BIT
  | HPHigh422
  | HPHigh42210BIT
  | HPMain
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

instance FromText H264Profile where
  parser =
    takeLowerText >>= \case
      "baseline" -> pure HPBaseline
      "high" -> pure HPHigh
      "high_10bit" -> pure HPHigh10BIT
      "high_422" -> pure HPHigh422
      "high_422_10bit" -> pure HPHigh42210BIT
      "main" -> pure HPMain
      e ->
        fromTextError $
          "Failure parsing H264Profile from value: '" <> e
            <> "'. Accepted values: baseline, high, high_10bit, high_422, high_422_10bit, main"

instance ToText H264Profile where
  toText = \case
    HPBaseline -> "BASELINE"
    HPHigh -> "HIGH"
    HPHigh10BIT -> "HIGH_10BIT"
    HPHigh422 -> "HIGH_422"
    HPHigh42210BIT -> "HIGH_422_10BIT"
    HPMain -> "MAIN"

instance Hashable H264Profile

instance NFData H264Profile

instance ToByteString H264Profile

instance ToQuery H264Profile

instance ToHeader H264Profile

instance ToJSON H264Profile where
  toJSON = toJSONText

instance FromJSON H264Profile where
  parseJSON = parseJSONText "H264Profile"

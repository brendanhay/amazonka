{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264LookAheadRateControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264LookAheadRateControl where

import Network.AWS.Prelude

-- | H264 Look Ahead Rate Control
data H264LookAheadRateControl
  = HLARCHigh
  | HLARCLow
  | HLARCMedium
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

instance FromText H264LookAheadRateControl where
  parser =
    takeLowerText >>= \case
      "high" -> pure HLARCHigh
      "low" -> pure HLARCLow
      "medium" -> pure HLARCMedium
      e ->
        fromTextError $
          "Failure parsing H264LookAheadRateControl from value: '" <> e
            <> "'. Accepted values: high, low, medium"

instance ToText H264LookAheadRateControl where
  toText = \case
    HLARCHigh -> "HIGH"
    HLARCLow -> "LOW"
    HLARCMedium -> "MEDIUM"

instance Hashable H264LookAheadRateControl

instance NFData H264LookAheadRateControl

instance ToByteString H264LookAheadRateControl

instance ToQuery H264LookAheadRateControl

instance ToHeader H264LookAheadRateControl

instance ToJSON H264LookAheadRateControl where
  toJSON = toJSONText

instance FromJSON H264LookAheadRateControl where
  parseJSON = parseJSONText "H264LookAheadRateControl"

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265LookAheadRateControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H265LookAheadRateControl where

import Network.AWS.Prelude

-- | H265 Look Ahead Rate Control
data H265LookAheadRateControl
  = High
  | Low
  | Medium
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

instance FromText H265LookAheadRateControl where
  parser =
    takeLowerText >>= \case
      "high" -> pure High
      "low" -> pure Low
      "medium" -> pure Medium
      e ->
        fromTextError $
          "Failure parsing H265LookAheadRateControl from value: '" <> e
            <> "'. Accepted values: high, low, medium"

instance ToText H265LookAheadRateControl where
  toText = \case
    High -> "HIGH"
    Low -> "LOW"
    Medium -> "MEDIUM"

instance Hashable H265LookAheadRateControl

instance NFData H265LookAheadRateControl

instance ToByteString H265LookAheadRateControl

instance ToQuery H265LookAheadRateControl

instance ToHeader H265LookAheadRateControl

instance ToJSON H265LookAheadRateControl where
  toJSON = toJSONText

instance FromJSON H265LookAheadRateControl where
  parseJSON = parseJSONText "H265LookAheadRateControl"

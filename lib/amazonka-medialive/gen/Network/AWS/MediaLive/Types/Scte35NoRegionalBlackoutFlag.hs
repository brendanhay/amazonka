{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35NoRegionalBlackoutFlag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35NoRegionalBlackoutFlag where

import Network.AWS.Prelude

-- | Corresponds to the no_regional_blackout_flag parameter. A value of REGIONAL_BLACKOUT corresponds to 0 (false) in the SCTE-35 specification. If you include one of the "restriction" flags then you must include all four of them.
data Scte35NoRegionalBlackoutFlag
  = NoRegionalBlackout
  | RegionalBlackout
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

instance FromText Scte35NoRegionalBlackoutFlag where
  parser =
    takeLowerText >>= \case
      "no_regional_blackout" -> pure NoRegionalBlackout
      "regional_blackout" -> pure RegionalBlackout
      e ->
        fromTextError $
          "Failure parsing Scte35NoRegionalBlackoutFlag from value: '" <> e
            <> "'. Accepted values: no_regional_blackout, regional_blackout"

instance ToText Scte35NoRegionalBlackoutFlag where
  toText = \case
    NoRegionalBlackout -> "NO_REGIONAL_BLACKOUT"
    RegionalBlackout -> "REGIONAL_BLACKOUT"

instance Hashable Scte35NoRegionalBlackoutFlag

instance NFData Scte35NoRegionalBlackoutFlag

instance ToByteString Scte35NoRegionalBlackoutFlag

instance ToQuery Scte35NoRegionalBlackoutFlag

instance ToHeader Scte35NoRegionalBlackoutFlag

instance ToJSON Scte35NoRegionalBlackoutFlag where
  toJSON = toJSONText

instance FromJSON Scte35NoRegionalBlackoutFlag where
  parseJSON = parseJSONText "Scte35NoRegionalBlackoutFlag"

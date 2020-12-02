{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ReservationVideoQuality
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ReservationVideoQuality where

import Network.AWS.Prelude

-- | Video quality, e.g. 'STANDARD' (Outputs only)
data ReservationVideoQuality
  = Enhanced
  | Premium
  | Standard
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

instance FromText ReservationVideoQuality where
  parser =
    takeLowerText >>= \case
      "enhanced" -> pure Enhanced
      "premium" -> pure Premium
      "standard" -> pure Standard
      e ->
        fromTextError $
          "Failure parsing ReservationVideoQuality from value: '" <> e
            <> "'. Accepted values: enhanced, premium, standard"

instance ToText ReservationVideoQuality where
  toText = \case
    Enhanced -> "ENHANCED"
    Premium -> "PREMIUM"
    Standard -> "STANDARD"

instance Hashable ReservationVideoQuality

instance NFData ReservationVideoQuality

instance ToByteString ReservationVideoQuality

instance ToQuery ReservationVideoQuality

instance ToHeader ReservationVideoQuality

instance FromJSON ReservationVideoQuality where
  parseJSON = parseJSONText "ReservationVideoQuality"

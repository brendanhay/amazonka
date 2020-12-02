{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ReservationResourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ReservationResourceType where

import Network.AWS.Prelude

-- | Resource type, 'INPUT', 'OUTPUT', 'MULTIPLEX', or 'CHANNEL'
data ReservationResourceType
  = RRTChannel
  | RRTInput
  | RRTMultiplex
  | RRTOutput
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

instance FromText ReservationResourceType where
  parser =
    takeLowerText >>= \case
      "channel" -> pure RRTChannel
      "input" -> pure RRTInput
      "multiplex" -> pure RRTMultiplex
      "output" -> pure RRTOutput
      e ->
        fromTextError $
          "Failure parsing ReservationResourceType from value: '" <> e
            <> "'. Accepted values: channel, input, multiplex, output"

instance ToText ReservationResourceType where
  toText = \case
    RRTChannel -> "CHANNEL"
    RRTInput -> "INPUT"
    RRTMultiplex -> "MULTIPLEX"
    RRTOutput -> "OUTPUT"

instance Hashable ReservationResourceType

instance NFData ReservationResourceType

instance ToByteString ReservationResourceType

instance ToQuery ReservationResourceType

instance ToHeader ReservationResourceType

instance FromJSON ReservationResourceType where
  parseJSON = parseJSONText "ReservationResourceType"

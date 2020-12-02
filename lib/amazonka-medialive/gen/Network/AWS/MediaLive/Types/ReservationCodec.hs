{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ReservationCodec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ReservationCodec where

import Network.AWS.Prelude

-- | Codec, 'MPEG2', 'AVC', 'HEVC', or 'AUDIO'
data ReservationCodec
  = RCAudio
  | RCAvc
  | RCHevc
  | RCLink
  | RCMPEG2
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

instance FromText ReservationCodec where
  parser =
    takeLowerText >>= \case
      "audio" -> pure RCAudio
      "avc" -> pure RCAvc
      "hevc" -> pure RCHevc
      "link" -> pure RCLink
      "mpeg2" -> pure RCMPEG2
      e ->
        fromTextError $
          "Failure parsing ReservationCodec from value: '" <> e
            <> "'. Accepted values: audio, avc, hevc, link, mpeg2"

instance ToText ReservationCodec where
  toText = \case
    RCAudio -> "AUDIO"
    RCAvc -> "AVC"
    RCHevc -> "HEVC"
    RCLink -> "LINK"
    RCMPEG2 -> "MPEG2"

instance Hashable ReservationCodec

instance NFData ReservationCodec

instance ToByteString ReservationCodec

instance ToQuery ReservationCodec

instance ToHeader ReservationCodec

instance FromJSON ReservationCodec where
  parseJSON = parseJSONText "ReservationCodec"

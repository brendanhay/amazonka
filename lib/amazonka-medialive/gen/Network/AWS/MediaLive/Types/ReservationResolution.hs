{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ReservationResolution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ReservationResolution where

import Network.AWS.Prelude

-- | Resolution based on lines of vertical resolution; SD is less than 720 lines, HD is 720 to 1080 lines, FHD is 1080 lines, UHD is greater than 1080 lines
data ReservationResolution
  = RRFhd
  | RRHD
  | RRSD
  | RRUhd
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

instance FromText ReservationResolution where
  parser =
    takeLowerText >>= \case
      "fhd" -> pure RRFhd
      "hd" -> pure RRHD
      "sd" -> pure RRSD
      "uhd" -> pure RRUhd
      e ->
        fromTextError $
          "Failure parsing ReservationResolution from value: '" <> e
            <> "'. Accepted values: fhd, hd, sd, uhd"

instance ToText ReservationResolution where
  toText = \case
    RRFhd -> "FHD"
    RRHD -> "HD"
    RRSD -> "SD"
    RRUhd -> "UHD"

instance Hashable ReservationResolution

instance NFData ReservationResolution

instance ToByteString ReservationResolution

instance ToQuery ReservationResolution

instance ToHeader ReservationResolution

instance FromJSON ReservationResolution where
  parseJSON = parseJSONText "ReservationResolution"

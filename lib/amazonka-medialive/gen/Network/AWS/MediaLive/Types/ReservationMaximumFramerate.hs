{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ReservationMaximumFramerate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ReservationMaximumFramerate where

import Network.AWS.Prelude

-- | Maximum framerate in frames per second (Outputs only)
data ReservationMaximumFramerate
  = Max30Fps
  | Max60Fps
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

instance FromText ReservationMaximumFramerate where
  parser =
    takeLowerText >>= \case
      "max_30_fps" -> pure Max30Fps
      "max_60_fps" -> pure Max60Fps
      e ->
        fromTextError $
          "Failure parsing ReservationMaximumFramerate from value: '" <> e
            <> "'. Accepted values: max_30_fps, max_60_fps"

instance ToText ReservationMaximumFramerate where
  toText = \case
    Max30Fps -> "MAX_30_FPS"
    Max60Fps -> "MAX_60_FPS"

instance Hashable ReservationMaximumFramerate

instance NFData ReservationMaximumFramerate

instance ToByteString ReservationMaximumFramerate

instance ToQuery ReservationMaximumFramerate

instance ToHeader ReservationMaximumFramerate

instance FromJSON ReservationMaximumFramerate where
  parseJSON = parseJSONText "ReservationMaximumFramerate"

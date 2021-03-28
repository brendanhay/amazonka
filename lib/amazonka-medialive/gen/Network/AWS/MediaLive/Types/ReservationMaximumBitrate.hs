{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ReservationMaximumBitrate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.ReservationMaximumBitrate
  ( ReservationMaximumBitrate
    ( ReservationMaximumBitrate'
    , ReservationMaximumBitrateMax10Mbps
    , ReservationMaximumBitrateMax20Mbps
    , ReservationMaximumBitrateMax50Mbps
    , fromReservationMaximumBitrate
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Maximum bitrate in megabits per second
newtype ReservationMaximumBitrate = ReservationMaximumBitrate'{fromReservationMaximumBitrate
                                                               :: Core.Text}
                                      deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                      Core.Generic)
                                      deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                        Core.ToJSONKey, Core.FromJSONKey,
                                                        Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                        Core.FromXML, Core.ToText, Core.FromText,
                                                        Core.ToByteString, Core.ToQuery,
                                                        Core.ToHeader)

pattern ReservationMaximumBitrateMax10Mbps :: ReservationMaximumBitrate
pattern ReservationMaximumBitrateMax10Mbps = ReservationMaximumBitrate' "MAX_10_MBPS"

pattern ReservationMaximumBitrateMax20Mbps :: ReservationMaximumBitrate
pattern ReservationMaximumBitrateMax20Mbps = ReservationMaximumBitrate' "MAX_20_MBPS"

pattern ReservationMaximumBitrateMax50Mbps :: ReservationMaximumBitrate
pattern ReservationMaximumBitrateMax50Mbps = ReservationMaximumBitrate' "MAX_50_MBPS"

{-# COMPLETE 
  ReservationMaximumBitrateMax10Mbps,

  ReservationMaximumBitrateMax20Mbps,

  ReservationMaximumBitrateMax50Mbps,
  ReservationMaximumBitrate'
  #-}

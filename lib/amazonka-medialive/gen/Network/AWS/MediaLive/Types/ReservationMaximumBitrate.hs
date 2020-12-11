-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ReservationMaximumBitrate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ReservationMaximumBitrate
  ( ReservationMaximumBitrate
      ( ReservationMaximumBitrate',
        RMBMax10Mbps,
        RMBMax20Mbps,
        RMBMax50Mbps
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Maximum bitrate in megabits per second
newtype ReservationMaximumBitrate = ReservationMaximumBitrate' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern RMBMax10Mbps :: ReservationMaximumBitrate
pattern RMBMax10Mbps = ReservationMaximumBitrate' "MAX_10_MBPS"

pattern RMBMax20Mbps :: ReservationMaximumBitrate
pattern RMBMax20Mbps = ReservationMaximumBitrate' "MAX_20_MBPS"

pattern RMBMax50Mbps :: ReservationMaximumBitrate
pattern RMBMax50Mbps = ReservationMaximumBitrate' "MAX_50_MBPS"

{-# COMPLETE
  RMBMax10Mbps,
  RMBMax20Mbps,
  RMBMax50Mbps,
  ReservationMaximumBitrate'
  #-}

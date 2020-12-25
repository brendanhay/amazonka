{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ReservationResolution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ReservationResolution
  ( ReservationResolution
      ( ReservationResolution',
        ReservationResolutionSD,
        ReservationResolutionHD,
        ReservationResolutionFhd,
        ReservationResolutionUhd,
        fromReservationResolution
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Resolution based on lines of vertical resolution; SD is less than 720 lines, HD is 720 to 1080 lines, FHD is 1080 lines, UHD is greater than 1080 lines
newtype ReservationResolution = ReservationResolution'
  { fromReservationResolution ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ReservationResolutionSD :: ReservationResolution
pattern ReservationResolutionSD = ReservationResolution' "SD"

pattern ReservationResolutionHD :: ReservationResolution
pattern ReservationResolutionHD = ReservationResolution' "HD"

pattern ReservationResolutionFhd :: ReservationResolution
pattern ReservationResolutionFhd = ReservationResolution' "FHD"

pattern ReservationResolutionUhd :: ReservationResolution
pattern ReservationResolutionUhd = ReservationResolution' "UHD"

{-# COMPLETE
  ReservationResolutionSD,
  ReservationResolutionHD,
  ReservationResolutionFhd,
  ReservationResolutionUhd,
  ReservationResolution'
  #-}

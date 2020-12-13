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
        RRSD,
        RRHD,
        RRFhd,
        RRUhd
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Resolution based on lines of vertical resolution; SD is less than 720 lines, HD is 720 to 1080 lines, FHD is 1080 lines, UHD is greater than 1080 lines
newtype ReservationResolution = ReservationResolution' Lude.Text
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

pattern RRSD :: ReservationResolution
pattern RRSD = ReservationResolution' "SD"

pattern RRHD :: ReservationResolution
pattern RRHD = ReservationResolution' "HD"

pattern RRFhd :: ReservationResolution
pattern RRFhd = ReservationResolution' "FHD"

pattern RRUhd :: ReservationResolution
pattern RRUhd = ReservationResolution' "UHD"

{-# COMPLETE
  RRSD,
  RRHD,
  RRFhd,
  RRUhd,
  ReservationResolution'
  #-}

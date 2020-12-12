{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ReservationResourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ReservationResourceType
  ( ReservationResourceType
      ( ReservationResourceType',
        RRTChannel,
        RRTInput,
        RRTMultiplex,
        RRTOutput
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Resource type, 'INPUT', 'OUTPUT', 'MULTIPLEX', or 'CHANNEL'
newtype ReservationResourceType = ReservationResourceType' Lude.Text
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

pattern RRTChannel :: ReservationResourceType
pattern RRTChannel = ReservationResourceType' "CHANNEL"

pattern RRTInput :: ReservationResourceType
pattern RRTInput = ReservationResourceType' "INPUT"

pattern RRTMultiplex :: ReservationResourceType
pattern RRTMultiplex = ReservationResourceType' "MULTIPLEX"

pattern RRTOutput :: ReservationResourceType
pattern RRTOutput = ReservationResourceType' "OUTPUT"

{-# COMPLETE
  RRTChannel,
  RRTInput,
  RRTMultiplex,
  RRTOutput,
  ReservationResourceType'
  #-}

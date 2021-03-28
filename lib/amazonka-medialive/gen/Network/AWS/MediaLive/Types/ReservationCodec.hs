{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ReservationCodec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.ReservationCodec
  ( ReservationCodec
    ( ReservationCodec'
    , ReservationCodecMPEG2
    , ReservationCodecAvc
    , ReservationCodecHevc
    , ReservationCodecAudio
    , ReservationCodecLink
    , fromReservationCodec
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Codec, 'MPEG2', 'AVC', 'HEVC', or 'AUDIO'
newtype ReservationCodec = ReservationCodec'{fromReservationCodec
                                             :: Core.Text}
                             deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                             Core.Generic)
                             deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                               Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                               Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                               Core.FromText, Core.ToByteString, Core.ToQuery,
                                               Core.ToHeader)

pattern ReservationCodecMPEG2 :: ReservationCodec
pattern ReservationCodecMPEG2 = ReservationCodec' "MPEG2"

pattern ReservationCodecAvc :: ReservationCodec
pattern ReservationCodecAvc = ReservationCodec' "AVC"

pattern ReservationCodecHevc :: ReservationCodec
pattern ReservationCodecHevc = ReservationCodec' "HEVC"

pattern ReservationCodecAudio :: ReservationCodec
pattern ReservationCodecAudio = ReservationCodec' "AUDIO"

pattern ReservationCodecLink :: ReservationCodec
pattern ReservationCodecLink = ReservationCodec' "LINK"

{-# COMPLETE 
  ReservationCodecMPEG2,

  ReservationCodecAvc,

  ReservationCodecHevc,

  ReservationCodecAudio,

  ReservationCodecLink,
  ReservationCodec'
  #-}

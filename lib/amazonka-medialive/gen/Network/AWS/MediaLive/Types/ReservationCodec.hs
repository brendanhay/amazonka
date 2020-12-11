-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ReservationCodec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ReservationCodec
  ( ReservationCodec
      ( ReservationCodec',
        RCAudio,
        RCAvc,
        RCHevc,
        RCLink,
        RCMPEG2
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Codec, 'MPEG2', 'AVC', 'HEVC', or 'AUDIO'
newtype ReservationCodec = ReservationCodec' Lude.Text
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

pattern RCAudio :: ReservationCodec
pattern RCAudio = ReservationCodec' "AUDIO"

pattern RCAvc :: ReservationCodec
pattern RCAvc = ReservationCodec' "AVC"

pattern RCHevc :: ReservationCodec
pattern RCHevc = ReservationCodec' "HEVC"

pattern RCLink :: ReservationCodec
pattern RCLink = ReservationCodec' "LINK"

pattern RCMPEG2 :: ReservationCodec
pattern RCMPEG2 = ReservationCodec' "MPEG2"

{-# COMPLETE
  RCAudio,
  RCAvc,
  RCHevc,
  RCLink,
  RCMPEG2,
  ReservationCodec'
  #-}

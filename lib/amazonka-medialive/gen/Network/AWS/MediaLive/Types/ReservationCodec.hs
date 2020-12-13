{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        RCMPEG2,
        RCAvc,
        RCHevc,
        RCAudio,
        RCLink
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

pattern RCMPEG2 :: ReservationCodec
pattern RCMPEG2 = ReservationCodec' "MPEG2"

pattern RCAvc :: ReservationCodec
pattern RCAvc = ReservationCodec' "AVC"

pattern RCHevc :: ReservationCodec
pattern RCHevc = ReservationCodec' "HEVC"

pattern RCAudio :: ReservationCodec
pattern RCAudio = ReservationCodec' "AUDIO"

pattern RCLink :: ReservationCodec
pattern RCLink = ReservationCodec' "LINK"

{-# COMPLETE
  RCMPEG2,
  RCAvc,
  RCHevc,
  RCAudio,
  RCLink,
  ReservationCodec'
  #-}

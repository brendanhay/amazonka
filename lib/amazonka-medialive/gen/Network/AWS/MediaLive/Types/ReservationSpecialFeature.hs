{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ReservationSpecialFeature
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ReservationSpecialFeature
  ( ReservationSpecialFeature
      ( ReservationSpecialFeature',
        AdvancedAudio,
        AudioNormalization
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Special features, 'ADVANCED_AUDIO' or 'AUDIO_NORMALIZATION'
newtype ReservationSpecialFeature = ReservationSpecialFeature' Lude.Text
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

pattern AdvancedAudio :: ReservationSpecialFeature
pattern AdvancedAudio = ReservationSpecialFeature' "ADVANCED_AUDIO"

pattern AudioNormalization :: ReservationSpecialFeature
pattern AudioNormalization = ReservationSpecialFeature' "AUDIO_NORMALIZATION"

{-# COMPLETE
  AdvancedAudio,
  AudioNormalization,
  ReservationSpecialFeature'
  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AacAudioDescriptionBroadcasterMix
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AacAudioDescriptionBroadcasterMix
  ( AacAudioDescriptionBroadcasterMix
      ( AacAudioDescriptionBroadcasterMix',
        AacAudioDescriptionBroadcasterMixBroadcasterMixedAd,
        AacAudioDescriptionBroadcasterMixNormal,
        fromAacAudioDescriptionBroadcasterMix
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Choose BROADCASTER_MIXED_AD when the input contains pre-mixed main audio + audio description (AD) as a stereo pair. The value for AudioType will be set to 3, which signals to downstream systems that this stream contains "broadcaster mixed AD". Note that the input received by the encoder must contain pre-mixed audio; the encoder does not perform the mixing. When you choose BROADCASTER_MIXED_AD, the encoder ignores any values you provide in AudioType and  FollowInputAudioType. Choose NORMAL when the input does not contain pre-mixed audio + audio description (AD). In this case, the encoder will use any values you provide for AudioType and FollowInputAudioType.
newtype AacAudioDescriptionBroadcasterMix = AacAudioDescriptionBroadcasterMix'
  { fromAacAudioDescriptionBroadcasterMix ::
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

pattern AacAudioDescriptionBroadcasterMixBroadcasterMixedAd :: AacAudioDescriptionBroadcasterMix
pattern AacAudioDescriptionBroadcasterMixBroadcasterMixedAd = AacAudioDescriptionBroadcasterMix' "BROADCASTER_MIXED_AD"

pattern AacAudioDescriptionBroadcasterMixNormal :: AacAudioDescriptionBroadcasterMix
pattern AacAudioDescriptionBroadcasterMixNormal = AacAudioDescriptionBroadcasterMix' "NORMAL"

{-# COMPLETE
  AacAudioDescriptionBroadcasterMixBroadcasterMixedAd,
  AacAudioDescriptionBroadcasterMixNormal,
  AacAudioDescriptionBroadcasterMix'
  #-}

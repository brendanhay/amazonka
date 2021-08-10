{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AacAudioDescriptionBroadcasterMix
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AacAudioDescriptionBroadcasterMix
  ( AacAudioDescriptionBroadcasterMix
      ( ..,
        AacAudioDescriptionBroadcasterMix_BROADCASTER_MIXED_AD,
        AacAudioDescriptionBroadcasterMix_NORMAL
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Choose BROADCASTER_MIXED_AD when the input contains pre-mixed main audio
-- + audio description (AD) as a stereo pair. The value for AudioType will
-- be set to 3, which signals to downstream systems that this stream
-- contains \"broadcaster mixed AD\". Note that the input received by the
-- encoder must contain pre-mixed audio; the encoder does not perform the
-- mixing. When you choose BROADCASTER_MIXED_AD, the encoder ignores any
-- values you provide in AudioType and FollowInputAudioType. Choose NORMAL
-- when the input does not contain pre-mixed audio + audio description
-- (AD). In this case, the encoder will use any values you provide for
-- AudioType and FollowInputAudioType.
newtype AacAudioDescriptionBroadcasterMix = AacAudioDescriptionBroadcasterMix'
  { fromAacAudioDescriptionBroadcasterMix ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern AacAudioDescriptionBroadcasterMix_BROADCASTER_MIXED_AD :: AacAudioDescriptionBroadcasterMix
pattern AacAudioDescriptionBroadcasterMix_BROADCASTER_MIXED_AD = AacAudioDescriptionBroadcasterMix' "BROADCASTER_MIXED_AD"

pattern AacAudioDescriptionBroadcasterMix_NORMAL :: AacAudioDescriptionBroadcasterMix
pattern AacAudioDescriptionBroadcasterMix_NORMAL = AacAudioDescriptionBroadcasterMix' "NORMAL"

{-# COMPLETE
  AacAudioDescriptionBroadcasterMix_BROADCASTER_MIXED_AD,
  AacAudioDescriptionBroadcasterMix_NORMAL,
  AacAudioDescriptionBroadcasterMix'
  #-}

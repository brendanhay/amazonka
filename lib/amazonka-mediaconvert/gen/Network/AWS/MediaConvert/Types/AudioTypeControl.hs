{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioTypeControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.AudioTypeControl
  ( AudioTypeControl
    ( AudioTypeControl'
    , AudioTypeControlFollowInput
    , AudioTypeControlUseConfigured
    , fromAudioTypeControl
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | When set to FOLLOW_INPUT, if the input contains an ISO 639 audio_type, then that value is passed through to the output. If the input contains no ISO 639 audio_type, the value in Audio Type is included in the output. Otherwise the value in Audio Type is included in the output. Note that this field and audioType are both ignored if audioDescriptionBroadcasterMix is set to BROADCASTER_MIXED_AD.
newtype AudioTypeControl = AudioTypeControl'{fromAudioTypeControl
                                             :: Core.Text}
                             deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                             Core.Generic)
                             deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                               Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                               Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                               Core.FromText, Core.ToByteString, Core.ToQuery,
                                               Core.ToHeader)

pattern AudioTypeControlFollowInput :: AudioTypeControl
pattern AudioTypeControlFollowInput = AudioTypeControl' "FOLLOW_INPUT"

pattern AudioTypeControlUseConfigured :: AudioTypeControl
pattern AudioTypeControlUseConfigured = AudioTypeControl' "USE_CONFIGURED"

{-# COMPLETE 
  AudioTypeControlFollowInput,

  AudioTypeControlUseConfigured,
  AudioTypeControl'
  #-}

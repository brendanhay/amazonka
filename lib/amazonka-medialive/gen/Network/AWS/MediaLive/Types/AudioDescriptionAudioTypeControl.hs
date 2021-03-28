{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioDescriptionAudioTypeControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.AudioDescriptionAudioTypeControl
  ( AudioDescriptionAudioTypeControl
    ( AudioDescriptionAudioTypeControl'
    , AudioDescriptionAudioTypeControlFollowInput
    , AudioDescriptionAudioTypeControlUseConfigured
    , fromAudioDescriptionAudioTypeControl
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Audio Description Audio Type Control
newtype AudioDescriptionAudioTypeControl = AudioDescriptionAudioTypeControl'{fromAudioDescriptionAudioTypeControl
                                                                             :: Core.Text}
                                             deriving stock (Core.Eq, Core.Ord, Core.Read,
                                                             Core.Show, Core.Generic)
                                             deriving newtype (Core.IsString, Core.Hashable,
                                                               Core.NFData, Core.ToJSONKey,
                                                               Core.FromJSONKey, Core.ToJSON,
                                                               Core.FromJSON, Core.ToXML,
                                                               Core.FromXML, Core.ToText,
                                                               Core.FromText, Core.ToByteString,
                                                               Core.ToQuery, Core.ToHeader)

pattern AudioDescriptionAudioTypeControlFollowInput :: AudioDescriptionAudioTypeControl
pattern AudioDescriptionAudioTypeControlFollowInput = AudioDescriptionAudioTypeControl' "FOLLOW_INPUT"

pattern AudioDescriptionAudioTypeControlUseConfigured :: AudioDescriptionAudioTypeControl
pattern AudioDescriptionAudioTypeControlUseConfigured = AudioDescriptionAudioTypeControl' "USE_CONFIGURED"

{-# COMPLETE 
  AudioDescriptionAudioTypeControlFollowInput,

  AudioDescriptionAudioTypeControlUseConfigured,
  AudioDescriptionAudioTypeControl'
  #-}

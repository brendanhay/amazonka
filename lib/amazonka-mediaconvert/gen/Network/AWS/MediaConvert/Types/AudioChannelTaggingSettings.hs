{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioChannelTaggingSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.AudioChannelTaggingSettings
  ( AudioChannelTaggingSettings (..)
  -- * Smart constructor
  , mkAudioChannelTaggingSettings
  -- * Lenses
  , actsChannelTag
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.AudioChannelTag as Types
import qualified Network.AWS.Prelude as Core

-- | When you mimic a multi-channel audio layout with multiple mono-channel tracks, you can tag each channel layout manually. For example, you would tag the tracks that contain your left, right, and center audio with Left (L), Right (R), and Center (C), respectively. When you don't specify a value, MediaConvert labels your track as Center (C) by default. To use audio layout tagging, your output must be in a QuickTime (.mov) container; your audio codec must be AAC, WAV, or AIFF; and you must set up your audio track to have only one channel.
--
-- /See:/ 'mkAudioChannelTaggingSettings' smart constructor.
newtype AudioChannelTaggingSettings = AudioChannelTaggingSettings'
  { channelTag :: Core.Maybe Types.AudioChannelTag
    -- ^ You can add a tag for this mono-channel audio track to mimic its placement in a multi-channel layout.  For example, if this track is the left surround channel, choose Left surround (LS).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AudioChannelTaggingSettings' value with any optional fields omitted.
mkAudioChannelTaggingSettings
    :: AudioChannelTaggingSettings
mkAudioChannelTaggingSettings
  = AudioChannelTaggingSettings'{channelTag = Core.Nothing}

-- | You can add a tag for this mono-channel audio track to mimic its placement in a multi-channel layout.  For example, if this track is the left surround channel, choose Left surround (LS).
--
-- /Note:/ Consider using 'channelTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
actsChannelTag :: Lens.Lens' AudioChannelTaggingSettings (Core.Maybe Types.AudioChannelTag)
actsChannelTag = Lens.field @"channelTag"
{-# INLINEABLE actsChannelTag #-}
{-# DEPRECATED channelTag "Use generic-lens or generic-optics with 'channelTag' instead"  #-}

instance Core.FromJSON AudioChannelTaggingSettings where
        toJSON AudioChannelTaggingSettings{..}
          = Core.object
              (Core.catMaybes [("channelTag" Core..=) Core.<$> channelTag])

instance Core.FromJSON AudioChannelTaggingSettings where
        parseJSON
          = Core.withObject "AudioChannelTaggingSettings" Core.$
              \ x ->
                AudioChannelTaggingSettings' Core.<$> (x Core..:? "channelTag")

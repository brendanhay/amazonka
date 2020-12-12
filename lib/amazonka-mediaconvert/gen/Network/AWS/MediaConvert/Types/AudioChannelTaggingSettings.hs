{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioChannelTaggingSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AudioChannelTaggingSettings
  ( AudioChannelTaggingSettings (..),

    -- * Smart constructor
    mkAudioChannelTaggingSettings,

    -- * Lenses
    actsChannelTag,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.AudioChannelTag
import qualified Network.AWS.Prelude as Lude

-- | When you mimic a multi-channel audio layout with multiple mono-channel tracks, you can tag each channel layout manually. For example, you would tag the tracks that contain your left, right, and center audio with Left (L), Right (R), and Center (C), respectively. When you don't specify a value, MediaConvert labels your track as Center (C) by default. To use audio layout tagging, your output must be in a QuickTime (.mov) container; your audio codec must be AAC, WAV, or AIFF; and you must set up your audio track to have only one channel.
--
-- /See:/ 'mkAudioChannelTaggingSettings' smart constructor.
newtype AudioChannelTaggingSettings = AudioChannelTaggingSettings'
  { channelTag ::
      Lude.Maybe AudioChannelTag
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AudioChannelTaggingSettings' with the minimum fields required to make a request.
--
-- * 'channelTag' - You can add a tag for this mono-channel audio track to mimic its placement in a multi-channel layout.  For example, if this track is the left surround channel, choose Left surround (LS).
mkAudioChannelTaggingSettings ::
  AudioChannelTaggingSettings
mkAudioChannelTaggingSettings =
  AudioChannelTaggingSettings' {channelTag = Lude.Nothing}

-- | You can add a tag for this mono-channel audio track to mimic its placement in a multi-channel layout.  For example, if this track is the left surround channel, choose Left surround (LS).
--
-- /Note:/ Consider using 'channelTag' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
actsChannelTag :: Lens.Lens' AudioChannelTaggingSettings (Lude.Maybe AudioChannelTag)
actsChannelTag = Lens.lens (channelTag :: AudioChannelTaggingSettings -> Lude.Maybe AudioChannelTag) (\s a -> s {channelTag = a} :: AudioChannelTaggingSettings)
{-# DEPRECATED actsChannelTag "Use generic-lens or generic-optics with 'channelTag' instead." #-}

instance Lude.FromJSON AudioChannelTaggingSettings where
  parseJSON =
    Lude.withObject
      "AudioChannelTaggingSettings"
      ( \x ->
          AudioChannelTaggingSettings' Lude.<$> (x Lude..:? "channelTag")
      )

instance Lude.ToJSON AudioChannelTaggingSettings where
  toJSON AudioChannelTaggingSettings' {..} =
    Lude.object
      (Lude.catMaybes [("channelTag" Lude..=) Lude.<$> channelTag])

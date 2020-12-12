{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Fmp4HlsSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Fmp4HlsSettings
  ( Fmp4HlsSettings (..),

    -- * Smart constructor
    mkFmp4HlsSettings,

    -- * Lenses
    fhsNielsenId3Behavior,
    fhsAudioRenditionSets,
    fhsTimedMetadataBehavior,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.Fmp4NielsenId3Behavior
import Network.AWS.MediaLive.Types.Fmp4TimedMetadataBehavior
import qualified Network.AWS.Prelude as Lude

-- | Fmp4 Hls Settings
--
-- /See:/ 'mkFmp4HlsSettings' smart constructor.
data Fmp4HlsSettings = Fmp4HlsSettings'
  { nielsenId3Behavior ::
      Lude.Maybe Fmp4NielsenId3Behavior,
    audioRenditionSets :: Lude.Maybe Lude.Text,
    timedMetadataBehavior ::
      Lude.Maybe Fmp4TimedMetadataBehavior
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Fmp4HlsSettings' with the minimum fields required to make a request.
--
-- * 'audioRenditionSets' - List all the audio groups that are used with the video output stream. Input all the audio GROUP-IDs that are associated to the video, separate by ','.
-- * 'nielsenId3Behavior' - If set to passthrough, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
-- * 'timedMetadataBehavior' - When set to passthrough, timed metadata is passed through from input to output.
mkFmp4HlsSettings ::
  Fmp4HlsSettings
mkFmp4HlsSettings =
  Fmp4HlsSettings'
    { nielsenId3Behavior = Lude.Nothing,
      audioRenditionSets = Lude.Nothing,
      timedMetadataBehavior = Lude.Nothing
    }

-- | If set to passthrough, Nielsen inaudible tones for media tracking will be detected in the input audio and an equivalent ID3 tag will be inserted in the output.
--
-- /Note:/ Consider using 'nielsenId3Behavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fhsNielsenId3Behavior :: Lens.Lens' Fmp4HlsSettings (Lude.Maybe Fmp4NielsenId3Behavior)
fhsNielsenId3Behavior = Lens.lens (nielsenId3Behavior :: Fmp4HlsSettings -> Lude.Maybe Fmp4NielsenId3Behavior) (\s a -> s {nielsenId3Behavior = a} :: Fmp4HlsSettings)
{-# DEPRECATED fhsNielsenId3Behavior "Use generic-lens or generic-optics with 'nielsenId3Behavior' instead." #-}

-- | List all the audio groups that are used with the video output stream. Input all the audio GROUP-IDs that are associated to the video, separate by ','.
--
-- /Note:/ Consider using 'audioRenditionSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fhsAudioRenditionSets :: Lens.Lens' Fmp4HlsSettings (Lude.Maybe Lude.Text)
fhsAudioRenditionSets = Lens.lens (audioRenditionSets :: Fmp4HlsSettings -> Lude.Maybe Lude.Text) (\s a -> s {audioRenditionSets = a} :: Fmp4HlsSettings)
{-# DEPRECATED fhsAudioRenditionSets "Use generic-lens or generic-optics with 'audioRenditionSets' instead." #-}

-- | When set to passthrough, timed metadata is passed through from input to output.
--
-- /Note:/ Consider using 'timedMetadataBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fhsTimedMetadataBehavior :: Lens.Lens' Fmp4HlsSettings (Lude.Maybe Fmp4TimedMetadataBehavior)
fhsTimedMetadataBehavior = Lens.lens (timedMetadataBehavior :: Fmp4HlsSettings -> Lude.Maybe Fmp4TimedMetadataBehavior) (\s a -> s {timedMetadataBehavior = a} :: Fmp4HlsSettings)
{-# DEPRECATED fhsTimedMetadataBehavior "Use generic-lens or generic-optics with 'timedMetadataBehavior' instead." #-}

instance Lude.FromJSON Fmp4HlsSettings where
  parseJSON =
    Lude.withObject
      "Fmp4HlsSettings"
      ( \x ->
          Fmp4HlsSettings'
            Lude.<$> (x Lude..:? "nielsenId3Behavior")
            Lude.<*> (x Lude..:? "audioRenditionSets")
            Lude.<*> (x Lude..:? "timedMetadataBehavior")
      )

instance Lude.ToJSON Fmp4HlsSettings where
  toJSON Fmp4HlsSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nielsenId3Behavior" Lude..=) Lude.<$> nielsenId3Behavior,
            ("audioRenditionSets" Lude..=) Lude.<$> audioRenditionSets,
            ("timedMetadataBehavior" Lude..=) Lude.<$> timedMetadataBehavior
          ]
      )

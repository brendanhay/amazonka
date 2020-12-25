{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.AudioMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.AudioMetadata
  ( AudioMetadata (..),

    -- * Smart constructor
    mkAudioMetadata,

    -- * Lenses
    amCodec,
    amDurationMillis,
    amNumberOfChannels,
    amSampleRate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.String as Types

-- | Metadata information about an audio stream. An array of @AudioMetadata@ objects for the audio streams found in a stored video is returned by 'GetSegmentDetection' .
--
-- /See:/ 'mkAudioMetadata' smart constructor.
data AudioMetadata = AudioMetadata'
  { -- | The audio codec used to encode or decode the audio stream.
    codec :: Core.Maybe Types.String,
    -- | The duration of the audio stream in milliseconds.
    durationMillis :: Core.Maybe Core.Natural,
    -- | The number of audio channels in the segment.
    numberOfChannels :: Core.Maybe Core.Natural,
    -- | The sample rate for the audio stream.
    sampleRate :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AudioMetadata' value with any optional fields omitted.
mkAudioMetadata ::
  AudioMetadata
mkAudioMetadata =
  AudioMetadata'
    { codec = Core.Nothing,
      durationMillis = Core.Nothing,
      numberOfChannels = Core.Nothing,
      sampleRate = Core.Nothing
    }

-- | The audio codec used to encode or decode the audio stream.
--
-- /Note:/ Consider using 'codec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amCodec :: Lens.Lens' AudioMetadata (Core.Maybe Types.String)
amCodec = Lens.field @"codec"
{-# DEPRECATED amCodec "Use generic-lens or generic-optics with 'codec' instead." #-}

-- | The duration of the audio stream in milliseconds.
--
-- /Note:/ Consider using 'durationMillis' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amDurationMillis :: Lens.Lens' AudioMetadata (Core.Maybe Core.Natural)
amDurationMillis = Lens.field @"durationMillis"
{-# DEPRECATED amDurationMillis "Use generic-lens or generic-optics with 'durationMillis' instead." #-}

-- | The number of audio channels in the segment.
--
-- /Note:/ Consider using 'numberOfChannels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amNumberOfChannels :: Lens.Lens' AudioMetadata (Core.Maybe Core.Natural)
amNumberOfChannels = Lens.field @"numberOfChannels"
{-# DEPRECATED amNumberOfChannels "Use generic-lens or generic-optics with 'numberOfChannels' instead." #-}

-- | The sample rate for the audio stream.
--
-- /Note:/ Consider using 'sampleRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amSampleRate :: Lens.Lens' AudioMetadata (Core.Maybe Core.Natural)
amSampleRate = Lens.field @"sampleRate"
{-# DEPRECATED amSampleRate "Use generic-lens or generic-optics with 'sampleRate' instead." #-}

instance Core.FromJSON AudioMetadata where
  parseJSON =
    Core.withObject "AudioMetadata" Core.$
      \x ->
        AudioMetadata'
          Core.<$> (x Core..:? "Codec")
          Core.<*> (x Core..:? "DurationMillis")
          Core.<*> (x Core..:? "NumberOfChannels")
          Core.<*> (x Core..:? "SampleRate")

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
    amSampleRate,
    amNumberOfChannels,
    amDurationMillis,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Metadata information about an audio stream. An array of @AudioMetadata@ objects for the audio streams found in a stored video is returned by 'GetSegmentDetection' .
--
-- /See:/ 'mkAudioMetadata' smart constructor.
data AudioMetadata = AudioMetadata'
  { codec :: Lude.Maybe Lude.Text,
    sampleRate :: Lude.Maybe Lude.Natural,
    numberOfChannels :: Lude.Maybe Lude.Natural,
    durationMillis :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AudioMetadata' with the minimum fields required to make a request.
--
-- * 'codec' - The audio codec used to encode or decode the audio stream.
-- * 'durationMillis' - The duration of the audio stream in milliseconds.
-- * 'numberOfChannels' - The number of audio channels in the segment.
-- * 'sampleRate' - The sample rate for the audio stream.
mkAudioMetadata ::
  AudioMetadata
mkAudioMetadata =
  AudioMetadata'
    { codec = Lude.Nothing,
      sampleRate = Lude.Nothing,
      numberOfChannels = Lude.Nothing,
      durationMillis = Lude.Nothing
    }

-- | The audio codec used to encode or decode the audio stream.
--
-- /Note:/ Consider using 'codec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amCodec :: Lens.Lens' AudioMetadata (Lude.Maybe Lude.Text)
amCodec = Lens.lens (codec :: AudioMetadata -> Lude.Maybe Lude.Text) (\s a -> s {codec = a} :: AudioMetadata)
{-# DEPRECATED amCodec "Use generic-lens or generic-optics with 'codec' instead." #-}

-- | The sample rate for the audio stream.
--
-- /Note:/ Consider using 'sampleRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amSampleRate :: Lens.Lens' AudioMetadata (Lude.Maybe Lude.Natural)
amSampleRate = Lens.lens (sampleRate :: AudioMetadata -> Lude.Maybe Lude.Natural) (\s a -> s {sampleRate = a} :: AudioMetadata)
{-# DEPRECATED amSampleRate "Use generic-lens or generic-optics with 'sampleRate' instead." #-}

-- | The number of audio channels in the segment.
--
-- /Note:/ Consider using 'numberOfChannels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amNumberOfChannels :: Lens.Lens' AudioMetadata (Lude.Maybe Lude.Natural)
amNumberOfChannels = Lens.lens (numberOfChannels :: AudioMetadata -> Lude.Maybe Lude.Natural) (\s a -> s {numberOfChannels = a} :: AudioMetadata)
{-# DEPRECATED amNumberOfChannels "Use generic-lens or generic-optics with 'numberOfChannels' instead." #-}

-- | The duration of the audio stream in milliseconds.
--
-- /Note:/ Consider using 'durationMillis' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amDurationMillis :: Lens.Lens' AudioMetadata (Lude.Maybe Lude.Natural)
amDurationMillis = Lens.lens (durationMillis :: AudioMetadata -> Lude.Maybe Lude.Natural) (\s a -> s {durationMillis = a} :: AudioMetadata)
{-# DEPRECATED amDurationMillis "Use generic-lens or generic-optics with 'durationMillis' instead." #-}

instance Lude.FromJSON AudioMetadata where
  parseJSON =
    Lude.withObject
      "AudioMetadata"
      ( \x ->
          AudioMetadata'
            Lude.<$> (x Lude..:? "Codec")
            Lude.<*> (x Lude..:? "SampleRate")
            Lude.<*> (x Lude..:? "NumberOfChannels")
            Lude.<*> (x Lude..:? "DurationMillis")
      )

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioNormalizationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioNormalizationSettings
  ( AudioNormalizationSettings (..),

    -- * Smart constructor
    mkAudioNormalizationSettings,

    -- * Lenses
    ansAlgorithmControl,
    ansTargetLkfs,
    ansAlgorithm,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AudioNormalizationAlgorithm
import Network.AWS.MediaLive.Types.AudioNormalizationAlgorithmControl
import qualified Network.AWS.Prelude as Lude

-- | Audio Normalization Settings
--
-- /See:/ 'mkAudioNormalizationSettings' smart constructor.
data AudioNormalizationSettings = AudioNormalizationSettings'
  { -- | When set to correctAudio the output audio is corrected using the chosen algorithm. If set to measureOnly, the audio will be measured but not adjusted.
    algorithmControl :: Lude.Maybe AudioNormalizationAlgorithmControl,
    -- | Target LKFS(loudness) to adjust volume to. If no value is entered, a default value will be used according to the chosen algorithm.  The CALM Act (1770-1) recommends a target of -24 LKFS. The EBU R-128 specification (1770-2) recommends a target of -23 LKFS.
    targetLkfs :: Lude.Maybe Lude.Double,
    -- | Audio normalization algorithm to use. itu17701 conforms to the CALM Act specification, itu17702 conforms to the EBU R-128 specification.
    algorithm :: Lude.Maybe AudioNormalizationAlgorithm
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AudioNormalizationSettings' with the minimum fields required to make a request.
--
-- * 'algorithmControl' - When set to correctAudio the output audio is corrected using the chosen algorithm. If set to measureOnly, the audio will be measured but not adjusted.
-- * 'targetLkfs' - Target LKFS(loudness) to adjust volume to. If no value is entered, a default value will be used according to the chosen algorithm.  The CALM Act (1770-1) recommends a target of -24 LKFS. The EBU R-128 specification (1770-2) recommends a target of -23 LKFS.
-- * 'algorithm' - Audio normalization algorithm to use. itu17701 conforms to the CALM Act specification, itu17702 conforms to the EBU R-128 specification.
mkAudioNormalizationSettings ::
  AudioNormalizationSettings
mkAudioNormalizationSettings =
  AudioNormalizationSettings'
    { algorithmControl = Lude.Nothing,
      targetLkfs = Lude.Nothing,
      algorithm = Lude.Nothing
    }

-- | When set to correctAudio the output audio is corrected using the chosen algorithm. If set to measureOnly, the audio will be measured but not adjusted.
--
-- /Note:/ Consider using 'algorithmControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ansAlgorithmControl :: Lens.Lens' AudioNormalizationSettings (Lude.Maybe AudioNormalizationAlgorithmControl)
ansAlgorithmControl = Lens.lens (algorithmControl :: AudioNormalizationSettings -> Lude.Maybe AudioNormalizationAlgorithmControl) (\s a -> s {algorithmControl = a} :: AudioNormalizationSettings)
{-# DEPRECATED ansAlgorithmControl "Use generic-lens or generic-optics with 'algorithmControl' instead." #-}

-- | Target LKFS(loudness) to adjust volume to. If no value is entered, a default value will be used according to the chosen algorithm.  The CALM Act (1770-1) recommends a target of -24 LKFS. The EBU R-128 specification (1770-2) recommends a target of -23 LKFS.
--
-- /Note:/ Consider using 'targetLkfs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ansTargetLkfs :: Lens.Lens' AudioNormalizationSettings (Lude.Maybe Lude.Double)
ansTargetLkfs = Lens.lens (targetLkfs :: AudioNormalizationSettings -> Lude.Maybe Lude.Double) (\s a -> s {targetLkfs = a} :: AudioNormalizationSettings)
{-# DEPRECATED ansTargetLkfs "Use generic-lens or generic-optics with 'targetLkfs' instead." #-}

-- | Audio normalization algorithm to use. itu17701 conforms to the CALM Act specification, itu17702 conforms to the EBU R-128 specification.
--
-- /Note:/ Consider using 'algorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ansAlgorithm :: Lens.Lens' AudioNormalizationSettings (Lude.Maybe AudioNormalizationAlgorithm)
ansAlgorithm = Lens.lens (algorithm :: AudioNormalizationSettings -> Lude.Maybe AudioNormalizationAlgorithm) (\s a -> s {algorithm = a} :: AudioNormalizationSettings)
{-# DEPRECATED ansAlgorithm "Use generic-lens or generic-optics with 'algorithm' instead." #-}

instance Lude.FromJSON AudioNormalizationSettings where
  parseJSON =
    Lude.withObject
      "AudioNormalizationSettings"
      ( \x ->
          AudioNormalizationSettings'
            Lude.<$> (x Lude..:? "algorithmControl")
            Lude.<*> (x Lude..:? "targetLkfs")
            Lude.<*> (x Lude..:? "algorithm")
      )

instance Lude.ToJSON AudioNormalizationSettings where
  toJSON AudioNormalizationSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("algorithmControl" Lude..=) Lude.<$> algorithmControl,
            ("targetLkfs" Lude..=) Lude.<$> targetLkfs,
            ("algorithm" Lude..=) Lude.<$> algorithm
          ]
      )

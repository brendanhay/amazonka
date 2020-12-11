-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioNormalizationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AudioNormalizationSettings
  ( AudioNormalizationSettings (..),

    -- * Smart constructor
    mkAudioNormalizationSettings,

    -- * Lenses
    ansAlgorithmControl,
    ansTargetLkfs,
    ansPeakCalculation,
    ansCorrectionGateLevel,
    ansAlgorithm,
    ansLoudnessLogging,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.AudioNormalizationAlgorithm
import Network.AWS.MediaConvert.Types.AudioNormalizationAlgorithmControl
import Network.AWS.MediaConvert.Types.AudioNormalizationLoudnessLogging
import Network.AWS.MediaConvert.Types.AudioNormalizationPeakCalculation
import qualified Network.AWS.Prelude as Lude

-- | Advanced audio normalization settings. Ignore these settings unless you need to comply with a loudness standard.
--
-- /See:/ 'mkAudioNormalizationSettings' smart constructor.
data AudioNormalizationSettings = AudioNormalizationSettings'
  { algorithmControl ::
      Lude.Maybe
        AudioNormalizationAlgorithmControl,
    targetLkfs :: Lude.Maybe Lude.Double,
    peakCalculation ::
      Lude.Maybe
        AudioNormalizationPeakCalculation,
    correctionGateLevel ::
      Lude.Maybe Lude.Int,
    algorithm ::
      Lude.Maybe
        AudioNormalizationAlgorithm,
    loudnessLogging ::
      Lude.Maybe
        AudioNormalizationLoudnessLogging
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AudioNormalizationSettings' with the minimum fields required to make a request.
--
-- * 'algorithm' - Choose one of the following audio normalization algorithms: ITU-R BS.1770-1: Ungated loudness. A measurement of ungated average loudness for an entire piece of content, suitable for measurement of short-form content under ATSC recommendation A/85. Supports up to 5.1 audio channels. ITU-R BS.1770-2: Gated loudness. A measurement of gated average loudness compliant with the requirements of EBU-R128. Supports up to 5.1 audio channels. ITU-R BS.1770-3: Modified peak. The same loudness measurement algorithm as 1770-2, with an updated true peak measurement. ITU-R BS.1770-4: Higher channel count. Allows for more audio channels than the other algorithms, including configurations such as 7.1.
-- * 'algorithmControl' - When enabled the output audio is corrected using the chosen algorithm. If disabled, the audio will be measured but not adjusted.
-- * 'correctionGateLevel' - Content measuring above this level will be corrected to the target level. Content measuring below this level will not be corrected.
-- * 'loudnessLogging' - If set to LOG, log each output's audio track loudness to a CSV file.
-- * 'peakCalculation' - If set to TRUE_PEAK, calculate and log the TruePeak for each output's audio track loudness.
-- * 'targetLkfs' - When you use Audio normalization (AudioNormalizationSettings), optionally use this setting to specify a target loudness. If you don't specify a value here, the encoder chooses a value for you, based on the algorithm that you choose for Algorithm (algorithm). If you choose algorithm 1770-1, the encoder will choose -24 LKFS; otherwise, the encoder will choose -23 LKFS.
mkAudioNormalizationSettings ::
  AudioNormalizationSettings
mkAudioNormalizationSettings =
  AudioNormalizationSettings'
    { algorithmControl = Lude.Nothing,
      targetLkfs = Lude.Nothing,
      peakCalculation = Lude.Nothing,
      correctionGateLevel = Lude.Nothing,
      algorithm = Lude.Nothing,
      loudnessLogging = Lude.Nothing
    }

-- | When enabled the output audio is corrected using the chosen algorithm. If disabled, the audio will be measured but not adjusted.
--
-- /Note:/ Consider using 'algorithmControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ansAlgorithmControl :: Lens.Lens' AudioNormalizationSettings (Lude.Maybe AudioNormalizationAlgorithmControl)
ansAlgorithmControl = Lens.lens (algorithmControl :: AudioNormalizationSettings -> Lude.Maybe AudioNormalizationAlgorithmControl) (\s a -> s {algorithmControl = a} :: AudioNormalizationSettings)
{-# DEPRECATED ansAlgorithmControl "Use generic-lens or generic-optics with 'algorithmControl' instead." #-}

-- | When you use Audio normalization (AudioNormalizationSettings), optionally use this setting to specify a target loudness. If you don't specify a value here, the encoder chooses a value for you, based on the algorithm that you choose for Algorithm (algorithm). If you choose algorithm 1770-1, the encoder will choose -24 LKFS; otherwise, the encoder will choose -23 LKFS.
--
-- /Note:/ Consider using 'targetLkfs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ansTargetLkfs :: Lens.Lens' AudioNormalizationSettings (Lude.Maybe Lude.Double)
ansTargetLkfs = Lens.lens (targetLkfs :: AudioNormalizationSettings -> Lude.Maybe Lude.Double) (\s a -> s {targetLkfs = a} :: AudioNormalizationSettings)
{-# DEPRECATED ansTargetLkfs "Use generic-lens or generic-optics with 'targetLkfs' instead." #-}

-- | If set to TRUE_PEAK, calculate and log the TruePeak for each output's audio track loudness.
--
-- /Note:/ Consider using 'peakCalculation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ansPeakCalculation :: Lens.Lens' AudioNormalizationSettings (Lude.Maybe AudioNormalizationPeakCalculation)
ansPeakCalculation = Lens.lens (peakCalculation :: AudioNormalizationSettings -> Lude.Maybe AudioNormalizationPeakCalculation) (\s a -> s {peakCalculation = a} :: AudioNormalizationSettings)
{-# DEPRECATED ansPeakCalculation "Use generic-lens or generic-optics with 'peakCalculation' instead." #-}

-- | Content measuring above this level will be corrected to the target level. Content measuring below this level will not be corrected.
--
-- /Note:/ Consider using 'correctionGateLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ansCorrectionGateLevel :: Lens.Lens' AudioNormalizationSettings (Lude.Maybe Lude.Int)
ansCorrectionGateLevel = Lens.lens (correctionGateLevel :: AudioNormalizationSettings -> Lude.Maybe Lude.Int) (\s a -> s {correctionGateLevel = a} :: AudioNormalizationSettings)
{-# DEPRECATED ansCorrectionGateLevel "Use generic-lens or generic-optics with 'correctionGateLevel' instead." #-}

-- | Choose one of the following audio normalization algorithms: ITU-R BS.1770-1: Ungated loudness. A measurement of ungated average loudness for an entire piece of content, suitable for measurement of short-form content under ATSC recommendation A/85. Supports up to 5.1 audio channels. ITU-R BS.1770-2: Gated loudness. A measurement of gated average loudness compliant with the requirements of EBU-R128. Supports up to 5.1 audio channels. ITU-R BS.1770-3: Modified peak. The same loudness measurement algorithm as 1770-2, with an updated true peak measurement. ITU-R BS.1770-4: Higher channel count. Allows for more audio channels than the other algorithms, including configurations such as 7.1.
--
-- /Note:/ Consider using 'algorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ansAlgorithm :: Lens.Lens' AudioNormalizationSettings (Lude.Maybe AudioNormalizationAlgorithm)
ansAlgorithm = Lens.lens (algorithm :: AudioNormalizationSettings -> Lude.Maybe AudioNormalizationAlgorithm) (\s a -> s {algorithm = a} :: AudioNormalizationSettings)
{-# DEPRECATED ansAlgorithm "Use generic-lens or generic-optics with 'algorithm' instead." #-}

-- | If set to LOG, log each output's audio track loudness to a CSV file.
--
-- /Note:/ Consider using 'loudnessLogging' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ansLoudnessLogging :: Lens.Lens' AudioNormalizationSettings (Lude.Maybe AudioNormalizationLoudnessLogging)
ansLoudnessLogging = Lens.lens (loudnessLogging :: AudioNormalizationSettings -> Lude.Maybe AudioNormalizationLoudnessLogging) (\s a -> s {loudnessLogging = a} :: AudioNormalizationSettings)
{-# DEPRECATED ansLoudnessLogging "Use generic-lens or generic-optics with 'loudnessLogging' instead." #-}

instance Lude.FromJSON AudioNormalizationSettings where
  parseJSON =
    Lude.withObject
      "AudioNormalizationSettings"
      ( \x ->
          AudioNormalizationSettings'
            Lude.<$> (x Lude..:? "algorithmControl")
            Lude.<*> (x Lude..:? "targetLkfs")
            Lude.<*> (x Lude..:? "peakCalculation")
            Lude.<*> (x Lude..:? "correctionGateLevel")
            Lude.<*> (x Lude..:? "algorithm")
            Lude.<*> (x Lude..:? "loudnessLogging")
      )

instance Lude.ToJSON AudioNormalizationSettings where
  toJSON AudioNormalizationSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("algorithmControl" Lude..=) Lude.<$> algorithmControl,
            ("targetLkfs" Lude..=) Lude.<$> targetLkfs,
            ("peakCalculation" Lude..=) Lude.<$> peakCalculation,
            ("correctionGateLevel" Lude..=) Lude.<$> correctionGateLevel,
            ("algorithm" Lude..=) Lude.<$> algorithm,
            ("loudnessLogging" Lude..=) Lude.<$> loudnessLogging
          ]
      )

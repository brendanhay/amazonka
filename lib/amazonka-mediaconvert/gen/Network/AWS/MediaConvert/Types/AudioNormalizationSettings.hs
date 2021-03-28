{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioNormalizationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.AudioNormalizationSettings
  ( AudioNormalizationSettings (..)
  -- * Smart constructor
  , mkAudioNormalizationSettings
  -- * Lenses
  , ansAlgorithm
  , ansAlgorithmControl
  , ansCorrectionGateLevel
  , ansLoudnessLogging
  , ansPeakCalculation
  , ansTargetLkfs
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.AudioNormalizationAlgorithm as Types
import qualified Network.AWS.MediaConvert.Types.AudioNormalizationAlgorithmControl as Types
import qualified Network.AWS.MediaConvert.Types.AudioNormalizationLoudnessLogging as Types
import qualified Network.AWS.MediaConvert.Types.AudioNormalizationPeakCalculation as Types
import qualified Network.AWS.Prelude as Core

-- | Advanced audio normalization settings. Ignore these settings unless you need to comply with a loudness standard.
--
-- /See:/ 'mkAudioNormalizationSettings' smart constructor.
data AudioNormalizationSettings = AudioNormalizationSettings'
  { algorithm :: Core.Maybe Types.AudioNormalizationAlgorithm
    -- ^ Choose one of the following audio normalization algorithms: ITU-R BS.1770-1: Ungated loudness. A measurement of ungated average loudness for an entire piece of content, suitable for measurement of short-form content under ATSC recommendation A/85. Supports up to 5.1 audio channels. ITU-R BS.1770-2: Gated loudness. A measurement of gated average loudness compliant with the requirements of EBU-R128. Supports up to 5.1 audio channels. ITU-R BS.1770-3: Modified peak. The same loudness measurement algorithm as 1770-2, with an updated true peak measurement. ITU-R BS.1770-4: Higher channel count. Allows for more audio channels than the other algorithms, including configurations such as 7.1.
  , algorithmControl :: Core.Maybe Types.AudioNormalizationAlgorithmControl
    -- ^ When enabled the output audio is corrected using the chosen algorithm. If disabled, the audio will be measured but not adjusted.
  , correctionGateLevel :: Core.Maybe Core.Int
    -- ^ Content measuring above this level will be corrected to the target level. Content measuring below this level will not be corrected.
  , loudnessLogging :: Core.Maybe Types.AudioNormalizationLoudnessLogging
    -- ^ If set to LOG, log each output's audio track loudness to a CSV file.
  , peakCalculation :: Core.Maybe Types.AudioNormalizationPeakCalculation
    -- ^ If set to TRUE_PEAK, calculate and log the TruePeak for each output's audio track loudness.
  , targetLkfs :: Core.Maybe Core.Double
    -- ^ When you use Audio normalization (AudioNormalizationSettings), optionally use this setting to specify a target loudness. If you don't specify a value here, the encoder chooses a value for you, based on the algorithm that you choose for Algorithm (algorithm). If you choose algorithm 1770-1, the encoder will choose -24 LKFS; otherwise, the encoder will choose -23 LKFS.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AudioNormalizationSettings' value with any optional fields omitted.
mkAudioNormalizationSettings
    :: AudioNormalizationSettings
mkAudioNormalizationSettings
  = AudioNormalizationSettings'{algorithm = Core.Nothing,
                                algorithmControl = Core.Nothing,
                                correctionGateLevel = Core.Nothing, loudnessLogging = Core.Nothing,
                                peakCalculation = Core.Nothing, targetLkfs = Core.Nothing}

-- | Choose one of the following audio normalization algorithms: ITU-R BS.1770-1: Ungated loudness. A measurement of ungated average loudness for an entire piece of content, suitable for measurement of short-form content under ATSC recommendation A/85. Supports up to 5.1 audio channels. ITU-R BS.1770-2: Gated loudness. A measurement of gated average loudness compliant with the requirements of EBU-R128. Supports up to 5.1 audio channels. ITU-R BS.1770-3: Modified peak. The same loudness measurement algorithm as 1770-2, with an updated true peak measurement. ITU-R BS.1770-4: Higher channel count. Allows for more audio channels than the other algorithms, including configurations such as 7.1.
--
-- /Note:/ Consider using 'algorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ansAlgorithm :: Lens.Lens' AudioNormalizationSettings (Core.Maybe Types.AudioNormalizationAlgorithm)
ansAlgorithm = Lens.field @"algorithm"
{-# INLINEABLE ansAlgorithm #-}
{-# DEPRECATED algorithm "Use generic-lens or generic-optics with 'algorithm' instead"  #-}

-- | When enabled the output audio is corrected using the chosen algorithm. If disabled, the audio will be measured but not adjusted.
--
-- /Note:/ Consider using 'algorithmControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ansAlgorithmControl :: Lens.Lens' AudioNormalizationSettings (Core.Maybe Types.AudioNormalizationAlgorithmControl)
ansAlgorithmControl = Lens.field @"algorithmControl"
{-# INLINEABLE ansAlgorithmControl #-}
{-# DEPRECATED algorithmControl "Use generic-lens or generic-optics with 'algorithmControl' instead"  #-}

-- | Content measuring above this level will be corrected to the target level. Content measuring below this level will not be corrected.
--
-- /Note:/ Consider using 'correctionGateLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ansCorrectionGateLevel :: Lens.Lens' AudioNormalizationSettings (Core.Maybe Core.Int)
ansCorrectionGateLevel = Lens.field @"correctionGateLevel"
{-# INLINEABLE ansCorrectionGateLevel #-}
{-# DEPRECATED correctionGateLevel "Use generic-lens or generic-optics with 'correctionGateLevel' instead"  #-}

-- | If set to LOG, log each output's audio track loudness to a CSV file.
--
-- /Note:/ Consider using 'loudnessLogging' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ansLoudnessLogging :: Lens.Lens' AudioNormalizationSettings (Core.Maybe Types.AudioNormalizationLoudnessLogging)
ansLoudnessLogging = Lens.field @"loudnessLogging"
{-# INLINEABLE ansLoudnessLogging #-}
{-# DEPRECATED loudnessLogging "Use generic-lens or generic-optics with 'loudnessLogging' instead"  #-}

-- | If set to TRUE_PEAK, calculate and log the TruePeak for each output's audio track loudness.
--
-- /Note:/ Consider using 'peakCalculation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ansPeakCalculation :: Lens.Lens' AudioNormalizationSettings (Core.Maybe Types.AudioNormalizationPeakCalculation)
ansPeakCalculation = Lens.field @"peakCalculation"
{-# INLINEABLE ansPeakCalculation #-}
{-# DEPRECATED peakCalculation "Use generic-lens or generic-optics with 'peakCalculation' instead"  #-}

-- | When you use Audio normalization (AudioNormalizationSettings), optionally use this setting to specify a target loudness. If you don't specify a value here, the encoder chooses a value for you, based on the algorithm that you choose for Algorithm (algorithm). If you choose algorithm 1770-1, the encoder will choose -24 LKFS; otherwise, the encoder will choose -23 LKFS.
--
-- /Note:/ Consider using 'targetLkfs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ansTargetLkfs :: Lens.Lens' AudioNormalizationSettings (Core.Maybe Core.Double)
ansTargetLkfs = Lens.field @"targetLkfs"
{-# INLINEABLE ansTargetLkfs #-}
{-# DEPRECATED targetLkfs "Use generic-lens or generic-optics with 'targetLkfs' instead"  #-}

instance Core.FromJSON AudioNormalizationSettings where
        toJSON AudioNormalizationSettings{..}
          = Core.object
              (Core.catMaybes
                 [("algorithm" Core..=) Core.<$> algorithm,
                  ("algorithmControl" Core..=) Core.<$> algorithmControl,
                  ("correctionGateLevel" Core..=) Core.<$> correctionGateLevel,
                  ("loudnessLogging" Core..=) Core.<$> loudnessLogging,
                  ("peakCalculation" Core..=) Core.<$> peakCalculation,
                  ("targetLkfs" Core..=) Core.<$> targetLkfs])

instance Core.FromJSON AudioNormalizationSettings where
        parseJSON
          = Core.withObject "AudioNormalizationSettings" Core.$
              \ x ->
                AudioNormalizationSettings' Core.<$>
                  (x Core..:? "algorithm") Core.<*> x Core..:? "algorithmControl"
                    Core.<*> x Core..:? "correctionGateLevel"
                    Core.<*> x Core..:? "loudnessLogging"
                    Core.<*> x Core..:? "peakCalculation"
                    Core.<*> x Core..:? "targetLkfs"

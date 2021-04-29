{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioNormalizationSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AudioNormalizationSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.AudioNormalizationAlgorithm
import Network.AWS.MediaConvert.Types.AudioNormalizationAlgorithmControl
import Network.AWS.MediaConvert.Types.AudioNormalizationLoudnessLogging
import Network.AWS.MediaConvert.Types.AudioNormalizationPeakCalculation
import qualified Network.AWS.Prelude as Prelude

-- | Advanced audio normalization settings. Ignore these settings unless you
-- need to comply with a loudness standard.
--
-- /See:/ 'newAudioNormalizationSettings' smart constructor.
data AudioNormalizationSettings = AudioNormalizationSettings'
  { -- | Content measuring above this level will be corrected to the target
    -- level. Content measuring below this level will not be corrected.
    correctionGateLevel :: Prelude.Maybe Prelude.Int,
    -- | Choose one of the following audio normalization algorithms: ITU-R
    -- BS.1770-1: Ungated loudness. A measurement of ungated average loudness
    -- for an entire piece of content, suitable for measurement of short-form
    -- content under ATSC recommendation A\/85. Supports up to 5.1 audio
    -- channels. ITU-R BS.1770-2: Gated loudness. A measurement of gated
    -- average loudness compliant with the requirements of EBU-R128. Supports
    -- up to 5.1 audio channels. ITU-R BS.1770-3: Modified peak. The same
    -- loudness measurement algorithm as 1770-2, with an updated true peak
    -- measurement. ITU-R BS.1770-4: Higher channel count. Allows for more
    -- audio channels than the other algorithms, including configurations such
    -- as 7.1.
    algorithm :: Prelude.Maybe AudioNormalizationAlgorithm,
    -- | If set to TRUE_PEAK, calculate and log the TruePeak for each output\'s
    -- audio track loudness.
    peakCalculation :: Prelude.Maybe AudioNormalizationPeakCalculation,
    -- | When you use Audio normalization (AudioNormalizationSettings),
    -- optionally use this setting to specify a target loudness. If you don\'t
    -- specify a value here, the encoder chooses a value for you, based on the
    -- algorithm that you choose for Algorithm (algorithm). If you choose
    -- algorithm 1770-1, the encoder will choose -24 LKFS; otherwise, the
    -- encoder will choose -23 LKFS.
    targetLkfs :: Prelude.Maybe Prelude.Double,
    -- | When enabled the output audio is corrected using the chosen algorithm.
    -- If disabled, the audio will be measured but not adjusted.
    algorithmControl :: Prelude.Maybe AudioNormalizationAlgorithmControl,
    -- | If set to LOG, log each output\'s audio track loudness to a CSV file.
    loudnessLogging :: Prelude.Maybe AudioNormalizationLoudnessLogging
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AudioNormalizationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'correctionGateLevel', 'audioNormalizationSettings_correctionGateLevel' - Content measuring above this level will be corrected to the target
-- level. Content measuring below this level will not be corrected.
--
-- 'algorithm', 'audioNormalizationSettings_algorithm' - Choose one of the following audio normalization algorithms: ITU-R
-- BS.1770-1: Ungated loudness. A measurement of ungated average loudness
-- for an entire piece of content, suitable for measurement of short-form
-- content under ATSC recommendation A\/85. Supports up to 5.1 audio
-- channels. ITU-R BS.1770-2: Gated loudness. A measurement of gated
-- average loudness compliant with the requirements of EBU-R128. Supports
-- up to 5.1 audio channels. ITU-R BS.1770-3: Modified peak. The same
-- loudness measurement algorithm as 1770-2, with an updated true peak
-- measurement. ITU-R BS.1770-4: Higher channel count. Allows for more
-- audio channels than the other algorithms, including configurations such
-- as 7.1.
--
-- 'peakCalculation', 'audioNormalizationSettings_peakCalculation' - If set to TRUE_PEAK, calculate and log the TruePeak for each output\'s
-- audio track loudness.
--
-- 'targetLkfs', 'audioNormalizationSettings_targetLkfs' - When you use Audio normalization (AudioNormalizationSettings),
-- optionally use this setting to specify a target loudness. If you don\'t
-- specify a value here, the encoder chooses a value for you, based on the
-- algorithm that you choose for Algorithm (algorithm). If you choose
-- algorithm 1770-1, the encoder will choose -24 LKFS; otherwise, the
-- encoder will choose -23 LKFS.
--
-- 'algorithmControl', 'audioNormalizationSettings_algorithmControl' - When enabled the output audio is corrected using the chosen algorithm.
-- If disabled, the audio will be measured but not adjusted.
--
-- 'loudnessLogging', 'audioNormalizationSettings_loudnessLogging' - If set to LOG, log each output\'s audio track loudness to a CSV file.
newAudioNormalizationSettings ::
  AudioNormalizationSettings
newAudioNormalizationSettings =
  AudioNormalizationSettings'
    { correctionGateLevel =
        Prelude.Nothing,
      algorithm = Prelude.Nothing,
      peakCalculation = Prelude.Nothing,
      targetLkfs = Prelude.Nothing,
      algorithmControl = Prelude.Nothing,
      loudnessLogging = Prelude.Nothing
    }

-- | Content measuring above this level will be corrected to the target
-- level. Content measuring below this level will not be corrected.
audioNormalizationSettings_correctionGateLevel :: Lens.Lens' AudioNormalizationSettings (Prelude.Maybe Prelude.Int)
audioNormalizationSettings_correctionGateLevel = Lens.lens (\AudioNormalizationSettings' {correctionGateLevel} -> correctionGateLevel) (\s@AudioNormalizationSettings' {} a -> s {correctionGateLevel = a} :: AudioNormalizationSettings)

-- | Choose one of the following audio normalization algorithms: ITU-R
-- BS.1770-1: Ungated loudness. A measurement of ungated average loudness
-- for an entire piece of content, suitable for measurement of short-form
-- content under ATSC recommendation A\/85. Supports up to 5.1 audio
-- channels. ITU-R BS.1770-2: Gated loudness. A measurement of gated
-- average loudness compliant with the requirements of EBU-R128. Supports
-- up to 5.1 audio channels. ITU-R BS.1770-3: Modified peak. The same
-- loudness measurement algorithm as 1770-2, with an updated true peak
-- measurement. ITU-R BS.1770-4: Higher channel count. Allows for more
-- audio channels than the other algorithms, including configurations such
-- as 7.1.
audioNormalizationSettings_algorithm :: Lens.Lens' AudioNormalizationSettings (Prelude.Maybe AudioNormalizationAlgorithm)
audioNormalizationSettings_algorithm = Lens.lens (\AudioNormalizationSettings' {algorithm} -> algorithm) (\s@AudioNormalizationSettings' {} a -> s {algorithm = a} :: AudioNormalizationSettings)

-- | If set to TRUE_PEAK, calculate and log the TruePeak for each output\'s
-- audio track loudness.
audioNormalizationSettings_peakCalculation :: Lens.Lens' AudioNormalizationSettings (Prelude.Maybe AudioNormalizationPeakCalculation)
audioNormalizationSettings_peakCalculation = Lens.lens (\AudioNormalizationSettings' {peakCalculation} -> peakCalculation) (\s@AudioNormalizationSettings' {} a -> s {peakCalculation = a} :: AudioNormalizationSettings)

-- | When you use Audio normalization (AudioNormalizationSettings),
-- optionally use this setting to specify a target loudness. If you don\'t
-- specify a value here, the encoder chooses a value for you, based on the
-- algorithm that you choose for Algorithm (algorithm). If you choose
-- algorithm 1770-1, the encoder will choose -24 LKFS; otherwise, the
-- encoder will choose -23 LKFS.
audioNormalizationSettings_targetLkfs :: Lens.Lens' AudioNormalizationSettings (Prelude.Maybe Prelude.Double)
audioNormalizationSettings_targetLkfs = Lens.lens (\AudioNormalizationSettings' {targetLkfs} -> targetLkfs) (\s@AudioNormalizationSettings' {} a -> s {targetLkfs = a} :: AudioNormalizationSettings)

-- | When enabled the output audio is corrected using the chosen algorithm.
-- If disabled, the audio will be measured but not adjusted.
audioNormalizationSettings_algorithmControl :: Lens.Lens' AudioNormalizationSettings (Prelude.Maybe AudioNormalizationAlgorithmControl)
audioNormalizationSettings_algorithmControl = Lens.lens (\AudioNormalizationSettings' {algorithmControl} -> algorithmControl) (\s@AudioNormalizationSettings' {} a -> s {algorithmControl = a} :: AudioNormalizationSettings)

-- | If set to LOG, log each output\'s audio track loudness to a CSV file.
audioNormalizationSettings_loudnessLogging :: Lens.Lens' AudioNormalizationSettings (Prelude.Maybe AudioNormalizationLoudnessLogging)
audioNormalizationSettings_loudnessLogging = Lens.lens (\AudioNormalizationSettings' {loudnessLogging} -> loudnessLogging) (\s@AudioNormalizationSettings' {} a -> s {loudnessLogging = a} :: AudioNormalizationSettings)

instance Prelude.FromJSON AudioNormalizationSettings where
  parseJSON =
    Prelude.withObject
      "AudioNormalizationSettings"
      ( \x ->
          AudioNormalizationSettings'
            Prelude.<$> (x Prelude..:? "correctionGateLevel")
            Prelude.<*> (x Prelude..:? "algorithm")
            Prelude.<*> (x Prelude..:? "peakCalculation")
            Prelude.<*> (x Prelude..:? "targetLkfs")
            Prelude.<*> (x Prelude..:? "algorithmControl")
            Prelude.<*> (x Prelude..:? "loudnessLogging")
      )

instance Prelude.Hashable AudioNormalizationSettings

instance Prelude.NFData AudioNormalizationSettings

instance Prelude.ToJSON AudioNormalizationSettings where
  toJSON AudioNormalizationSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("correctionGateLevel" Prelude..=)
              Prelude.<$> correctionGateLevel,
            ("algorithm" Prelude..=) Prelude.<$> algorithm,
            ("peakCalculation" Prelude..=)
              Prelude.<$> peakCalculation,
            ("targetLkfs" Prelude..=) Prelude.<$> targetLkfs,
            ("algorithmControl" Prelude..=)
              Prelude.<$> algorithmControl,
            ("loudnessLogging" Prelude..=)
              Prelude.<$> loudnessLogging
          ]
      )

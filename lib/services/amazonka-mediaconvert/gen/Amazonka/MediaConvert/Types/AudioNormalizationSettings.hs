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
-- Module      : Amazonka.MediaConvert.Types.AudioNormalizationSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.AudioNormalizationSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.AudioNormalizationAlgorithm
import Amazonka.MediaConvert.Types.AudioNormalizationAlgorithmControl
import Amazonka.MediaConvert.Types.AudioNormalizationLoudnessLogging
import Amazonka.MediaConvert.Types.AudioNormalizationPeakCalculation
import qualified Amazonka.Prelude as Prelude

-- | Advanced audio normalization settings. Ignore these settings unless you
-- need to comply with a loudness standard.
--
-- /See:/ 'newAudioNormalizationSettings' smart constructor.
data AudioNormalizationSettings = AudioNormalizationSettings'
  { -- | Choose one of the following audio normalization algorithms: ITU-R
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
    -- | When enabled the output audio is corrected using the chosen algorithm.
    -- If disabled, the audio will be measured but not adjusted.
    algorithmControl :: Prelude.Maybe AudioNormalizationAlgorithmControl,
    -- | Content measuring above this level will be corrected to the target
    -- level. Content measuring below this level will not be corrected.
    correctionGateLevel :: Prelude.Maybe Prelude.Int,
    -- | If set to LOG, log each output\'s audio track loudness to a CSV file.
    loudnessLogging :: Prelude.Maybe AudioNormalizationLoudnessLogging,
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
    -- | Specify the True-peak limiter threshold in decibels relative to full
    -- scale (dBFS). The peak inter-audio sample loudness in your output will
    -- be limited to the value that you specify, without affecting the overall
    -- target LKFS. Enter a value from 0 to -8. Leave blank to use the default
    -- value 0.
    truePeakLimiterThreshold :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AudioNormalizationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'algorithmControl', 'audioNormalizationSettings_algorithmControl' - When enabled the output audio is corrected using the chosen algorithm.
-- If disabled, the audio will be measured but not adjusted.
--
-- 'correctionGateLevel', 'audioNormalizationSettings_correctionGateLevel' - Content measuring above this level will be corrected to the target
-- level. Content measuring below this level will not be corrected.
--
-- 'loudnessLogging', 'audioNormalizationSettings_loudnessLogging' - If set to LOG, log each output\'s audio track loudness to a CSV file.
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
-- 'truePeakLimiterThreshold', 'audioNormalizationSettings_truePeakLimiterThreshold' - Specify the True-peak limiter threshold in decibels relative to full
-- scale (dBFS). The peak inter-audio sample loudness in your output will
-- be limited to the value that you specify, without affecting the overall
-- target LKFS. Enter a value from 0 to -8. Leave blank to use the default
-- value 0.
newAudioNormalizationSettings ::
  AudioNormalizationSettings
newAudioNormalizationSettings =
  AudioNormalizationSettings'
    { algorithm =
        Prelude.Nothing,
      algorithmControl = Prelude.Nothing,
      correctionGateLevel = Prelude.Nothing,
      loudnessLogging = Prelude.Nothing,
      peakCalculation = Prelude.Nothing,
      targetLkfs = Prelude.Nothing,
      truePeakLimiterThreshold = Prelude.Nothing
    }

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

-- | When enabled the output audio is corrected using the chosen algorithm.
-- If disabled, the audio will be measured but not adjusted.
audioNormalizationSettings_algorithmControl :: Lens.Lens' AudioNormalizationSettings (Prelude.Maybe AudioNormalizationAlgorithmControl)
audioNormalizationSettings_algorithmControl = Lens.lens (\AudioNormalizationSettings' {algorithmControl} -> algorithmControl) (\s@AudioNormalizationSettings' {} a -> s {algorithmControl = a} :: AudioNormalizationSettings)

-- | Content measuring above this level will be corrected to the target
-- level. Content measuring below this level will not be corrected.
audioNormalizationSettings_correctionGateLevel :: Lens.Lens' AudioNormalizationSettings (Prelude.Maybe Prelude.Int)
audioNormalizationSettings_correctionGateLevel = Lens.lens (\AudioNormalizationSettings' {correctionGateLevel} -> correctionGateLevel) (\s@AudioNormalizationSettings' {} a -> s {correctionGateLevel = a} :: AudioNormalizationSettings)

-- | If set to LOG, log each output\'s audio track loudness to a CSV file.
audioNormalizationSettings_loudnessLogging :: Lens.Lens' AudioNormalizationSettings (Prelude.Maybe AudioNormalizationLoudnessLogging)
audioNormalizationSettings_loudnessLogging = Lens.lens (\AudioNormalizationSettings' {loudnessLogging} -> loudnessLogging) (\s@AudioNormalizationSettings' {} a -> s {loudnessLogging = a} :: AudioNormalizationSettings)

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

-- | Specify the True-peak limiter threshold in decibels relative to full
-- scale (dBFS). The peak inter-audio sample loudness in your output will
-- be limited to the value that you specify, without affecting the overall
-- target LKFS. Enter a value from 0 to -8. Leave blank to use the default
-- value 0.
audioNormalizationSettings_truePeakLimiterThreshold :: Lens.Lens' AudioNormalizationSettings (Prelude.Maybe Prelude.Double)
audioNormalizationSettings_truePeakLimiterThreshold = Lens.lens (\AudioNormalizationSettings' {truePeakLimiterThreshold} -> truePeakLimiterThreshold) (\s@AudioNormalizationSettings' {} a -> s {truePeakLimiterThreshold = a} :: AudioNormalizationSettings)

instance Data.FromJSON AudioNormalizationSettings where
  parseJSON =
    Data.withObject
      "AudioNormalizationSettings"
      ( \x ->
          AudioNormalizationSettings'
            Prelude.<$> (x Data..:? "algorithm")
            Prelude.<*> (x Data..:? "algorithmControl")
            Prelude.<*> (x Data..:? "correctionGateLevel")
            Prelude.<*> (x Data..:? "loudnessLogging")
            Prelude.<*> (x Data..:? "peakCalculation")
            Prelude.<*> (x Data..:? "targetLkfs")
            Prelude.<*> (x Data..:? "truePeakLimiterThreshold")
      )

instance Prelude.Hashable AudioNormalizationSettings where
  hashWithSalt _salt AudioNormalizationSettings' {..} =
    _salt
      `Prelude.hashWithSalt` algorithm
      `Prelude.hashWithSalt` algorithmControl
      `Prelude.hashWithSalt` correctionGateLevel
      `Prelude.hashWithSalt` loudnessLogging
      `Prelude.hashWithSalt` peakCalculation
      `Prelude.hashWithSalt` targetLkfs
      `Prelude.hashWithSalt` truePeakLimiterThreshold

instance Prelude.NFData AudioNormalizationSettings where
  rnf AudioNormalizationSettings' {..} =
    Prelude.rnf algorithm
      `Prelude.seq` Prelude.rnf algorithmControl
      `Prelude.seq` Prelude.rnf correctionGateLevel
      `Prelude.seq` Prelude.rnf loudnessLogging
      `Prelude.seq` Prelude.rnf peakCalculation
      `Prelude.seq` Prelude.rnf targetLkfs
      `Prelude.seq` Prelude.rnf truePeakLimiterThreshold

instance Data.ToJSON AudioNormalizationSettings where
  toJSON AudioNormalizationSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("algorithm" Data..=) Prelude.<$> algorithm,
            ("algorithmControl" Data..=)
              Prelude.<$> algorithmControl,
            ("correctionGateLevel" Data..=)
              Prelude.<$> correctionGateLevel,
            ("loudnessLogging" Data..=)
              Prelude.<$> loudnessLogging,
            ("peakCalculation" Data..=)
              Prelude.<$> peakCalculation,
            ("targetLkfs" Data..=) Prelude.<$> targetLkfs,
            ("truePeakLimiterThreshold" Data..=)
              Prelude.<$> truePeakLimiterThreshold
          ]
      )

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioNormalizationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AudioNormalizationSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.AudioNormalizationAlgorithm
import Network.AWS.MediaConvert.Types.AudioNormalizationAlgorithmControl
import Network.AWS.MediaConvert.Types.AudioNormalizationLoudnessLogging
import Network.AWS.MediaConvert.Types.AudioNormalizationPeakCalculation
import Network.AWS.Prelude

-- | Advanced audio normalization settings. Ignore these settings unless you need to comply with a loudness standard.
--
-- /See:/ 'audioNormalizationSettings' smart constructor.
data AudioNormalizationSettings = AudioNormalizationSettings'
  { _ansAlgorithmControl ::
      !( Maybe
           AudioNormalizationAlgorithmControl
       ),
    _ansTargetLkfs :: !(Maybe Double),
    _ansPeakCalculation ::
      !( Maybe
           AudioNormalizationPeakCalculation
       ),
    _ansCorrectionGateLevel ::
      !(Maybe Int),
    _ansAlgorithm ::
      !(Maybe AudioNormalizationAlgorithm),
    _ansLoudnessLogging ::
      !( Maybe
           AudioNormalizationLoudnessLogging
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AudioNormalizationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ansAlgorithmControl' - When enabled the output audio is corrected using the chosen algorithm. If disabled, the audio will be measured but not adjusted.
--
-- * 'ansTargetLkfs' - When you use Audio normalization (AudioNormalizationSettings), optionally use this setting to specify a target loudness. If you don't specify a value here, the encoder chooses a value for you, based on the algorithm that you choose for Algorithm (algorithm). If you choose algorithm 1770-1, the encoder will choose -24 LKFS; otherwise, the encoder will choose -23 LKFS.
--
-- * 'ansPeakCalculation' - If set to TRUE_PEAK, calculate and log the TruePeak for each output's audio track loudness.
--
-- * 'ansCorrectionGateLevel' - Content measuring above this level will be corrected to the target level. Content measuring below this level will not be corrected.
--
-- * 'ansAlgorithm' - Choose one of the following audio normalization algorithms: ITU-R BS.1770-1: Ungated loudness. A measurement of ungated average loudness for an entire piece of content, suitable for measurement of short-form content under ATSC recommendation A/85. Supports up to 5.1 audio channels. ITU-R BS.1770-2: Gated loudness. A measurement of gated average loudness compliant with the requirements of EBU-R128. Supports up to 5.1 audio channels. ITU-R BS.1770-3: Modified peak. The same loudness measurement algorithm as 1770-2, with an updated true peak measurement. ITU-R BS.1770-4: Higher channel count. Allows for more audio channels than the other algorithms, including configurations such as 7.1.
--
-- * 'ansLoudnessLogging' - If set to LOG, log each output's audio track loudness to a CSV file.
audioNormalizationSettings ::
  AudioNormalizationSettings
audioNormalizationSettings =
  AudioNormalizationSettings'
    { _ansAlgorithmControl = Nothing,
      _ansTargetLkfs = Nothing,
      _ansPeakCalculation = Nothing,
      _ansCorrectionGateLevel = Nothing,
      _ansAlgorithm = Nothing,
      _ansLoudnessLogging = Nothing
    }

-- | When enabled the output audio is corrected using the chosen algorithm. If disabled, the audio will be measured but not adjusted.
ansAlgorithmControl :: Lens' AudioNormalizationSettings (Maybe AudioNormalizationAlgorithmControl)
ansAlgorithmControl = lens _ansAlgorithmControl (\s a -> s {_ansAlgorithmControl = a})

-- | When you use Audio normalization (AudioNormalizationSettings), optionally use this setting to specify a target loudness. If you don't specify a value here, the encoder chooses a value for you, based on the algorithm that you choose for Algorithm (algorithm). If you choose algorithm 1770-1, the encoder will choose -24 LKFS; otherwise, the encoder will choose -23 LKFS.
ansTargetLkfs :: Lens' AudioNormalizationSettings (Maybe Double)
ansTargetLkfs = lens _ansTargetLkfs (\s a -> s {_ansTargetLkfs = a})

-- | If set to TRUE_PEAK, calculate and log the TruePeak for each output's audio track loudness.
ansPeakCalculation :: Lens' AudioNormalizationSettings (Maybe AudioNormalizationPeakCalculation)
ansPeakCalculation = lens _ansPeakCalculation (\s a -> s {_ansPeakCalculation = a})

-- | Content measuring above this level will be corrected to the target level. Content measuring below this level will not be corrected.
ansCorrectionGateLevel :: Lens' AudioNormalizationSettings (Maybe Int)
ansCorrectionGateLevel = lens _ansCorrectionGateLevel (\s a -> s {_ansCorrectionGateLevel = a})

-- | Choose one of the following audio normalization algorithms: ITU-R BS.1770-1: Ungated loudness. A measurement of ungated average loudness for an entire piece of content, suitable for measurement of short-form content under ATSC recommendation A/85. Supports up to 5.1 audio channels. ITU-R BS.1770-2: Gated loudness. A measurement of gated average loudness compliant with the requirements of EBU-R128. Supports up to 5.1 audio channels. ITU-R BS.1770-3: Modified peak. The same loudness measurement algorithm as 1770-2, with an updated true peak measurement. ITU-R BS.1770-4: Higher channel count. Allows for more audio channels than the other algorithms, including configurations such as 7.1.
ansAlgorithm :: Lens' AudioNormalizationSettings (Maybe AudioNormalizationAlgorithm)
ansAlgorithm = lens _ansAlgorithm (\s a -> s {_ansAlgorithm = a})

-- | If set to LOG, log each output's audio track loudness to a CSV file.
ansLoudnessLogging :: Lens' AudioNormalizationSettings (Maybe AudioNormalizationLoudnessLogging)
ansLoudnessLogging = lens _ansLoudnessLogging (\s a -> s {_ansLoudnessLogging = a})

instance FromJSON AudioNormalizationSettings where
  parseJSON =
    withObject
      "AudioNormalizationSettings"
      ( \x ->
          AudioNormalizationSettings'
            <$> (x .:? "algorithmControl")
            <*> (x .:? "targetLkfs")
            <*> (x .:? "peakCalculation")
            <*> (x .:? "correctionGateLevel")
            <*> (x .:? "algorithm")
            <*> (x .:? "loudnessLogging")
      )

instance Hashable AudioNormalizationSettings

instance NFData AudioNormalizationSettings

instance ToJSON AudioNormalizationSettings where
  toJSON AudioNormalizationSettings' {..} =
    object
      ( catMaybes
          [ ("algorithmControl" .=) <$> _ansAlgorithmControl,
            ("targetLkfs" .=) <$> _ansTargetLkfs,
            ("peakCalculation" .=) <$> _ansPeakCalculation,
            ("correctionGateLevel" .=) <$> _ansCorrectionGateLevel,
            ("algorithm" .=) <$> _ansAlgorithm,
            ("loudnessLogging" .=) <$> _ansLoudnessLogging
          ]
      )

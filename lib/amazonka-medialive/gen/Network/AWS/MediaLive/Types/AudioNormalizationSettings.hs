{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioNormalizationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioNormalizationSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.AudioNormalizationAlgorithm
import Network.AWS.MediaLive.Types.AudioNormalizationAlgorithmControl
import Network.AWS.Prelude

-- | Audio Normalization Settings
--
-- /See:/ 'audioNormalizationSettings' smart constructor.
data AudioNormalizationSettings = AudioNormalizationSettings'
  { _ansAlgorithmControl ::
      !( Maybe
           AudioNormalizationAlgorithmControl
       ),
    _ansTargetLkfs :: !(Maybe Double),
    _ansAlgorithm ::
      !(Maybe AudioNormalizationAlgorithm)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AudioNormalizationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ansAlgorithmControl' - When set to correctAudio the output audio is corrected using the chosen algorithm. If set to measureOnly, the audio will be measured but not adjusted.
--
-- * 'ansTargetLkfs' - Target LKFS(loudness) to adjust volume to. If no value is entered, a default value will be used according to the chosen algorithm.  The CALM Act (1770-1) recommends a target of -24 LKFS. The EBU R-128 specification (1770-2) recommends a target of -23 LKFS.
--
-- * 'ansAlgorithm' - Audio normalization algorithm to use. itu17701 conforms to the CALM Act specification, itu17702 conforms to the EBU R-128 specification.
audioNormalizationSettings ::
  AudioNormalizationSettings
audioNormalizationSettings =
  AudioNormalizationSettings'
    { _ansAlgorithmControl = Nothing,
      _ansTargetLkfs = Nothing,
      _ansAlgorithm = Nothing
    }

-- | When set to correctAudio the output audio is corrected using the chosen algorithm. If set to measureOnly, the audio will be measured but not adjusted.
ansAlgorithmControl :: Lens' AudioNormalizationSettings (Maybe AudioNormalizationAlgorithmControl)
ansAlgorithmControl = lens _ansAlgorithmControl (\s a -> s {_ansAlgorithmControl = a})

-- | Target LKFS(loudness) to adjust volume to. If no value is entered, a default value will be used according to the chosen algorithm.  The CALM Act (1770-1) recommends a target of -24 LKFS. The EBU R-128 specification (1770-2) recommends a target of -23 LKFS.
ansTargetLkfs :: Lens' AudioNormalizationSettings (Maybe Double)
ansTargetLkfs = lens _ansTargetLkfs (\s a -> s {_ansTargetLkfs = a})

-- | Audio normalization algorithm to use. itu17701 conforms to the CALM Act specification, itu17702 conforms to the EBU R-128 specification.
ansAlgorithm :: Lens' AudioNormalizationSettings (Maybe AudioNormalizationAlgorithm)
ansAlgorithm = lens _ansAlgorithm (\s a -> s {_ansAlgorithm = a})

instance FromJSON AudioNormalizationSettings where
  parseJSON =
    withObject
      "AudioNormalizationSettings"
      ( \x ->
          AudioNormalizationSettings'
            <$> (x .:? "algorithmControl")
            <*> (x .:? "targetLkfs")
            <*> (x .:? "algorithm")
      )

instance Hashable AudioNormalizationSettings

instance NFData AudioNormalizationSettings

instance ToJSON AudioNormalizationSettings where
  toJSON AudioNormalizationSettings' {..} =
    object
      ( catMaybes
          [ ("algorithmControl" .=) <$> _ansAlgorithmControl,
            ("targetLkfs" .=) <$> _ansTargetLkfs,
            ("algorithm" .=) <$> _ansAlgorithm
          ]
      )

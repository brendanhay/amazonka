{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265QvbrSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265QvbrSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Settings for quality-defined variable bitrate encoding with the H.265 codec. Required when you set Rate control mode to QVBR. Not valid when you set Rate control mode to a value other than QVBR, or when you don't define Rate control mode.
--
-- /See:/ 'h265QvbrSettings' smart constructor.
data H265QvbrSettings = H265QvbrSettings'
  { _hqsQvbrQualityLevelFineTune ::
      !(Maybe Double),
    _hqsMaxAverageBitrate :: !(Maybe Nat),
    _hqsQvbrQualityLevel :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'H265QvbrSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hqsQvbrQualityLevelFineTune' - Optional. Specify a value here to set the QVBR quality to a level that is between whole numbers. For example, if you want your QVBR quality level to be 7.33, set qvbrQualityLevel to 7 and set qvbrQualityLevelFineTune to .33. MediaConvert rounds your QVBR quality level to the nearest third of a whole number. For example, if you set qvbrQualityLevel to 7 and you set qvbrQualityLevelFineTune to .25, your actual QVBR quality level is 7.33.
--
-- * 'hqsMaxAverageBitrate' - Use this setting only when Rate control mode is QVBR and Quality tuning level is Multi-pass HQ. For Max average bitrate values suited to the complexity of your input video, the service limits the average bitrate of the video part of this output to the value that you choose. That is, the total size of the video element is less than or equal to the value you set multiplied by the number of seconds of encoded output.
--
-- * 'hqsQvbrQualityLevel' - Required when you use QVBR rate control mode. That is, when you specify qvbrSettings within h265Settings. Specify the general target quality level for this output, from 1 to 10. Use higher numbers for greater quality. Level 10 results in nearly lossless compression. The quality level for most broadcast-quality transcodes is between 6 and 9. Optionally, to specify a value between whole numbers, also provide a value for the setting qvbrQualityLevelFineTune. For example, if you want your QVBR quality level to be 7.33, set qvbrQualityLevel to 7 and set qvbrQualityLevelFineTune to .33.
h265QvbrSettings ::
  H265QvbrSettings
h265QvbrSettings =
  H265QvbrSettings'
    { _hqsQvbrQualityLevelFineTune = Nothing,
      _hqsMaxAverageBitrate = Nothing,
      _hqsQvbrQualityLevel = Nothing
    }

-- | Optional. Specify a value here to set the QVBR quality to a level that is between whole numbers. For example, if you want your QVBR quality level to be 7.33, set qvbrQualityLevel to 7 and set qvbrQualityLevelFineTune to .33. MediaConvert rounds your QVBR quality level to the nearest third of a whole number. For example, if you set qvbrQualityLevel to 7 and you set qvbrQualityLevelFineTune to .25, your actual QVBR quality level is 7.33.
hqsQvbrQualityLevelFineTune :: Lens' H265QvbrSettings (Maybe Double)
hqsQvbrQualityLevelFineTune = lens _hqsQvbrQualityLevelFineTune (\s a -> s {_hqsQvbrQualityLevelFineTune = a})

-- | Use this setting only when Rate control mode is QVBR and Quality tuning level is Multi-pass HQ. For Max average bitrate values suited to the complexity of your input video, the service limits the average bitrate of the video part of this output to the value that you choose. That is, the total size of the video element is less than or equal to the value you set multiplied by the number of seconds of encoded output.
hqsMaxAverageBitrate :: Lens' H265QvbrSettings (Maybe Natural)
hqsMaxAverageBitrate = lens _hqsMaxAverageBitrate (\s a -> s {_hqsMaxAverageBitrate = a}) . mapping _Nat

-- | Required when you use QVBR rate control mode. That is, when you specify qvbrSettings within h265Settings. Specify the general target quality level for this output, from 1 to 10. Use higher numbers for greater quality. Level 10 results in nearly lossless compression. The quality level for most broadcast-quality transcodes is between 6 and 9. Optionally, to specify a value between whole numbers, also provide a value for the setting qvbrQualityLevelFineTune. For example, if you want your QVBR quality level to be 7.33, set qvbrQualityLevel to 7 and set qvbrQualityLevelFineTune to .33.
hqsQvbrQualityLevel :: Lens' H265QvbrSettings (Maybe Natural)
hqsQvbrQualityLevel = lens _hqsQvbrQualityLevel (\s a -> s {_hqsQvbrQualityLevel = a}) . mapping _Nat

instance FromJSON H265QvbrSettings where
  parseJSON =
    withObject
      "H265QvbrSettings"
      ( \x ->
          H265QvbrSettings'
            <$> (x .:? "qvbrQualityLevelFineTune")
            <*> (x .:? "maxAverageBitrate")
            <*> (x .:? "qvbrQualityLevel")
      )

instance Hashable H265QvbrSettings

instance NFData H265QvbrSettings

instance ToJSON H265QvbrSettings where
  toJSON H265QvbrSettings' {..} =
    object
      ( catMaybes
          [ ("qvbrQualityLevelFineTune" .=) <$> _hqsQvbrQualityLevelFineTune,
            ("maxAverageBitrate" .=) <$> _hqsMaxAverageBitrate,
            ("qvbrQualityLevel" .=) <$> _hqsQvbrQualityLevel
          ]
      )

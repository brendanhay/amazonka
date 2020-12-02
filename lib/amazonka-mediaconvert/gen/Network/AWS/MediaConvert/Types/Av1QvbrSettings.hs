{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Av1QvbrSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Av1QvbrSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Settings for quality-defined variable bitrate encoding with the AV1 codec. Required when you set Rate control mode to QVBR. Not valid when you set Rate control mode to a value other than QVBR, or when you don't define Rate control mode.
--
-- /See:/ 'av1QvbrSettings' smart constructor.
data Av1QvbrSettings = Av1QvbrSettings'
  { _aqsQvbrQualityLevelFineTune ::
      !(Maybe Double),
    _aqsQvbrQualityLevel :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Av1QvbrSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aqsQvbrQualityLevelFineTune' - Optional. Specify a value here to set the QVBR quality to a level that is between whole numbers. For example, if you want your QVBR quality level to be 7.33, set qvbrQualityLevel to 7 and set qvbrQualityLevelFineTune to .33. MediaConvert rounds your QVBR quality level to the nearest third of a whole number. For example, if you set qvbrQualityLevel to 7 and you set qvbrQualityLevelFineTune to .25, your actual QVBR quality level is 7.33.
--
-- * 'aqsQvbrQualityLevel' - Required when you use QVBR rate control mode. That is, when you specify qvbrSettings within av1Settings. Specify the general target quality level for this output, from 1 to 10. Use higher numbers for greater quality. Level 10 results in nearly lossless compression. The quality level for most broadcast-quality transcodes is between 6 and 9. Optionally, to specify a value between whole numbers, also provide a value for the setting qvbrQualityLevelFineTune. For example, if you want your QVBR quality level to be 7.33, set qvbrQualityLevel to 7 and set qvbrQualityLevelFineTune to .33.
av1QvbrSettings ::
  Av1QvbrSettings
av1QvbrSettings =
  Av1QvbrSettings'
    { _aqsQvbrQualityLevelFineTune = Nothing,
      _aqsQvbrQualityLevel = Nothing
    }

-- | Optional. Specify a value here to set the QVBR quality to a level that is between whole numbers. For example, if you want your QVBR quality level to be 7.33, set qvbrQualityLevel to 7 and set qvbrQualityLevelFineTune to .33. MediaConvert rounds your QVBR quality level to the nearest third of a whole number. For example, if you set qvbrQualityLevel to 7 and you set qvbrQualityLevelFineTune to .25, your actual QVBR quality level is 7.33.
aqsQvbrQualityLevelFineTune :: Lens' Av1QvbrSettings (Maybe Double)
aqsQvbrQualityLevelFineTune = lens _aqsQvbrQualityLevelFineTune (\s a -> s {_aqsQvbrQualityLevelFineTune = a})

-- | Required when you use QVBR rate control mode. That is, when you specify qvbrSettings within av1Settings. Specify the general target quality level for this output, from 1 to 10. Use higher numbers for greater quality. Level 10 results in nearly lossless compression. The quality level for most broadcast-quality transcodes is between 6 and 9. Optionally, to specify a value between whole numbers, also provide a value for the setting qvbrQualityLevelFineTune. For example, if you want your QVBR quality level to be 7.33, set qvbrQualityLevel to 7 and set qvbrQualityLevelFineTune to .33.
aqsQvbrQualityLevel :: Lens' Av1QvbrSettings (Maybe Natural)
aqsQvbrQualityLevel = lens _aqsQvbrQualityLevel (\s a -> s {_aqsQvbrQualityLevel = a}) . mapping _Nat

instance FromJSON Av1QvbrSettings where
  parseJSON =
    withObject
      "Av1QvbrSettings"
      ( \x ->
          Av1QvbrSettings'
            <$> (x .:? "qvbrQualityLevelFineTune") <*> (x .:? "qvbrQualityLevel")
      )

instance Hashable Av1QvbrSettings

instance NFData Av1QvbrSettings

instance ToJSON Av1QvbrSettings where
  toJSON Av1QvbrSettings' {..} =
    object
      ( catMaybes
          [ ("qvbrQualityLevelFineTune" .=) <$> _aqsQvbrQualityLevelFineTune,
            ("qvbrQualityLevel" .=) <$> _aqsQvbrQualityLevel
          ]
      )

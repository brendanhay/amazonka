{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AutomatedAbrSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AutomatedAbrSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Use automated ABR to have MediaConvert set up the renditions in your ABR package for you automatically, based on characteristics of your input video. This feature optimizes video quality while minimizing the overall size of your ABR package.
--
-- /See:/ 'automatedAbrSettings' smart constructor.
data AutomatedAbrSettings = AutomatedAbrSettings'
  { _aasMaxRenditions ::
      !(Maybe Nat),
    _aasMaxAbrBitrate :: !(Maybe Nat),
    _aasMinAbrBitrate :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AutomatedAbrSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aasMaxRenditions' - Optional. The maximum number of renditions that MediaConvert will create in your automated ABR stack. The number of renditions is determined automatically, based on analysis of each job, but will never exceed this limit. When you set this to Auto in the console, which is equivalent to excluding it from your JSON job specification, MediaConvert defaults to a limit of 15.
--
-- * 'aasMaxAbrBitrate' - Optional. The maximum target bit rate used in your automated ABR stack. Use this value to set an upper limit on the bandwidth consumed by the highest-quality rendition. This is the rendition that is delivered to viewers with the fastest internet connections. If you don't specify a value, MediaConvert uses 8,000,000 (8 mb/s) by default.
--
-- * 'aasMinAbrBitrate' - Optional. The minimum target bitrate used in your automated ABR stack. Use this value to set a lower limit on the bitrate of video delivered to viewers with slow internet connections. If you don't specify a value, MediaConvert uses 600,000 (600 kb/s) by default.
automatedAbrSettings ::
  AutomatedAbrSettings
automatedAbrSettings =
  AutomatedAbrSettings'
    { _aasMaxRenditions = Nothing,
      _aasMaxAbrBitrate = Nothing,
      _aasMinAbrBitrate = Nothing
    }

-- | Optional. The maximum number of renditions that MediaConvert will create in your automated ABR stack. The number of renditions is determined automatically, based on analysis of each job, but will never exceed this limit. When you set this to Auto in the console, which is equivalent to excluding it from your JSON job specification, MediaConvert defaults to a limit of 15.
aasMaxRenditions :: Lens' AutomatedAbrSettings (Maybe Natural)
aasMaxRenditions = lens _aasMaxRenditions (\s a -> s {_aasMaxRenditions = a}) . mapping _Nat

-- | Optional. The maximum target bit rate used in your automated ABR stack. Use this value to set an upper limit on the bandwidth consumed by the highest-quality rendition. This is the rendition that is delivered to viewers with the fastest internet connections. If you don't specify a value, MediaConvert uses 8,000,000 (8 mb/s) by default.
aasMaxAbrBitrate :: Lens' AutomatedAbrSettings (Maybe Natural)
aasMaxAbrBitrate = lens _aasMaxAbrBitrate (\s a -> s {_aasMaxAbrBitrate = a}) . mapping _Nat

-- | Optional. The minimum target bitrate used in your automated ABR stack. Use this value to set a lower limit on the bitrate of video delivered to viewers with slow internet connections. If you don't specify a value, MediaConvert uses 600,000 (600 kb/s) by default.
aasMinAbrBitrate :: Lens' AutomatedAbrSettings (Maybe Natural)
aasMinAbrBitrate = lens _aasMinAbrBitrate (\s a -> s {_aasMinAbrBitrate = a}) . mapping _Nat

instance FromJSON AutomatedAbrSettings where
  parseJSON =
    withObject
      "AutomatedAbrSettings"
      ( \x ->
          AutomatedAbrSettings'
            <$> (x .:? "maxRenditions")
            <*> (x .:? "maxAbrBitrate")
            <*> (x .:? "minAbrBitrate")
      )

instance Hashable AutomatedAbrSettings

instance NFData AutomatedAbrSettings

instance ToJSON AutomatedAbrSettings where
  toJSON AutomatedAbrSettings' {..} =
    object
      ( catMaybes
          [ ("maxRenditions" .=) <$> _aasMaxRenditions,
            ("maxAbrBitrate" .=) <$> _aasMaxAbrBitrate,
            ("minAbrBitrate" .=) <$> _aasMinAbrBitrate
          ]
      )

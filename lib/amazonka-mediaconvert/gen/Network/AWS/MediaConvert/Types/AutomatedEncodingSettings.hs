{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AutomatedEncodingSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AutomatedEncodingSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.AutomatedAbrSettings
import Network.AWS.Prelude

-- | Use automated encoding to have MediaConvert choose your encoding settings for you, based on characteristics of your input video.
--
-- /See:/ 'automatedEncodingSettings' smart constructor.
newtype AutomatedEncodingSettings = AutomatedEncodingSettings'
  { _aesAbrSettings ::
      Maybe AutomatedAbrSettings
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AutomatedEncodingSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aesAbrSettings' - Use automated ABR to have MediaConvert set up the renditions in your ABR package for you automatically, based on characteristics of your input video. This feature optimizes video quality while minimizing the overall size of your ABR package.
automatedEncodingSettings ::
  AutomatedEncodingSettings
automatedEncodingSettings =
  AutomatedEncodingSettings' {_aesAbrSettings = Nothing}

-- | Use automated ABR to have MediaConvert set up the renditions in your ABR package for you automatically, based on characteristics of your input video. This feature optimizes video quality while minimizing the overall size of your ABR package.
aesAbrSettings :: Lens' AutomatedEncodingSettings (Maybe AutomatedAbrSettings)
aesAbrSettings = lens _aesAbrSettings (\s a -> s {_aesAbrSettings = a})

instance FromJSON AutomatedEncodingSettings where
  parseJSON =
    withObject
      "AutomatedEncodingSettings"
      (\x -> AutomatedEncodingSettings' <$> (x .:? "abrSettings"))

instance Hashable AutomatedEncodingSettings

instance NFData AutomatedEncodingSettings

instance ToJSON AutomatedEncodingSettings where
  toJSON AutomatedEncodingSettings' {..} =
    object (catMaybes [("abrSettings" .=) <$> _aesAbrSettings])

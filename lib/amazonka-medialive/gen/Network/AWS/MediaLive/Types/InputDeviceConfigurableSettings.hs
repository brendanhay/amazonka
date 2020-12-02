{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputDeviceConfigurableSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDeviceConfigurableSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.InputDeviceConfiguredInput
import Network.AWS.Prelude

-- | Configurable settings for the input device.
--
-- /See:/ 'inputDeviceConfigurableSettings' smart constructor.
data InputDeviceConfigurableSettings = InputDeviceConfigurableSettings'
  { _idcsConfiguredInput ::
      !( Maybe
           InputDeviceConfiguredInput
       ),
    _idcsMaxBitrate ::
      !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputDeviceConfigurableSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idcsConfiguredInput' - The input source that you want to use. If the device has a source connected to only one of its input ports, or if you don't care which source the device sends, specify Auto. If the device has sources connected to both its input ports, and you want to use a specific source, specify the source.
--
-- * 'idcsMaxBitrate' - The maximum bitrate in bits per second. Set a value here to throttle the bitrate of the source video.
inputDeviceConfigurableSettings ::
  InputDeviceConfigurableSettings
inputDeviceConfigurableSettings =
  InputDeviceConfigurableSettings'
    { _idcsConfiguredInput = Nothing,
      _idcsMaxBitrate = Nothing
    }

-- | The input source that you want to use. If the device has a source connected to only one of its input ports, or if you don't care which source the device sends, specify Auto. If the device has sources connected to both its input ports, and you want to use a specific source, specify the source.
idcsConfiguredInput :: Lens' InputDeviceConfigurableSettings (Maybe InputDeviceConfiguredInput)
idcsConfiguredInput = lens _idcsConfiguredInput (\s a -> s {_idcsConfiguredInput = a})

-- | The maximum bitrate in bits per second. Set a value here to throttle the bitrate of the source video.
idcsMaxBitrate :: Lens' InputDeviceConfigurableSettings (Maybe Int)
idcsMaxBitrate = lens _idcsMaxBitrate (\s a -> s {_idcsMaxBitrate = a})

instance Hashable InputDeviceConfigurableSettings

instance NFData InputDeviceConfigurableSettings

instance ToJSON InputDeviceConfigurableSettings where
  toJSON InputDeviceConfigurableSettings' {..} =
    object
      ( catMaybes
          [ ("configuredInput" .=) <$> _idcsConfiguredInput,
            ("maxBitrate" .=) <$> _idcsMaxBitrate
          ]
      )

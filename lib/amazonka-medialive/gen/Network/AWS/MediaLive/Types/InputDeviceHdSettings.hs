{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputDeviceHdSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDeviceHdSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.InputDeviceActiveInput
import Network.AWS.MediaLive.Types.InputDeviceConfiguredInput
import Network.AWS.MediaLive.Types.InputDeviceScanType
import Network.AWS.MediaLive.Types.InputDeviceState
import Network.AWS.Prelude

-- | Settings that describe the active source from the input device, and the video characteristics of that source.
--
-- /See:/ 'inputDeviceHdSettings' smart constructor.
data InputDeviceHdSettings = InputDeviceHdSettings'
  { _idhsFramerate ::
      !(Maybe Double),
    _idhsScanType :: !(Maybe InputDeviceScanType),
    _idhsDeviceState :: !(Maybe InputDeviceState),
    _idhsHeight :: !(Maybe Int),
    _idhsActiveInput ::
      !(Maybe InputDeviceActiveInput),
    _idhsWidth :: !(Maybe Int),
    _idhsConfiguredInput ::
      !(Maybe InputDeviceConfiguredInput),
    _idhsMaxBitrate :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputDeviceHdSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idhsFramerate' - The frame rate of the video source.
--
-- * 'idhsScanType' - The scan type of the video source.
--
-- * 'idhsDeviceState' - The state of the input device.
--
-- * 'idhsHeight' - The height of the video source, in pixels.
--
-- * 'idhsActiveInput' - If you specified Auto as the configured input, specifies which of the sources is currently active (SDI or HDMI).
--
-- * 'idhsWidth' - The width of the video source, in pixels.
--
-- * 'idhsConfiguredInput' - The source at the input device that is currently active. You can specify this source.
--
-- * 'idhsMaxBitrate' - The current maximum bitrate for ingesting this source, in bits per second. You can specify this maximum.
inputDeviceHdSettings ::
  InputDeviceHdSettings
inputDeviceHdSettings =
  InputDeviceHdSettings'
    { _idhsFramerate = Nothing,
      _idhsScanType = Nothing,
      _idhsDeviceState = Nothing,
      _idhsHeight = Nothing,
      _idhsActiveInput = Nothing,
      _idhsWidth = Nothing,
      _idhsConfiguredInput = Nothing,
      _idhsMaxBitrate = Nothing
    }

-- | The frame rate of the video source.
idhsFramerate :: Lens' InputDeviceHdSettings (Maybe Double)
idhsFramerate = lens _idhsFramerate (\s a -> s {_idhsFramerate = a})

-- | The scan type of the video source.
idhsScanType :: Lens' InputDeviceHdSettings (Maybe InputDeviceScanType)
idhsScanType = lens _idhsScanType (\s a -> s {_idhsScanType = a})

-- | The state of the input device.
idhsDeviceState :: Lens' InputDeviceHdSettings (Maybe InputDeviceState)
idhsDeviceState = lens _idhsDeviceState (\s a -> s {_idhsDeviceState = a})

-- | The height of the video source, in pixels.
idhsHeight :: Lens' InputDeviceHdSettings (Maybe Int)
idhsHeight = lens _idhsHeight (\s a -> s {_idhsHeight = a})

-- | If you specified Auto as the configured input, specifies which of the sources is currently active (SDI or HDMI).
idhsActiveInput :: Lens' InputDeviceHdSettings (Maybe InputDeviceActiveInput)
idhsActiveInput = lens _idhsActiveInput (\s a -> s {_idhsActiveInput = a})

-- | The width of the video source, in pixels.
idhsWidth :: Lens' InputDeviceHdSettings (Maybe Int)
idhsWidth = lens _idhsWidth (\s a -> s {_idhsWidth = a})

-- | The source at the input device that is currently active. You can specify this source.
idhsConfiguredInput :: Lens' InputDeviceHdSettings (Maybe InputDeviceConfiguredInput)
idhsConfiguredInput = lens _idhsConfiguredInput (\s a -> s {_idhsConfiguredInput = a})

-- | The current maximum bitrate for ingesting this source, in bits per second. You can specify this maximum.
idhsMaxBitrate :: Lens' InputDeviceHdSettings (Maybe Int)
idhsMaxBitrate = lens _idhsMaxBitrate (\s a -> s {_idhsMaxBitrate = a})

instance FromJSON InputDeviceHdSettings where
  parseJSON =
    withObject
      "InputDeviceHdSettings"
      ( \x ->
          InputDeviceHdSettings'
            <$> (x .:? "framerate")
            <*> (x .:? "scanType")
            <*> (x .:? "deviceState")
            <*> (x .:? "height")
            <*> (x .:? "activeInput")
            <*> (x .:? "width")
            <*> (x .:? "configuredInput")
            <*> (x .:? "maxBitrate")
      )

instance Hashable InputDeviceHdSettings

instance NFData InputDeviceHdSettings

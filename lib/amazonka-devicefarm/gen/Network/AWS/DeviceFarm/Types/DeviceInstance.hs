{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.DeviceInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.DeviceInstance where

import Network.AWS.DeviceFarm.Types.InstanceProfile
import Network.AWS.DeviceFarm.Types.InstanceStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the device instance.
--
--
--
-- /See:/ 'deviceInstance' smart constructor.
data DeviceInstance = DeviceInstance'
  { _diStatus ::
      !(Maybe InstanceStatus),
    _diUdid :: !(Maybe Text),
    _diInstanceProfile :: !(Maybe InstanceProfile),
    _diArn :: !(Maybe Text),
    _diDeviceARN :: !(Maybe Text),
    _diLabels :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeviceInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diStatus' - The status of the device instance. Valid values are listed here.
--
-- * 'diUdid' - Unique device identifier for the device instance.
--
-- * 'diInstanceProfile' - A object that contains information about the instance profile.
--
-- * 'diArn' - The Amazon Resource Name (ARN) of the device instance.
--
-- * 'diDeviceARN' - The ARN of the device.
--
-- * 'diLabels' - An array of strings that describe the device instance.
deviceInstance ::
  DeviceInstance
deviceInstance =
  DeviceInstance'
    { _diStatus = Nothing,
      _diUdid = Nothing,
      _diInstanceProfile = Nothing,
      _diArn = Nothing,
      _diDeviceARN = Nothing,
      _diLabels = Nothing
    }

-- | The status of the device instance. Valid values are listed here.
diStatus :: Lens' DeviceInstance (Maybe InstanceStatus)
diStatus = lens _diStatus (\s a -> s {_diStatus = a})

-- | Unique device identifier for the device instance.
diUdid :: Lens' DeviceInstance (Maybe Text)
diUdid = lens _diUdid (\s a -> s {_diUdid = a})

-- | A object that contains information about the instance profile.
diInstanceProfile :: Lens' DeviceInstance (Maybe InstanceProfile)
diInstanceProfile = lens _diInstanceProfile (\s a -> s {_diInstanceProfile = a})

-- | The Amazon Resource Name (ARN) of the device instance.
diArn :: Lens' DeviceInstance (Maybe Text)
diArn = lens _diArn (\s a -> s {_diArn = a})

-- | The ARN of the device.
diDeviceARN :: Lens' DeviceInstance (Maybe Text)
diDeviceARN = lens _diDeviceARN (\s a -> s {_diDeviceARN = a})

-- | An array of strings that describe the device instance.
diLabels :: Lens' DeviceInstance [Text]
diLabels = lens _diLabels (\s a -> s {_diLabels = a}) . _Default . _Coerce

instance FromJSON DeviceInstance where
  parseJSON =
    withObject
      "DeviceInstance"
      ( \x ->
          DeviceInstance'
            <$> (x .:? "status")
            <*> (x .:? "udid")
            <*> (x .:? "instanceProfile")
            <*> (x .:? "arn")
            <*> (x .:? "deviceArn")
            <*> (x .:? "labels" .!= mempty)
      )

instance Hashable DeviceInstance

instance NFData DeviceInstance

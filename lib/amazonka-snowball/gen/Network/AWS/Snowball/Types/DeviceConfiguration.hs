{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.DeviceConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.DeviceConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Snowball.Types.SnowconeDeviceConfiguration

-- | The container for @SnowconeDeviceConfiguration@ .
--
--
--
-- /See:/ 'deviceConfiguration' smart constructor.
newtype DeviceConfiguration = DeviceConfiguration'
  { _dcSnowconeDeviceConfiguration ::
      Maybe SnowconeDeviceConfiguration
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeviceConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcSnowconeDeviceConfiguration' - Returns information about the device configuration for an AWS Snowcone job.
deviceConfiguration ::
  DeviceConfiguration
deviceConfiguration =
  DeviceConfiguration' {_dcSnowconeDeviceConfiguration = Nothing}

-- | Returns information about the device configuration for an AWS Snowcone job.
dcSnowconeDeviceConfiguration :: Lens' DeviceConfiguration (Maybe SnowconeDeviceConfiguration)
dcSnowconeDeviceConfiguration = lens _dcSnowconeDeviceConfiguration (\s a -> s {_dcSnowconeDeviceConfiguration = a})

instance FromJSON DeviceConfiguration where
  parseJSON =
    withObject
      "DeviceConfiguration"
      ( \x ->
          DeviceConfiguration' <$> (x .:? "SnowconeDeviceConfiguration")
      )

instance Hashable DeviceConfiguration

instance NFData DeviceConfiguration

instance ToJSON DeviceConfiguration where
  toJSON DeviceConfiguration' {..} =
    object
      ( catMaybes
          [ ("SnowconeDeviceConfiguration" .=)
              <$> _dcSnowconeDeviceConfiguration
          ]
      )

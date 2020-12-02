{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.SnowconeDeviceConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.SnowconeDeviceConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Snowball.Types.WirelessConnection

-- | Specifies the device configuration for an AWS Snowcone job.
--
--
--
-- /See:/ 'snowconeDeviceConfiguration' smart constructor.
newtype SnowconeDeviceConfiguration = SnowconeDeviceConfiguration'
  { _sdcWirelessConnection ::
      Maybe WirelessConnection
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SnowconeDeviceConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdcWirelessConnection' - Configures the wireless connection for the AWS Snowcone device.
snowconeDeviceConfiguration ::
  SnowconeDeviceConfiguration
snowconeDeviceConfiguration =
  SnowconeDeviceConfiguration' {_sdcWirelessConnection = Nothing}

-- | Configures the wireless connection for the AWS Snowcone device.
sdcWirelessConnection :: Lens' SnowconeDeviceConfiguration (Maybe WirelessConnection)
sdcWirelessConnection = lens _sdcWirelessConnection (\s a -> s {_sdcWirelessConnection = a})

instance FromJSON SnowconeDeviceConfiguration where
  parseJSON =
    withObject
      "SnowconeDeviceConfiguration"
      ( \x ->
          SnowconeDeviceConfiguration' <$> (x .:? "WirelessConnection")
      )

instance Hashable SnowconeDeviceConfiguration

instance NFData SnowconeDeviceConfiguration

instance ToJSON SnowconeDeviceConfiguration where
  toJSON SnowconeDeviceConfiguration' {..} =
    object
      (catMaybes [("WirelessConnection" .=) <$> _sdcWirelessConnection])

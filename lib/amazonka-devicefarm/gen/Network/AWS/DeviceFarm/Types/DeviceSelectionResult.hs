{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.DeviceSelectionResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.DeviceSelectionResult where

import Network.AWS.DeviceFarm.Types.DeviceFilter
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the run results requested by the device selection configuration and how many devices were returned. For an example of the JSON response syntax, see 'ScheduleRun' .
--
--
--
-- /See:/ 'deviceSelectionResult' smart constructor.
data DeviceSelectionResult = DeviceSelectionResult'
  { _dsrMatchedDevicesCount ::
      !(Maybe Int),
    _dsrFilters :: !(Maybe [DeviceFilter]),
    _dsrMaxDevices :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeviceSelectionResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsrMatchedDevicesCount' - The number of devices that matched the device filter selection criteria.
--
-- * 'dsrFilters' - The filters in a device selection result.
--
-- * 'dsrMaxDevices' - The maximum number of devices to be selected by a device filter and included in a test run.
deviceSelectionResult ::
  DeviceSelectionResult
deviceSelectionResult =
  DeviceSelectionResult'
    { _dsrMatchedDevicesCount = Nothing,
      _dsrFilters = Nothing,
      _dsrMaxDevices = Nothing
    }

-- | The number of devices that matched the device filter selection criteria.
dsrMatchedDevicesCount :: Lens' DeviceSelectionResult (Maybe Int)
dsrMatchedDevicesCount = lens _dsrMatchedDevicesCount (\s a -> s {_dsrMatchedDevicesCount = a})

-- | The filters in a device selection result.
dsrFilters :: Lens' DeviceSelectionResult [DeviceFilter]
dsrFilters = lens _dsrFilters (\s a -> s {_dsrFilters = a}) . _Default . _Coerce

-- | The maximum number of devices to be selected by a device filter and included in a test run.
dsrMaxDevices :: Lens' DeviceSelectionResult (Maybe Int)
dsrMaxDevices = lens _dsrMaxDevices (\s a -> s {_dsrMaxDevices = a})

instance FromJSON DeviceSelectionResult where
  parseJSON =
    withObject
      "DeviceSelectionResult"
      ( \x ->
          DeviceSelectionResult'
            <$> (x .:? "matchedDevicesCount")
            <*> (x .:? "filters" .!= mempty)
            <*> (x .:? "maxDevices")
      )

instance Hashable DeviceSelectionResult

instance NFData DeviceSelectionResult

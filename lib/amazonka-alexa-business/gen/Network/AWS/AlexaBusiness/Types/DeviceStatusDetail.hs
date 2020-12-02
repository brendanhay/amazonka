{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.DeviceStatusDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.DeviceStatusDetail where

import Network.AWS.AlexaBusiness.Types.DeviceStatusDetailCode
import Network.AWS.AlexaBusiness.Types.Feature
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details of a device’s status.
--
--
--
-- /See:/ 'deviceStatusDetail' smart constructor.
data DeviceStatusDetail = DeviceStatusDetail'
  { _dsdFeature ::
      !(Maybe Feature),
    _dsdCode :: !(Maybe DeviceStatusDetailCode)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeviceStatusDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsdFeature' - The list of available features on the device.
--
-- * 'dsdCode' - The device status detail code.
deviceStatusDetail ::
  DeviceStatusDetail
deviceStatusDetail =
  DeviceStatusDetail' {_dsdFeature = Nothing, _dsdCode = Nothing}

-- | The list of available features on the device.
dsdFeature :: Lens' DeviceStatusDetail (Maybe Feature)
dsdFeature = lens _dsdFeature (\s a -> s {_dsdFeature = a})

-- | The device status detail code.
dsdCode :: Lens' DeviceStatusDetail (Maybe DeviceStatusDetailCode)
dsdCode = lens _dsdCode (\s a -> s {_dsdCode = a})

instance FromJSON DeviceStatusDetail where
  parseJSON =
    withObject
      "DeviceStatusDetail"
      ( \x ->
          DeviceStatusDetail' <$> (x .:? "Feature") <*> (x .:? "Code")
      )

instance Hashable DeviceStatusDetail

instance NFData DeviceStatusDetail

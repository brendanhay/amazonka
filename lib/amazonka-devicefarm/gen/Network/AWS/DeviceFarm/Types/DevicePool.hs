{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.DevicePool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.DevicePool where

import Network.AWS.DeviceFarm.Types.DevicePoolType
import Network.AWS.DeviceFarm.Types.Rule
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a collection of device types.
--
--
--
-- /See:/ 'devicePool' smart constructor.
data DevicePool = DevicePool'
  { _devArn :: !(Maybe Text),
    _devRules :: !(Maybe [Rule]),
    _devName :: !(Maybe Text),
    _devMaxDevices :: !(Maybe Int),
    _devType :: !(Maybe DevicePoolType),
    _devDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DevicePool' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'devArn' - The device pool's ARN.
--
-- * 'devRules' - Information about the device pool's rules.
--
-- * 'devName' - The device pool's name.
--
-- * 'devMaxDevices' - The number of devices that Device Farm can add to your device pool. Device Farm adds devices that are available and meet the criteria that you assign for the @rules@ parameter. Depending on how many devices meet these constraints, your device pool might contain fewer devices than the value for this parameter. By specifying the maximum number of devices, you can control the costs that you incur by running tests.
--
-- * 'devType' - The device pool's type. Allowed values include:     * CURATED: A device pool that is created and managed by AWS Device Farm.     * PRIVATE: A device pool that is created and managed by the device pool developer.
--
-- * 'devDescription' - The device pool's description.
devicePool ::
  DevicePool
devicePool =
  DevicePool'
    { _devArn = Nothing,
      _devRules = Nothing,
      _devName = Nothing,
      _devMaxDevices = Nothing,
      _devType = Nothing,
      _devDescription = Nothing
    }

-- | The device pool's ARN.
devArn :: Lens' DevicePool (Maybe Text)
devArn = lens _devArn (\s a -> s {_devArn = a})

-- | Information about the device pool's rules.
devRules :: Lens' DevicePool [Rule]
devRules = lens _devRules (\s a -> s {_devRules = a}) . _Default . _Coerce

-- | The device pool's name.
devName :: Lens' DevicePool (Maybe Text)
devName = lens _devName (\s a -> s {_devName = a})

-- | The number of devices that Device Farm can add to your device pool. Device Farm adds devices that are available and meet the criteria that you assign for the @rules@ parameter. Depending on how many devices meet these constraints, your device pool might contain fewer devices than the value for this parameter. By specifying the maximum number of devices, you can control the costs that you incur by running tests.
devMaxDevices :: Lens' DevicePool (Maybe Int)
devMaxDevices = lens _devMaxDevices (\s a -> s {_devMaxDevices = a})

-- | The device pool's type. Allowed values include:     * CURATED: A device pool that is created and managed by AWS Device Farm.     * PRIVATE: A device pool that is created and managed by the device pool developer.
devType :: Lens' DevicePool (Maybe DevicePoolType)
devType = lens _devType (\s a -> s {_devType = a})

-- | The device pool's description.
devDescription :: Lens' DevicePool (Maybe Text)
devDescription = lens _devDescription (\s a -> s {_devDescription = a})

instance FromJSON DevicePool where
  parseJSON =
    withObject
      "DevicePool"
      ( \x ->
          DevicePool'
            <$> (x .:? "arn")
            <*> (x .:? "rules" .!= mempty)
            <*> (x .:? "name")
            <*> (x .:? "maxDevices")
            <*> (x .:? "type")
            <*> (x .:? "description")
      )

instance Hashable DevicePool

instance NFData DevicePool

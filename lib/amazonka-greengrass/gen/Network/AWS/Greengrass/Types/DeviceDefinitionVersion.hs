{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.DeviceDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.DeviceDefinitionVersion where

import Network.AWS.Greengrass.Types.Device
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a device definition version.
--
-- /See:/ 'deviceDefinitionVersion' smart constructor.
newtype DeviceDefinitionVersion = DeviceDefinitionVersion'
  { _ddvDevices ::
      Maybe [Device]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeviceDefinitionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddvDevices' - A list of devices in the definition version.
deviceDefinitionVersion ::
  DeviceDefinitionVersion
deviceDefinitionVersion =
  DeviceDefinitionVersion' {_ddvDevices = Nothing}

-- | A list of devices in the definition version.
ddvDevices :: Lens' DeviceDefinitionVersion [Device]
ddvDevices = lens _ddvDevices (\s a -> s {_ddvDevices = a}) . _Default . _Coerce

instance FromJSON DeviceDefinitionVersion where
  parseJSON =
    withObject
      "DeviceDefinitionVersion"
      (\x -> DeviceDefinitionVersion' <$> (x .:? "Devices" .!= mempty))

instance Hashable DeviceDefinitionVersion

instance NFData DeviceDefinitionVersion

instance ToJSON DeviceDefinitionVersion where
  toJSON DeviceDefinitionVersion' {..} =
    object (catMaybes [("Devices" .=) <$> _ddvDevices])

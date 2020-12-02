{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Device
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Device where

import Network.AWS.ECS.Types.DeviceCgroupPermission
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object representing a container instance host device.
--
--
--
-- /See:/ 'device' smart constructor.
data Device = Device'
  { _dContainerPath :: !(Maybe Text),
    _dPermissions :: !(Maybe [DeviceCgroupPermission]),
    _dHostPath :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Device' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dContainerPath' - The path inside the container at which to expose the host device.
--
-- * 'dPermissions' - The explicit permissions to provide to the container for the device. By default, the container has permissions for @read@ , @write@ , and @mknod@ for the device.
--
-- * 'dHostPath' - The path for the device on the host container instance.
device ::
  -- | 'dHostPath'
  Text ->
  Device
device pHostPath_ =
  Device'
    { _dContainerPath = Nothing,
      _dPermissions = Nothing,
      _dHostPath = pHostPath_
    }

-- | The path inside the container at which to expose the host device.
dContainerPath :: Lens' Device (Maybe Text)
dContainerPath = lens _dContainerPath (\s a -> s {_dContainerPath = a})

-- | The explicit permissions to provide to the container for the device. By default, the container has permissions for @read@ , @write@ , and @mknod@ for the device.
dPermissions :: Lens' Device [DeviceCgroupPermission]
dPermissions = lens _dPermissions (\s a -> s {_dPermissions = a}) . _Default . _Coerce

-- | The path for the device on the host container instance.
dHostPath :: Lens' Device Text
dHostPath = lens _dHostPath (\s a -> s {_dHostPath = a})

instance FromJSON Device where
  parseJSON =
    withObject
      "Device"
      ( \x ->
          Device'
            <$> (x .:? "containerPath")
            <*> (x .:? "permissions" .!= mempty)
            <*> (x .: "hostPath")
      )

instance Hashable Device

instance NFData Device

instance ToJSON Device where
  toJSON Device' {..} =
    object
      ( catMaybes
          [ ("containerPath" .=) <$> _dContainerPath,
            ("permissions" .=) <$> _dPermissions,
            Just ("hostPath" .= _dHostPath)
          ]
      )

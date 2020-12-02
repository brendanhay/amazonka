{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.PlatformDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.PlatformDevice where

import Network.AWS.ECS.Types.PlatformDeviceType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The devices that are available on the container instance. The only supported device type is a GPU.
--
--
--
-- /See:/ 'platformDevice' smart constructor.
data PlatformDevice = PlatformDevice'
  { _pdId :: !Text,
    _pdType :: !PlatformDeviceType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PlatformDevice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdId' - The ID for the GPU(s) on the container instance. The available GPU IDs can also be obtained on the container instance in the @/var/lib/ecs/gpu/nvidia_gpu_info.json@ file.
--
-- * 'pdType' - The type of device that is available on the container instance. The only supported value is @GPU@ .
platformDevice ::
  -- | 'pdId'
  Text ->
  -- | 'pdType'
  PlatformDeviceType ->
  PlatformDevice
platformDevice pId_ pType_ =
  PlatformDevice' {_pdId = pId_, _pdType = pType_}

-- | The ID for the GPU(s) on the container instance. The available GPU IDs can also be obtained on the container instance in the @/var/lib/ecs/gpu/nvidia_gpu_info.json@ file.
pdId :: Lens' PlatformDevice Text
pdId = lens _pdId (\s a -> s {_pdId = a})

-- | The type of device that is available on the container instance. The only supported value is @GPU@ .
pdType :: Lens' PlatformDevice PlatformDeviceType
pdType = lens _pdType (\s a -> s {_pdType = a})

instance Hashable PlatformDevice

instance NFData PlatformDevice

instance ToJSON PlatformDevice where
  toJSON PlatformDevice' {..} =
    object
      (catMaybes [Just ("id" .= _pdId), Just ("type" .= _pdType)])

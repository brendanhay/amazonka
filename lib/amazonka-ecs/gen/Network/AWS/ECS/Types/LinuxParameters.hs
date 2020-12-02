{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.LinuxParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.LinuxParameters where

import Network.AWS.ECS.Types.Device
import Network.AWS.ECS.Types.KernelCapabilities
import Network.AWS.ECS.Types.Tmpfs
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Linux-specific options that are applied to the container, such as Linux 'KernelCapabilities' .
--
--
--
-- /See:/ 'linuxParameters' smart constructor.
data LinuxParameters = LinuxParameters'
  { _lpSharedMemorySize ::
      !(Maybe Int),
    _lpInitProcessEnabled :: !(Maybe Bool),
    _lpTmpfs :: !(Maybe [Tmpfs]),
    _lpSwappiness :: !(Maybe Int),
    _lpDevices :: !(Maybe [Device]),
    _lpCapabilities :: !(Maybe KernelCapabilities),
    _lpMaxSwap :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LinuxParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpSharedMemorySize' - The value for the size (in MiB) of the @/dev/shm@ volume. This parameter maps to the @--shm-size@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- * 'lpInitProcessEnabled' - Run an @init@ process inside the container that forwards signals and reaps processes. This parameter maps to the @--init@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . This parameter requires version 1.25 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log in to your container instance and run the following command: @sudo docker version --format '{{.Server.APIVersion}}'@
--
-- * 'lpTmpfs' - The container path, mount options, and size (in MiB) of the tmpfs mount. This parameter maps to the @--tmpfs@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- * 'lpSwappiness' - This allows you to tune a container's memory swappiness behavior. A @swappiness@ value of @0@ will cause swapping to not happen unless absolutely necessary. A @swappiness@ value of @100@ will cause pages to be swapped very aggressively. Accepted values are whole numbers between @0@ and @100@ . If the @swappiness@ parameter is not specified, a default value of @60@ is used. If a value is not specified for @maxSwap@ then this parameter is ignored. This parameter maps to the @--memory-swappiness@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- * 'lpDevices' - Any host devices to expose to the container. This parameter maps to @Devices@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--device@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
-- * 'lpCapabilities' - The Linux capabilities for the container that are added to or dropped from the default configuration provided by Docker.
--
-- * 'lpMaxSwap' - The total amount of swap memory (in MiB) a container can use. This parameter will be translated to the @--memory-swap@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> where the value would be the sum of the container memory plus the @maxSwap@ value. If a @maxSwap@ value of @0@ is specified, the container will not use swap. Accepted values are @0@ or any positive integer. If the @maxSwap@ parameter is omitted, the container will use the swap configuration for the container instance it is running on. A @maxSwap@ value must be set for the @swappiness@ parameter to be used.
linuxParameters ::
  LinuxParameters
linuxParameters =
  LinuxParameters'
    { _lpSharedMemorySize = Nothing,
      _lpInitProcessEnabled = Nothing,
      _lpTmpfs = Nothing,
      _lpSwappiness = Nothing,
      _lpDevices = Nothing,
      _lpCapabilities = Nothing,
      _lpMaxSwap = Nothing
    }

-- | The value for the size (in MiB) of the @/dev/shm@ volume. This parameter maps to the @--shm-size@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
lpSharedMemorySize :: Lens' LinuxParameters (Maybe Int)
lpSharedMemorySize = lens _lpSharedMemorySize (\s a -> s {_lpSharedMemorySize = a})

-- | Run an @init@ process inside the container that forwards signals and reaps processes. This parameter maps to the @--init@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> . This parameter requires version 1.25 of the Docker Remote API or greater on your container instance. To check the Docker Remote API version on your container instance, log in to your container instance and run the following command: @sudo docker version --format '{{.Server.APIVersion}}'@
lpInitProcessEnabled :: Lens' LinuxParameters (Maybe Bool)
lpInitProcessEnabled = lens _lpInitProcessEnabled (\s a -> s {_lpInitProcessEnabled = a})

-- | The container path, mount options, and size (in MiB) of the tmpfs mount. This parameter maps to the @--tmpfs@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
lpTmpfs :: Lens' LinuxParameters [Tmpfs]
lpTmpfs = lens _lpTmpfs (\s a -> s {_lpTmpfs = a}) . _Default . _Coerce

-- | This allows you to tune a container's memory swappiness behavior. A @swappiness@ value of @0@ will cause swapping to not happen unless absolutely necessary. A @swappiness@ value of @100@ will cause pages to be swapped very aggressively. Accepted values are whole numbers between @0@ and @100@ . If the @swappiness@ parameter is not specified, a default value of @60@ is used. If a value is not specified for @maxSwap@ then this parameter is ignored. This parameter maps to the @--memory-swappiness@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
lpSwappiness :: Lens' LinuxParameters (Maybe Int)
lpSwappiness = lens _lpSwappiness (\s a -> s {_lpSwappiness = a})

-- | Any host devices to expose to the container. This parameter maps to @Devices@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--device@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
lpDevices :: Lens' LinuxParameters [Device]
lpDevices = lens _lpDevices (\s a -> s {_lpDevices = a}) . _Default . _Coerce

-- | The Linux capabilities for the container that are added to or dropped from the default configuration provided by Docker.
lpCapabilities :: Lens' LinuxParameters (Maybe KernelCapabilities)
lpCapabilities = lens _lpCapabilities (\s a -> s {_lpCapabilities = a})

-- | The total amount of swap memory (in MiB) a container can use. This parameter will be translated to the @--memory-swap@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> where the value would be the sum of the container memory plus the @maxSwap@ value. If a @maxSwap@ value of @0@ is specified, the container will not use swap. Accepted values are @0@ or any positive integer. If the @maxSwap@ parameter is omitted, the container will use the swap configuration for the container instance it is running on. A @maxSwap@ value must be set for the @swappiness@ parameter to be used.
lpMaxSwap :: Lens' LinuxParameters (Maybe Int)
lpMaxSwap = lens _lpMaxSwap (\s a -> s {_lpMaxSwap = a})

instance FromJSON LinuxParameters where
  parseJSON =
    withObject
      "LinuxParameters"
      ( \x ->
          LinuxParameters'
            <$> (x .:? "sharedMemorySize")
            <*> (x .:? "initProcessEnabled")
            <*> (x .:? "tmpfs" .!= mempty)
            <*> (x .:? "swappiness")
            <*> (x .:? "devices" .!= mempty)
            <*> (x .:? "capabilities")
            <*> (x .:? "maxSwap")
      )

instance Hashable LinuxParameters

instance NFData LinuxParameters

instance ToJSON LinuxParameters where
  toJSON LinuxParameters' {..} =
    object
      ( catMaybes
          [ ("sharedMemorySize" .=) <$> _lpSharedMemorySize,
            ("initProcessEnabled" .=) <$> _lpInitProcessEnabled,
            ("tmpfs" .=) <$> _lpTmpfs,
            ("swappiness" .=) <$> _lpSwappiness,
            ("devices" .=) <$> _lpDevices,
            ("capabilities" .=) <$> _lpCapabilities,
            ("maxSwap" .=) <$> _lpMaxSwap
          ]
      )

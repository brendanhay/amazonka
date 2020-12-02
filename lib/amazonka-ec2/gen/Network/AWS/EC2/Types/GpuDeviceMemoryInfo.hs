{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.GpuDeviceMemoryInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.GpuDeviceMemoryInfo where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the memory available to the GPU accelerator.
--
--
--
-- /See:/ 'gpuDeviceMemoryInfo' smart constructor.
newtype GpuDeviceMemoryInfo = GpuDeviceMemoryInfo'
  { _gdmiSizeInMiB ::
      Maybe Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GpuDeviceMemoryInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdmiSizeInMiB' - The size of the memory available to the GPU accelerator, in MiB.
gpuDeviceMemoryInfo ::
  GpuDeviceMemoryInfo
gpuDeviceMemoryInfo =
  GpuDeviceMemoryInfo' {_gdmiSizeInMiB = Nothing}

-- | The size of the memory available to the GPU accelerator, in MiB.
gdmiSizeInMiB :: Lens' GpuDeviceMemoryInfo (Maybe Int)
gdmiSizeInMiB = lens _gdmiSizeInMiB (\s a -> s {_gdmiSizeInMiB = a})

instance FromXML GpuDeviceMemoryInfo where
  parseXML x = GpuDeviceMemoryInfo' <$> (x .@? "sizeInMiB")

instance Hashable GpuDeviceMemoryInfo

instance NFData GpuDeviceMemoryInfo

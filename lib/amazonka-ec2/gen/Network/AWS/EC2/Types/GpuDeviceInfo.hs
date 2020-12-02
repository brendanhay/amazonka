{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.GpuDeviceInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.GpuDeviceInfo where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.GpuDeviceMemoryInfo
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the GPU accelerators for the instance type.
--
--
--
-- /See:/ 'gpuDeviceInfo' smart constructor.
data GpuDeviceInfo = GpuDeviceInfo'
  { _gdiMemoryInfo ::
      !(Maybe GpuDeviceMemoryInfo),
    _gdiManufacturer :: !(Maybe Text),
    _gdiCount :: !(Maybe Int),
    _gdiName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GpuDeviceInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdiMemoryInfo' - Describes the memory available to the GPU accelerator.
--
-- * 'gdiManufacturer' - The manufacturer of the GPU accelerator.
--
-- * 'gdiCount' - The number of GPUs for the instance type.
--
-- * 'gdiName' - The name of the GPU accelerator.
gpuDeviceInfo ::
  GpuDeviceInfo
gpuDeviceInfo =
  GpuDeviceInfo'
    { _gdiMemoryInfo = Nothing,
      _gdiManufacturer = Nothing,
      _gdiCount = Nothing,
      _gdiName = Nothing
    }

-- | Describes the memory available to the GPU accelerator.
gdiMemoryInfo :: Lens' GpuDeviceInfo (Maybe GpuDeviceMemoryInfo)
gdiMemoryInfo = lens _gdiMemoryInfo (\s a -> s {_gdiMemoryInfo = a})

-- | The manufacturer of the GPU accelerator.
gdiManufacturer :: Lens' GpuDeviceInfo (Maybe Text)
gdiManufacturer = lens _gdiManufacturer (\s a -> s {_gdiManufacturer = a})

-- | The number of GPUs for the instance type.
gdiCount :: Lens' GpuDeviceInfo (Maybe Int)
gdiCount = lens _gdiCount (\s a -> s {_gdiCount = a})

-- | The name of the GPU accelerator.
gdiName :: Lens' GpuDeviceInfo (Maybe Text)
gdiName = lens _gdiName (\s a -> s {_gdiName = a})

instance FromXML GpuDeviceInfo where
  parseXML x =
    GpuDeviceInfo'
      <$> (x .@? "memoryInfo")
      <*> (x .@? "manufacturer")
      <*> (x .@? "count")
      <*> (x .@? "name")

instance Hashable GpuDeviceInfo

instance NFData GpuDeviceInfo

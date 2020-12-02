{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.GpuInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.GpuInfo where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.GpuDeviceInfo
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the GPU accelerators for the instance type.
--
--
--
-- /See:/ 'gpuInfo' smart constructor.
data GpuInfo = GpuInfo'
  { _giTotalGpuMemoryInMiB :: !(Maybe Int),
    _giGpus :: !(Maybe [GpuDeviceInfo])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GpuInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'giTotalGpuMemoryInMiB' - The total size of the memory for the GPU accelerators for the instance type, in MiB.
--
-- * 'giGpus' - Describes the GPU accelerators for the instance type.
gpuInfo ::
  GpuInfo
gpuInfo =
  GpuInfo' {_giTotalGpuMemoryInMiB = Nothing, _giGpus = Nothing}

-- | The total size of the memory for the GPU accelerators for the instance type, in MiB.
giTotalGpuMemoryInMiB :: Lens' GpuInfo (Maybe Int)
giTotalGpuMemoryInMiB = lens _giTotalGpuMemoryInMiB (\s a -> s {_giTotalGpuMemoryInMiB = a})

-- | Describes the GPU accelerators for the instance type.
giGpus :: Lens' GpuInfo [GpuDeviceInfo]
giGpus = lens _giGpus (\s a -> s {_giGpus = a}) . _Default . _Coerce

instance FromXML GpuInfo where
  parseXML x =
    GpuInfo'
      <$> (x .@? "totalGpuMemoryInMiB")
      <*> (x .@? "gpus" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable GpuInfo

instance NFData GpuInfo

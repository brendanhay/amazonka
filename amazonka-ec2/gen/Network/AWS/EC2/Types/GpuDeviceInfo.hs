{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.GpuDeviceInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.GpuDeviceInfo where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.GpuDeviceMemoryInfo
import qualified Network.AWS.Lens as Lens

-- | Describes the GPU accelerators for the instance type.
--
-- /See:/ 'newGpuDeviceInfo' smart constructor.
data GpuDeviceInfo = GpuDeviceInfo'
  { -- | Describes the memory available to the GPU accelerator.
    memoryInfo :: Core.Maybe GpuDeviceMemoryInfo,
    -- | The manufacturer of the GPU accelerator.
    manufacturer :: Core.Maybe Core.Text,
    -- | The name of the GPU accelerator.
    name :: Core.Maybe Core.Text,
    -- | The number of GPUs for the instance type.
    count :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GpuDeviceInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'memoryInfo', 'gpuDeviceInfo_memoryInfo' - Describes the memory available to the GPU accelerator.
--
-- 'manufacturer', 'gpuDeviceInfo_manufacturer' - The manufacturer of the GPU accelerator.
--
-- 'name', 'gpuDeviceInfo_name' - The name of the GPU accelerator.
--
-- 'count', 'gpuDeviceInfo_count' - The number of GPUs for the instance type.
newGpuDeviceInfo ::
  GpuDeviceInfo
newGpuDeviceInfo =
  GpuDeviceInfo'
    { memoryInfo = Core.Nothing,
      manufacturer = Core.Nothing,
      name = Core.Nothing,
      count = Core.Nothing
    }

-- | Describes the memory available to the GPU accelerator.
gpuDeviceInfo_memoryInfo :: Lens.Lens' GpuDeviceInfo (Core.Maybe GpuDeviceMemoryInfo)
gpuDeviceInfo_memoryInfo = Lens.lens (\GpuDeviceInfo' {memoryInfo} -> memoryInfo) (\s@GpuDeviceInfo' {} a -> s {memoryInfo = a} :: GpuDeviceInfo)

-- | The manufacturer of the GPU accelerator.
gpuDeviceInfo_manufacturer :: Lens.Lens' GpuDeviceInfo (Core.Maybe Core.Text)
gpuDeviceInfo_manufacturer = Lens.lens (\GpuDeviceInfo' {manufacturer} -> manufacturer) (\s@GpuDeviceInfo' {} a -> s {manufacturer = a} :: GpuDeviceInfo)

-- | The name of the GPU accelerator.
gpuDeviceInfo_name :: Lens.Lens' GpuDeviceInfo (Core.Maybe Core.Text)
gpuDeviceInfo_name = Lens.lens (\GpuDeviceInfo' {name} -> name) (\s@GpuDeviceInfo' {} a -> s {name = a} :: GpuDeviceInfo)

-- | The number of GPUs for the instance type.
gpuDeviceInfo_count :: Lens.Lens' GpuDeviceInfo (Core.Maybe Core.Int)
gpuDeviceInfo_count = Lens.lens (\GpuDeviceInfo' {count} -> count) (\s@GpuDeviceInfo' {} a -> s {count = a} :: GpuDeviceInfo)

instance Core.FromXML GpuDeviceInfo where
  parseXML x =
    GpuDeviceInfo'
      Core.<$> (x Core..@? "memoryInfo")
      Core.<*> (x Core..@? "manufacturer")
      Core.<*> (x Core..@? "name")
      Core.<*> (x Core..@? "count")

instance Core.Hashable GpuDeviceInfo

instance Core.NFData GpuDeviceInfo

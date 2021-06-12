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
-- Module      : Network.AWS.EC2.Types.GpuInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.GpuInfo where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.GpuDeviceInfo
import qualified Network.AWS.Lens as Lens

-- | Describes the GPU accelerators for the instance type.
--
-- /See:/ 'newGpuInfo' smart constructor.
data GpuInfo = GpuInfo'
  { -- | Describes the GPU accelerators for the instance type.
    gpus :: Core.Maybe [GpuDeviceInfo],
    -- | The total size of the memory for the GPU accelerators for the instance
    -- type, in MiB.
    totalGpuMemoryInMiB :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GpuInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gpus', 'gpuInfo_gpus' - Describes the GPU accelerators for the instance type.
--
-- 'totalGpuMemoryInMiB', 'gpuInfo_totalGpuMemoryInMiB' - The total size of the memory for the GPU accelerators for the instance
-- type, in MiB.
newGpuInfo ::
  GpuInfo
newGpuInfo =
  GpuInfo'
    { gpus = Core.Nothing,
      totalGpuMemoryInMiB = Core.Nothing
    }

-- | Describes the GPU accelerators for the instance type.
gpuInfo_gpus :: Lens.Lens' GpuInfo (Core.Maybe [GpuDeviceInfo])
gpuInfo_gpus = Lens.lens (\GpuInfo' {gpus} -> gpus) (\s@GpuInfo' {} a -> s {gpus = a} :: GpuInfo) Core.. Lens.mapping Lens._Coerce

-- | The total size of the memory for the GPU accelerators for the instance
-- type, in MiB.
gpuInfo_totalGpuMemoryInMiB :: Lens.Lens' GpuInfo (Core.Maybe Core.Int)
gpuInfo_totalGpuMemoryInMiB = Lens.lens (\GpuInfo' {totalGpuMemoryInMiB} -> totalGpuMemoryInMiB) (\s@GpuInfo' {} a -> s {totalGpuMemoryInMiB = a} :: GpuInfo)

instance Core.FromXML GpuInfo where
  parseXML x =
    GpuInfo'
      Core.<$> ( x Core..@? "gpus" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "totalGpuMemoryInMiB")

instance Core.Hashable GpuInfo

instance Core.NFData GpuInfo

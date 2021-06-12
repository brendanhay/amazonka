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
-- Module      : Network.AWS.EC2.Types.GpuDeviceMemoryInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.GpuDeviceMemoryInfo where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes the memory available to the GPU accelerator.
--
-- /See:/ 'newGpuDeviceMemoryInfo' smart constructor.
data GpuDeviceMemoryInfo = GpuDeviceMemoryInfo'
  { -- | The size of the memory available to the GPU accelerator, in MiB.
    sizeInMiB :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GpuDeviceMemoryInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sizeInMiB', 'gpuDeviceMemoryInfo_sizeInMiB' - The size of the memory available to the GPU accelerator, in MiB.
newGpuDeviceMemoryInfo ::
  GpuDeviceMemoryInfo
newGpuDeviceMemoryInfo =
  GpuDeviceMemoryInfo' {sizeInMiB = Core.Nothing}

-- | The size of the memory available to the GPU accelerator, in MiB.
gpuDeviceMemoryInfo_sizeInMiB :: Lens.Lens' GpuDeviceMemoryInfo (Core.Maybe Core.Int)
gpuDeviceMemoryInfo_sizeInMiB = Lens.lens (\GpuDeviceMemoryInfo' {sizeInMiB} -> sizeInMiB) (\s@GpuDeviceMemoryInfo' {} a -> s {sizeInMiB = a} :: GpuDeviceMemoryInfo)

instance Core.FromXML GpuDeviceMemoryInfo where
  parseXML x =
    GpuDeviceMemoryInfo'
      Core.<$> (x Core..@? "sizeInMiB")

instance Core.Hashable GpuDeviceMemoryInfo

instance Core.NFData GpuDeviceMemoryInfo

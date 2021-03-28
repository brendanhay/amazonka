{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.GpuInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.GpuInfo
  ( GpuInfo (..)
  -- * Smart constructor
  , mkGpuInfo
  -- * Lenses
  , giGpus
  , giTotalGpuMemoryInMiB
  ) where

import qualified Network.AWS.EC2.Types.GpuDeviceInfo as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the GPU accelerators for the instance type.
--
-- /See:/ 'mkGpuInfo' smart constructor.
data GpuInfo = GpuInfo'
  { gpus :: Core.Maybe [Types.GpuDeviceInfo]
    -- ^ Describes the GPU accelerators for the instance type.
  , totalGpuMemoryInMiB :: Core.Maybe Core.Int
    -- ^ The total size of the memory for the GPU accelerators for the instance type, in MiB.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GpuInfo' value with any optional fields omitted.
mkGpuInfo
    :: GpuInfo
mkGpuInfo
  = GpuInfo'{gpus = Core.Nothing, totalGpuMemoryInMiB = Core.Nothing}

-- | Describes the GPU accelerators for the instance type.
--
-- /Note:/ Consider using 'gpus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giGpus :: Lens.Lens' GpuInfo (Core.Maybe [Types.GpuDeviceInfo])
giGpus = Lens.field @"gpus"
{-# INLINEABLE giGpus #-}
{-# DEPRECATED gpus "Use generic-lens or generic-optics with 'gpus' instead"  #-}

-- | The total size of the memory for the GPU accelerators for the instance type, in MiB.
--
-- /Note:/ Consider using 'totalGpuMemoryInMiB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giTotalGpuMemoryInMiB :: Lens.Lens' GpuInfo (Core.Maybe Core.Int)
giTotalGpuMemoryInMiB = Lens.field @"totalGpuMemoryInMiB"
{-# INLINEABLE giTotalGpuMemoryInMiB #-}
{-# DEPRECATED totalGpuMemoryInMiB "Use generic-lens or generic-optics with 'totalGpuMemoryInMiB' instead"  #-}

instance Core.FromXML GpuInfo where
        parseXML x
          = GpuInfo' Core.<$>
              (x Core..@? "gpus" Core..<@> Core.parseXMLList "item") Core.<*>
                x Core..@? "totalGpuMemoryInMiB"

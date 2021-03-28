{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.GpuDeviceInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.GpuDeviceInfo
  ( GpuDeviceInfo (..)
  -- * Smart constructor
  , mkGpuDeviceInfo
  -- * Lenses
  , gdiCount
  , gdiManufacturer
  , gdiMemoryInfo
  , gdiName
  ) where

import qualified Network.AWS.EC2.Types.GpuDeviceManufacturerName as Types
import qualified Network.AWS.EC2.Types.GpuDeviceMemoryInfo as Types
import qualified Network.AWS.EC2.Types.GpuDeviceName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the GPU accelerators for the instance type.
--
-- /See:/ 'mkGpuDeviceInfo' smart constructor.
data GpuDeviceInfo = GpuDeviceInfo'
  { count :: Core.Maybe Core.Int
    -- ^ The number of GPUs for the instance type.
  , manufacturer :: Core.Maybe Types.GpuDeviceManufacturerName
    -- ^ The manufacturer of the GPU accelerator.
  , memoryInfo :: Core.Maybe Types.GpuDeviceMemoryInfo
    -- ^ Describes the memory available to the GPU accelerator.
  , name :: Core.Maybe Types.GpuDeviceName
    -- ^ The name of the GPU accelerator.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GpuDeviceInfo' value with any optional fields omitted.
mkGpuDeviceInfo
    :: GpuDeviceInfo
mkGpuDeviceInfo
  = GpuDeviceInfo'{count = Core.Nothing, manufacturer = Core.Nothing,
                   memoryInfo = Core.Nothing, name = Core.Nothing}

-- | The number of GPUs for the instance type.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdiCount :: Lens.Lens' GpuDeviceInfo (Core.Maybe Core.Int)
gdiCount = Lens.field @"count"
{-# INLINEABLE gdiCount #-}
{-# DEPRECATED count "Use generic-lens or generic-optics with 'count' instead"  #-}

-- | The manufacturer of the GPU accelerator.
--
-- /Note:/ Consider using 'manufacturer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdiManufacturer :: Lens.Lens' GpuDeviceInfo (Core.Maybe Types.GpuDeviceManufacturerName)
gdiManufacturer = Lens.field @"manufacturer"
{-# INLINEABLE gdiManufacturer #-}
{-# DEPRECATED manufacturer "Use generic-lens or generic-optics with 'manufacturer' instead"  #-}

-- | Describes the memory available to the GPU accelerator.
--
-- /Note:/ Consider using 'memoryInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdiMemoryInfo :: Lens.Lens' GpuDeviceInfo (Core.Maybe Types.GpuDeviceMemoryInfo)
gdiMemoryInfo = Lens.field @"memoryInfo"
{-# INLINEABLE gdiMemoryInfo #-}
{-# DEPRECATED memoryInfo "Use generic-lens or generic-optics with 'memoryInfo' instead"  #-}

-- | The name of the GPU accelerator.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdiName :: Lens.Lens' GpuDeviceInfo (Core.Maybe Types.GpuDeviceName)
gdiName = Lens.field @"name"
{-# INLINEABLE gdiName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromXML GpuDeviceInfo where
        parseXML x
          = GpuDeviceInfo' Core.<$>
              (x Core..@? "count") Core.<*> x Core..@? "manufacturer" Core.<*>
                x Core..@? "memoryInfo"
                Core.<*> x Core..@? "name"

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FpgaDeviceInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.FpgaDeviceInfo
  ( FpgaDeviceInfo (..)
  -- * Smart constructor
  , mkFpgaDeviceInfo
  -- * Lenses
  , fdiCount
  , fdiManufacturer
  , fdiMemoryInfo
  , fdiName
  ) where

import qualified Network.AWS.EC2.Types.FpgaDeviceManufacturerName as Types
import qualified Network.AWS.EC2.Types.FpgaDeviceMemoryInfo as Types
import qualified Network.AWS.EC2.Types.FpgaDeviceName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the FPGA accelerator for the instance type.
--
-- /See:/ 'mkFpgaDeviceInfo' smart constructor.
data FpgaDeviceInfo = FpgaDeviceInfo'
  { count :: Core.Maybe Core.Int
    -- ^ The count of FPGA accelerators for the instance type.
  , manufacturer :: Core.Maybe Types.FpgaDeviceManufacturerName
    -- ^ The manufacturer of the FPGA accelerator.
  , memoryInfo :: Core.Maybe Types.FpgaDeviceMemoryInfo
    -- ^ Describes the memory for the FPGA accelerator for the instance type.
  , name :: Core.Maybe Types.FpgaDeviceName
    -- ^ The name of the FPGA accelerator.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FpgaDeviceInfo' value with any optional fields omitted.
mkFpgaDeviceInfo
    :: FpgaDeviceInfo
mkFpgaDeviceInfo
  = FpgaDeviceInfo'{count = Core.Nothing,
                    manufacturer = Core.Nothing, memoryInfo = Core.Nothing,
                    name = Core.Nothing}

-- | The count of FPGA accelerators for the instance type.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdiCount :: Lens.Lens' FpgaDeviceInfo (Core.Maybe Core.Int)
fdiCount = Lens.field @"count"
{-# INLINEABLE fdiCount #-}
{-# DEPRECATED count "Use generic-lens or generic-optics with 'count' instead"  #-}

-- | The manufacturer of the FPGA accelerator.
--
-- /Note:/ Consider using 'manufacturer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdiManufacturer :: Lens.Lens' FpgaDeviceInfo (Core.Maybe Types.FpgaDeviceManufacturerName)
fdiManufacturer = Lens.field @"manufacturer"
{-# INLINEABLE fdiManufacturer #-}
{-# DEPRECATED manufacturer "Use generic-lens or generic-optics with 'manufacturer' instead"  #-}

-- | Describes the memory for the FPGA accelerator for the instance type.
--
-- /Note:/ Consider using 'memoryInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdiMemoryInfo :: Lens.Lens' FpgaDeviceInfo (Core.Maybe Types.FpgaDeviceMemoryInfo)
fdiMemoryInfo = Lens.field @"memoryInfo"
{-# INLINEABLE fdiMemoryInfo #-}
{-# DEPRECATED memoryInfo "Use generic-lens or generic-optics with 'memoryInfo' instead"  #-}

-- | The name of the FPGA accelerator.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdiName :: Lens.Lens' FpgaDeviceInfo (Core.Maybe Types.FpgaDeviceName)
fdiName = Lens.field @"name"
{-# INLINEABLE fdiName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromXML FpgaDeviceInfo where
        parseXML x
          = FpgaDeviceInfo' Core.<$>
              (x Core..@? "count") Core.<*> x Core..@? "manufacturer" Core.<*>
                x Core..@? "memoryInfo"
                Core.<*> x Core..@? "name"

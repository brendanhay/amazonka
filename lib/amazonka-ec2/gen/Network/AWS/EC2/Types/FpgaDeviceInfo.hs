{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FpgaDeviceInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FpgaDeviceInfo
  ( FpgaDeviceInfo (..),

    -- * Smart constructor
    mkFpgaDeviceInfo,

    -- * Lenses
    fdiCount,
    fdiManufacturer,
    fdiMemoryInfo,
    fdiName,
  )
where

import qualified Network.AWS.EC2.Types.FpgaDeviceManufacturerName as Types
import qualified Network.AWS.EC2.Types.FpgaDeviceMemoryInfo as Types
import qualified Network.AWS.EC2.Types.FpgaDeviceName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the FPGA accelerator for the instance type.
--
-- /See:/ 'mkFpgaDeviceInfo' smart constructor.
data FpgaDeviceInfo = FpgaDeviceInfo'
  { -- | The count of FPGA accelerators for the instance type.
    count :: Core.Maybe Core.Int,
    -- | The manufacturer of the FPGA accelerator.
    manufacturer :: Core.Maybe Types.FpgaDeviceManufacturerName,
    -- | Describes the memory for the FPGA accelerator for the instance type.
    memoryInfo :: Core.Maybe Types.FpgaDeviceMemoryInfo,
    -- | The name of the FPGA accelerator.
    name :: Core.Maybe Types.FpgaDeviceName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FpgaDeviceInfo' value with any optional fields omitted.
mkFpgaDeviceInfo ::
  FpgaDeviceInfo
mkFpgaDeviceInfo =
  FpgaDeviceInfo'
    { count = Core.Nothing,
      manufacturer = Core.Nothing,
      memoryInfo = Core.Nothing,
      name = Core.Nothing
    }

-- | The count of FPGA accelerators for the instance type.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdiCount :: Lens.Lens' FpgaDeviceInfo (Core.Maybe Core.Int)
fdiCount = Lens.field @"count"
{-# DEPRECATED fdiCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | The manufacturer of the FPGA accelerator.
--
-- /Note:/ Consider using 'manufacturer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdiManufacturer :: Lens.Lens' FpgaDeviceInfo (Core.Maybe Types.FpgaDeviceManufacturerName)
fdiManufacturer = Lens.field @"manufacturer"
{-# DEPRECATED fdiManufacturer "Use generic-lens or generic-optics with 'manufacturer' instead." #-}

-- | Describes the memory for the FPGA accelerator for the instance type.
--
-- /Note:/ Consider using 'memoryInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdiMemoryInfo :: Lens.Lens' FpgaDeviceInfo (Core.Maybe Types.FpgaDeviceMemoryInfo)
fdiMemoryInfo = Lens.field @"memoryInfo"
{-# DEPRECATED fdiMemoryInfo "Use generic-lens or generic-optics with 'memoryInfo' instead." #-}

-- | The name of the FPGA accelerator.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdiName :: Lens.Lens' FpgaDeviceInfo (Core.Maybe Types.FpgaDeviceName)
fdiName = Lens.field @"name"
{-# DEPRECATED fdiName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromXML FpgaDeviceInfo where
  parseXML x =
    FpgaDeviceInfo'
      Core.<$> (x Core..@? "count")
      Core.<*> (x Core..@? "manufacturer")
      Core.<*> (x Core..@? "memoryInfo")
      Core.<*> (x Core..@? "name")

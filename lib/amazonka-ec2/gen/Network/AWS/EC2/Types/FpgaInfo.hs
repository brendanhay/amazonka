{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FpgaInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FpgaInfo
  ( FpgaInfo (..),

    -- * Smart constructor
    mkFpgaInfo,

    -- * Lenses
    fiFpgas,
    fiTotalFpgaMemoryInMiB,
  )
where

import qualified Network.AWS.EC2.Types.FpgaDeviceInfo as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the FPGAs for the instance type.
--
-- /See:/ 'mkFpgaInfo' smart constructor.
data FpgaInfo = FpgaInfo'
  { -- | Describes the FPGAs for the instance type.
    fpgas :: Core.Maybe [Types.FpgaDeviceInfo],
    -- | The total memory of all FPGA accelerators for the instance type.
    totalFpgaMemoryInMiB :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FpgaInfo' value with any optional fields omitted.
mkFpgaInfo ::
  FpgaInfo
mkFpgaInfo =
  FpgaInfo'
    { fpgas = Core.Nothing,
      totalFpgaMemoryInMiB = Core.Nothing
    }

-- | Describes the FPGAs for the instance type.
--
-- /Note:/ Consider using 'fpgas' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiFpgas :: Lens.Lens' FpgaInfo (Core.Maybe [Types.FpgaDeviceInfo])
fiFpgas = Lens.field @"fpgas"
{-# DEPRECATED fiFpgas "Use generic-lens or generic-optics with 'fpgas' instead." #-}

-- | The total memory of all FPGA accelerators for the instance type.
--
-- /Note:/ Consider using 'totalFpgaMemoryInMiB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiTotalFpgaMemoryInMiB :: Lens.Lens' FpgaInfo (Core.Maybe Core.Int)
fiTotalFpgaMemoryInMiB = Lens.field @"totalFpgaMemoryInMiB"
{-# DEPRECATED fiTotalFpgaMemoryInMiB "Use generic-lens or generic-optics with 'totalFpgaMemoryInMiB' instead." #-}

instance Core.FromXML FpgaInfo where
  parseXML x =
    FpgaInfo'
      Core.<$> (x Core..@? "fpgas" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "totalFpgaMemoryInMiB")

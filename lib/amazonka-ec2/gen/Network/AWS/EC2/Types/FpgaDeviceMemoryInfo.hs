{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FpgaDeviceMemoryInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.FpgaDeviceMemoryInfo
  ( FpgaDeviceMemoryInfo (..)
  -- * Smart constructor
  , mkFpgaDeviceMemoryInfo
  -- * Lenses
  , fdmiSizeInMiB
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the memory for the FPGA accelerator for the instance type.
--
-- /See:/ 'mkFpgaDeviceMemoryInfo' smart constructor.
newtype FpgaDeviceMemoryInfo = FpgaDeviceMemoryInfo'
  { sizeInMiB :: Core.Maybe Core.Int
    -- ^ The size of the memory available to the FPGA accelerator, in MiB.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'FpgaDeviceMemoryInfo' value with any optional fields omitted.
mkFpgaDeviceMemoryInfo
    :: FpgaDeviceMemoryInfo
mkFpgaDeviceMemoryInfo
  = FpgaDeviceMemoryInfo'{sizeInMiB = Core.Nothing}

-- | The size of the memory available to the FPGA accelerator, in MiB.
--
-- /Note:/ Consider using 'sizeInMiB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdmiSizeInMiB :: Lens.Lens' FpgaDeviceMemoryInfo (Core.Maybe Core.Int)
fdmiSizeInMiB = Lens.field @"sizeInMiB"
{-# INLINEABLE fdmiSizeInMiB #-}
{-# DEPRECATED sizeInMiB "Use generic-lens or generic-optics with 'sizeInMiB' instead"  #-}

instance Core.FromXML FpgaDeviceMemoryInfo where
        parseXML x
          = FpgaDeviceMemoryInfo' Core.<$> (x Core..@? "sizeInMiB")

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.GpuDeviceMemoryInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.GpuDeviceMemoryInfo
  ( GpuDeviceMemoryInfo (..)
  -- * Smart constructor
  , mkGpuDeviceMemoryInfo
  -- * Lenses
  , gdmiSizeInMiB
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the memory available to the GPU accelerator.
--
-- /See:/ 'mkGpuDeviceMemoryInfo' smart constructor.
newtype GpuDeviceMemoryInfo = GpuDeviceMemoryInfo'
  { sizeInMiB :: Core.Maybe Core.Int
    -- ^ The size of the memory available to the GPU accelerator, in MiB.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GpuDeviceMemoryInfo' value with any optional fields omitted.
mkGpuDeviceMemoryInfo
    :: GpuDeviceMemoryInfo
mkGpuDeviceMemoryInfo
  = GpuDeviceMemoryInfo'{sizeInMiB = Core.Nothing}

-- | The size of the memory available to the GPU accelerator, in MiB.
--
-- /Note:/ Consider using 'sizeInMiB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdmiSizeInMiB :: Lens.Lens' GpuDeviceMemoryInfo (Core.Maybe Core.Int)
gdmiSizeInMiB = Lens.field @"sizeInMiB"
{-# INLINEABLE gdmiSizeInMiB #-}
{-# DEPRECATED sizeInMiB "Use generic-lens or generic-optics with 'sizeInMiB' instead"  #-}

instance Core.FromXML GpuDeviceMemoryInfo where
        parseXML x = GpuDeviceMemoryInfo' Core.<$> (x Core..@? "sizeInMiB")

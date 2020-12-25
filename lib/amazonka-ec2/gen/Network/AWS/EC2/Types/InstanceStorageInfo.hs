{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceStorageInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceStorageInfo
  ( InstanceStorageInfo (..),

    -- * Smart constructor
    mkInstanceStorageInfo,

    -- * Lenses
    isiDisks,
    isiNvmeSupport,
    isiTotalSizeInGB,
  )
where

import qualified Network.AWS.EC2.Types.DiskInfo as Types
import qualified Network.AWS.EC2.Types.EphemeralNvmeSupport as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the disks that are available for the instance type.
--
-- /See:/ 'mkInstanceStorageInfo' smart constructor.
data InstanceStorageInfo = InstanceStorageInfo'
  { -- | Describes the disks that are available for the instance type.
    disks :: Core.Maybe [Types.DiskInfo],
    -- | Indicates whether non-volatile memory express (NVMe) is supported for instance store.
    nvmeSupport :: Core.Maybe Types.EphemeralNvmeSupport,
    -- | The total size of the disks, in GB.
    totalSizeInGB :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceStorageInfo' value with any optional fields omitted.
mkInstanceStorageInfo ::
  InstanceStorageInfo
mkInstanceStorageInfo =
  InstanceStorageInfo'
    { disks = Core.Nothing,
      nvmeSupport = Core.Nothing,
      totalSizeInGB = Core.Nothing
    }

-- | Describes the disks that are available for the instance type.
--
-- /Note:/ Consider using 'disks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isiDisks :: Lens.Lens' InstanceStorageInfo (Core.Maybe [Types.DiskInfo])
isiDisks = Lens.field @"disks"
{-# DEPRECATED isiDisks "Use generic-lens or generic-optics with 'disks' instead." #-}

-- | Indicates whether non-volatile memory express (NVMe) is supported for instance store.
--
-- /Note:/ Consider using 'nvmeSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isiNvmeSupport :: Lens.Lens' InstanceStorageInfo (Core.Maybe Types.EphemeralNvmeSupport)
isiNvmeSupport = Lens.field @"nvmeSupport"
{-# DEPRECATED isiNvmeSupport "Use generic-lens or generic-optics with 'nvmeSupport' instead." #-}

-- | The total size of the disks, in GB.
--
-- /Note:/ Consider using 'totalSizeInGB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isiTotalSizeInGB :: Lens.Lens' InstanceStorageInfo (Core.Maybe Core.Integer)
isiTotalSizeInGB = Lens.field @"totalSizeInGB"
{-# DEPRECATED isiTotalSizeInGB "Use generic-lens or generic-optics with 'totalSizeInGB' instead." #-}

instance Core.FromXML InstanceStorageInfo where
  parseXML x =
    InstanceStorageInfo'
      Core.<$> (x Core..@? "disks" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "nvmeSupport")
      Core.<*> (x Core..@? "totalSizeInGB")

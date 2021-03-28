{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DiskInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.DiskInfo
  ( DiskInfo (..)
  -- * Smart constructor
  , mkDiskInfo
  -- * Lenses
  , diCount
  , diSizeInGB
  , diType
  ) where

import qualified Network.AWS.EC2.Types.DiskType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the disk.
--
-- /See:/ 'mkDiskInfo' smart constructor.
data DiskInfo = DiskInfo'
  { count :: Core.Maybe Core.Int
    -- ^ The number of disks with this configuration.
  , sizeInGB :: Core.Maybe Core.Integer
    -- ^ The size of the disk in GB.
  , type' :: Core.Maybe Types.DiskType
    -- ^ The type of disk.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DiskInfo' value with any optional fields omitted.
mkDiskInfo
    :: DiskInfo
mkDiskInfo
  = DiskInfo'{count = Core.Nothing, sizeInGB = Core.Nothing,
              type' = Core.Nothing}

-- | The number of disks with this configuration.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diCount :: Lens.Lens' DiskInfo (Core.Maybe Core.Int)
diCount = Lens.field @"count"
{-# INLINEABLE diCount #-}
{-# DEPRECATED count "Use generic-lens or generic-optics with 'count' instead"  #-}

-- | The size of the disk in GB.
--
-- /Note:/ Consider using 'sizeInGB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diSizeInGB :: Lens.Lens' DiskInfo (Core.Maybe Core.Integer)
diSizeInGB = Lens.field @"sizeInGB"
{-# INLINEABLE diSizeInGB #-}
{-# DEPRECATED sizeInGB "Use generic-lens or generic-optics with 'sizeInGB' instead"  #-}

-- | The type of disk.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diType :: Lens.Lens' DiskInfo (Core.Maybe Types.DiskType)
diType = Lens.field @"type'"
{-# INLINEABLE diType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromXML DiskInfo where
        parseXML x
          = DiskInfo' Core.<$>
              (x Core..@? "count") Core.<*> x Core..@? "sizeInGB" Core.<*>
                x Core..@? "type"

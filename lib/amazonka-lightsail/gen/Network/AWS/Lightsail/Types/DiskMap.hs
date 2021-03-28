{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.DiskMap
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.DiskMap
  ( DiskMap (..)
  -- * Smart constructor
  , mkDiskMap
  -- * Lenses
  , dmNewDiskName
  , dmOriginalDiskPath
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.OriginalDiskPath as Types
import qualified Network.AWS.Lightsail.Types.ResourceName as Types
import qualified Network.AWS.Prelude as Core

-- | Describes a block storage disk mapping.
--
-- /See:/ 'mkDiskMap' smart constructor.
data DiskMap = DiskMap'
  { newDiskName :: Core.Maybe Types.ResourceName
    -- ^ The new disk name (e.g., @my-new-disk@ ).
  , originalDiskPath :: Core.Maybe Types.OriginalDiskPath
    -- ^ The original disk path exposed to the instance (for example, @/dev/sdh@ ).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DiskMap' value with any optional fields omitted.
mkDiskMap
    :: DiskMap
mkDiskMap
  = DiskMap'{newDiskName = Core.Nothing,
             originalDiskPath = Core.Nothing}

-- | The new disk name (e.g., @my-new-disk@ ).
--
-- /Note:/ Consider using 'newDiskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmNewDiskName :: Lens.Lens' DiskMap (Core.Maybe Types.ResourceName)
dmNewDiskName = Lens.field @"newDiskName"
{-# INLINEABLE dmNewDiskName #-}
{-# DEPRECATED newDiskName "Use generic-lens or generic-optics with 'newDiskName' instead"  #-}

-- | The original disk path exposed to the instance (for example, @/dev/sdh@ ).
--
-- /Note:/ Consider using 'originalDiskPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmOriginalDiskPath :: Lens.Lens' DiskMap (Core.Maybe Types.OriginalDiskPath)
dmOriginalDiskPath = Lens.field @"originalDiskPath"
{-# INLINEABLE dmOriginalDiskPath #-}
{-# DEPRECATED originalDiskPath "Use generic-lens or generic-optics with 'originalDiskPath' instead"  #-}

instance Core.FromJSON DiskMap where
        toJSON DiskMap{..}
          = Core.object
              (Core.catMaybes
                 [("newDiskName" Core..=) Core.<$> newDiskName,
                  ("originalDiskPath" Core..=) Core.<$> originalDiskPath])

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstanceSnapshotInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.InstanceSnapshotInfo
  ( InstanceSnapshotInfo (..)
  -- * Smart constructor
  , mkInstanceSnapshotInfo
  -- * Lenses
  , isiFromBlueprintId
  , isiFromBundleId
  , isiFromDiskInfo
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.DiskInfo as Types
import qualified Network.AWS.Lightsail.Types.NonEmptyString as Types
import qualified Network.AWS.Prelude as Core

-- | Describes an instance snapshot.
--
-- /See:/ 'mkInstanceSnapshotInfo' smart constructor.
data InstanceSnapshotInfo = InstanceSnapshotInfo'
  { fromBlueprintId :: Core.Maybe Types.NonEmptyString
    -- ^ The blueprint ID from which the source instance (e.g., @os_debian_8_3@ ).
  , fromBundleId :: Core.Maybe Types.NonEmptyString
    -- ^ The bundle ID from which the source instance was created (e.g., @micro_1_0@ ).
  , fromDiskInfo :: Core.Maybe [Types.DiskInfo]
    -- ^ A list of objects describing the disks that were attached to the source instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceSnapshotInfo' value with any optional fields omitted.
mkInstanceSnapshotInfo
    :: InstanceSnapshotInfo
mkInstanceSnapshotInfo
  = InstanceSnapshotInfo'{fromBlueprintId = Core.Nothing,
                          fromBundleId = Core.Nothing, fromDiskInfo = Core.Nothing}

-- | The blueprint ID from which the source instance (e.g., @os_debian_8_3@ ).
--
-- /Note:/ Consider using 'fromBlueprintId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isiFromBlueprintId :: Lens.Lens' InstanceSnapshotInfo (Core.Maybe Types.NonEmptyString)
isiFromBlueprintId = Lens.field @"fromBlueprintId"
{-# INLINEABLE isiFromBlueprintId #-}
{-# DEPRECATED fromBlueprintId "Use generic-lens or generic-optics with 'fromBlueprintId' instead"  #-}

-- | The bundle ID from which the source instance was created (e.g., @micro_1_0@ ).
--
-- /Note:/ Consider using 'fromBundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isiFromBundleId :: Lens.Lens' InstanceSnapshotInfo (Core.Maybe Types.NonEmptyString)
isiFromBundleId = Lens.field @"fromBundleId"
{-# INLINEABLE isiFromBundleId #-}
{-# DEPRECATED fromBundleId "Use generic-lens or generic-optics with 'fromBundleId' instead"  #-}

-- | A list of objects describing the disks that were attached to the source instance.
--
-- /Note:/ Consider using 'fromDiskInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isiFromDiskInfo :: Lens.Lens' InstanceSnapshotInfo (Core.Maybe [Types.DiskInfo])
isiFromDiskInfo = Lens.field @"fromDiskInfo"
{-# INLINEABLE isiFromDiskInfo #-}
{-# DEPRECATED fromDiskInfo "Use generic-lens or generic-optics with 'fromDiskInfo' instead"  #-}

instance Core.FromJSON InstanceSnapshotInfo where
        parseJSON
          = Core.withObject "InstanceSnapshotInfo" Core.$
              \ x ->
                InstanceSnapshotInfo' Core.<$>
                  (x Core..:? "fromBlueprintId") Core.<*> x Core..:? "fromBundleId"
                    Core.<*> x Core..:? "fromDiskInfo"

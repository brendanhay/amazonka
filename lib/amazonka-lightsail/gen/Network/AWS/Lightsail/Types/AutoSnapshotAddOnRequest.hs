{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.AutoSnapshotAddOnRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.AutoSnapshotAddOnRequest
  ( AutoSnapshotAddOnRequest (..)
  -- * Smart constructor
  , mkAutoSnapshotAddOnRequest
  -- * Lenses
  , asaorSnapshotTimeOfDay
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.SnapshotTimeOfDay as Types
import qualified Network.AWS.Prelude as Core

-- | Describes a request to enable or modify the automatic snapshot add-on for an Amazon Lightsail instance or disk.
--
-- When you modify the automatic snapshot time for a resource, it is typically effective immediately except under the following conditions:
--
--     * If an automatic snapshot has been created for the current day, and you change the snapshot time to a later time of day, then the new snapshot time will be effective the following day. This ensures that two snapshots are not created for the current day.
--
--
--     * If an automatic snapshot has not yet been created for the current day, and you change the snapshot time to an earlier time of day, then the new snapshot time will be effective the following day and a snapshot is automatically created at the previously set time for the current day. This ensures that a snapshot is created for the current day.
--
--
--     * If an automatic snapshot has not yet been created for the current day, and you change the snapshot time to a time that is within 30 minutes from your current time, then the new snapshot time will be effective the following day and a snapshot is automatically created at the previously set time for the current day. This ensures that a snapshot is created for the current day, because 30 minutes is required between your current time and the new snapshot time that you specify.
--
--
--     * If an automatic snapshot is scheduled to be created within 30 minutes from your current time and you change the snapshot time, then the new snapshot time will be effective the following day and a snapshot is automatically created at the previously set time for the current day. This ensures that a snapshot is created for the current day, because 30 minutes is required between your current time and the new snapshot time that you specify.
--
--
--
-- /See:/ 'mkAutoSnapshotAddOnRequest' smart constructor.
newtype AutoSnapshotAddOnRequest = AutoSnapshotAddOnRequest'
  { snapshotTimeOfDay :: Core.Maybe Types.SnapshotTimeOfDay
    -- ^ The daily time when an automatic snapshot will be created.
--
-- Constraints:
--
--     * Must be in @HH:00@ format, and in an hourly increment.
--
--
--     * Specified in Coordinated Universal Time (UTC).
--
--
--     * The snapshot will be automatically created between the time specified and up to 45 minutes after.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AutoSnapshotAddOnRequest' value with any optional fields omitted.
mkAutoSnapshotAddOnRequest
    :: AutoSnapshotAddOnRequest
mkAutoSnapshotAddOnRequest
  = AutoSnapshotAddOnRequest'{snapshotTimeOfDay = Core.Nothing}

-- | The daily time when an automatic snapshot will be created.
--
-- Constraints:
--
--     * Must be in @HH:00@ format, and in an hourly increment.
--
--
--     * Specified in Coordinated Universal Time (UTC).
--
--
--     * The snapshot will be automatically created between the time specified and up to 45 minutes after.
--
--
--
-- /Note:/ Consider using 'snapshotTimeOfDay' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asaorSnapshotTimeOfDay :: Lens.Lens' AutoSnapshotAddOnRequest (Core.Maybe Types.SnapshotTimeOfDay)
asaorSnapshotTimeOfDay = Lens.field @"snapshotTimeOfDay"
{-# INLINEABLE asaorSnapshotTimeOfDay #-}
{-# DEPRECATED snapshotTimeOfDay "Use generic-lens or generic-optics with 'snapshotTimeOfDay' instead"  #-}

instance Core.FromJSON AutoSnapshotAddOnRequest where
        toJSON AutoSnapshotAddOnRequest{..}
          = Core.object
              (Core.catMaybes
                 [("snapshotTimeOfDay" Core..=) Core.<$> snapshotTimeOfDay])

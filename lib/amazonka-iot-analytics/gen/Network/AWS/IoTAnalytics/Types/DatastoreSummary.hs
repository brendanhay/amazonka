{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatastoreSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatastoreSummary
  ( DatastoreSummary (..),

    -- * Smart constructor
    mkDatastoreSummary,

    -- * Lenses
    dsCreationTime,
    dsDatastoreName,
    dsDatastoreStorage,
    dsLastMessageArrivalTime,
    dsLastUpdateTime,
    dsStatus,
  )
where

import qualified Network.AWS.IoTAnalytics.Types.DatastoreName as Types
import qualified Network.AWS.IoTAnalytics.Types.DatastoreStatus as Types
import qualified Network.AWS.IoTAnalytics.Types.DatastoreStorageSummary as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A summary of information about a data store.
--
-- /See:/ 'mkDatastoreSummary' smart constructor.
data DatastoreSummary = DatastoreSummary'
  { -- | When the data store was created.
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the data store.
    datastoreName :: Core.Maybe Types.DatastoreName,
    -- | Where data store data is stored.
    datastoreStorage :: Core.Maybe Types.DatastoreStorageSummary,
    -- | The last time when a new message arrived in the data store.
    --
    -- AWS IoT Analytics updates this value at most once per minute for one data store. Hence, the @lastMessageArrivalTime@ value is an approximation.
    -- This feature only applies to messages that arrived in the data store after October 23, 2020.
    lastMessageArrivalTime :: Core.Maybe Core.NominalDiffTime,
    -- | The last time the data store was updated.
    lastUpdateTime :: Core.Maybe Core.NominalDiffTime,
    -- | The status of the data store.
    status :: Core.Maybe Types.DatastoreStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DatastoreSummary' value with any optional fields omitted.
mkDatastoreSummary ::
  DatastoreSummary
mkDatastoreSummary =
  DatastoreSummary'
    { creationTime = Core.Nothing,
      datastoreName = Core.Nothing,
      datastoreStorage = Core.Nothing,
      lastMessageArrivalTime = Core.Nothing,
      lastUpdateTime = Core.Nothing,
      status = Core.Nothing
    }

-- | When the data store was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsCreationTime :: Lens.Lens' DatastoreSummary (Core.Maybe Core.NominalDiffTime)
dsCreationTime = Lens.field @"creationTime"
{-# DEPRECATED dsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The name of the data store.
--
-- /Note:/ Consider using 'datastoreName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDatastoreName :: Lens.Lens' DatastoreSummary (Core.Maybe Types.DatastoreName)
dsDatastoreName = Lens.field @"datastoreName"
{-# DEPRECATED dsDatastoreName "Use generic-lens or generic-optics with 'datastoreName' instead." #-}

-- | Where data store data is stored.
--
-- /Note:/ Consider using 'datastoreStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDatastoreStorage :: Lens.Lens' DatastoreSummary (Core.Maybe Types.DatastoreStorageSummary)
dsDatastoreStorage = Lens.field @"datastoreStorage"
{-# DEPRECATED dsDatastoreStorage "Use generic-lens or generic-optics with 'datastoreStorage' instead." #-}

-- | The last time when a new message arrived in the data store.
--
-- AWS IoT Analytics updates this value at most once per minute for one data store. Hence, the @lastMessageArrivalTime@ value is an approximation.
-- This feature only applies to messages that arrived in the data store after October 23, 2020.
--
-- /Note:/ Consider using 'lastMessageArrivalTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsLastMessageArrivalTime :: Lens.Lens' DatastoreSummary (Core.Maybe Core.NominalDiffTime)
dsLastMessageArrivalTime = Lens.field @"lastMessageArrivalTime"
{-# DEPRECATED dsLastMessageArrivalTime "Use generic-lens or generic-optics with 'lastMessageArrivalTime' instead." #-}

-- | The last time the data store was updated.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsLastUpdateTime :: Lens.Lens' DatastoreSummary (Core.Maybe Core.NominalDiffTime)
dsLastUpdateTime = Lens.field @"lastUpdateTime"
{-# DEPRECATED dsLastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead." #-}

-- | The status of the data store.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsStatus :: Lens.Lens' DatastoreSummary (Core.Maybe Types.DatastoreStatus)
dsStatus = Lens.field @"status"
{-# DEPRECATED dsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON DatastoreSummary where
  parseJSON =
    Core.withObject "DatastoreSummary" Core.$
      \x ->
        DatastoreSummary'
          Core.<$> (x Core..:? "creationTime")
          Core.<*> (x Core..:? "datastoreName")
          Core.<*> (x Core..:? "datastoreStorage")
          Core.<*> (x Core..:? "lastMessageArrivalTime")
          Core.<*> (x Core..:? "lastUpdateTime")
          Core.<*> (x Core..:? "status")

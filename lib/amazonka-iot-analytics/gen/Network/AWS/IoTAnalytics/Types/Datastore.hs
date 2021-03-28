{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.Datastore
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.Datastore
  ( Datastore (..)
  -- * Smart constructor
  , mkDatastore
  -- * Lenses
  , dfArn
  , dfCreationTime
  , dfLastMessageArrivalTime
  , dfLastUpdateTime
  , dfName
  , dfRetentionPeriod
  , dfStatus
  , dfStorage
  ) where

import qualified Network.AWS.IoTAnalytics.Types.DatastoreArn as Types
import qualified Network.AWS.IoTAnalytics.Types.DatastoreName as Types
import qualified Network.AWS.IoTAnalytics.Types.DatastoreStatus as Types
import qualified Network.AWS.IoTAnalytics.Types.DatastoreStorage as Types
import qualified Network.AWS.IoTAnalytics.Types.RetentionPeriod as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a data store.
--
-- /See:/ 'mkDatastore' smart constructor.
data Datastore = Datastore'
  { arn :: Core.Maybe Types.DatastoreArn
    -- ^ The ARN of the data store.
  , creationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ When the data store was created.
  , lastMessageArrivalTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The last time when a new message arrived in the data store.
--
-- AWS IoT Analytics updates this value at most once per minute for one data store. Hence, the @lastMessageArrivalTime@ value is an approximation.
-- This feature only applies to messages that arrived in the data store after October 23, 2020. 
  , lastUpdateTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The last time the data store was updated.
  , name :: Core.Maybe Types.DatastoreName
    -- ^ The name of the data store.
  , retentionPeriod :: Core.Maybe Types.RetentionPeriod
    -- ^ How long, in days, message data is kept for the data store. When @customerManagedS3@ storage is selected, this parameter is ignored.
  , status :: Core.Maybe Types.DatastoreStatus
    -- ^ The status of a data store:
--
--
--     * CREATING
--
--     * The data store is being created.
--
--
--     * ACTIVE
--
--     * The data store has been created and can be used.
--
--
--     * DELETING
--
--     * The data store is being deleted.
--
--
  , storage :: Core.Maybe Types.DatastoreStorage
    -- ^ Where data store data is stored. You can choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . You cannot change this storage option after the data store is created.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Datastore' value with any optional fields omitted.
mkDatastore
    :: Datastore
mkDatastore
  = Datastore'{arn = Core.Nothing, creationTime = Core.Nothing,
               lastMessageArrivalTime = Core.Nothing,
               lastUpdateTime = Core.Nothing, name = Core.Nothing,
               retentionPeriod = Core.Nothing, status = Core.Nothing,
               storage = Core.Nothing}

-- | The ARN of the data store.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfArn :: Lens.Lens' Datastore (Core.Maybe Types.DatastoreArn)
dfArn = Lens.field @"arn"
{-# INLINEABLE dfArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | When the data store was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfCreationTime :: Lens.Lens' Datastore (Core.Maybe Core.NominalDiffTime)
dfCreationTime = Lens.field @"creationTime"
{-# INLINEABLE dfCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The last time when a new message arrived in the data store.
--
-- AWS IoT Analytics updates this value at most once per minute for one data store. Hence, the @lastMessageArrivalTime@ value is an approximation.
-- This feature only applies to messages that arrived in the data store after October 23, 2020. 
--
-- /Note:/ Consider using 'lastMessageArrivalTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfLastMessageArrivalTime :: Lens.Lens' Datastore (Core.Maybe Core.NominalDiffTime)
dfLastMessageArrivalTime = Lens.field @"lastMessageArrivalTime"
{-# INLINEABLE dfLastMessageArrivalTime #-}
{-# DEPRECATED lastMessageArrivalTime "Use generic-lens or generic-optics with 'lastMessageArrivalTime' instead"  #-}

-- | The last time the data store was updated.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfLastUpdateTime :: Lens.Lens' Datastore (Core.Maybe Core.NominalDiffTime)
dfLastUpdateTime = Lens.field @"lastUpdateTime"
{-# INLINEABLE dfLastUpdateTime #-}
{-# DEPRECATED lastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead"  #-}

-- | The name of the data store.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfName :: Lens.Lens' Datastore (Core.Maybe Types.DatastoreName)
dfName = Lens.field @"name"
{-# INLINEABLE dfName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | How long, in days, message data is kept for the data store. When @customerManagedS3@ storage is selected, this parameter is ignored.
--
-- /Note:/ Consider using 'retentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfRetentionPeriod :: Lens.Lens' Datastore (Core.Maybe Types.RetentionPeriod)
dfRetentionPeriod = Lens.field @"retentionPeriod"
{-# INLINEABLE dfRetentionPeriod #-}
{-# DEPRECATED retentionPeriod "Use generic-lens or generic-optics with 'retentionPeriod' instead"  #-}

-- | The status of a data store:
--
--
--     * CREATING
--
--     * The data store is being created.
--
--
--     * ACTIVE
--
--     * The data store has been created and can be used.
--
--
--     * DELETING
--
--     * The data store is being deleted.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfStatus :: Lens.Lens' Datastore (Core.Maybe Types.DatastoreStatus)
dfStatus = Lens.field @"status"
{-# INLINEABLE dfStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | Where data store data is stored. You can choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . You cannot change this storage option after the data store is created.
--
-- /Note:/ Consider using 'storage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfStorage :: Lens.Lens' Datastore (Core.Maybe Types.DatastoreStorage)
dfStorage = Lens.field @"storage"
{-# INLINEABLE dfStorage #-}
{-# DEPRECATED storage "Use generic-lens or generic-optics with 'storage' instead"  #-}

instance Core.FromJSON Datastore where
        parseJSON
          = Core.withObject "Datastore" Core.$
              \ x ->
                Datastore' Core.<$>
                  (x Core..:? "arn") Core.<*> x Core..:? "creationTime" Core.<*>
                    x Core..:? "lastMessageArrivalTime"
                    Core.<*> x Core..:? "lastUpdateTime"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "retentionPeriod"
                    Core.<*> x Core..:? "status"
                    Core.<*> x Core..:? "storage"

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.UpdateDatastore
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the settings of a data store.
module Network.AWS.IoTAnalytics.UpdateDatastore
  ( -- * Creating a request
    UpdateDatastore (..),
    mkUpdateDatastore,

    -- ** Request lenses
    uDatastoreName,
    uDatastoreStorage,
    uRetentionPeriod,

    -- * Destructuring the response
    UpdateDatastoreResponse (..),
    mkUpdateDatastoreResponse,
  )
where

import qualified Network.AWS.IoTAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateDatastore' smart constructor.
data UpdateDatastore = UpdateDatastore'
  { -- | The name of the data store to be updated.
    datastoreName :: Types.DatastoreName,
    -- | Where data store data is stored. You can choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is@serviceManagedS3@ . You cannot change this storage option after the data store is created.
    datastoreStorage :: Core.Maybe Types.DatastoreStorage,
    -- | How long, in days, message data is kept for the data store. The retention period cannot be updated if the data store's S3 storage is customer-managed.
    retentionPeriod :: Core.Maybe Types.RetentionPeriod
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDatastore' value with any optional fields omitted.
mkUpdateDatastore ::
  -- | 'datastoreName'
  Types.DatastoreName ->
  UpdateDatastore
mkUpdateDatastore datastoreName =
  UpdateDatastore'
    { datastoreName,
      datastoreStorage = Core.Nothing,
      retentionPeriod = Core.Nothing
    }

-- | The name of the data store to be updated.
--
-- /Note:/ Consider using 'datastoreName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uDatastoreName :: Lens.Lens' UpdateDatastore Types.DatastoreName
uDatastoreName = Lens.field @"datastoreName"
{-# DEPRECATED uDatastoreName "Use generic-lens or generic-optics with 'datastoreName' instead." #-}

-- | Where data store data is stored. You can choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is@serviceManagedS3@ . You cannot change this storage option after the data store is created.
--
-- /Note:/ Consider using 'datastoreStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uDatastoreStorage :: Lens.Lens' UpdateDatastore (Core.Maybe Types.DatastoreStorage)
uDatastoreStorage = Lens.field @"datastoreStorage"
{-# DEPRECATED uDatastoreStorage "Use generic-lens or generic-optics with 'datastoreStorage' instead." #-}

-- | How long, in days, message data is kept for the data store. The retention period cannot be updated if the data store's S3 storage is customer-managed.
--
-- /Note:/ Consider using 'retentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uRetentionPeriod :: Lens.Lens' UpdateDatastore (Core.Maybe Types.RetentionPeriod)
uRetentionPeriod = Lens.field @"retentionPeriod"
{-# DEPRECATED uRetentionPeriod "Use generic-lens or generic-optics with 'retentionPeriod' instead." #-}

instance Core.FromJSON UpdateDatastore where
  toJSON UpdateDatastore {..} =
    Core.object
      ( Core.catMaybes
          [ ("datastoreStorage" Core..=) Core.<$> datastoreStorage,
            ("retentionPeriod" Core..=) Core.<$> retentionPeriod
          ]
      )

instance Core.AWSRequest UpdateDatastore where
  type Rs UpdateDatastore = UpdateDatastoreResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath ("/datastores/" Core.<> (Core.toText datastoreName)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull UpdateDatastoreResponse'

-- | /See:/ 'mkUpdateDatastoreResponse' smart constructor.
data UpdateDatastoreResponse = UpdateDatastoreResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDatastoreResponse' value with any optional fields omitted.
mkUpdateDatastoreResponse ::
  UpdateDatastoreResponse
mkUpdateDatastoreResponse = UpdateDatastoreResponse'

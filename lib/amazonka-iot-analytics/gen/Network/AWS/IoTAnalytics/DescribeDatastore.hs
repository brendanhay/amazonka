{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.DescribeDatastore
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a data store.
module Network.AWS.IoTAnalytics.DescribeDatastore
  ( -- * Creating a request
    DescribeDatastore (..),
    mkDescribeDatastore,

    -- ** Request lenses
    dDatastoreName,
    dIncludeStatistics,

    -- * Destructuring the response
    DescribeDatastoreResponse (..),
    mkDescribeDatastoreResponse,

    -- ** Response lenses
    drsDatastore,
    drsStatistics,
    drsResponseStatus,
  )
where

import qualified Network.AWS.IoTAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeDatastore' smart constructor.
data DescribeDatastore = DescribeDatastore'
  { -- | The name of the data store
    datastoreName :: Types.DatastoreName,
    -- | If true, additional statistical information about the data store is included in the response. This feature cannot be used with a data store whose S3 storage is customer-managed.
    includeStatistics :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDatastore' value with any optional fields omitted.
mkDescribeDatastore ::
  -- | 'datastoreName'
  Types.DatastoreName ->
  DescribeDatastore
mkDescribeDatastore datastoreName =
  DescribeDatastore'
    { datastoreName,
      includeStatistics = Core.Nothing
    }

-- | The name of the data store
--
-- /Note:/ Consider using 'datastoreName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDatastoreName :: Lens.Lens' DescribeDatastore Types.DatastoreName
dDatastoreName = Lens.field @"datastoreName"
{-# DEPRECATED dDatastoreName "Use generic-lens or generic-optics with 'datastoreName' instead." #-}

-- | If true, additional statistical information about the data store is included in the response. This feature cannot be used with a data store whose S3 storage is customer-managed.
--
-- /Note:/ Consider using 'includeStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dIncludeStatistics :: Lens.Lens' DescribeDatastore (Core.Maybe Core.Bool)
dIncludeStatistics = Lens.field @"includeStatistics"
{-# DEPRECATED dIncludeStatistics "Use generic-lens or generic-optics with 'includeStatistics' instead." #-}

instance Core.AWSRequest DescribeDatastore where
  type Rs DescribeDatastore = DescribeDatastoreResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath ("/datastores/" Core.<> (Core.toText datastoreName)),
        Core._rqQuery =
          Core.toQueryValue "includeStatistics" Core.<$> includeStatistics,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDatastoreResponse'
            Core.<$> (x Core..:? "datastore")
            Core.<*> (x Core..:? "statistics")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeDatastoreResponse' smart constructor.
data DescribeDatastoreResponse = DescribeDatastoreResponse'
  { -- | Information about the data store.
    datastore :: Core.Maybe Types.Datastore,
    -- | Additional statistical information about the data store. Included if the @includeStatistics@ parameter is set to @true@ in the request.
    statistics :: Core.Maybe Types.DatastoreStatistics,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeDatastoreResponse' value with any optional fields omitted.
mkDescribeDatastoreResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeDatastoreResponse
mkDescribeDatastoreResponse responseStatus =
  DescribeDatastoreResponse'
    { datastore = Core.Nothing,
      statistics = Core.Nothing,
      responseStatus
    }

-- | Information about the data store.
--
-- /Note:/ Consider using 'datastore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsDatastore :: Lens.Lens' DescribeDatastoreResponse (Core.Maybe Types.Datastore)
drsDatastore = Lens.field @"datastore"
{-# DEPRECATED drsDatastore "Use generic-lens or generic-optics with 'datastore' instead." #-}

-- | Additional statistical information about the data store. Included if the @includeStatistics@ parameter is set to @true@ in the request.
--
-- /Note:/ Consider using 'statistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsStatistics :: Lens.Lens' DescribeDatastoreResponse (Core.Maybe Types.DatastoreStatistics)
drsStatistics = Lens.field @"statistics"
{-# DEPRECATED drsStatistics "Use generic-lens or generic-optics with 'statistics' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeDatastoreResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

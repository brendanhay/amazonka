{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.DeleteDataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns the DELETED status to a @DataSource@ , rendering it unusable.
--
-- After using the @DeleteDataSource@ operation, you can use the 'GetDataSource' operation to verify that the status of the @DataSource@ changed to DELETED.
-- __Caution:__ The results of the @DeleteDataSource@ operation are irreversible.
module Network.AWS.MachineLearning.DeleteDataSource
  ( -- * Creating a request
    DeleteDataSource (..),
    mkDeleteDataSource,

    -- ** Request lenses
    ddsDataSourceId,

    -- * Destructuring the response
    DeleteDataSourceResponse (..),
    mkDeleteDataSourceResponse,

    -- ** Response lenses
    ddsrrsDataSourceId,
    ddsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MachineLearning.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteDataSource' smart constructor.
newtype DeleteDataSource = DeleteDataSource'
  { -- | A user-supplied ID that uniquely identifies the @DataSource@ .
    dataSourceId :: Types.DataSourceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDataSource' value with any optional fields omitted.
mkDeleteDataSource ::
  -- | 'dataSourceId'
  Types.DataSourceId ->
  DeleteDataSource
mkDeleteDataSource dataSourceId = DeleteDataSource' {dataSourceId}

-- | A user-supplied ID that uniquely identifies the @DataSource@ .
--
-- /Note:/ Consider using 'dataSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsDataSourceId :: Lens.Lens' DeleteDataSource Types.DataSourceId
ddsDataSourceId = Lens.field @"dataSourceId"
{-# DEPRECATED ddsDataSourceId "Use generic-lens or generic-optics with 'dataSourceId' instead." #-}

instance Core.FromJSON DeleteDataSource where
  toJSON DeleteDataSource {..} =
    Core.object
      (Core.catMaybes [Core.Just ("DataSourceId" Core..= dataSourceId)])

instance Core.AWSRequest DeleteDataSource where
  type Rs DeleteDataSource = DeleteDataSourceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonML_20141212.DeleteDataSource")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDataSourceResponse'
            Core.<$> (x Core..:? "DataSourceId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the output of a @DeleteDataSource@ operation.
--
-- /See:/ 'mkDeleteDataSourceResponse' smart constructor.
data DeleteDataSourceResponse = DeleteDataSourceResponse'
  { -- | A user-supplied ID that uniquely identifies the @DataSource@ . This value should be identical to the value of the @DataSourceID@ in the request.
    dataSourceId :: Core.Maybe Types.DataSourceId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDataSourceResponse' value with any optional fields omitted.
mkDeleteDataSourceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteDataSourceResponse
mkDeleteDataSourceResponse responseStatus =
  DeleteDataSourceResponse'
    { dataSourceId = Core.Nothing,
      responseStatus
    }

-- | A user-supplied ID that uniquely identifies the @DataSource@ . This value should be identical to the value of the @DataSourceID@ in the request.
--
-- /Note:/ Consider using 'dataSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsrrsDataSourceId :: Lens.Lens' DeleteDataSourceResponse (Core.Maybe Types.DataSourceId)
ddsrrsDataSourceId = Lens.field @"dataSourceId"
{-# DEPRECATED ddsrrsDataSourceId "Use generic-lens or generic-optics with 'dataSourceId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsrrsResponseStatus :: Lens.Lens' DeleteDataSourceResponse Core.Int
ddsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

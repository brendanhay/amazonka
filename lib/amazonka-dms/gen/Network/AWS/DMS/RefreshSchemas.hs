{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.RefreshSchemas
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Populates the schema for the specified endpoint. This is an asynchronous operation and can take several minutes. You can check the status of this operation by calling the DescribeRefreshSchemasStatus operation.
module Network.AWS.DMS.RefreshSchemas
  ( -- * Creating a request
    RefreshSchemas (..),
    mkRefreshSchemas,

    -- ** Request lenses
    rsEndpointArn,
    rsReplicationInstanceArn,

    -- * Destructuring the response
    RefreshSchemasResponse (..),
    mkRefreshSchemasResponse,

    -- ** Response lenses
    rsrrsRefreshSchemasStatus,
    rsrrsResponseStatus,
  )
where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkRefreshSchemas' smart constructor.
data RefreshSchemas = RefreshSchemas'
  { -- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
    endpointArn :: Types.String,
    -- | The Amazon Resource Name (ARN) of the replication instance.
    replicationInstanceArn :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RefreshSchemas' value with any optional fields omitted.
mkRefreshSchemas ::
  -- | 'endpointArn'
  Types.String ->
  -- | 'replicationInstanceArn'
  Types.String ->
  RefreshSchemas
mkRefreshSchemas endpointArn replicationInstanceArn =
  RefreshSchemas' {endpointArn, replicationInstanceArn}

-- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
--
-- /Note:/ Consider using 'endpointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsEndpointArn :: Lens.Lens' RefreshSchemas Types.String
rsEndpointArn = Lens.field @"endpointArn"
{-# DEPRECATED rsEndpointArn "Use generic-lens or generic-optics with 'endpointArn' instead." #-}

-- | The Amazon Resource Name (ARN) of the replication instance.
--
-- /Note:/ Consider using 'replicationInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsReplicationInstanceArn :: Lens.Lens' RefreshSchemas Types.String
rsReplicationInstanceArn = Lens.field @"replicationInstanceArn"
{-# DEPRECATED rsReplicationInstanceArn "Use generic-lens or generic-optics with 'replicationInstanceArn' instead." #-}

instance Core.FromJSON RefreshSchemas where
  toJSON RefreshSchemas {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("EndpointArn" Core..= endpointArn),
            Core.Just
              ("ReplicationInstanceArn" Core..= replicationInstanceArn)
          ]
      )

instance Core.AWSRequest RefreshSchemas where
  type Rs RefreshSchemas = RefreshSchemasResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonDMSv20160101.RefreshSchemas")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          RefreshSchemasResponse'
            Core.<$> (x Core..:? "RefreshSchemasStatus")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- |
--
-- /See:/ 'mkRefreshSchemasResponse' smart constructor.
data RefreshSchemasResponse = RefreshSchemasResponse'
  { -- | The status of the refreshed schema.
    refreshSchemasStatus :: Core.Maybe Types.RefreshSchemasStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'RefreshSchemasResponse' value with any optional fields omitted.
mkRefreshSchemasResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RefreshSchemasResponse
mkRefreshSchemasResponse responseStatus =
  RefreshSchemasResponse'
    { refreshSchemasStatus = Core.Nothing,
      responseStatus
    }

-- | The status of the refreshed schema.
--
-- /Note:/ Consider using 'refreshSchemasStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsrrsRefreshSchemasStatus :: Lens.Lens' RefreshSchemasResponse (Core.Maybe Types.RefreshSchemasStatus)
rsrrsRefreshSchemasStatus = Lens.field @"refreshSchemasStatus"
{-# DEPRECATED rsrrsRefreshSchemasStatus "Use generic-lens or generic-optics with 'refreshSchemasStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsrrsResponseStatus :: Lens.Lens' RefreshSchemasResponse Core.Int
rsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribeRefreshSchemasStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the status of the RefreshSchemas operation.
module Network.AWS.DMS.DescribeRefreshSchemasStatus
  ( -- * Creating a request
    DescribeRefreshSchemasStatus (..),
    mkDescribeRefreshSchemasStatus,

    -- ** Request lenses
    drssEndpointArn,

    -- * Destructuring the response
    DescribeRefreshSchemasStatusResponse (..),
    mkDescribeRefreshSchemasStatusResponse,

    -- ** Response lenses
    drssrrsRefreshSchemasStatus,
    drssrrsResponseStatus,
  )
where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkDescribeRefreshSchemasStatus' smart constructor.
newtype DescribeRefreshSchemasStatus = DescribeRefreshSchemasStatus'
  { -- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
    endpointArn :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeRefreshSchemasStatus' value with any optional fields omitted.
mkDescribeRefreshSchemasStatus ::
  -- | 'endpointArn'
  Types.String ->
  DescribeRefreshSchemasStatus
mkDescribeRefreshSchemasStatus endpointArn =
  DescribeRefreshSchemasStatus' {endpointArn}

-- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
--
-- /Note:/ Consider using 'endpointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drssEndpointArn :: Lens.Lens' DescribeRefreshSchemasStatus Types.String
drssEndpointArn = Lens.field @"endpointArn"
{-# DEPRECATED drssEndpointArn "Use generic-lens or generic-optics with 'endpointArn' instead." #-}

instance Core.FromJSON DescribeRefreshSchemasStatus where
  toJSON DescribeRefreshSchemasStatus {..} =
    Core.object
      (Core.catMaybes [Core.Just ("EndpointArn" Core..= endpointArn)])

instance Core.AWSRequest DescribeRefreshSchemasStatus where
  type
    Rs DescribeRefreshSchemasStatus =
      DescribeRefreshSchemasStatusResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AmazonDMSv20160101.DescribeRefreshSchemasStatus")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRefreshSchemasStatusResponse'
            Core.<$> (x Core..:? "RefreshSchemasStatus")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- |
--
-- /See:/ 'mkDescribeRefreshSchemasStatusResponse' smart constructor.
data DescribeRefreshSchemasStatusResponse = DescribeRefreshSchemasStatusResponse'
  { -- | The status of the schema.
    refreshSchemasStatus :: Core.Maybe Types.RefreshSchemasStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeRefreshSchemasStatusResponse' value with any optional fields omitted.
mkDescribeRefreshSchemasStatusResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeRefreshSchemasStatusResponse
mkDescribeRefreshSchemasStatusResponse responseStatus =
  DescribeRefreshSchemasStatusResponse'
    { refreshSchemasStatus =
        Core.Nothing,
      responseStatus
    }

-- | The status of the schema.
--
-- /Note:/ Consider using 'refreshSchemasStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drssrrsRefreshSchemasStatus :: Lens.Lens' DescribeRefreshSchemasStatusResponse (Core.Maybe Types.RefreshSchemasStatus)
drssrrsRefreshSchemasStatus = Lens.field @"refreshSchemasStatus"
{-# DEPRECATED drssrrsRefreshSchemasStatus "Use generic-lens or generic-optics with 'refreshSchemasStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drssrrsResponseStatus :: Lens.Lens' DescribeRefreshSchemasStatusResponse Core.Int
drssrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drssrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

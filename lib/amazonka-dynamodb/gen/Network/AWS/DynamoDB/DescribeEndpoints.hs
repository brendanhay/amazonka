{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.DescribeEndpoints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the regional endpoint information.
module Network.AWS.DynamoDB.DescribeEndpoints
  ( -- * Creating a request
    DescribeEndpoints (..),
    mkDescribeEndpoints,

    -- * Destructuring the response
    DescribeEndpointsResponse (..),
    mkDescribeEndpointsResponse,

    -- ** Response lenses
    derrsEndpoints,
    derrsResponseStatus,
  )
where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeEndpoints' smart constructor.
data DescribeEndpoints = DescribeEndpoints'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEndpoints' value with any optional fields omitted.
mkDescribeEndpoints ::
  DescribeEndpoints
mkDescribeEndpoints = DescribeEndpoints'

instance Core.FromJSON DescribeEndpoints where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest DescribeEndpoints where
  type Rs DescribeEndpoints = DescribeEndpointsResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DynamoDB_20120810.DescribeEndpoints")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEndpointsResponse'
            Core.<$> (x Core..:? "Endpoints" Core..!= Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeEndpointsResponse' smart constructor.
data DescribeEndpointsResponse = DescribeEndpointsResponse'
  { -- | List of endpoints.
    endpoints :: [Types.Endpoint],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeEndpointsResponse' value with any optional fields omitted.
mkDescribeEndpointsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeEndpointsResponse
mkDescribeEndpointsResponse responseStatus =
  DescribeEndpointsResponse'
    { endpoints = Core.mempty,
      responseStatus
    }

-- | List of endpoints.
--
-- /Note:/ Consider using 'endpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsEndpoints :: Lens.Lens' DescribeEndpointsResponse [Types.Endpoint]
derrsEndpoints = Lens.field @"endpoints"
{-# DEPRECATED derrsEndpoints "Use generic-lens or generic-optics with 'endpoints' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsResponseStatus :: Lens.Lens' DescribeEndpointsResponse Core.Int
derrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED derrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

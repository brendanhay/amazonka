{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetDevEndpoints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all the development endpoints in this AWS account.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetDevEndpoints
  ( -- * Creating a request
    GetDevEndpoints (..),
    mkGetDevEndpoints,

    -- ** Request lenses
    gdeMaxResults,
    gdeNextToken,

    -- * Destructuring the response
    GetDevEndpointsResponse (..),
    mkGetDevEndpointsResponse,

    -- ** Response lenses
    gderrsDevEndpoints,
    gderrsNextToken,
    gderrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDevEndpoints' smart constructor.
data GetDevEndpoints = GetDevEndpoints'
  { -- | The maximum size of information to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | A continuation token, if this is a continuation call.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDevEndpoints' value with any optional fields omitted.
mkGetDevEndpoints ::
  GetDevEndpoints
mkGetDevEndpoints =
  GetDevEndpoints'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The maximum size of information to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdeMaxResults :: Lens.Lens' GetDevEndpoints (Core.Maybe Core.Natural)
gdeMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gdeMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A continuation token, if this is a continuation call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdeNextToken :: Lens.Lens' GetDevEndpoints (Core.Maybe Types.NextToken)
gdeNextToken = Lens.field @"nextToken"
{-# DEPRECATED gdeNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON GetDevEndpoints where
  toJSON GetDevEndpoints {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest GetDevEndpoints where
  type Rs GetDevEndpoints = GetDevEndpointsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.GetDevEndpoints")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDevEndpointsResponse'
            Core.<$> (x Core..:? "DevEndpoints")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetDevEndpoints where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"devEndpoints" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkGetDevEndpointsResponse' smart constructor.
data GetDevEndpointsResponse = GetDevEndpointsResponse'
  { -- | A list of @DevEndpoint@ definitions.
    devEndpoints :: Core.Maybe [Types.DevEndpoint],
    -- | A continuation token, if not all @DevEndpoint@ definitions have yet been returned.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetDevEndpointsResponse' value with any optional fields omitted.
mkGetDevEndpointsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetDevEndpointsResponse
mkGetDevEndpointsResponse responseStatus =
  GetDevEndpointsResponse'
    { devEndpoints = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of @DevEndpoint@ definitions.
--
-- /Note:/ Consider using 'devEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gderrsDevEndpoints :: Lens.Lens' GetDevEndpointsResponse (Core.Maybe [Types.DevEndpoint])
gderrsDevEndpoints = Lens.field @"devEndpoints"
{-# DEPRECATED gderrsDevEndpoints "Use generic-lens or generic-optics with 'devEndpoints' instead." #-}

-- | A continuation token, if not all @DevEndpoint@ definitions have yet been returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gderrsNextToken :: Lens.Lens' GetDevEndpointsResponse (Core.Maybe Types.NextToken)
gderrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gderrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gderrsResponseStatus :: Lens.Lens' GetDevEndpointsResponse Core.Int
gderrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gderrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

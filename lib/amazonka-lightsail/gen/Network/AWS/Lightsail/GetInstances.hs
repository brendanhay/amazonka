{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all Amazon Lightsail virtual private servers, or /instances/ .
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetInstances
  ( -- * Creating a request
    GetInstances (..),
    mkGetInstances,

    -- ** Request lenses
    giPageToken,

    -- * Destructuring the response
    GetInstancesResponse (..),
    mkGetInstancesResponse,

    -- ** Response lenses
    girrsInstances,
    girrsNextPageToken,
    girrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetInstances' smart constructor.
newtype GetInstances = GetInstances'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetInstances@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
    pageToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetInstances' value with any optional fields omitted.
mkGetInstances ::
  GetInstances
mkGetInstances = GetInstances' {pageToken = Core.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetInstances@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giPageToken :: Lens.Lens' GetInstances (Core.Maybe Types.String)
giPageToken = Lens.field @"pageToken"
{-# DEPRECATED giPageToken "Use generic-lens or generic-optics with 'pageToken' instead." #-}

instance Core.FromJSON GetInstances where
  toJSON GetInstances {..} =
    Core.object
      (Core.catMaybes [("pageToken" Core..=) Core.<$> pageToken])

instance Core.AWSRequest GetInstances where
  type Rs GetInstances = GetInstancesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Lightsail_20161128.GetInstances")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInstancesResponse'
            Core.<$> (x Core..:? "instances")
            Core.<*> (x Core..:? "nextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetInstances where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextPageToken") =
      Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"instances" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"pageToken"
            Lens..~ rs Lens.^. Lens.field @"nextPageToken"
        )

-- | /See:/ 'mkGetInstancesResponse' smart constructor.
data GetInstancesResponse = GetInstancesResponse'
  { -- | An array of key-value pairs containing information about your instances.
    instances :: Core.Maybe [Types.Instance],
    -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to display.
    -- To get the next page of results, perform another @GetInstances@ request and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Core.Maybe Types.NextPageToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetInstancesResponse' value with any optional fields omitted.
mkGetInstancesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetInstancesResponse
mkGetInstancesResponse responseStatus =
  GetInstancesResponse'
    { instances = Core.Nothing,
      nextPageToken = Core.Nothing,
      responseStatus
    }

-- | An array of key-value pairs containing information about your instances.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrsInstances :: Lens.Lens' GetInstancesResponse (Core.Maybe [Types.Instance])
girrsInstances = Lens.field @"instances"
{-# DEPRECATED girrsInstances "Use generic-lens or generic-optics with 'instances' instead." #-}

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to display.
-- To get the next page of results, perform another @GetInstances@ request and specify the next page token using the @pageToken@ parameter.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrsNextPageToken :: Lens.Lens' GetInstancesResponse (Core.Maybe Types.NextPageToken)
girrsNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED girrsNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrsResponseStatus :: Lens.Lens' GetInstancesResponse Core.Int
girrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED girrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

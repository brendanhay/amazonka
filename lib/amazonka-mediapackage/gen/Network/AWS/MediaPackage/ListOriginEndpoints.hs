{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.ListOriginEndpoints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a collection of OriginEndpoint records.
--
-- This operation returns paginated results.
module Network.AWS.MediaPackage.ListOriginEndpoints
  ( -- * Creating a request
    ListOriginEndpoints (..),
    mkListOriginEndpoints,

    -- ** Request lenses
    loeChannelId,
    loeMaxResults,
    loeNextToken,

    -- * Destructuring the response
    ListOriginEndpointsResponse (..),
    mkListOriginEndpointsResponse,

    -- ** Response lenses
    loerrsNextToken,
    loerrsOriginEndpoints,
    loerrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaPackage.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListOriginEndpoints' smart constructor.
data ListOriginEndpoints = ListOriginEndpoints'
  { -- | When specified, the request will return only OriginEndpoints associated with the given Channel ID.
    channelId :: Core.Maybe Core.Text,
    -- | The upper bound on the number of records to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | A token used to resume pagination from the end of a previous request.
    nextToken :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListOriginEndpoints' value with any optional fields omitted.
mkListOriginEndpoints ::
  ListOriginEndpoints
mkListOriginEndpoints =
  ListOriginEndpoints'
    { channelId = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | When specified, the request will return only OriginEndpoints associated with the given Channel ID.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loeChannelId :: Lens.Lens' ListOriginEndpoints (Core.Maybe Core.Text)
loeChannelId = Lens.field @"channelId"
{-# DEPRECATED loeChannelId "Use generic-lens or generic-optics with 'channelId' instead." #-}

-- | The upper bound on the number of records to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loeMaxResults :: Lens.Lens' ListOriginEndpoints (Core.Maybe Core.Natural)
loeMaxResults = Lens.field @"maxResults"
{-# DEPRECATED loeMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A token used to resume pagination from the end of a previous request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loeNextToken :: Lens.Lens' ListOriginEndpoints (Core.Maybe Core.Text)
loeNextToken = Lens.field @"nextToken"
{-# DEPRECATED loeNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListOriginEndpoints where
  type Rs ListOriginEndpoints = ListOriginEndpointsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/origin_endpoints",
        Core._rqQuery =
          Core.toQueryValue "channelId" Core.<$> channelId
            Core.<> (Core.toQueryValue "maxResults" Core.<$> maxResults)
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken),
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOriginEndpointsResponse'
            Core.<$> (x Core..:? "nextToken")
            Core.<*> (x Core..:? "originEndpoints")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListOriginEndpoints where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"originEndpoints" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListOriginEndpointsResponse' smart constructor.
data ListOriginEndpointsResponse = ListOriginEndpointsResponse'
  { -- | A token that can be used to resume pagination from the end of the collection.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of OriginEndpoint records.
    originEndpoints :: Core.Maybe [Types.OriginEndpoint],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListOriginEndpointsResponse' value with any optional fields omitted.
mkListOriginEndpointsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListOriginEndpointsResponse
mkListOriginEndpointsResponse responseStatus =
  ListOriginEndpointsResponse'
    { nextToken = Core.Nothing,
      originEndpoints = Core.Nothing,
      responseStatus
    }

-- | A token that can be used to resume pagination from the end of the collection.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loerrsNextToken :: Lens.Lens' ListOriginEndpointsResponse (Core.Maybe Core.Text)
loerrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED loerrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of OriginEndpoint records.
--
-- /Note:/ Consider using 'originEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loerrsOriginEndpoints :: Lens.Lens' ListOriginEndpointsResponse (Core.Maybe [Types.OriginEndpoint])
loerrsOriginEndpoints = Lens.field @"originEndpoints"
{-# DEPRECATED loerrsOriginEndpoints "Use generic-lens or generic-optics with 'originEndpoints' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loerrsResponseStatus :: Lens.Lens' ListOriginEndpointsResponse Core.Int
loerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED loerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

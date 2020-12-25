{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.ListApprovedOrigins
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of all approved origins associated with the instance.
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListApprovedOrigins
  ( -- * Creating a request
    ListApprovedOrigins (..),
    mkListApprovedOrigins,

    -- ** Request lenses
    laoInstanceId,
    laoMaxResults,
    laoNextToken,

    -- * Destructuring the response
    ListApprovedOriginsResponse (..),
    mkListApprovedOriginsResponse,

    -- ** Response lenses
    laorrsNextToken,
    laorrsOrigins,
    laorrsResponseStatus,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListApprovedOrigins' smart constructor.
data ListApprovedOrigins = ListApprovedOrigins'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId,
    -- | The maximimum number of results to return per page.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListApprovedOrigins' value with any optional fields omitted.
mkListApprovedOrigins ::
  -- | 'instanceId'
  Types.InstanceId ->
  ListApprovedOrigins
mkListApprovedOrigins instanceId =
  ListApprovedOrigins'
    { instanceId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laoInstanceId :: Lens.Lens' ListApprovedOrigins Types.InstanceId
laoInstanceId = Lens.field @"instanceId"
{-# DEPRECATED laoInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The maximimum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laoMaxResults :: Lens.Lens' ListApprovedOrigins (Core.Maybe Core.Natural)
laoMaxResults = Lens.field @"maxResults"
{-# DEPRECATED laoMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laoNextToken :: Lens.Lens' ListApprovedOrigins (Core.Maybe Types.NextToken)
laoNextToken = Lens.field @"nextToken"
{-# DEPRECATED laoNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListApprovedOrigins where
  type Rs ListApprovedOrigins = ListApprovedOriginsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/instance/" Core.<> (Core.toText instanceId)
                Core.<> ("/approved-origins")
            ),
        Core._rqQuery =
          Core.toQueryValue "maxResults" Core.<$> maxResults
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken),
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListApprovedOriginsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Origins")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListApprovedOrigins where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"origins" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListApprovedOriginsResponse' smart constructor.
data ListApprovedOriginsResponse = ListApprovedOriginsResponse'
  { -- | If there are additional results, this is the token for the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The approved origins.
    origins :: Core.Maybe [Types.Origin],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListApprovedOriginsResponse' value with any optional fields omitted.
mkListApprovedOriginsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListApprovedOriginsResponse
mkListApprovedOriginsResponse responseStatus =
  ListApprovedOriginsResponse'
    { nextToken = Core.Nothing,
      origins = Core.Nothing,
      responseStatus
    }

-- | If there are additional results, this is the token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laorrsNextToken :: Lens.Lens' ListApprovedOriginsResponse (Core.Maybe Types.NextToken)
laorrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED laorrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The approved origins.
--
-- /Note:/ Consider using 'origins' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laorrsOrigins :: Lens.Lens' ListApprovedOriginsResponse (Core.Maybe [Types.Origin])
laorrsOrigins = Lens.field @"origins"
{-# DEPRECATED laorrsOrigins "Use generic-lens or generic-optics with 'origins' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laorrsResponseStatus :: Lens.Lens' ListApprovedOriginsResponse Core.Int
laorrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED laorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

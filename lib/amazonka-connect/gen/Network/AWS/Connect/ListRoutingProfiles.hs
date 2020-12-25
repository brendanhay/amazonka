{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.ListRoutingProfiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides summary information about the routing profiles for the specified Amazon Connect instance.
--
-- For more information about routing profiles, see <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing.html Routing Profiles> and <https://docs.aws.amazon.com/connect/latest/adminguide/routing-profiles.html Create a Routing Profile> in the /Amazon Connect Administrator Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListRoutingProfiles
  ( -- * Creating a request
    ListRoutingProfiles (..),
    mkListRoutingProfiles,

    -- ** Request lenses
    lrpInstanceId,
    lrpMaxResults,
    lrpNextToken,

    -- * Destructuring the response
    ListRoutingProfilesResponse (..),
    mkListRoutingProfilesResponse,

    -- ** Response lenses
    lrprrsNextToken,
    lrprrsRoutingProfileSummaryList,
    lrprrsResponseStatus,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListRoutingProfiles' smart constructor.
data ListRoutingProfiles = ListRoutingProfiles'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId,
    -- | The maximimum number of results to return per page.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListRoutingProfiles' value with any optional fields omitted.
mkListRoutingProfiles ::
  -- | 'instanceId'
  Types.InstanceId ->
  ListRoutingProfiles
mkListRoutingProfiles instanceId =
  ListRoutingProfiles'
    { instanceId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrpInstanceId :: Lens.Lens' ListRoutingProfiles Types.InstanceId
lrpInstanceId = Lens.field @"instanceId"
{-# DEPRECATED lrpInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The maximimum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrpMaxResults :: Lens.Lens' ListRoutingProfiles (Core.Maybe Core.Natural)
lrpMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lrpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrpNextToken :: Lens.Lens' ListRoutingProfiles (Core.Maybe Types.NextToken)
lrpNextToken = Lens.field @"nextToken"
{-# DEPRECATED lrpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListRoutingProfiles where
  type Rs ListRoutingProfiles = ListRoutingProfilesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ("/routing-profiles-summary/" Core.<> (Core.toText instanceId)),
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
          ListRoutingProfilesResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "RoutingProfileSummaryList")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListRoutingProfiles where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"routingProfileSummaryList" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListRoutingProfilesResponse' smart constructor.
data ListRoutingProfilesResponse = ListRoutingProfilesResponse'
  { -- | If there are additional results, this is the token for the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Information about the routing profiles.
    routingProfileSummaryList :: Core.Maybe [Types.RoutingProfileSummary],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListRoutingProfilesResponse' value with any optional fields omitted.
mkListRoutingProfilesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListRoutingProfilesResponse
mkListRoutingProfilesResponse responseStatus =
  ListRoutingProfilesResponse'
    { nextToken = Core.Nothing,
      routingProfileSummaryList = Core.Nothing,
      responseStatus
    }

-- | If there are additional results, this is the token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprrsNextToken :: Lens.Lens' ListRoutingProfilesResponse (Core.Maybe Types.NextToken)
lrprrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lrprrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the routing profiles.
--
-- /Note:/ Consider using 'routingProfileSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprrsRoutingProfileSummaryList :: Lens.Lens' ListRoutingProfilesResponse (Core.Maybe [Types.RoutingProfileSummary])
lrprrsRoutingProfileSummaryList = Lens.field @"routingProfileSummaryList"
{-# DEPRECATED lrprrsRoutingProfileSummaryList "Use generic-lens or generic-optics with 'routingProfileSummaryList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprrsResponseStatus :: Lens.Lens' ListRoutingProfilesResponse Core.Int
lrprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lrprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

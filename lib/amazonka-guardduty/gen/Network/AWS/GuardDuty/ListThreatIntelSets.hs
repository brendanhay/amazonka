{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.ListThreatIntelSets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the ThreatIntelSets of the GuardDuty service specified by the detector ID. If you use this operation from a member account, the ThreatIntelSets associated with the master account are returned.
--
-- This operation returns paginated results.
module Network.AWS.GuardDuty.ListThreatIntelSets
  ( -- * Creating a request
    ListThreatIntelSets (..),
    mkListThreatIntelSets,

    -- ** Request lenses
    ltisDetectorId,
    ltisMaxResults,
    ltisNextToken,

    -- * Destructuring the response
    ListThreatIntelSetsResponse (..),
    mkListThreatIntelSetsResponse,

    -- ** Response lenses
    ltisrrsThreatIntelSetIds,
    ltisrrsNextToken,
    ltisrrsResponseStatus,
  )
where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListThreatIntelSets' smart constructor.
data ListThreatIntelSets = ListThreatIntelSets'
  { -- | The unique ID of the detector that the threatIntelSet is associated with.
    detectorId :: Types.DetectorId,
    -- | You can use this parameter to indicate the maximum number of items that you want in the response. The default value is 50. The maximum value is 50.
    maxResults :: Core.Maybe Core.Natural,
    -- | You can use this parameter to paginate results in the response. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action, fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListThreatIntelSets' value with any optional fields omitted.
mkListThreatIntelSets ::
  -- | 'detectorId'
  Types.DetectorId ->
  ListThreatIntelSets
mkListThreatIntelSets detectorId =
  ListThreatIntelSets'
    { detectorId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The unique ID of the detector that the threatIntelSet is associated with.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltisDetectorId :: Lens.Lens' ListThreatIntelSets Types.DetectorId
ltisDetectorId = Lens.field @"detectorId"
{-# DEPRECATED ltisDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | You can use this parameter to indicate the maximum number of items that you want in the response. The default value is 50. The maximum value is 50.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltisMaxResults :: Lens.Lens' ListThreatIntelSets (Core.Maybe Core.Natural)
ltisMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ltisMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | You can use this parameter to paginate results in the response. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action, fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltisNextToken :: Lens.Lens' ListThreatIntelSets (Core.Maybe Types.String)
ltisNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltisNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListThreatIntelSets where
  type Rs ListThreatIntelSets = ListThreatIntelSetsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/detector/" Core.<> (Core.toText detectorId)
                Core.<> ("/threatintelset")
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
          ListThreatIntelSetsResponse'
            Core.<$> (x Core..:? "threatIntelSetIds" Core..!= Core.mempty)
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListThreatIntelSets where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^. Lens.field @"threatIntelSetIds") =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListThreatIntelSetsResponse' smart constructor.
data ListThreatIntelSetsResponse = ListThreatIntelSetsResponse'
  { -- | The IDs of the ThreatIntelSet resources.
    threatIntelSetIds :: [Types.String],
    -- | The pagination parameter to be used on the next list operation to retrieve more items.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListThreatIntelSetsResponse' value with any optional fields omitted.
mkListThreatIntelSetsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListThreatIntelSetsResponse
mkListThreatIntelSetsResponse responseStatus =
  ListThreatIntelSetsResponse'
    { threatIntelSetIds = Core.mempty,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The IDs of the ThreatIntelSet resources.
--
-- /Note:/ Consider using 'threatIntelSetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltisrrsThreatIntelSetIds :: Lens.Lens' ListThreatIntelSetsResponse [Types.String]
ltisrrsThreatIntelSetIds = Lens.field @"threatIntelSetIds"
{-# DEPRECATED ltisrrsThreatIntelSetIds "Use generic-lens or generic-optics with 'threatIntelSetIds' instead." #-}

-- | The pagination parameter to be used on the next list operation to retrieve more items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltisrrsNextToken :: Lens.Lens' ListThreatIntelSetsResponse (Core.Maybe Types.NextToken)
ltisrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltisrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltisrrsResponseStatus :: Lens.Lens' ListThreatIntelSetsResponse Core.Int
ltisrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ltisrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

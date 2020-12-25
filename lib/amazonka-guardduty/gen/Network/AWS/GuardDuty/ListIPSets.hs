{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.ListIPSets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the IPSets of the GuardDuty service specified by the detector ID. If you use this operation from a member account, the IPSets returned are the IPSets from the associated master account.
--
-- This operation returns paginated results.
module Network.AWS.GuardDuty.ListIPSets
  ( -- * Creating a request
    ListIPSets (..),
    mkListIPSets,

    -- ** Request lenses
    lipsDetectorId,
    lipsMaxResults,
    lipsNextToken,

    -- * Destructuring the response
    ListIPSetsResponse (..),
    mkListIPSetsResponse,

    -- ** Response lenses
    lipsrrsIpSetIds,
    lipsrrsNextToken,
    lipsrrsResponseStatus,
  )
where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListIPSets' smart constructor.
data ListIPSets = ListIPSets'
  { -- | The unique ID of the detector that the IPSet is associated with.
    detectorId :: Types.DetectorId,
    -- | You can use this parameter to indicate the maximum number of items you want in the response. The default value is 50. The maximum value is 50.
    maxResults :: Core.Maybe Core.Natural,
    -- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action, fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListIPSets' value with any optional fields omitted.
mkListIPSets ::
  -- | 'detectorId'
  Types.DetectorId ->
  ListIPSets
mkListIPSets detectorId =
  ListIPSets'
    { detectorId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The unique ID of the detector that the IPSet is associated with.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipsDetectorId :: Lens.Lens' ListIPSets Types.DetectorId
lipsDetectorId = Lens.field @"detectorId"
{-# DEPRECATED lipsDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | You can use this parameter to indicate the maximum number of items you want in the response. The default value is 50. The maximum value is 50.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipsMaxResults :: Lens.Lens' ListIPSets (Core.Maybe Core.Natural)
lipsMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lipsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action, fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipsNextToken :: Lens.Lens' ListIPSets (Core.Maybe Types.String)
lipsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lipsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListIPSets where
  type Rs ListIPSets = ListIPSetsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ("/detector/" Core.<> (Core.toText detectorId) Core.<> ("/ipset")),
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
          ListIPSetsResponse'
            Core.<$> (x Core..:? "ipSetIds" Core..!= Core.mempty)
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListIPSets where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^. Lens.field @"ipSetIds") = Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListIPSetsResponse' smart constructor.
data ListIPSetsResponse = ListIPSetsResponse'
  { -- | The IDs of the IPSet resources.
    ipSetIds :: [Types.String],
    -- | The pagination parameter to be used on the next list operation to retrieve more items.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListIPSetsResponse' value with any optional fields omitted.
mkListIPSetsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListIPSetsResponse
mkListIPSetsResponse responseStatus =
  ListIPSetsResponse'
    { ipSetIds = Core.mempty,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The IDs of the IPSet resources.
--
-- /Note:/ Consider using 'ipSetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipsrrsIpSetIds :: Lens.Lens' ListIPSetsResponse [Types.String]
lipsrrsIpSetIds = Lens.field @"ipSetIds"
{-# DEPRECATED lipsrrsIpSetIds "Use generic-lens or generic-optics with 'ipSetIds' instead." #-}

-- | The pagination parameter to be used on the next list operation to retrieve more items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipsrrsNextToken :: Lens.Lens' ListIPSetsResponse (Core.Maybe Types.String)
lipsrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lipsrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipsrrsResponseStatus :: Lens.Lens' ListIPSetsResponse Core.Int
lipsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lipsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.ListContactFlows
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the contact flows for the specified Amazon Connect instance.
--
-- You can also create and update contact flows using the <https://docs.aws.amazon.com/connect/latest/adminguide/flow-language.html Amazon Connect Flow language> .
-- For more information about contact flows, see <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-contact-flows.html Contact Flows> in the /Amazon Connect Administrator Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListContactFlows
    (
    -- * Creating a request
      ListContactFlows (..)
    , mkListContactFlows
    -- ** Request lenses
    , lcfInstanceId
    , lcfContactFlowTypes
    , lcfMaxResults
    , lcfNextToken

    -- * Destructuring the response
    , ListContactFlowsResponse (..)
    , mkListContactFlowsResponse
    -- ** Response lenses
    , lcfrrsContactFlowSummaryList
    , lcfrrsNextToken
    , lcfrrsResponseStatus
    ) where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListContactFlows' smart constructor.
data ListContactFlows = ListContactFlows'
  { instanceId :: Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  , contactFlowTypes :: Core.Maybe [Types.ContactFlowType]
    -- ^ The type of contact flow.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximimum number of results to return per page.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListContactFlows' value with any optional fields omitted.
mkListContactFlows
    :: Types.InstanceId -- ^ 'instanceId'
    -> ListContactFlows
mkListContactFlows instanceId
  = ListContactFlows'{instanceId, contactFlowTypes = Core.Nothing,
                      maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfInstanceId :: Lens.Lens' ListContactFlows Types.InstanceId
lcfInstanceId = Lens.field @"instanceId"
{-# INLINEABLE lcfInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The type of contact flow.
--
-- /Note:/ Consider using 'contactFlowTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfContactFlowTypes :: Lens.Lens' ListContactFlows (Core.Maybe [Types.ContactFlowType])
lcfContactFlowTypes = Lens.field @"contactFlowTypes"
{-# INLINEABLE lcfContactFlowTypes #-}
{-# DEPRECATED contactFlowTypes "Use generic-lens or generic-optics with 'contactFlowTypes' instead"  #-}

-- | The maximimum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfMaxResults :: Lens.Lens' ListContactFlows (Core.Maybe Core.Natural)
lcfMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lcfMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfNextToken :: Lens.Lens' ListContactFlows (Core.Maybe Types.NextToken)
lcfNextToken = Lens.field @"nextToken"
{-# INLINEABLE lcfNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListContactFlows where
        toQuery ListContactFlows{..}
          = Core.toQueryPair "contactFlowTypes"
              (Core.maybe Core.mempty (Core.toQueryList "member")
                 contactFlowTypes)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders ListContactFlows where
        toHeaders ListContactFlows{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListContactFlows where
        type Rs ListContactFlows = ListContactFlowsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/contact-flows-summary/" Core.<> Core.toText instanceId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListContactFlowsResponse' Core.<$>
                   (x Core..:? "ContactFlowSummaryList") Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListContactFlows where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"contactFlowSummaryList" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListContactFlowsResponse' smart constructor.
data ListContactFlowsResponse = ListContactFlowsResponse'
  { contactFlowSummaryList :: Core.Maybe [Types.ContactFlowSummary]
    -- ^ Information about the contact flows.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If there are additional results, this is the token for the next set of results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListContactFlowsResponse' value with any optional fields omitted.
mkListContactFlowsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListContactFlowsResponse
mkListContactFlowsResponse responseStatus
  = ListContactFlowsResponse'{contactFlowSummaryList = Core.Nothing,
                              nextToken = Core.Nothing, responseStatus}

-- | Information about the contact flows.
--
-- /Note:/ Consider using 'contactFlowSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfrrsContactFlowSummaryList :: Lens.Lens' ListContactFlowsResponse (Core.Maybe [Types.ContactFlowSummary])
lcfrrsContactFlowSummaryList = Lens.field @"contactFlowSummaryList"
{-# INLINEABLE lcfrrsContactFlowSummaryList #-}
{-# DEPRECATED contactFlowSummaryList "Use generic-lens or generic-optics with 'contactFlowSummaryList' instead"  #-}

-- | If there are additional results, this is the token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfrrsNextToken :: Lens.Lens' ListContactFlowsResponse (Core.Maybe Types.NextToken)
lcfrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lcfrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfrrsResponseStatus :: Lens.Lens' ListContactFlowsResponse Core.Int
lcfrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lcfrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

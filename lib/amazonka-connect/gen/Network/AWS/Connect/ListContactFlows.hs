{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ListContactFlows (..),
    mkListContactFlows,

    -- ** Request lenses
    lcfInstanceId,
    lcfContactFlowTypes,
    lcfMaxResults,
    lcfNextToken,

    -- * Destructuring the response
    ListContactFlowsResponse (..),
    mkListContactFlowsResponse,

    -- ** Response lenses
    lcfrrsContactFlowSummaryList,
    lcfrrsNextToken,
    lcfrrsResponseStatus,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListContactFlows' smart constructor.
data ListContactFlows = ListContactFlows'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId,
    -- | The type of contact flow.
    contactFlowTypes :: Core.Maybe [Types.ContactFlowType],
    -- | The maximimum number of results to return per page.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListContactFlows' value with any optional fields omitted.
mkListContactFlows ::
  -- | 'instanceId'
  Types.InstanceId ->
  ListContactFlows
mkListContactFlows instanceId =
  ListContactFlows'
    { instanceId,
      contactFlowTypes = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfInstanceId :: Lens.Lens' ListContactFlows Types.InstanceId
lcfInstanceId = Lens.field @"instanceId"
{-# DEPRECATED lcfInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The type of contact flow.
--
-- /Note:/ Consider using 'contactFlowTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfContactFlowTypes :: Lens.Lens' ListContactFlows (Core.Maybe [Types.ContactFlowType])
lcfContactFlowTypes = Lens.field @"contactFlowTypes"
{-# DEPRECATED lcfContactFlowTypes "Use generic-lens or generic-optics with 'contactFlowTypes' instead." #-}

-- | The maximimum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfMaxResults :: Lens.Lens' ListContactFlows (Core.Maybe Core.Natural)
lcfMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lcfMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfNextToken :: Lens.Lens' ListContactFlows (Core.Maybe Types.NextToken)
lcfNextToken = Lens.field @"nextToken"
{-# DEPRECATED lcfNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListContactFlows where
  type Rs ListContactFlows = ListContactFlowsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ("/contact-flows-summary/" Core.<> (Core.toText instanceId)),
        Core._rqQuery =
          Core.toQueryValue
            "contactFlowTypes"
            (Core.toQueryList "member" Core.<$> contactFlowTypes)
            Core.<> (Core.toQueryValue "maxResults" Core.<$> maxResults)
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken),
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListContactFlowsResponse'
            Core.<$> (x Core..:? "ContactFlowSummaryList")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListContactFlows where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"contactFlowSummaryList" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListContactFlowsResponse' smart constructor.
data ListContactFlowsResponse = ListContactFlowsResponse'
  { -- | Information about the contact flows.
    contactFlowSummaryList :: Core.Maybe [Types.ContactFlowSummary],
    -- | If there are additional results, this is the token for the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListContactFlowsResponse' value with any optional fields omitted.
mkListContactFlowsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListContactFlowsResponse
mkListContactFlowsResponse responseStatus =
  ListContactFlowsResponse'
    { contactFlowSummaryList = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Information about the contact flows.
--
-- /Note:/ Consider using 'contactFlowSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfrrsContactFlowSummaryList :: Lens.Lens' ListContactFlowsResponse (Core.Maybe [Types.ContactFlowSummary])
lcfrrsContactFlowSummaryList = Lens.field @"contactFlowSummaryList"
{-# DEPRECATED lcfrrsContactFlowSummaryList "Use generic-lens or generic-optics with 'contactFlowSummaryList' instead." #-}

-- | If there are additional results, this is the token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfrrsNextToken :: Lens.Lens' ListContactFlowsResponse (Core.Maybe Types.NextToken)
lcfrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lcfrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfrrsResponseStatus :: Lens.Lens' ListContactFlowsResponse Core.Int
lcfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lcfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

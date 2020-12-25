{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.ListGatewayGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of gateway group summaries. Use GetGatewayGroup to retrieve details of a specific gateway group.
module Network.AWS.AlexaBusiness.ListGatewayGroups
  ( -- * Creating a request
    ListGatewayGroups (..),
    mkListGatewayGroups,

    -- ** Request lenses
    lggMaxResults,
    lggNextToken,

    -- * Destructuring the response
    ListGatewayGroupsResponse (..),
    mkListGatewayGroupsResponse,

    -- ** Response lenses
    lggrrsGatewayGroups,
    lggrrsNextToken,
    lggrrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListGatewayGroups' smart constructor.
data ListGatewayGroups = ListGatewayGroups'
  { -- | The maximum number of gateway group summaries to return. The default is 50.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token used to paginate though multiple pages of gateway group summaries.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListGatewayGroups' value with any optional fields omitted.
mkListGatewayGroups ::
  ListGatewayGroups
mkListGatewayGroups =
  ListGatewayGroups'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The maximum number of gateway group summaries to return. The default is 50.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lggMaxResults :: Lens.Lens' ListGatewayGroups (Core.Maybe Core.Natural)
lggMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lggMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token used to paginate though multiple pages of gateway group summaries.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lggNextToken :: Lens.Lens' ListGatewayGroups (Core.Maybe Types.NextToken)
lggNextToken = Lens.field @"nextToken"
{-# DEPRECATED lggNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListGatewayGroups where
  toJSON ListGatewayGroups {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListGatewayGroups where
  type Rs ListGatewayGroups = ListGatewayGroupsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AlexaForBusiness.ListGatewayGroups")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGatewayGroupsResponse'
            Core.<$> (x Core..:? "GatewayGroups")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListGatewayGroupsResponse' smart constructor.
data ListGatewayGroupsResponse = ListGatewayGroupsResponse'
  { -- | The gateway groups in the list.
    gatewayGroups :: Core.Maybe [Types.GatewayGroupSummary],
    -- | The token used to paginate though multiple pages of gateway group summaries.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListGatewayGroupsResponse' value with any optional fields omitted.
mkListGatewayGroupsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListGatewayGroupsResponse
mkListGatewayGroupsResponse responseStatus =
  ListGatewayGroupsResponse'
    { gatewayGroups = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The gateway groups in the list.
--
-- /Note:/ Consider using 'gatewayGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lggrrsGatewayGroups :: Lens.Lens' ListGatewayGroupsResponse (Core.Maybe [Types.GatewayGroupSummary])
lggrrsGatewayGroups = Lens.field @"gatewayGroups"
{-# DEPRECATED lggrrsGatewayGroups "Use generic-lens or generic-optics with 'gatewayGroups' instead." #-}

-- | The token used to paginate though multiple pages of gateway group summaries.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lggrrsNextToken :: Lens.Lens' ListGatewayGroupsResponse (Core.Maybe Types.NextToken)
lggrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lggrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lggrrsResponseStatus :: Lens.Lens' ListGatewayGroupsResponse Core.Int
lggrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lggrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

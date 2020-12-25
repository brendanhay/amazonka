{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.ListGateways
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of gateway summaries. Use GetGateway to retrieve details of a specific gateway. An optional gateway group ARN can be provided to only retrieve gateway summaries of gateways that are associated with that gateway group ARN.
module Network.AWS.AlexaBusiness.ListGateways
  ( -- * Creating a request
    ListGateways (..),
    mkListGateways,

    -- ** Request lenses
    lgGatewayGroupArn,
    lgMaxResults,
    lgNextToken,

    -- * Destructuring the response
    ListGatewaysResponse (..),
    mkListGatewaysResponse,

    -- ** Response lenses
    lgrrsGateways,
    lgrrsNextToken,
    lgrrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListGateways' smart constructor.
data ListGateways = ListGateways'
  { -- | The gateway group ARN for which to list gateways.
    gatewayGroupArn :: Core.Maybe Types.Arn,
    -- | The maximum number of gateway summaries to return. The default is 50.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token used to paginate though multiple pages of gateway summaries.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListGateways' value with any optional fields omitted.
mkListGateways ::
  ListGateways
mkListGateways =
  ListGateways'
    { gatewayGroupArn = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The gateway group ARN for which to list gateways.
--
-- /Note:/ Consider using 'gatewayGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgGatewayGroupArn :: Lens.Lens' ListGateways (Core.Maybe Types.Arn)
lgGatewayGroupArn = Lens.field @"gatewayGroupArn"
{-# DEPRECATED lgGatewayGroupArn "Use generic-lens or generic-optics with 'gatewayGroupArn' instead." #-}

-- | The maximum number of gateway summaries to return. The default is 50.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgMaxResults :: Lens.Lens' ListGateways (Core.Maybe Core.Natural)
lgMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lgMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token used to paginate though multiple pages of gateway summaries.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgNextToken :: Lens.Lens' ListGateways (Core.Maybe Types.NextToken)
lgNextToken = Lens.field @"nextToken"
{-# DEPRECATED lgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListGateways where
  toJSON ListGateways {..} =
    Core.object
      ( Core.catMaybes
          [ ("GatewayGroupArn" Core..=) Core.<$> gatewayGroupArn,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListGateways where
  type Rs ListGateways = ListGatewaysResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AlexaForBusiness.ListGateways")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGatewaysResponse'
            Core.<$> (x Core..:? "Gateways")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListGatewaysResponse' smart constructor.
data ListGatewaysResponse = ListGatewaysResponse'
  { -- | The gateways in the list.
    gateways :: Core.Maybe [Types.GatewaySummary],
    -- | The token used to paginate though multiple pages of gateway summaries.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListGatewaysResponse' value with any optional fields omitted.
mkListGatewaysResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListGatewaysResponse
mkListGatewaysResponse responseStatus =
  ListGatewaysResponse'
    { gateways = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The gateways in the list.
--
-- /Note:/ Consider using 'gateways' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrrsGateways :: Lens.Lens' ListGatewaysResponse (Core.Maybe [Types.GatewaySummary])
lgrrsGateways = Lens.field @"gateways"
{-# DEPRECATED lgrrsGateways "Use generic-lens or generic-optics with 'gateways' instead." #-}

-- | The token used to paginate though multiple pages of gateway summaries.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrrsNextToken :: Lens.Lens' ListGatewaysResponse (Core.Maybe Types.NextToken)
lgrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lgrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgrrsResponseStatus :: Lens.Lens' ListGatewaysResponse Core.Int
lgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

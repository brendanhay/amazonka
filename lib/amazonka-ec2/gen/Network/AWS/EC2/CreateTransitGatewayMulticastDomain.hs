{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateTransitGatewayMulticastDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a multicast domain using the specified transit gateway.
--
-- The transit gateway must be in the available state before you create a domain. Use <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeTransitGateways.html DescribeTransitGateways> to see the state of transit gateway.
module Network.AWS.EC2.CreateTransitGatewayMulticastDomain
  ( -- * Creating a request
    CreateTransitGatewayMulticastDomain (..),
    mkCreateTransitGatewayMulticastDomain,

    -- ** Request lenses
    ctgmdTransitGatewayId,
    ctgmdDryRun,
    ctgmdTagSpecifications,

    -- * Destructuring the response
    CreateTransitGatewayMulticastDomainResponse (..),
    mkCreateTransitGatewayMulticastDomainResponse,

    -- ** Response lenses
    ctgmdrrsTransitGatewayMulticastDomain,
    ctgmdrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateTransitGatewayMulticastDomain' smart constructor.
data CreateTransitGatewayMulticastDomain = CreateTransitGatewayMulticastDomain'
  { -- | The ID of the transit gateway.
    transitGatewayId :: Types.TransitGatewayId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The tags for the transit gateway multicast domain.
    tagSpecifications :: Core.Maybe [Types.TagSpecification]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTransitGatewayMulticastDomain' value with any optional fields omitted.
mkCreateTransitGatewayMulticastDomain ::
  -- | 'transitGatewayId'
  Types.TransitGatewayId ->
  CreateTransitGatewayMulticastDomain
mkCreateTransitGatewayMulticastDomain transitGatewayId =
  CreateTransitGatewayMulticastDomain'
    { transitGatewayId,
      dryRun = Core.Nothing,
      tagSpecifications = Core.Nothing
    }

-- | The ID of the transit gateway.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgmdTransitGatewayId :: Lens.Lens' CreateTransitGatewayMulticastDomain Types.TransitGatewayId
ctgmdTransitGatewayId = Lens.field @"transitGatewayId"
{-# DEPRECATED ctgmdTransitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgmdDryRun :: Lens.Lens' CreateTransitGatewayMulticastDomain (Core.Maybe Core.Bool)
ctgmdDryRun = Lens.field @"dryRun"
{-# DEPRECATED ctgmdDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The tags for the transit gateway multicast domain.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgmdTagSpecifications :: Lens.Lens' CreateTransitGatewayMulticastDomain (Core.Maybe [Types.TagSpecification])
ctgmdTagSpecifications = Lens.field @"tagSpecifications"
{-# DEPRECATED ctgmdTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

instance Core.AWSRequest CreateTransitGatewayMulticastDomain where
  type
    Rs CreateTransitGatewayMulticastDomain =
      CreateTransitGatewayMulticastDomainResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "CreateTransitGatewayMulticastDomain")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "TransitGatewayId" transitGatewayId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "TagSpecification" Core.<$> tagSpecifications)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CreateTransitGatewayMulticastDomainResponse'
            Core.<$> (x Core..@? "transitGatewayMulticastDomain")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateTransitGatewayMulticastDomainResponse' smart constructor.
data CreateTransitGatewayMulticastDomainResponse = CreateTransitGatewayMulticastDomainResponse'
  { -- | Information about the transit gateway multicast domain.
    transitGatewayMulticastDomain :: Core.Maybe Types.TransitGatewayMulticastDomain,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateTransitGatewayMulticastDomainResponse' value with any optional fields omitted.
mkCreateTransitGatewayMulticastDomainResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateTransitGatewayMulticastDomainResponse
mkCreateTransitGatewayMulticastDomainResponse responseStatus =
  CreateTransitGatewayMulticastDomainResponse'
    { transitGatewayMulticastDomain =
        Core.Nothing,
      responseStatus
    }

-- | Information about the transit gateway multicast domain.
--
-- /Note:/ Consider using 'transitGatewayMulticastDomain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgmdrrsTransitGatewayMulticastDomain :: Lens.Lens' CreateTransitGatewayMulticastDomainResponse (Core.Maybe Types.TransitGatewayMulticastDomain)
ctgmdrrsTransitGatewayMulticastDomain = Lens.field @"transitGatewayMulticastDomain"
{-# DEPRECATED ctgmdrrsTransitGatewayMulticastDomain "Use generic-lens or generic-optics with 'transitGatewayMulticastDomain' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgmdrrsResponseStatus :: Lens.Lens' CreateTransitGatewayMulticastDomainResponse Core.Int
ctgmdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ctgmdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

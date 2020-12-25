{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateTransitGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a transit gateway.
--
-- You can use a transit gateway to interconnect your virtual private clouds (VPC) and on-premises networks. After the transit gateway enters the @available@ state, you can attach your VPCs and VPN connections to the transit gateway.
-- To attach your VPCs, use 'CreateTransitGatewayVpcAttachment' .
-- To attach a VPN connection, use 'CreateCustomerGateway' to create a customer gateway and specify the ID of the customer gateway and the ID of the transit gateway in a call to 'CreateVpnConnection' .
-- When you create a transit gateway, we create a default transit gateway route table and use it as the default association route table and the default propagation route table. You can use 'CreateTransitGatewayRouteTable' to create additional transit gateway route tables. If you disable automatic route propagation, we do not create a default transit gateway route table. You can use 'EnableTransitGatewayRouteTablePropagation' to propagate routes from a resource attachment to a transit gateway route table. If you disable automatic associations, you can use 'AssociateTransitGatewayRouteTable' to associate a resource attachment with a transit gateway route table.
module Network.AWS.EC2.CreateTransitGateway
  ( -- * Creating a request
    CreateTransitGateway (..),
    mkCreateTransitGateway,

    -- ** Request lenses
    ctgDescription,
    ctgDryRun,
    ctgOptions,
    ctgTagSpecifications,

    -- * Destructuring the response
    CreateTransitGatewayResponse (..),
    mkCreateTransitGatewayResponse,

    -- ** Response lenses
    ctgrrsTransitGateway,
    ctgrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateTransitGateway' smart constructor.
data CreateTransitGateway = CreateTransitGateway'
  { -- | A description of the transit gateway.
    description :: Core.Maybe Types.String,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The transit gateway options.
    options :: Core.Maybe Types.TransitGatewayRequestOptions,
    -- | The tags to apply to the transit gateway.
    tagSpecifications :: Core.Maybe [Types.TagSpecification]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTransitGateway' value with any optional fields omitted.
mkCreateTransitGateway ::
  CreateTransitGateway
mkCreateTransitGateway =
  CreateTransitGateway'
    { description = Core.Nothing,
      dryRun = Core.Nothing,
      options = Core.Nothing,
      tagSpecifications = Core.Nothing
    }

-- | A description of the transit gateway.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgDescription :: Lens.Lens' CreateTransitGateway (Core.Maybe Types.String)
ctgDescription = Lens.field @"description"
{-# DEPRECATED ctgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgDryRun :: Lens.Lens' CreateTransitGateway (Core.Maybe Core.Bool)
ctgDryRun = Lens.field @"dryRun"
{-# DEPRECATED ctgDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The transit gateway options.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgOptions :: Lens.Lens' CreateTransitGateway (Core.Maybe Types.TransitGatewayRequestOptions)
ctgOptions = Lens.field @"options"
{-# DEPRECATED ctgOptions "Use generic-lens or generic-optics with 'options' instead." #-}

-- | The tags to apply to the transit gateway.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgTagSpecifications :: Lens.Lens' CreateTransitGateway (Core.Maybe [Types.TagSpecification])
ctgTagSpecifications = Lens.field @"tagSpecifications"
{-# DEPRECATED ctgTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

instance Core.AWSRequest CreateTransitGateway where
  type Rs CreateTransitGateway = CreateTransitGatewayResponse
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
            ( Core.pure ("Action", "CreateTransitGateway")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "Description" Core.<$> description)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "Options" Core.<$> options)
                Core.<> (Core.toQueryList "TagSpecification" Core.<$> tagSpecifications)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CreateTransitGatewayResponse'
            Core.<$> (x Core..@? "transitGateway")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateTransitGatewayResponse' smart constructor.
data CreateTransitGatewayResponse = CreateTransitGatewayResponse'
  { -- | Information about the transit gateway.
    transitGateway :: Core.Maybe Types.TransitGateway,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateTransitGatewayResponse' value with any optional fields omitted.
mkCreateTransitGatewayResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateTransitGatewayResponse
mkCreateTransitGatewayResponse responseStatus =
  CreateTransitGatewayResponse'
    { transitGateway = Core.Nothing,
      responseStatus
    }

-- | Information about the transit gateway.
--
-- /Note:/ Consider using 'transitGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrrsTransitGateway :: Lens.Lens' CreateTransitGatewayResponse (Core.Maybe Types.TransitGateway)
ctgrrsTransitGateway = Lens.field @"transitGateway"
{-# DEPRECATED ctgrrsTransitGateway "Use generic-lens or generic-optics with 'transitGateway' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrrsResponseStatus :: Lens.Lens' CreateTransitGatewayResponse Core.Int
ctgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ctgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateTransitGateway (..)
    , mkCreateTransitGateway
    -- ** Request lenses
    , ctgDescription
    , ctgDryRun
    , ctgOptions
    , ctgTagSpecifications

    -- * Destructuring the response
    , CreateTransitGatewayResponse (..)
    , mkCreateTransitGatewayResponse
    -- ** Response lenses
    , ctgrrsTransitGateway
    , ctgrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateTransitGateway' smart constructor.
data CreateTransitGateway = CreateTransitGateway'
  { description :: Core.Maybe Core.Text
    -- ^ A description of the transit gateway.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , options :: Core.Maybe Types.TransitGatewayRequestOptions
    -- ^ The transit gateway options.
  , tagSpecifications :: Core.Maybe [Types.TagSpecification]
    -- ^ The tags to apply to the transit gateway.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTransitGateway' value with any optional fields omitted.
mkCreateTransitGateway
    :: CreateTransitGateway
mkCreateTransitGateway
  = CreateTransitGateway'{description = Core.Nothing,
                          dryRun = Core.Nothing, options = Core.Nothing,
                          tagSpecifications = Core.Nothing}

-- | A description of the transit gateway.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgDescription :: Lens.Lens' CreateTransitGateway (Core.Maybe Core.Text)
ctgDescription = Lens.field @"description"
{-# INLINEABLE ctgDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgDryRun :: Lens.Lens' CreateTransitGateway (Core.Maybe Core.Bool)
ctgDryRun = Lens.field @"dryRun"
{-# INLINEABLE ctgDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The transit gateway options.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgOptions :: Lens.Lens' CreateTransitGateway (Core.Maybe Types.TransitGatewayRequestOptions)
ctgOptions = Lens.field @"options"
{-# INLINEABLE ctgOptions #-}
{-# DEPRECATED options "Use generic-lens or generic-optics with 'options' instead"  #-}

-- | The tags to apply to the transit gateway.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgTagSpecifications :: Lens.Lens' CreateTransitGateway (Core.Maybe [Types.TagSpecification])
ctgTagSpecifications = Lens.field @"tagSpecifications"
{-# INLINEABLE ctgTagSpecifications #-}
{-# DEPRECATED tagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead"  #-}

instance Core.ToQuery CreateTransitGateway where
        toQuery CreateTransitGateway{..}
          = Core.toQueryPair "Action" ("CreateTransitGateway" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Options") options
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TagSpecification")
                tagSpecifications

instance Core.ToHeaders CreateTransitGateway where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateTransitGateway where
        type Rs CreateTransitGateway = CreateTransitGatewayResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 CreateTransitGatewayResponse' Core.<$>
                   (x Core..@? "transitGateway") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateTransitGatewayResponse' smart constructor.
data CreateTransitGatewayResponse = CreateTransitGatewayResponse'
  { transitGateway :: Core.Maybe Types.TransitGateway
    -- ^ Information about the transit gateway.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateTransitGatewayResponse' value with any optional fields omitted.
mkCreateTransitGatewayResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateTransitGatewayResponse
mkCreateTransitGatewayResponse responseStatus
  = CreateTransitGatewayResponse'{transitGateway = Core.Nothing,
                                  responseStatus}

-- | Information about the transit gateway.
--
-- /Note:/ Consider using 'transitGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrrsTransitGateway :: Lens.Lens' CreateTransitGatewayResponse (Core.Maybe Types.TransitGateway)
ctgrrsTransitGateway = Lens.field @"transitGateway"
{-# INLINEABLE ctgrrsTransitGateway #-}
{-# DEPRECATED transitGateway "Use generic-lens or generic-optics with 'transitGateway' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgrrsResponseStatus :: Lens.Lens' CreateTransitGatewayResponse Core.Int
ctgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ctgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateEgressOnlyInternetGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- [IPv6 only] Creates an egress-only internet gateway for your VPC. An egress-only internet gateway is used to enable outbound communication over IPv6 from instances in your VPC to the internet, and prevents hosts outside of your VPC from initiating an IPv6 connection with your instance.
module Network.AWS.EC2.CreateEgressOnlyInternetGateway
    (
    -- * Creating a request
      CreateEgressOnlyInternetGateway (..)
    , mkCreateEgressOnlyInternetGateway
    -- ** Request lenses
    , ceoigVpcId
    , ceoigClientToken
    , ceoigDryRun
    , ceoigTagSpecifications

    -- * Destructuring the response
    , CreateEgressOnlyInternetGatewayResponse (..)
    , mkCreateEgressOnlyInternetGatewayResponse
    -- ** Response lenses
    , ceoigrrsClientToken
    , ceoigrrsEgressOnlyInternetGateway
    , ceoigrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateEgressOnlyInternetGateway' smart constructor.
data CreateEgressOnlyInternetGateway = CreateEgressOnlyInternetGateway'
  { vpcId :: Types.VpcId
    -- ^ The ID of the VPC for which to create the egress-only internet gateway.
  , clientToken :: Core.Maybe Core.Text
    -- ^ Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> .
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , tagSpecifications :: Core.Maybe [Types.TagSpecification]
    -- ^ The tags to assign to the egress-only internet gateway.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateEgressOnlyInternetGateway' value with any optional fields omitted.
mkCreateEgressOnlyInternetGateway
    :: Types.VpcId -- ^ 'vpcId'
    -> CreateEgressOnlyInternetGateway
mkCreateEgressOnlyInternetGateway vpcId
  = CreateEgressOnlyInternetGateway'{vpcId,
                                     clientToken = Core.Nothing, dryRun = Core.Nothing,
                                     tagSpecifications = Core.Nothing}

-- | The ID of the VPC for which to create the egress-only internet gateway.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceoigVpcId :: Lens.Lens' CreateEgressOnlyInternetGateway Types.VpcId
ceoigVpcId = Lens.field @"vpcId"
{-# INLINEABLE ceoigVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceoigClientToken :: Lens.Lens' CreateEgressOnlyInternetGateway (Core.Maybe Core.Text)
ceoigClientToken = Lens.field @"clientToken"
{-# INLINEABLE ceoigClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceoigDryRun :: Lens.Lens' CreateEgressOnlyInternetGateway (Core.Maybe Core.Bool)
ceoigDryRun = Lens.field @"dryRun"
{-# INLINEABLE ceoigDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The tags to assign to the egress-only internet gateway.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceoigTagSpecifications :: Lens.Lens' CreateEgressOnlyInternetGateway (Core.Maybe [Types.TagSpecification])
ceoigTagSpecifications = Lens.field @"tagSpecifications"
{-# INLINEABLE ceoigTagSpecifications #-}
{-# DEPRECATED tagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead"  #-}

instance Core.ToQuery CreateEgressOnlyInternetGateway where
        toQuery CreateEgressOnlyInternetGateway{..}
          = Core.toQueryPair "Action"
              ("CreateEgressOnlyInternetGateway" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "VpcId" vpcId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClientToken") clientToken
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TagSpecification")
                tagSpecifications

instance Core.ToHeaders CreateEgressOnlyInternetGateway where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateEgressOnlyInternetGateway where
        type Rs CreateEgressOnlyInternetGateway =
             CreateEgressOnlyInternetGatewayResponse
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
                 CreateEgressOnlyInternetGatewayResponse' Core.<$>
                   (x Core..@? "clientToken") Core.<*>
                     x Core..@? "egressOnlyInternetGateway"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateEgressOnlyInternetGatewayResponse' smart constructor.
data CreateEgressOnlyInternetGatewayResponse = CreateEgressOnlyInternetGatewayResponse'
  { clientToken :: Core.Maybe Core.Text
    -- ^ Unique, case-sensitive identifier that you provide to ensure the idempotency of the request.
  , egressOnlyInternetGateway :: Core.Maybe Types.EgressOnlyInternetGateway
    -- ^ Information about the egress-only internet gateway.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateEgressOnlyInternetGatewayResponse' value with any optional fields omitted.
mkCreateEgressOnlyInternetGatewayResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateEgressOnlyInternetGatewayResponse
mkCreateEgressOnlyInternetGatewayResponse responseStatus
  = CreateEgressOnlyInternetGatewayResponse'{clientToken =
                                               Core.Nothing,
                                             egressOnlyInternetGateway = Core.Nothing,
                                             responseStatus}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceoigrrsClientToken :: Lens.Lens' CreateEgressOnlyInternetGatewayResponse (Core.Maybe Core.Text)
ceoigrrsClientToken = Lens.field @"clientToken"
{-# INLINEABLE ceoigrrsClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | Information about the egress-only internet gateway.
--
-- /Note:/ Consider using 'egressOnlyInternetGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceoigrrsEgressOnlyInternetGateway :: Lens.Lens' CreateEgressOnlyInternetGatewayResponse (Core.Maybe Types.EgressOnlyInternetGateway)
ceoigrrsEgressOnlyInternetGateway = Lens.field @"egressOnlyInternetGateway"
{-# INLINEABLE ceoigrrsEgressOnlyInternetGateway #-}
{-# DEPRECATED egressOnlyInternetGateway "Use generic-lens or generic-optics with 'egressOnlyInternetGateway' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceoigrrsResponseStatus :: Lens.Lens' CreateEgressOnlyInternetGatewayResponse Core.Int
ceoigrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ceoigrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

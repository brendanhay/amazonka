{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateNatGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a NAT gateway in the specified public subnet. This action creates a network interface in the specified subnet with a private IP address from the IP address range of the subnet. Internet-bound traffic from a private subnet can be routed to the NAT gateway, therefore enabling instances in the private subnet to connect to the internet. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/vpc-nat-gateway.html NAT Gateways> in the /Amazon Virtual Private Cloud User Guide/ .
module Network.AWS.EC2.CreateNatGateway
    (
    -- * Creating a request
      CreateNatGateway (..)
    , mkCreateNatGateway
    -- ** Request lenses
    , cngAllocationId
    , cngSubnetId
    , cngClientToken
    , cngDryRun
    , cngTagSpecifications

    -- * Destructuring the response
    , CreateNatGatewayResponse (..)
    , mkCreateNatGatewayResponse
    -- ** Response lenses
    , cngrrsClientToken
    , cngrrsNatGateway
    , cngrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateNatGateway' smart constructor.
data CreateNatGateway = CreateNatGateway'
  { allocationId :: Types.AllocationId
    -- ^ The allocation ID of an Elastic IP address to associate with the NAT gateway. If the Elastic IP address is associated with another resource, you must first disassociate it.
  , subnetId :: Types.SubnetId
    -- ^ The subnet in which to create the NAT gateway.
  , clientToken :: Core.Maybe Core.Text
    -- ^ Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- Constraint: Maximum 64 ASCII characters.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , tagSpecifications :: Core.Maybe [Types.TagSpecification]
    -- ^ The tags to assign to the NAT gateway.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateNatGateway' value with any optional fields omitted.
mkCreateNatGateway
    :: Types.AllocationId -- ^ 'allocationId'
    -> Types.SubnetId -- ^ 'subnetId'
    -> CreateNatGateway
mkCreateNatGateway allocationId subnetId
  = CreateNatGateway'{allocationId, subnetId,
                      clientToken = Core.Nothing, dryRun = Core.Nothing,
                      tagSpecifications = Core.Nothing}

-- | The allocation ID of an Elastic IP address to associate with the NAT gateway. If the Elastic IP address is associated with another resource, you must first disassociate it.
--
-- /Note:/ Consider using 'allocationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cngAllocationId :: Lens.Lens' CreateNatGateway Types.AllocationId
cngAllocationId = Lens.field @"allocationId"
{-# INLINEABLE cngAllocationId #-}
{-# DEPRECATED allocationId "Use generic-lens or generic-optics with 'allocationId' instead"  #-}

-- | The subnet in which to create the NAT gateway.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cngSubnetId :: Lens.Lens' CreateNatGateway Types.SubnetId
cngSubnetId = Lens.field @"subnetId"
{-# INLINEABLE cngSubnetId #-}
{-# DEPRECATED subnetId "Use generic-lens or generic-optics with 'subnetId' instead"  #-}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- Constraint: Maximum 64 ASCII characters.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cngClientToken :: Lens.Lens' CreateNatGateway (Core.Maybe Core.Text)
cngClientToken = Lens.field @"clientToken"
{-# INLINEABLE cngClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cngDryRun :: Lens.Lens' CreateNatGateway (Core.Maybe Core.Bool)
cngDryRun = Lens.field @"dryRun"
{-# INLINEABLE cngDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | The tags to assign to the NAT gateway.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cngTagSpecifications :: Lens.Lens' CreateNatGateway (Core.Maybe [Types.TagSpecification])
cngTagSpecifications = Lens.field @"tagSpecifications"
{-# INLINEABLE cngTagSpecifications #-}
{-# DEPRECATED tagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead"  #-}

instance Core.ToQuery CreateNatGateway where
        toQuery CreateNatGateway{..}
          = Core.toQueryPair "Action" ("CreateNatGateway" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "AllocationId" allocationId
              Core.<> Core.toQueryPair "SubnetId" subnetId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ClientToken") clientToken
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "TagSpecification")
                tagSpecifications

instance Core.ToHeaders CreateNatGateway where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateNatGateway where
        type Rs CreateNatGateway = CreateNatGatewayResponse
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
                 CreateNatGatewayResponse' Core.<$>
                   (x Core..@? "clientToken") Core.<*> x Core..@? "natGateway"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateNatGatewayResponse' smart constructor.
data CreateNatGatewayResponse = CreateNatGatewayResponse'
  { clientToken :: Core.Maybe Core.Text
    -- ^ Unique, case-sensitive identifier to ensure the idempotency of the request. Only returned if a client token was provided in the request.
  , natGateway :: Core.Maybe Types.NatGateway
    -- ^ Information about the NAT gateway.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateNatGatewayResponse' value with any optional fields omitted.
mkCreateNatGatewayResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateNatGatewayResponse
mkCreateNatGatewayResponse responseStatus
  = CreateNatGatewayResponse'{clientToken = Core.Nothing,
                              natGateway = Core.Nothing, responseStatus}

-- | Unique, case-sensitive identifier to ensure the idempotency of the request. Only returned if a client token was provided in the request.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cngrrsClientToken :: Lens.Lens' CreateNatGatewayResponse (Core.Maybe Core.Text)
cngrrsClientToken = Lens.field @"clientToken"
{-# INLINEABLE cngrrsClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | Information about the NAT gateway.
--
-- /Note:/ Consider using 'natGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cngrrsNatGateway :: Lens.Lens' CreateNatGatewayResponse (Core.Maybe Types.NatGateway)
cngrrsNatGateway = Lens.field @"natGateway"
{-# INLINEABLE cngrrsNatGateway #-}
{-# DEPRECATED natGateway "Use generic-lens or generic-optics with 'natGateway' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cngrrsResponseStatus :: Lens.Lens' CreateNatGatewayResponse Core.Int
cngrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cngrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

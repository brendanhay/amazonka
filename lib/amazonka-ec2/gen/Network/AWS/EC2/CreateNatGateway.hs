{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateNatGateway (..),
    mkCreateNatGateway,

    -- ** Request lenses
    cngAllocationId,
    cngSubnetId,
    cngClientToken,
    cngDryRun,
    cngTagSpecifications,

    -- * Destructuring the response
    CreateNatGatewayResponse (..),
    mkCreateNatGatewayResponse,

    -- ** Response lenses
    cngrrsClientToken,
    cngrrsNatGateway,
    cngrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateNatGateway' smart constructor.
data CreateNatGateway = CreateNatGateway'
  { -- | The allocation ID of an Elastic IP address to associate with the NAT gateway. If the Elastic IP address is associated with another resource, you must first disassociate it.
    allocationId :: Types.AllocationId,
    -- | The subnet in which to create the NAT gateway.
    subnetId :: Types.SubnetId,
    -- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
    --
    -- Constraint: Maximum 64 ASCII characters.
    clientToken :: Core.Maybe Types.String,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The tags to assign to the NAT gateway.
    tagSpecifications :: Core.Maybe [Types.TagSpecification]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateNatGateway' value with any optional fields omitted.
mkCreateNatGateway ::
  -- | 'allocationId'
  Types.AllocationId ->
  -- | 'subnetId'
  Types.SubnetId ->
  CreateNatGateway
mkCreateNatGateway allocationId subnetId =
  CreateNatGateway'
    { allocationId,
      subnetId,
      clientToken = Core.Nothing,
      dryRun = Core.Nothing,
      tagSpecifications = Core.Nothing
    }

-- | The allocation ID of an Elastic IP address to associate with the NAT gateway. If the Elastic IP address is associated with another resource, you must first disassociate it.
--
-- /Note:/ Consider using 'allocationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cngAllocationId :: Lens.Lens' CreateNatGateway Types.AllocationId
cngAllocationId = Lens.field @"allocationId"
{-# DEPRECATED cngAllocationId "Use generic-lens or generic-optics with 'allocationId' instead." #-}

-- | The subnet in which to create the NAT gateway.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cngSubnetId :: Lens.Lens' CreateNatGateway Types.SubnetId
cngSubnetId = Lens.field @"subnetId"
{-# DEPRECATED cngSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- Constraint: Maximum 64 ASCII characters.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cngClientToken :: Lens.Lens' CreateNatGateway (Core.Maybe Types.String)
cngClientToken = Lens.field @"clientToken"
{-# DEPRECATED cngClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cngDryRun :: Lens.Lens' CreateNatGateway (Core.Maybe Core.Bool)
cngDryRun = Lens.field @"dryRun"
{-# DEPRECATED cngDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The tags to assign to the NAT gateway.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cngTagSpecifications :: Lens.Lens' CreateNatGateway (Core.Maybe [Types.TagSpecification])
cngTagSpecifications = Lens.field @"tagSpecifications"
{-# DEPRECATED cngTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

instance Core.AWSRequest CreateNatGateway where
  type Rs CreateNatGateway = CreateNatGatewayResponse
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
            ( Core.pure ("Action", "CreateNatGateway")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "AllocationId" allocationId)
                Core.<> (Core.toQueryValue "SubnetId" subnetId)
                Core.<> (Core.toQueryValue "ClientToken" Core.<$> clientToken)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "TagSpecification" Core.<$> tagSpecifications)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CreateNatGatewayResponse'
            Core.<$> (x Core..@? "clientToken")
            Core.<*> (x Core..@? "natGateway")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateNatGatewayResponse' smart constructor.
data CreateNatGatewayResponse = CreateNatGatewayResponse'
  { -- | Unique, case-sensitive identifier to ensure the idempotency of the request. Only returned if a client token was provided in the request.
    clientToken :: Core.Maybe Types.String,
    -- | Information about the NAT gateway.
    natGateway :: Core.Maybe Types.NatGateway,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateNatGatewayResponse' value with any optional fields omitted.
mkCreateNatGatewayResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateNatGatewayResponse
mkCreateNatGatewayResponse responseStatus =
  CreateNatGatewayResponse'
    { clientToken = Core.Nothing,
      natGateway = Core.Nothing,
      responseStatus
    }

-- | Unique, case-sensitive identifier to ensure the idempotency of the request. Only returned if a client token was provided in the request.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cngrrsClientToken :: Lens.Lens' CreateNatGatewayResponse (Core.Maybe Types.String)
cngrrsClientToken = Lens.field @"clientToken"
{-# DEPRECATED cngrrsClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | Information about the NAT gateway.
--
-- /Note:/ Consider using 'natGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cngrrsNatGateway :: Lens.Lens' CreateNatGatewayResponse (Core.Maybe Types.NatGateway)
cngrrsNatGateway = Lens.field @"natGateway"
{-# DEPRECATED cngrrsNatGateway "Use generic-lens or generic-optics with 'natGateway' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cngrrsResponseStatus :: Lens.Lens' CreateNatGatewayResponse Core.Int
cngrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cngrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

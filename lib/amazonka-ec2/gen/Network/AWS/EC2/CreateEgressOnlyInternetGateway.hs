{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateEgressOnlyInternetGateway (..),
    mkCreateEgressOnlyInternetGateway,

    -- ** Request lenses
    ceoigVpcId,
    ceoigClientToken,
    ceoigDryRun,
    ceoigTagSpecifications,

    -- * Destructuring the response
    CreateEgressOnlyInternetGatewayResponse (..),
    mkCreateEgressOnlyInternetGatewayResponse,

    -- ** Response lenses
    ceoigrrsClientToken,
    ceoigrrsEgressOnlyInternetGateway,
    ceoigrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateEgressOnlyInternetGateway' smart constructor.
data CreateEgressOnlyInternetGateway = CreateEgressOnlyInternetGateway'
  { -- | The ID of the VPC for which to create the egress-only internet gateway.
    vpcId :: Types.VpcId,
    -- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> .
    clientToken :: Core.Maybe Types.String,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The tags to assign to the egress-only internet gateway.
    tagSpecifications :: Core.Maybe [Types.TagSpecification]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateEgressOnlyInternetGateway' value with any optional fields omitted.
mkCreateEgressOnlyInternetGateway ::
  -- | 'vpcId'
  Types.VpcId ->
  CreateEgressOnlyInternetGateway
mkCreateEgressOnlyInternetGateway vpcId =
  CreateEgressOnlyInternetGateway'
    { vpcId,
      clientToken = Core.Nothing,
      dryRun = Core.Nothing,
      tagSpecifications = Core.Nothing
    }

-- | The ID of the VPC for which to create the egress-only internet gateway.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceoigVpcId :: Lens.Lens' CreateEgressOnlyInternetGateway Types.VpcId
ceoigVpcId = Lens.field @"vpcId"
{-# DEPRECATED ceoigVpcId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceoigClientToken :: Lens.Lens' CreateEgressOnlyInternetGateway (Core.Maybe Types.String)
ceoigClientToken = Lens.field @"clientToken"
{-# DEPRECATED ceoigClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceoigDryRun :: Lens.Lens' CreateEgressOnlyInternetGateway (Core.Maybe Core.Bool)
ceoigDryRun = Lens.field @"dryRun"
{-# DEPRECATED ceoigDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The tags to assign to the egress-only internet gateway.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceoigTagSpecifications :: Lens.Lens' CreateEgressOnlyInternetGateway (Core.Maybe [Types.TagSpecification])
ceoigTagSpecifications = Lens.field @"tagSpecifications"
{-# DEPRECATED ceoigTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

instance Core.AWSRequest CreateEgressOnlyInternetGateway where
  type
    Rs CreateEgressOnlyInternetGateway =
      CreateEgressOnlyInternetGatewayResponse
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
            ( Core.pure ("Action", "CreateEgressOnlyInternetGateway")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "VpcId" vpcId)
                Core.<> (Core.toQueryValue "ClientToken" Core.<$> clientToken)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "TagSpecification" Core.<$> tagSpecifications)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CreateEgressOnlyInternetGatewayResponse'
            Core.<$> (x Core..@? "clientToken")
            Core.<*> (x Core..@? "egressOnlyInternetGateway")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateEgressOnlyInternetGatewayResponse' smart constructor.
data CreateEgressOnlyInternetGatewayResponse = CreateEgressOnlyInternetGatewayResponse'
  { -- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request.
    clientToken :: Core.Maybe Types.ClientToken,
    -- | Information about the egress-only internet gateway.
    egressOnlyInternetGateway :: Core.Maybe Types.EgressOnlyInternetGateway,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateEgressOnlyInternetGatewayResponse' value with any optional fields omitted.
mkCreateEgressOnlyInternetGatewayResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateEgressOnlyInternetGatewayResponse
mkCreateEgressOnlyInternetGatewayResponse responseStatus =
  CreateEgressOnlyInternetGatewayResponse'
    { clientToken =
        Core.Nothing,
      egressOnlyInternetGateway = Core.Nothing,
      responseStatus
    }

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceoigrrsClientToken :: Lens.Lens' CreateEgressOnlyInternetGatewayResponse (Core.Maybe Types.ClientToken)
ceoigrrsClientToken = Lens.field @"clientToken"
{-# DEPRECATED ceoigrrsClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | Information about the egress-only internet gateway.
--
-- /Note:/ Consider using 'egressOnlyInternetGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceoigrrsEgressOnlyInternetGateway :: Lens.Lens' CreateEgressOnlyInternetGatewayResponse (Core.Maybe Types.EgressOnlyInternetGateway)
ceoigrrsEgressOnlyInternetGateway = Lens.field @"egressOnlyInternetGateway"
{-# DEPRECATED ceoigrrsEgressOnlyInternetGateway "Use generic-lens or generic-optics with 'egressOnlyInternetGateway' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceoigrrsResponseStatus :: Lens.Lens' CreateEgressOnlyInternetGatewayResponse Core.Int
ceoigrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ceoigrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

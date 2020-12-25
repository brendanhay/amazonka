{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteNatGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified NAT gateway. Deleting a NAT gateway disassociates its Elastic IP address, but does not release the address from your account. Deleting a NAT gateway does not delete any NAT gateway routes in your route tables.
module Network.AWS.EC2.DeleteNatGateway
  ( -- * Creating a request
    DeleteNatGateway (..),
    mkDeleteNatGateway,

    -- ** Request lenses
    dngfNatGatewayId,
    dngfDryRun,

    -- * Destructuring the response
    DeleteNatGatewayResponse (..),
    mkDeleteNatGatewayResponse,

    -- ** Response lenses
    dngrfrsNatGatewayId,
    dngrfrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteNatGateway' smart constructor.
data DeleteNatGateway = DeleteNatGateway'
  { -- | The ID of the NAT gateway.
    natGatewayId :: Types.NatGatewayId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNatGateway' value with any optional fields omitted.
mkDeleteNatGateway ::
  -- | 'natGatewayId'
  Types.NatGatewayId ->
  DeleteNatGateway
mkDeleteNatGateway natGatewayId =
  DeleteNatGateway' {natGatewayId, dryRun = Core.Nothing}

-- | The ID of the NAT gateway.
--
-- /Note:/ Consider using 'natGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngfNatGatewayId :: Lens.Lens' DeleteNatGateway Types.NatGatewayId
dngfNatGatewayId = Lens.field @"natGatewayId"
{-# DEPRECATED dngfNatGatewayId "Use generic-lens or generic-optics with 'natGatewayId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngfDryRun :: Lens.Lens' DeleteNatGateway (Core.Maybe Core.Bool)
dngfDryRun = Lens.field @"dryRun"
{-# DEPRECATED dngfDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest DeleteNatGateway where
  type Rs DeleteNatGateway = DeleteNatGatewayResponse
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
            ( Core.pure ("Action", "DeleteNatGateway")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "NatGatewayId" natGatewayId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteNatGatewayResponse'
            Core.<$> (x Core..@? "natGatewayId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteNatGatewayResponse' smart constructor.
data DeleteNatGatewayResponse = DeleteNatGatewayResponse'
  { -- | The ID of the NAT gateway.
    natGatewayId :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNatGatewayResponse' value with any optional fields omitted.
mkDeleteNatGatewayResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteNatGatewayResponse
mkDeleteNatGatewayResponse responseStatus =
  DeleteNatGatewayResponse'
    { natGatewayId = Core.Nothing,
      responseStatus
    }

-- | The ID of the NAT gateway.
--
-- /Note:/ Consider using 'natGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngrfrsNatGatewayId :: Lens.Lens' DeleteNatGatewayResponse (Core.Maybe Types.String)
dngrfrsNatGatewayId = Lens.field @"natGatewayId"
{-# DEPRECATED dngrfrsNatGatewayId "Use generic-lens or generic-optics with 'natGatewayId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngrfrsResponseStatus :: Lens.Lens' DeleteNatGatewayResponse Core.Int
dngrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dngrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteCustomerGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified customer gateway. You must delete the VPN connection before you can delete the customer gateway.
module Network.AWS.EC2.DeleteCustomerGateway
  ( -- * Creating a request
    DeleteCustomerGateway (..),
    mkDeleteCustomerGateway,

    -- ** Request lenses
    dcggCustomerGatewayId,
    dcggDryRun,

    -- * Destructuring the response
    DeleteCustomerGatewayResponse (..),
    mkDeleteCustomerGatewayResponse,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DeleteCustomerGateway.
--
-- /See:/ 'mkDeleteCustomerGateway' smart constructor.
data DeleteCustomerGateway = DeleteCustomerGateway'
  { -- | The ID of the customer gateway.
    customerGatewayId :: Types.CustomerGatewayId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCustomerGateway' value with any optional fields omitted.
mkDeleteCustomerGateway ::
  -- | 'customerGatewayId'
  Types.CustomerGatewayId ->
  DeleteCustomerGateway
mkDeleteCustomerGateway customerGatewayId =
  DeleteCustomerGateway' {customerGatewayId, dryRun = Core.Nothing}

-- | The ID of the customer gateway.
--
-- /Note:/ Consider using 'customerGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcggCustomerGatewayId :: Lens.Lens' DeleteCustomerGateway Types.CustomerGatewayId
dcggCustomerGatewayId = Lens.field @"customerGatewayId"
{-# DEPRECATED dcggCustomerGatewayId "Use generic-lens or generic-optics with 'customerGatewayId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcggDryRun :: Lens.Lens' DeleteCustomerGateway (Core.Maybe Core.Bool)
dcggDryRun = Lens.field @"dryRun"
{-# DEPRECATED dcggDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest DeleteCustomerGateway where
  type Rs DeleteCustomerGateway = DeleteCustomerGatewayResponse
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
            ( Core.pure ("Action", "DeleteCustomerGateway")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "CustomerGatewayId" customerGatewayId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response = Response.receiveNull DeleteCustomerGatewayResponse'

-- | /See:/ 'mkDeleteCustomerGatewayResponse' smart constructor.
data DeleteCustomerGatewayResponse = DeleteCustomerGatewayResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCustomerGatewayResponse' value with any optional fields omitted.
mkDeleteCustomerGatewayResponse ::
  DeleteCustomerGatewayResponse
mkDeleteCustomerGatewayResponse = DeleteCustomerGatewayResponse'

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteEgressOnlyInternetGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an egress-only internet gateway.
module Network.AWS.EC2.DeleteEgressOnlyInternetGateway
  ( -- * Creating a request
    DeleteEgressOnlyInternetGateway (..),
    mkDeleteEgressOnlyInternetGateway,

    -- ** Request lenses
    deoigfEgressOnlyInternetGatewayId,
    deoigfDryRun,

    -- * Destructuring the response
    DeleteEgressOnlyInternetGatewayResponse (..),
    mkDeleteEgressOnlyInternetGatewayResponse,

    -- ** Response lenses
    deoigrfrsReturnCode,
    deoigrfrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteEgressOnlyInternetGateway' smart constructor.
data DeleteEgressOnlyInternetGateway = DeleteEgressOnlyInternetGateway'
  { -- | The ID of the egress-only internet gateway.
    egressOnlyInternetGatewayId :: Types.EgressOnlyInternetGatewayId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteEgressOnlyInternetGateway' value with any optional fields omitted.
mkDeleteEgressOnlyInternetGateway ::
  -- | 'egressOnlyInternetGatewayId'
  Types.EgressOnlyInternetGatewayId ->
  DeleteEgressOnlyInternetGateway
mkDeleteEgressOnlyInternetGateway egressOnlyInternetGatewayId =
  DeleteEgressOnlyInternetGateway'
    { egressOnlyInternetGatewayId,
      dryRun = Core.Nothing
    }

-- | The ID of the egress-only internet gateway.
--
-- /Note:/ Consider using 'egressOnlyInternetGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deoigfEgressOnlyInternetGatewayId :: Lens.Lens' DeleteEgressOnlyInternetGateway Types.EgressOnlyInternetGatewayId
deoigfEgressOnlyInternetGatewayId = Lens.field @"egressOnlyInternetGatewayId"
{-# DEPRECATED deoigfEgressOnlyInternetGatewayId "Use generic-lens or generic-optics with 'egressOnlyInternetGatewayId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deoigfDryRun :: Lens.Lens' DeleteEgressOnlyInternetGateway (Core.Maybe Core.Bool)
deoigfDryRun = Lens.field @"dryRun"
{-# DEPRECATED deoigfDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest DeleteEgressOnlyInternetGateway where
  type
    Rs DeleteEgressOnlyInternetGateway =
      DeleteEgressOnlyInternetGatewayResponse
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
            ( Core.pure ("Action", "DeleteEgressOnlyInternetGateway")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> ( Core.toQueryValue
                            "EgressOnlyInternetGatewayId"
                            egressOnlyInternetGatewayId
                        )
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteEgressOnlyInternetGatewayResponse'
            Core.<$> (x Core..@? "returnCode") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteEgressOnlyInternetGatewayResponse' smart constructor.
data DeleteEgressOnlyInternetGatewayResponse = DeleteEgressOnlyInternetGatewayResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    returnCode :: Core.Maybe Core.Bool,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteEgressOnlyInternetGatewayResponse' value with any optional fields omitted.
mkDeleteEgressOnlyInternetGatewayResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteEgressOnlyInternetGatewayResponse
mkDeleteEgressOnlyInternetGatewayResponse responseStatus =
  DeleteEgressOnlyInternetGatewayResponse'
    { returnCode =
        Core.Nothing,
      responseStatus
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'returnCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deoigrfrsReturnCode :: Lens.Lens' DeleteEgressOnlyInternetGatewayResponse (Core.Maybe Core.Bool)
deoigrfrsReturnCode = Lens.field @"returnCode"
{-# DEPRECATED deoigrfrsReturnCode "Use generic-lens or generic-optics with 'returnCode' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deoigrfrsResponseStatus :: Lens.Lens' DeleteEgressOnlyInternetGatewayResponse Core.Int
deoigrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED deoigrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

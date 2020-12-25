{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteInternetGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified internet gateway. You must detach the internet gateway from the VPC before you can delete it.
module Network.AWS.EC2.DeleteInternetGateway
  ( -- * Creating a request
    DeleteInternetGateway (..),
    mkDeleteInternetGateway,

    -- ** Request lenses
    digfInternetGatewayId,
    digfDryRun,

    -- * Destructuring the response
    DeleteInternetGatewayResponse (..),
    mkDeleteInternetGatewayResponse,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteInternetGateway' smart constructor.
data DeleteInternetGateway = DeleteInternetGateway'
  { -- | The ID of the internet gateway.
    internetGatewayId :: Types.InternetGatewayId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteInternetGateway' value with any optional fields omitted.
mkDeleteInternetGateway ::
  -- | 'internetGatewayId'
  Types.InternetGatewayId ->
  DeleteInternetGateway
mkDeleteInternetGateway internetGatewayId =
  DeleteInternetGateway' {internetGatewayId, dryRun = Core.Nothing}

-- | The ID of the internet gateway.
--
-- /Note:/ Consider using 'internetGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digfInternetGatewayId :: Lens.Lens' DeleteInternetGateway Types.InternetGatewayId
digfInternetGatewayId = Lens.field @"internetGatewayId"
{-# DEPRECATED digfInternetGatewayId "Use generic-lens or generic-optics with 'internetGatewayId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digfDryRun :: Lens.Lens' DeleteInternetGateway (Core.Maybe Core.Bool)
digfDryRun = Lens.field @"dryRun"
{-# DEPRECATED digfDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest DeleteInternetGateway where
  type Rs DeleteInternetGateway = DeleteInternetGatewayResponse
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
            ( Core.pure ("Action", "DeleteInternetGateway")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "InternetGatewayId" internetGatewayId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response = Response.receiveNull DeleteInternetGatewayResponse'

-- | /See:/ 'mkDeleteInternetGatewayResponse' smart constructor.
data DeleteInternetGatewayResponse = DeleteInternetGatewayResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteInternetGatewayResponse' value with any optional fields omitted.
mkDeleteInternetGatewayResponse ::
  DeleteInternetGatewayResponse
mkDeleteInternetGatewayResponse = DeleteInternetGatewayResponse'

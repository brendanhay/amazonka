{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteNetworkInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified network interface. You must detach the network interface before you can delete it.
module Network.AWS.EC2.DeleteNetworkInterface
  ( -- * Creating a request
    DeleteNetworkInterface (..),
    mkDeleteNetworkInterface,

    -- ** Request lenses
    dnifNetworkInterfaceId,
    dnifDryRun,

    -- * Destructuring the response
    DeleteNetworkInterfaceResponse (..),
    mkDeleteNetworkInterfaceResponse,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DeleteNetworkInterface.
--
-- /See:/ 'mkDeleteNetworkInterface' smart constructor.
data DeleteNetworkInterface = DeleteNetworkInterface'
  { -- | The ID of the network interface.
    networkInterfaceId :: Types.NetworkInterfaceId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNetworkInterface' value with any optional fields omitted.
mkDeleteNetworkInterface ::
  -- | 'networkInterfaceId'
  Types.NetworkInterfaceId ->
  DeleteNetworkInterface
mkDeleteNetworkInterface networkInterfaceId =
  DeleteNetworkInterface'
    { networkInterfaceId,
      dryRun = Core.Nothing
    }

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnifNetworkInterfaceId :: Lens.Lens' DeleteNetworkInterface Types.NetworkInterfaceId
dnifNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# DEPRECATED dnifNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnifDryRun :: Lens.Lens' DeleteNetworkInterface (Core.Maybe Core.Bool)
dnifDryRun = Lens.field @"dryRun"
{-# DEPRECATED dnifDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest DeleteNetworkInterface where
  type Rs DeleteNetworkInterface = DeleteNetworkInterfaceResponse
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
            ( Core.pure ("Action", "DeleteNetworkInterface")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "NetworkInterfaceId" networkInterfaceId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response = Response.receiveNull DeleteNetworkInterfaceResponse'

-- | /See:/ 'mkDeleteNetworkInterfaceResponse' smart constructor.
data DeleteNetworkInterfaceResponse = DeleteNetworkInterfaceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNetworkInterfaceResponse' value with any optional fields omitted.
mkDeleteNetworkInterfaceResponse ::
  DeleteNetworkInterfaceResponse
mkDeleteNetworkInterfaceResponse = DeleteNetworkInterfaceResponse'

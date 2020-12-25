{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteNetworkInterfacePermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a permission for a network interface. By default, you cannot delete the permission if the account for which you're removing the permission has attached the network interface to an instance. However, you can force delete the permission, regardless of any attachment.
module Network.AWS.EC2.DeleteNetworkInterfacePermission
  ( -- * Creating a request
    DeleteNetworkInterfacePermission (..),
    mkDeleteNetworkInterfacePermission,

    -- ** Request lenses
    dnipNetworkInterfacePermissionId,
    dnipDryRun,
    dnipForce,

    -- * Destructuring the response
    DeleteNetworkInterfacePermissionResponse (..),
    mkDeleteNetworkInterfacePermissionResponse,

    -- ** Response lenses
    dniprrsReturn,
    dniprrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DeleteNetworkInterfacePermission.
--
-- /See:/ 'mkDeleteNetworkInterfacePermission' smart constructor.
data DeleteNetworkInterfacePermission = DeleteNetworkInterfacePermission'
  { -- | The ID of the network interface permission.
    networkInterfacePermissionId :: Types.NetworkInterfacePermissionId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | Specify @true@ to remove the permission even if the network interface is attached to an instance.
    force :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNetworkInterfacePermission' value with any optional fields omitted.
mkDeleteNetworkInterfacePermission ::
  -- | 'networkInterfacePermissionId'
  Types.NetworkInterfacePermissionId ->
  DeleteNetworkInterfacePermission
mkDeleteNetworkInterfacePermission networkInterfacePermissionId =
  DeleteNetworkInterfacePermission'
    { networkInterfacePermissionId,
      dryRun = Core.Nothing,
      force = Core.Nothing
    }

-- | The ID of the network interface permission.
--
-- /Note:/ Consider using 'networkInterfacePermissionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnipNetworkInterfacePermissionId :: Lens.Lens' DeleteNetworkInterfacePermission Types.NetworkInterfacePermissionId
dnipNetworkInterfacePermissionId = Lens.field @"networkInterfacePermissionId"
{-# DEPRECATED dnipNetworkInterfacePermissionId "Use generic-lens or generic-optics with 'networkInterfacePermissionId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnipDryRun :: Lens.Lens' DeleteNetworkInterfacePermission (Core.Maybe Core.Bool)
dnipDryRun = Lens.field @"dryRun"
{-# DEPRECATED dnipDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | Specify @true@ to remove the permission even if the network interface is attached to an instance.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnipForce :: Lens.Lens' DeleteNetworkInterfacePermission (Core.Maybe Core.Bool)
dnipForce = Lens.field @"force"
{-# DEPRECATED dnipForce "Use generic-lens or generic-optics with 'force' instead." #-}

instance Core.AWSRequest DeleteNetworkInterfacePermission where
  type
    Rs DeleteNetworkInterfacePermission =
      DeleteNetworkInterfacePermissionResponse
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
            ( Core.pure ("Action", "DeleteNetworkInterfacePermission")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> ( Core.toQueryValue
                            "NetworkInterfacePermissionId"
                            networkInterfacePermissionId
                        )
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "Force" Core.<$> force)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteNetworkInterfacePermissionResponse'
            Core.<$> (x Core..@? "return") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output for DeleteNetworkInterfacePermission.
--
-- /See:/ 'mkDeleteNetworkInterfacePermissionResponse' smart constructor.
data DeleteNetworkInterfacePermissionResponse = DeleteNetworkInterfacePermissionResponse'
  { -- | Returns @true@ if the request succeeds, otherwise returns an error.
    return :: Core.Maybe Core.Bool,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNetworkInterfacePermissionResponse' value with any optional fields omitted.
mkDeleteNetworkInterfacePermissionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteNetworkInterfacePermissionResponse
mkDeleteNetworkInterfacePermissionResponse responseStatus =
  DeleteNetworkInterfacePermissionResponse'
    { return = Core.Nothing,
      responseStatus
    }

-- | Returns @true@ if the request succeeds, otherwise returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dniprrsReturn :: Lens.Lens' DeleteNetworkInterfacePermissionResponse (Core.Maybe Core.Bool)
dniprrsReturn = Lens.field @"return"
{-# DEPRECATED dniprrsReturn "Use generic-lens or generic-optics with 'return' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dniprrsResponseStatus :: Lens.Lens' DeleteNetworkInterfacePermissionResponse Core.Int
dniprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dniprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

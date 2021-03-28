{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DeleteNetworkInterfacePermission (..)
    , mkDeleteNetworkInterfacePermission
    -- ** Request lenses
    , dnipNetworkInterfacePermissionId
    , dnipDryRun
    , dnipForce

    -- * Destructuring the response
    , DeleteNetworkInterfacePermissionResponse (..)
    , mkDeleteNetworkInterfacePermissionResponse
    -- ** Response lenses
    , dniprrsReturn
    , dniprrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DeleteNetworkInterfacePermission.
--
-- /See:/ 'mkDeleteNetworkInterfacePermission' smart constructor.
data DeleteNetworkInterfacePermission = DeleteNetworkInterfacePermission'
  { networkInterfacePermissionId :: Types.NetworkInterfacePermissionId
    -- ^ The ID of the network interface permission.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , force :: Core.Maybe Core.Bool
    -- ^ Specify @true@ to remove the permission even if the network interface is attached to an instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNetworkInterfacePermission' value with any optional fields omitted.
mkDeleteNetworkInterfacePermission
    :: Types.NetworkInterfacePermissionId -- ^ 'networkInterfacePermissionId'
    -> DeleteNetworkInterfacePermission
mkDeleteNetworkInterfacePermission networkInterfacePermissionId
  = DeleteNetworkInterfacePermission'{networkInterfacePermissionId,
                                      dryRun = Core.Nothing, force = Core.Nothing}

-- | The ID of the network interface permission.
--
-- /Note:/ Consider using 'networkInterfacePermissionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnipNetworkInterfacePermissionId :: Lens.Lens' DeleteNetworkInterfacePermission Types.NetworkInterfacePermissionId
dnipNetworkInterfacePermissionId = Lens.field @"networkInterfacePermissionId"
{-# INLINEABLE dnipNetworkInterfacePermissionId #-}
{-# DEPRECATED networkInterfacePermissionId "Use generic-lens or generic-optics with 'networkInterfacePermissionId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnipDryRun :: Lens.Lens' DeleteNetworkInterfacePermission (Core.Maybe Core.Bool)
dnipDryRun = Lens.field @"dryRun"
{-# INLINEABLE dnipDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | Specify @true@ to remove the permission even if the network interface is attached to an instance.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnipForce :: Lens.Lens' DeleteNetworkInterfacePermission (Core.Maybe Core.Bool)
dnipForce = Lens.field @"force"
{-# INLINEABLE dnipForce #-}
{-# DEPRECATED force "Use generic-lens or generic-optics with 'force' instead"  #-}

instance Core.ToQuery DeleteNetworkInterfacePermission where
        toQuery DeleteNetworkInterfacePermission{..}
          = Core.toQueryPair "Action"
              ("DeleteNetworkInterfacePermission" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "NetworkInterfacePermissionId"
                networkInterfacePermissionId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Force") force

instance Core.ToHeaders DeleteNetworkInterfacePermission where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteNetworkInterfacePermission where
        type Rs DeleteNetworkInterfacePermission =
             DeleteNetworkInterfacePermissionResponse
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
                 DeleteNetworkInterfacePermissionResponse' Core.<$>
                   (x Core..@? "return") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output for DeleteNetworkInterfacePermission.
--
-- /See:/ 'mkDeleteNetworkInterfacePermissionResponse' smart constructor.
data DeleteNetworkInterfacePermissionResponse = DeleteNetworkInterfacePermissionResponse'
  { return :: Core.Maybe Core.Bool
    -- ^ Returns @true@ if the request succeeds, otherwise returns an error.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNetworkInterfacePermissionResponse' value with any optional fields omitted.
mkDeleteNetworkInterfacePermissionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteNetworkInterfacePermissionResponse
mkDeleteNetworkInterfacePermissionResponse responseStatus
  = DeleteNetworkInterfacePermissionResponse'{return = Core.Nothing,
                                              responseStatus}

-- | Returns @true@ if the request succeeds, otherwise returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dniprrsReturn :: Lens.Lens' DeleteNetworkInterfacePermissionResponse (Core.Maybe Core.Bool)
dniprrsReturn = Lens.field @"return"
{-# INLINEABLE dniprrsReturn #-}
{-# DEPRECATED return "Use generic-lens or generic-optics with 'return' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dniprrsResponseStatus :: Lens.Lens' DeleteNetworkInterfacePermissionResponse Core.Int
dniprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dniprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

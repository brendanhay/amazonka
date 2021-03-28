{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DeleteNetworkInterface (..)
    , mkDeleteNetworkInterface
    -- ** Request lenses
    , dnifNetworkInterfaceId
    , dnifDryRun

    -- * Destructuring the response
    , DeleteNetworkInterfaceResponse (..)
    , mkDeleteNetworkInterfaceResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DeleteNetworkInterface.
--
-- /See:/ 'mkDeleteNetworkInterface' smart constructor.
data DeleteNetworkInterface = DeleteNetworkInterface'
  { networkInterfaceId :: Types.NetworkInterfaceId
    -- ^ The ID of the network interface.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNetworkInterface' value with any optional fields omitted.
mkDeleteNetworkInterface
    :: Types.NetworkInterfaceId -- ^ 'networkInterfaceId'
    -> DeleteNetworkInterface
mkDeleteNetworkInterface networkInterfaceId
  = DeleteNetworkInterface'{networkInterfaceId,
                            dryRun = Core.Nothing}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnifNetworkInterfaceId :: Lens.Lens' DeleteNetworkInterface Types.NetworkInterfaceId
dnifNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# INLINEABLE dnifNetworkInterfaceId #-}
{-# DEPRECATED networkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnifDryRun :: Lens.Lens' DeleteNetworkInterface (Core.Maybe Core.Bool)
dnifDryRun = Lens.field @"dryRun"
{-# INLINEABLE dnifDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DeleteNetworkInterface where
        toQuery DeleteNetworkInterface{..}
          = Core.toQueryPair "Action" ("DeleteNetworkInterface" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "NetworkInterfaceId" networkInterfaceId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DeleteNetworkInterface where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteNetworkInterface where
        type Rs DeleteNetworkInterface = DeleteNetworkInterfaceResponse
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
          = Response.receiveNull DeleteNetworkInterfaceResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteNetworkInterfaceResponse' smart constructor.
data DeleteNetworkInterfaceResponse = DeleteNetworkInterfaceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteNetworkInterfaceResponse' value with any optional fields omitted.
mkDeleteNetworkInterfaceResponse
    :: DeleteNetworkInterfaceResponse
mkDeleteNetworkInterfaceResponse = DeleteNetworkInterfaceResponse'

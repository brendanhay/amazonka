{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.PeerVpc
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Tries to peer the Lightsail VPC with the user's default VPC.
module Network.AWS.Lightsail.PeerVpc
    (
    -- * Creating a request
      PeerVpc (..)
    , mkPeerVpc

    -- * Destructuring the response
    , PeerVpcResponse (..)
    , mkPeerVpcResponse
    -- ** Response lenses
    , pvrrsOperation
    , pvrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPeerVpc' smart constructor.
data PeerVpc = PeerVpc'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PeerVpc' value with any optional fields omitted.
mkPeerVpc
    :: PeerVpc
mkPeerVpc = PeerVpc'

instance Core.ToQuery PeerVpc where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PeerVpc where
        toHeaders PeerVpc{..}
          = Core.pure ("X-Amz-Target", "Lightsail_20161128.PeerVpc") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PeerVpc where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest PeerVpc where
        type Rs PeerVpc = PeerVpcResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PeerVpcResponse' Core.<$>
                   (x Core..:? "operation") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPeerVpcResponse' smart constructor.
data PeerVpcResponse = PeerVpcResponse'
  { operation :: Core.Maybe Types.Operation
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'PeerVpcResponse' value with any optional fields omitted.
mkPeerVpcResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PeerVpcResponse
mkPeerVpcResponse responseStatus
  = PeerVpcResponse'{operation = Core.Nothing, responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvrrsOperation :: Lens.Lens' PeerVpcResponse (Core.Maybe Types.Operation)
pvrrsOperation = Lens.field @"operation"
{-# INLINEABLE pvrrsOperation #-}
{-# DEPRECATED operation "Use generic-lens or generic-optics with 'operation' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvrrsResponseStatus :: Lens.Lens' PeerVpcResponse Core.Int
pvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE pvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

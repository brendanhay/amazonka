{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.UnpeerVpc
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attempts to unpeer the Lightsail VPC from the user's default VPC.
module Network.AWS.Lightsail.UnpeerVpc
    (
    -- * Creating a request
      UnpeerVpc (..)
    , mkUnpeerVpc

    -- * Destructuring the response
    , UnpeerVpcResponse (..)
    , mkUnpeerVpcResponse
    -- ** Response lenses
    , uvrrsOperation
    , uvrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUnpeerVpc' smart constructor.
data UnpeerVpc = UnpeerVpc'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UnpeerVpc' value with any optional fields omitted.
mkUnpeerVpc
    :: UnpeerVpc
mkUnpeerVpc = UnpeerVpc'

instance Core.ToQuery UnpeerVpc where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UnpeerVpc where
        toHeaders UnpeerVpc{..}
          = Core.pure ("X-Amz-Target", "Lightsail_20161128.UnpeerVpc")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UnpeerVpc where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest UnpeerVpc where
        type Rs UnpeerVpc = UnpeerVpcResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UnpeerVpcResponse' Core.<$>
                   (x Core..:? "operation") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUnpeerVpcResponse' smart constructor.
data UnpeerVpcResponse = UnpeerVpcResponse'
  { operation :: Core.Maybe Types.Operation
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UnpeerVpcResponse' value with any optional fields omitted.
mkUnpeerVpcResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UnpeerVpcResponse
mkUnpeerVpcResponse responseStatus
  = UnpeerVpcResponse'{operation = Core.Nothing, responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvrrsOperation :: Lens.Lens' UnpeerVpcResponse (Core.Maybe Types.Operation)
uvrrsOperation = Lens.field @"operation"
{-# INLINEABLE uvrrsOperation #-}
{-# DEPRECATED operation "Use generic-lens or generic-optics with 'operation' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uvrrsResponseStatus :: Lens.Lens' UnpeerVpcResponse Core.Int
uvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.AllocateStaticIp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allocates a static IP address.
module Network.AWS.Lightsail.AllocateStaticIp
    (
    -- * Creating a request
      AllocateStaticIp (..)
    , mkAllocateStaticIp
    -- ** Request lenses
    , asiStaticIpName

    -- * Destructuring the response
    , AllocateStaticIpResponse (..)
    , mkAllocateStaticIpResponse
    -- ** Response lenses
    , asirrsOperations
    , asirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAllocateStaticIp' smart constructor.
newtype AllocateStaticIp = AllocateStaticIp'
  { staticIpName :: Types.ResourceName
    -- ^ The name of the static IP address.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AllocateStaticIp' value with any optional fields omitted.
mkAllocateStaticIp
    :: Types.ResourceName -- ^ 'staticIpName'
    -> AllocateStaticIp
mkAllocateStaticIp staticIpName = AllocateStaticIp'{staticIpName}

-- | The name of the static IP address.
--
-- /Note:/ Consider using 'staticIpName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asiStaticIpName :: Lens.Lens' AllocateStaticIp Types.ResourceName
asiStaticIpName = Lens.field @"staticIpName"
{-# INLINEABLE asiStaticIpName #-}
{-# DEPRECATED staticIpName "Use generic-lens or generic-optics with 'staticIpName' instead"  #-}

instance Core.ToQuery AllocateStaticIp where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AllocateStaticIp where
        toHeaders AllocateStaticIp{..}
          = Core.pure ("X-Amz-Target", "Lightsail_20161128.AllocateStaticIp")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AllocateStaticIp where
        toJSON AllocateStaticIp{..}
          = Core.object
              (Core.catMaybes [Core.Just ("staticIpName" Core..= staticIpName)])

instance Core.AWSRequest AllocateStaticIp where
        type Rs AllocateStaticIp = AllocateStaticIpResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 AllocateStaticIpResponse' Core.<$>
                   (x Core..:? "operations") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAllocateStaticIpResponse' smart constructor.
data AllocateStaticIpResponse = AllocateStaticIpResponse'
  { operations :: Core.Maybe [Types.Operation]
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AllocateStaticIpResponse' value with any optional fields omitted.
mkAllocateStaticIpResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AllocateStaticIpResponse
mkAllocateStaticIpResponse responseStatus
  = AllocateStaticIpResponse'{operations = Core.Nothing,
                              responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asirrsOperations :: Lens.Lens' AllocateStaticIpResponse (Core.Maybe [Types.Operation])
asirrsOperations = Lens.field @"operations"
{-# INLINEABLE asirrsOperations #-}
{-# DEPRECATED operations "Use generic-lens or generic-optics with 'operations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asirrsResponseStatus :: Lens.Lens' AllocateStaticIpResponse Core.Int
asirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE asirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

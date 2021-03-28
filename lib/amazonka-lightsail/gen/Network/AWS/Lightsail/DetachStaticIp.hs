{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DetachStaticIp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a static IP from the Amazon Lightsail instance to which it is attached.
module Network.AWS.Lightsail.DetachStaticIp
    (
    -- * Creating a request
      DetachStaticIp (..)
    , mkDetachStaticIp
    -- ** Request lenses
    , dsiStaticIpName

    -- * Destructuring the response
    , DetachStaticIpResponse (..)
    , mkDetachStaticIpResponse
    -- ** Response lenses
    , dsirrsOperations
    , dsirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDetachStaticIp' smart constructor.
newtype DetachStaticIp = DetachStaticIp'
  { staticIpName :: Types.ResourceName
    -- ^ The name of the static IP to detach from the instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DetachStaticIp' value with any optional fields omitted.
mkDetachStaticIp
    :: Types.ResourceName -- ^ 'staticIpName'
    -> DetachStaticIp
mkDetachStaticIp staticIpName = DetachStaticIp'{staticIpName}

-- | The name of the static IP to detach from the instance.
--
-- /Note:/ Consider using 'staticIpName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsiStaticIpName :: Lens.Lens' DetachStaticIp Types.ResourceName
dsiStaticIpName = Lens.field @"staticIpName"
{-# INLINEABLE dsiStaticIpName #-}
{-# DEPRECATED staticIpName "Use generic-lens or generic-optics with 'staticIpName' instead"  #-}

instance Core.ToQuery DetachStaticIp where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DetachStaticIp where
        toHeaders DetachStaticIp{..}
          = Core.pure ("X-Amz-Target", "Lightsail_20161128.DetachStaticIp")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DetachStaticIp where
        toJSON DetachStaticIp{..}
          = Core.object
              (Core.catMaybes [Core.Just ("staticIpName" Core..= staticIpName)])

instance Core.AWSRequest DetachStaticIp where
        type Rs DetachStaticIp = DetachStaticIpResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DetachStaticIpResponse' Core.<$>
                   (x Core..:? "operations") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDetachStaticIpResponse' smart constructor.
data DetachStaticIpResponse = DetachStaticIpResponse'
  { operations :: Core.Maybe [Types.Operation]
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DetachStaticIpResponse' value with any optional fields omitted.
mkDetachStaticIpResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DetachStaticIpResponse
mkDetachStaticIpResponse responseStatus
  = DetachStaticIpResponse'{operations = Core.Nothing,
                            responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsirrsOperations :: Lens.Lens' DetachStaticIpResponse (Core.Maybe [Types.Operation])
dsirrsOperations = Lens.field @"operations"
{-# INLINEABLE dsirrsOperations #-}
{-# DEPRECATED operations "Use generic-lens or generic-optics with 'operations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsirrsResponseStatus :: Lens.Lens' DetachStaticIpResponse Core.Int
dsirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

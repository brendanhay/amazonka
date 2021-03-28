{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.ReleaseStaticIp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specific static IP from your account.
module Network.AWS.Lightsail.ReleaseStaticIp
    (
    -- * Creating a request
      ReleaseStaticIp (..)
    , mkReleaseStaticIp
    -- ** Request lenses
    , rsiStaticIpName

    -- * Destructuring the response
    , ReleaseStaticIpResponse (..)
    , mkReleaseStaticIpResponse
    -- ** Response lenses
    , rsirrsOperations
    , rsirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkReleaseStaticIp' smart constructor.
newtype ReleaseStaticIp = ReleaseStaticIp'
  { staticIpName :: Types.ResourceName
    -- ^ The name of the static IP to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ReleaseStaticIp' value with any optional fields omitted.
mkReleaseStaticIp
    :: Types.ResourceName -- ^ 'staticIpName'
    -> ReleaseStaticIp
mkReleaseStaticIp staticIpName = ReleaseStaticIp'{staticIpName}

-- | The name of the static IP to delete.
--
-- /Note:/ Consider using 'staticIpName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsiStaticIpName :: Lens.Lens' ReleaseStaticIp Types.ResourceName
rsiStaticIpName = Lens.field @"staticIpName"
{-# INLINEABLE rsiStaticIpName #-}
{-# DEPRECATED staticIpName "Use generic-lens or generic-optics with 'staticIpName' instead"  #-}

instance Core.ToQuery ReleaseStaticIp where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ReleaseStaticIp where
        toHeaders ReleaseStaticIp{..}
          = Core.pure ("X-Amz-Target", "Lightsail_20161128.ReleaseStaticIp")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ReleaseStaticIp where
        toJSON ReleaseStaticIp{..}
          = Core.object
              (Core.catMaybes [Core.Just ("staticIpName" Core..= staticIpName)])

instance Core.AWSRequest ReleaseStaticIp where
        type Rs ReleaseStaticIp = ReleaseStaticIpResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ReleaseStaticIpResponse' Core.<$>
                   (x Core..:? "operations") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkReleaseStaticIpResponse' smart constructor.
data ReleaseStaticIpResponse = ReleaseStaticIpResponse'
  { operations :: Core.Maybe [Types.Operation]
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ReleaseStaticIpResponse' value with any optional fields omitted.
mkReleaseStaticIpResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ReleaseStaticIpResponse
mkReleaseStaticIpResponse responseStatus
  = ReleaseStaticIpResponse'{operations = Core.Nothing,
                             responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsirrsOperations :: Lens.Lens' ReleaseStaticIpResponse (Core.Maybe [Types.Operation])
rsirrsOperations = Lens.field @"operations"
{-# INLINEABLE rsirrsOperations #-}
{-# DEPRECATED operations "Use generic-lens or generic-optics with 'operations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsirrsResponseStatus :: Lens.Lens' ReleaseStaticIpResponse Core.Int
rsirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rsirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

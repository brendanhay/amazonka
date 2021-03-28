{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.AttachStaticIp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a static IP address to a specific Amazon Lightsail instance.
module Network.AWS.Lightsail.AttachStaticIp
    (
    -- * Creating a request
      AttachStaticIp (..)
    , mkAttachStaticIp
    -- ** Request lenses
    , aStaticIpName
    , aInstanceName

    -- * Destructuring the response
    , AttachStaticIpResponse (..)
    , mkAttachStaticIpResponse
    -- ** Response lenses
    , arsOperations
    , arsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAttachStaticIp' smart constructor.
data AttachStaticIp = AttachStaticIp'
  { staticIpName :: Types.ResourceName
    -- ^ The name of the static IP.
  , instanceName :: Types.ResourceName
    -- ^ The instance name to which you want to attach the static IP address.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachStaticIp' value with any optional fields omitted.
mkAttachStaticIp
    :: Types.ResourceName -- ^ 'staticIpName'
    -> Types.ResourceName -- ^ 'instanceName'
    -> AttachStaticIp
mkAttachStaticIp staticIpName instanceName
  = AttachStaticIp'{staticIpName, instanceName}

-- | The name of the static IP.
--
-- /Note:/ Consider using 'staticIpName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aStaticIpName :: Lens.Lens' AttachStaticIp Types.ResourceName
aStaticIpName = Lens.field @"staticIpName"
{-# INLINEABLE aStaticIpName #-}
{-# DEPRECATED staticIpName "Use generic-lens or generic-optics with 'staticIpName' instead"  #-}

-- | The instance name to which you want to attach the static IP address.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aInstanceName :: Lens.Lens' AttachStaticIp Types.ResourceName
aInstanceName = Lens.field @"instanceName"
{-# INLINEABLE aInstanceName #-}
{-# DEPRECATED instanceName "Use generic-lens or generic-optics with 'instanceName' instead"  #-}

instance Core.ToQuery AttachStaticIp where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AttachStaticIp where
        toHeaders AttachStaticIp{..}
          = Core.pure ("X-Amz-Target", "Lightsail_20161128.AttachStaticIp")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AttachStaticIp where
        toJSON AttachStaticIp{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("staticIpName" Core..= staticIpName),
                  Core.Just ("instanceName" Core..= instanceName)])

instance Core.AWSRequest AttachStaticIp where
        type Rs AttachStaticIp = AttachStaticIpResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 AttachStaticIpResponse' Core.<$>
                   (x Core..:? "operations") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAttachStaticIpResponse' smart constructor.
data AttachStaticIpResponse = AttachStaticIpResponse'
  { operations :: Core.Maybe [Types.Operation]
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AttachStaticIpResponse' value with any optional fields omitted.
mkAttachStaticIpResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AttachStaticIpResponse
mkAttachStaticIpResponse responseStatus
  = AttachStaticIpResponse'{operations = Core.Nothing,
                            responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arsOperations :: Lens.Lens' AttachStaticIpResponse (Core.Maybe [Types.Operation])
arsOperations = Lens.field @"operations"
{-# INLINEABLE arsOperations #-}
{-# DEPRECATED operations "Use generic-lens or generic-optics with 'operations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arsResponseStatus :: Lens.Lens' AttachStaticIpResponse Core.Int
arsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE arsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

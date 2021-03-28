{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetStaticIp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific static IP.
module Network.AWS.Lightsail.GetStaticIp
    (
    -- * Creating a request
      GetStaticIp (..)
    , mkGetStaticIp
    -- ** Request lenses
    , gsiStaticIpName

    -- * Destructuring the response
    , GetStaticIpResponse (..)
    , mkGetStaticIpResponse
    -- ** Response lenses
    , gsirfrsStaticIp
    , gsirfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetStaticIp' smart constructor.
newtype GetStaticIp = GetStaticIp'
  { staticIpName :: Types.ResourceName
    -- ^ The name of the static IP in Lightsail.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetStaticIp' value with any optional fields omitted.
mkGetStaticIp
    :: Types.ResourceName -- ^ 'staticIpName'
    -> GetStaticIp
mkGetStaticIp staticIpName = GetStaticIp'{staticIpName}

-- | The name of the static IP in Lightsail.
--
-- /Note:/ Consider using 'staticIpName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiStaticIpName :: Lens.Lens' GetStaticIp Types.ResourceName
gsiStaticIpName = Lens.field @"staticIpName"
{-# INLINEABLE gsiStaticIpName #-}
{-# DEPRECATED staticIpName "Use generic-lens or generic-optics with 'staticIpName' instead"  #-}

instance Core.ToQuery GetStaticIp where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetStaticIp where
        toHeaders GetStaticIp{..}
          = Core.pure ("X-Amz-Target", "Lightsail_20161128.GetStaticIp")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetStaticIp where
        toJSON GetStaticIp{..}
          = Core.object
              (Core.catMaybes [Core.Just ("staticIpName" Core..= staticIpName)])

instance Core.AWSRequest GetStaticIp where
        type Rs GetStaticIp = GetStaticIpResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetStaticIpResponse' Core.<$>
                   (x Core..:? "staticIp") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetStaticIpResponse' smart constructor.
data GetStaticIpResponse = GetStaticIpResponse'
  { staticIp :: Core.Maybe Types.StaticIp
    -- ^ An array of key-value pairs containing information about the requested static IP.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetStaticIpResponse' value with any optional fields omitted.
mkGetStaticIpResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetStaticIpResponse
mkGetStaticIpResponse responseStatus
  = GetStaticIpResponse'{staticIp = Core.Nothing, responseStatus}

-- | An array of key-value pairs containing information about the requested static IP.
--
-- /Note:/ Consider using 'staticIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsirfrsStaticIp :: Lens.Lens' GetStaticIpResponse (Core.Maybe Types.StaticIp)
gsirfrsStaticIp = Lens.field @"staticIp"
{-# INLINEABLE gsirfrsStaticIp #-}
{-# DEPRECATED staticIp "Use generic-lens or generic-optics with 'staticIp' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsirfrsResponseStatus :: Lens.Lens' GetStaticIpResponse Core.Int
gsirfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gsirfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetLoadBalancerTlsCertificates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the TLS certificates that are associated with the specified Lightsail load balancer.
--
-- TLS is just an updated, more secure version of Secure Socket Layer (SSL).
-- You can have a maximum of 2 certificates associated with a Lightsail load balancer. One is active and the other is inactive.
module Network.AWS.Lightsail.GetLoadBalancerTlsCertificates
    (
    -- * Creating a request
      GetLoadBalancerTlsCertificates (..)
    , mkGetLoadBalancerTlsCertificates
    -- ** Request lenses
    , glbtcLoadBalancerName

    -- * Destructuring the response
    , GetLoadBalancerTlsCertificatesResponse (..)
    , mkGetLoadBalancerTlsCertificatesResponse
    -- ** Response lenses
    , glbtcrrsTlsCertificates
    , glbtcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetLoadBalancerTlsCertificates' smart constructor.
newtype GetLoadBalancerTlsCertificates = GetLoadBalancerTlsCertificates'
  { loadBalancerName :: Types.ResourceName
    -- ^ The name of the load balancer you associated with your SSL/TLS certificate.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetLoadBalancerTlsCertificates' value with any optional fields omitted.
mkGetLoadBalancerTlsCertificates
    :: Types.ResourceName -- ^ 'loadBalancerName'
    -> GetLoadBalancerTlsCertificates
mkGetLoadBalancerTlsCertificates loadBalancerName
  = GetLoadBalancerTlsCertificates'{loadBalancerName}

-- | The name of the load balancer you associated with your SSL/TLS certificate.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glbtcLoadBalancerName :: Lens.Lens' GetLoadBalancerTlsCertificates Types.ResourceName
glbtcLoadBalancerName = Lens.field @"loadBalancerName"
{-# INLINEABLE glbtcLoadBalancerName #-}
{-# DEPRECATED loadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead"  #-}

instance Core.ToQuery GetLoadBalancerTlsCertificates where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetLoadBalancerTlsCertificates where
        toHeaders GetLoadBalancerTlsCertificates{..}
          = Core.pure
              ("X-Amz-Target",
               "Lightsail_20161128.GetLoadBalancerTlsCertificates")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetLoadBalancerTlsCertificates where
        toJSON GetLoadBalancerTlsCertificates{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("loadBalancerName" Core..= loadBalancerName)])

instance Core.AWSRequest GetLoadBalancerTlsCertificates where
        type Rs GetLoadBalancerTlsCertificates =
             GetLoadBalancerTlsCertificatesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetLoadBalancerTlsCertificatesResponse' Core.<$>
                   (x Core..:? "tlsCertificates") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetLoadBalancerTlsCertificatesResponse' smart constructor.
data GetLoadBalancerTlsCertificatesResponse = GetLoadBalancerTlsCertificatesResponse'
  { tlsCertificates :: Core.Maybe [Types.LoadBalancerTlsCertificate]
    -- ^ An array of LoadBalancerTlsCertificate objects describing your SSL/TLS certificates.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetLoadBalancerTlsCertificatesResponse' value with any optional fields omitted.
mkGetLoadBalancerTlsCertificatesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetLoadBalancerTlsCertificatesResponse
mkGetLoadBalancerTlsCertificatesResponse responseStatus
  = GetLoadBalancerTlsCertificatesResponse'{tlsCertificates =
                                              Core.Nothing,
                                            responseStatus}

-- | An array of LoadBalancerTlsCertificate objects describing your SSL/TLS certificates.
--
-- /Note:/ Consider using 'tlsCertificates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glbtcrrsTlsCertificates :: Lens.Lens' GetLoadBalancerTlsCertificatesResponse (Core.Maybe [Types.LoadBalancerTlsCertificate])
glbtcrrsTlsCertificates = Lens.field @"tlsCertificates"
{-# INLINEABLE glbtcrrsTlsCertificates #-}
{-# DEPRECATED tlsCertificates "Use generic-lens or generic-optics with 'tlsCertificates' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glbtcrrsResponseStatus :: Lens.Lens' GetLoadBalancerTlsCertificatesResponse Core.Int
glbtcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE glbtcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

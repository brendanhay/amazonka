{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteLoadBalancerTlsCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an SSL/TLS certificate associated with a Lightsail load balancer.
--
-- The @DeleteLoadBalancerTlsCertificate@ operation supports tag-based access control via resource tags applied to the resource identified by @load balancer name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.DeleteLoadBalancerTlsCertificate
    (
    -- * Creating a request
      DeleteLoadBalancerTlsCertificate (..)
    , mkDeleteLoadBalancerTlsCertificate
    -- ** Request lenses
    , dlbtcLoadBalancerName
    , dlbtcCertificateName
    , dlbtcForce

    -- * Destructuring the response
    , DeleteLoadBalancerTlsCertificateResponse (..)
    , mkDeleteLoadBalancerTlsCertificateResponse
    -- ** Response lenses
    , dlbtcrrsOperations
    , dlbtcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteLoadBalancerTlsCertificate' smart constructor.
data DeleteLoadBalancerTlsCertificate = DeleteLoadBalancerTlsCertificate'
  { loadBalancerName :: Types.LoadBalancerName
    -- ^ The load balancer name.
  , certificateName :: Types.CertificateName
    -- ^ The SSL/TLS certificate name.
  , force :: Core.Maybe Core.Bool
    -- ^ When @true@ , forces the deletion of an SSL/TLS certificate.
--
-- There can be two certificates associated with a Lightsail load balancer: the primary and the backup. The @force@ parameter is required when the primary SSL/TLS certificate is in use by an instance attached to the load balancer.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteLoadBalancerTlsCertificate' value with any optional fields omitted.
mkDeleteLoadBalancerTlsCertificate
    :: Types.LoadBalancerName -- ^ 'loadBalancerName'
    -> Types.CertificateName -- ^ 'certificateName'
    -> DeleteLoadBalancerTlsCertificate
mkDeleteLoadBalancerTlsCertificate loadBalancerName certificateName
  = DeleteLoadBalancerTlsCertificate'{loadBalancerName,
                                      certificateName, force = Core.Nothing}

-- | The load balancer name.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbtcLoadBalancerName :: Lens.Lens' DeleteLoadBalancerTlsCertificate Types.LoadBalancerName
dlbtcLoadBalancerName = Lens.field @"loadBalancerName"
{-# INLINEABLE dlbtcLoadBalancerName #-}
{-# DEPRECATED loadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead"  #-}

-- | The SSL/TLS certificate name.
--
-- /Note:/ Consider using 'certificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbtcCertificateName :: Lens.Lens' DeleteLoadBalancerTlsCertificate Types.CertificateName
dlbtcCertificateName = Lens.field @"certificateName"
{-# INLINEABLE dlbtcCertificateName #-}
{-# DEPRECATED certificateName "Use generic-lens or generic-optics with 'certificateName' instead"  #-}

-- | When @true@ , forces the deletion of an SSL/TLS certificate.
--
-- There can be two certificates associated with a Lightsail load balancer: the primary and the backup. The @force@ parameter is required when the primary SSL/TLS certificate is in use by an instance attached to the load balancer.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbtcForce :: Lens.Lens' DeleteLoadBalancerTlsCertificate (Core.Maybe Core.Bool)
dlbtcForce = Lens.field @"force"
{-# INLINEABLE dlbtcForce #-}
{-# DEPRECATED force "Use generic-lens or generic-optics with 'force' instead"  #-}

instance Core.ToQuery DeleteLoadBalancerTlsCertificate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteLoadBalancerTlsCertificate where
        toHeaders DeleteLoadBalancerTlsCertificate{..}
          = Core.pure
              ("X-Amz-Target",
               "Lightsail_20161128.DeleteLoadBalancerTlsCertificate")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteLoadBalancerTlsCertificate where
        toJSON DeleteLoadBalancerTlsCertificate{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("loadBalancerName" Core..= loadBalancerName),
                  Core.Just ("certificateName" Core..= certificateName),
                  ("force" Core..=) Core.<$> force])

instance Core.AWSRequest DeleteLoadBalancerTlsCertificate where
        type Rs DeleteLoadBalancerTlsCertificate =
             DeleteLoadBalancerTlsCertificateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteLoadBalancerTlsCertificateResponse' Core.<$>
                   (x Core..:? "operations") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteLoadBalancerTlsCertificateResponse' smart constructor.
data DeleteLoadBalancerTlsCertificateResponse = DeleteLoadBalancerTlsCertificateResponse'
  { operations :: Core.Maybe [Types.Operation]
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteLoadBalancerTlsCertificateResponse' value with any optional fields omitted.
mkDeleteLoadBalancerTlsCertificateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteLoadBalancerTlsCertificateResponse
mkDeleteLoadBalancerTlsCertificateResponse responseStatus
  = DeleteLoadBalancerTlsCertificateResponse'{operations =
                                                Core.Nothing,
                                              responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbtcrrsOperations :: Lens.Lens' DeleteLoadBalancerTlsCertificateResponse (Core.Maybe [Types.Operation])
dlbtcrrsOperations = Lens.field @"operations"
{-# INLINEABLE dlbtcrrsOperations #-}
{-# DEPRECATED operations "Use generic-lens or generic-optics with 'operations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbtcrrsResponseStatus :: Lens.Lens' DeleteLoadBalancerTlsCertificateResponse Core.Int
dlbtcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dlbtcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

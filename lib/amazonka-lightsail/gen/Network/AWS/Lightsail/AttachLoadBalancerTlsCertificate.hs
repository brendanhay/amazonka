{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.AttachLoadBalancerTlsCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a Transport Layer Security (TLS) certificate to your load balancer. TLS is just an updated, more secure version of Secure Socket Layer (SSL).
--
-- Once you create and validate your certificate, you can attach it to your load balancer. You can also use this API to rotate the certificates on your account. Use the @AttachLoadBalancerTlsCertificate@ action with the non-attached certificate, and it will replace the existing one and become the attached certificate.
-- The @AttachLoadBalancerTlsCertificate@ operation supports tag-based access control via resource tags applied to the resource identified by @load balancer name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.AttachLoadBalancerTlsCertificate
    (
    -- * Creating a request
      AttachLoadBalancerTlsCertificate (..)
    , mkAttachLoadBalancerTlsCertificate
    -- ** Request lenses
    , albtcLoadBalancerName
    , albtcCertificateName

    -- * Destructuring the response
    , AttachLoadBalancerTlsCertificateResponse (..)
    , mkAttachLoadBalancerTlsCertificateResponse
    -- ** Response lenses
    , albtcrrsOperations
    , albtcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAttachLoadBalancerTlsCertificate' smart constructor.
data AttachLoadBalancerTlsCertificate = AttachLoadBalancerTlsCertificate'
  { loadBalancerName :: Types.ResourceName
    -- ^ The name of the load balancer to which you want to associate the SSL/TLS certificate.
  , certificateName :: Types.ResourceName
    -- ^ The name of your SSL/TLS certificate.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AttachLoadBalancerTlsCertificate' value with any optional fields omitted.
mkAttachLoadBalancerTlsCertificate
    :: Types.ResourceName -- ^ 'loadBalancerName'
    -> Types.ResourceName -- ^ 'certificateName'
    -> AttachLoadBalancerTlsCertificate
mkAttachLoadBalancerTlsCertificate loadBalancerName certificateName
  = AttachLoadBalancerTlsCertificate'{loadBalancerName,
                                      certificateName}

-- | The name of the load balancer to which you want to associate the SSL/TLS certificate.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albtcLoadBalancerName :: Lens.Lens' AttachLoadBalancerTlsCertificate Types.ResourceName
albtcLoadBalancerName = Lens.field @"loadBalancerName"
{-# INLINEABLE albtcLoadBalancerName #-}
{-# DEPRECATED loadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead"  #-}

-- | The name of your SSL/TLS certificate.
--
-- /Note:/ Consider using 'certificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albtcCertificateName :: Lens.Lens' AttachLoadBalancerTlsCertificate Types.ResourceName
albtcCertificateName = Lens.field @"certificateName"
{-# INLINEABLE albtcCertificateName #-}
{-# DEPRECATED certificateName "Use generic-lens or generic-optics with 'certificateName' instead"  #-}

instance Core.ToQuery AttachLoadBalancerTlsCertificate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AttachLoadBalancerTlsCertificate where
        toHeaders AttachLoadBalancerTlsCertificate{..}
          = Core.pure
              ("X-Amz-Target",
               "Lightsail_20161128.AttachLoadBalancerTlsCertificate")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AttachLoadBalancerTlsCertificate where
        toJSON AttachLoadBalancerTlsCertificate{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("loadBalancerName" Core..= loadBalancerName),
                  Core.Just ("certificateName" Core..= certificateName)])

instance Core.AWSRequest AttachLoadBalancerTlsCertificate where
        type Rs AttachLoadBalancerTlsCertificate =
             AttachLoadBalancerTlsCertificateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 AttachLoadBalancerTlsCertificateResponse' Core.<$>
                   (x Core..:? "operations") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAttachLoadBalancerTlsCertificateResponse' smart constructor.
data AttachLoadBalancerTlsCertificateResponse = AttachLoadBalancerTlsCertificateResponse'
  { operations :: Core.Maybe [Types.Operation]
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- These SSL/TLS certificates are only usable by Lightsail load balancers. You can't get the certificate and use it for another purpose.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AttachLoadBalancerTlsCertificateResponse' value with any optional fields omitted.
mkAttachLoadBalancerTlsCertificateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AttachLoadBalancerTlsCertificateResponse
mkAttachLoadBalancerTlsCertificateResponse responseStatus
  = AttachLoadBalancerTlsCertificateResponse'{operations =
                                                Core.Nothing,
                                              responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- These SSL/TLS certificates are only usable by Lightsail load balancers. You can't get the certificate and use it for another purpose.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albtcrrsOperations :: Lens.Lens' AttachLoadBalancerTlsCertificateResponse (Core.Maybe [Types.Operation])
albtcrrsOperations = Lens.field @"operations"
{-# INLINEABLE albtcrrsOperations #-}
{-# DEPRECATED operations "Use generic-lens or generic-optics with 'operations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albtcrrsResponseStatus :: Lens.Lens' AttachLoadBalancerTlsCertificateResponse Core.Int
albtcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE albtcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

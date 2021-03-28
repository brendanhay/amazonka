{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateLoadBalancerTlsCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Lightsail load balancer TLS certificate.
--
-- TLS is just an updated, more secure version of Secure Socket Layer (SSL).
-- The @CreateLoadBalancerTlsCertificate@ operation supports tag-based access control via resource tags applied to the resource identified by @load balancer name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.CreateLoadBalancerTlsCertificate
    (
    -- * Creating a request
      CreateLoadBalancerTlsCertificate (..)
    , mkCreateLoadBalancerTlsCertificate
    -- ** Request lenses
    , clbtcLoadBalancerName
    , clbtcCertificateName
    , clbtcCertificateDomainName
    , clbtcCertificateAlternativeNames
    , clbtcTags

    -- * Destructuring the response
    , CreateLoadBalancerTlsCertificateResponse (..)
    , mkCreateLoadBalancerTlsCertificateResponse
    -- ** Response lenses
    , clbtcrrsOperations
    , clbtcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateLoadBalancerTlsCertificate' smart constructor.
data CreateLoadBalancerTlsCertificate = CreateLoadBalancerTlsCertificate'
  { loadBalancerName :: Types.ResourceName
    -- ^ The load balancer name where you want to create the SSL/TLS certificate.
  , certificateName :: Types.ResourceName
    -- ^ The SSL/TLS certificate name.
--
-- You can have up to 10 certificates in your account at one time. Each Lightsail load balancer can have up to 2 certificates associated with it at one time. There is also an overall limit to the number of certificates that can be issue in a 365-day period. For more information, see <http://docs.aws.amazon.com/acm/latest/userguide/acm-limits.html Limits> .
  , certificateDomainName :: Types.DomainName
    -- ^ The domain name (e.g., @example.com@ ) for your SSL/TLS certificate.
  , certificateAlternativeNames :: Core.Maybe [Types.DomainName]
    -- ^ An array of strings listing alternative domains and subdomains for your SSL/TLS certificate. Lightsail will de-dupe the names for you. You can have a maximum of 9 alternative names (in addition to the 1 primary domain). We do not support wildcards (e.g., @*.example.com@ ).
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLoadBalancerTlsCertificate' value with any optional fields omitted.
mkCreateLoadBalancerTlsCertificate
    :: Types.ResourceName -- ^ 'loadBalancerName'
    -> Types.ResourceName -- ^ 'certificateName'
    -> Types.DomainName -- ^ 'certificateDomainName'
    -> CreateLoadBalancerTlsCertificate
mkCreateLoadBalancerTlsCertificate loadBalancerName certificateName
  certificateDomainName
  = CreateLoadBalancerTlsCertificate'{loadBalancerName,
                                      certificateName, certificateDomainName,
                                      certificateAlternativeNames = Core.Nothing,
                                      tags = Core.Nothing}

-- | The load balancer name where you want to create the SSL/TLS certificate.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbtcLoadBalancerName :: Lens.Lens' CreateLoadBalancerTlsCertificate Types.ResourceName
clbtcLoadBalancerName = Lens.field @"loadBalancerName"
{-# INLINEABLE clbtcLoadBalancerName #-}
{-# DEPRECATED loadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead"  #-}

-- | The SSL/TLS certificate name.
--
-- You can have up to 10 certificates in your account at one time. Each Lightsail load balancer can have up to 2 certificates associated with it at one time. There is also an overall limit to the number of certificates that can be issue in a 365-day period. For more information, see <http://docs.aws.amazon.com/acm/latest/userguide/acm-limits.html Limits> .
--
-- /Note:/ Consider using 'certificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbtcCertificateName :: Lens.Lens' CreateLoadBalancerTlsCertificate Types.ResourceName
clbtcCertificateName = Lens.field @"certificateName"
{-# INLINEABLE clbtcCertificateName #-}
{-# DEPRECATED certificateName "Use generic-lens or generic-optics with 'certificateName' instead"  #-}

-- | The domain name (e.g., @example.com@ ) for your SSL/TLS certificate.
--
-- /Note:/ Consider using 'certificateDomainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbtcCertificateDomainName :: Lens.Lens' CreateLoadBalancerTlsCertificate Types.DomainName
clbtcCertificateDomainName = Lens.field @"certificateDomainName"
{-# INLINEABLE clbtcCertificateDomainName #-}
{-# DEPRECATED certificateDomainName "Use generic-lens or generic-optics with 'certificateDomainName' instead"  #-}

-- | An array of strings listing alternative domains and subdomains for your SSL/TLS certificate. Lightsail will de-dupe the names for you. You can have a maximum of 9 alternative names (in addition to the 1 primary domain). We do not support wildcards (e.g., @*.example.com@ ).
--
-- /Note:/ Consider using 'certificateAlternativeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbtcCertificateAlternativeNames :: Lens.Lens' CreateLoadBalancerTlsCertificate (Core.Maybe [Types.DomainName])
clbtcCertificateAlternativeNames = Lens.field @"certificateAlternativeNames"
{-# INLINEABLE clbtcCertificateAlternativeNames #-}
{-# DEPRECATED certificateAlternativeNames "Use generic-lens or generic-optics with 'certificateAlternativeNames' instead"  #-}

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbtcTags :: Lens.Lens' CreateLoadBalancerTlsCertificate (Core.Maybe [Types.Tag])
clbtcTags = Lens.field @"tags"
{-# INLINEABLE clbtcTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateLoadBalancerTlsCertificate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateLoadBalancerTlsCertificate where
        toHeaders CreateLoadBalancerTlsCertificate{..}
          = Core.pure
              ("X-Amz-Target",
               "Lightsail_20161128.CreateLoadBalancerTlsCertificate")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateLoadBalancerTlsCertificate where
        toJSON CreateLoadBalancerTlsCertificate{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("loadBalancerName" Core..= loadBalancerName),
                  Core.Just ("certificateName" Core..= certificateName),
                  Core.Just ("certificateDomainName" Core..= certificateDomainName),
                  ("certificateAlternativeNames" Core..=) Core.<$>
                    certificateAlternativeNames,
                  ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateLoadBalancerTlsCertificate where
        type Rs CreateLoadBalancerTlsCertificate =
             CreateLoadBalancerTlsCertificateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateLoadBalancerTlsCertificateResponse' Core.<$>
                   (x Core..:? "operations") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateLoadBalancerTlsCertificateResponse' smart constructor.
data CreateLoadBalancerTlsCertificateResponse = CreateLoadBalancerTlsCertificateResponse'
  { operations :: Core.Maybe [Types.Operation]
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateLoadBalancerTlsCertificateResponse' value with any optional fields omitted.
mkCreateLoadBalancerTlsCertificateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateLoadBalancerTlsCertificateResponse
mkCreateLoadBalancerTlsCertificateResponse responseStatus
  = CreateLoadBalancerTlsCertificateResponse'{operations =
                                                Core.Nothing,
                                              responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbtcrrsOperations :: Lens.Lens' CreateLoadBalancerTlsCertificateResponse (Core.Maybe [Types.Operation])
clbtcrrsOperations = Lens.field @"operations"
{-# INLINEABLE clbtcrrsOperations #-}
{-# DEPRECATED operations "Use generic-lens or generic-optics with 'operations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbtcrrsResponseStatus :: Lens.Lens' CreateLoadBalancerTlsCertificateResponse Core.Int
clbtcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE clbtcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateLoadBalancer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Lightsail load balancer. To learn more about deciding whether to load balance your application, see <https://lightsail.aws.amazon.com/ls/docs/how-to/article/configure-lightsail-instances-for-load-balancing Configure your Lightsail instances for load balancing> . You can create up to 5 load balancers per AWS Region in your account.
--
-- When you create a load balancer, you can specify a unique name and port settings. To change additional load balancer settings, use the @UpdateLoadBalancerAttribute@ operation.
-- The @create load balancer@ operation supports tag-based access control via request tags. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.CreateLoadBalancer
    (
    -- * Creating a request
      CreateLoadBalancer (..)
    , mkCreateLoadBalancer
    -- ** Request lenses
    , clbLoadBalancerName
    , clbInstancePort
    , clbCertificateAlternativeNames
    , clbCertificateDomainName
    , clbCertificateName
    , clbHealthCheckPath
    , clbTags

    -- * Destructuring the response
    , CreateLoadBalancerResponse (..)
    , mkCreateLoadBalancerResponse
    -- ** Response lenses
    , clbrrsOperations
    , clbrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateLoadBalancer' smart constructor.
data CreateLoadBalancer = CreateLoadBalancer'
  { loadBalancerName :: Types.ResourceName
    -- ^ The name of your load balancer.
  , instancePort :: Core.Int
    -- ^ The instance port where you're creating your load balancer.
  , certificateAlternativeNames :: Core.Maybe [Types.DomainName]
    -- ^ The optional alternative domains and subdomains to use with your SSL/TLS certificate (e.g., @www.example.com@ , @example.com@ , @m.example.com@ , @blog.example.com@ ).
  , certificateDomainName :: Core.Maybe Types.DomainName
    -- ^ The domain name with which your certificate is associated (e.g., @example.com@ ).
--
-- If you specify @certificateDomainName@ , then @certificateName@ is required (and vice-versa).
  , certificateName :: Core.Maybe Types.ResourceName
    -- ^ The name of the SSL/TLS certificate.
--
-- If you specify @certificateName@ , then @certificateDomainName@ is required (and vice-versa).
  , healthCheckPath :: Core.Maybe Core.Text
    -- ^ The path you provided to perform the load balancer health check. If you didn't specify a health check path, Lightsail uses the root path of your website (e.g., @"/"@ ).
--
-- You may want to specify a custom health check path other than the root of your application if your home page loads slowly or has a lot of media or scripting on it.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLoadBalancer' value with any optional fields omitted.
mkCreateLoadBalancer
    :: Types.ResourceName -- ^ 'loadBalancerName'
    -> Core.Int -- ^ 'instancePort'
    -> CreateLoadBalancer
mkCreateLoadBalancer loadBalancerName instancePort
  = CreateLoadBalancer'{loadBalancerName, instancePort,
                        certificateAlternativeNames = Core.Nothing,
                        certificateDomainName = Core.Nothing,
                        certificateName = Core.Nothing, healthCheckPath = Core.Nothing,
                        tags = Core.Nothing}

-- | The name of your load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbLoadBalancerName :: Lens.Lens' CreateLoadBalancer Types.ResourceName
clbLoadBalancerName = Lens.field @"loadBalancerName"
{-# INLINEABLE clbLoadBalancerName #-}
{-# DEPRECATED loadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead"  #-}

-- | The instance port where you're creating your load balancer.
--
-- /Note:/ Consider using 'instancePort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbInstancePort :: Lens.Lens' CreateLoadBalancer Core.Int
clbInstancePort = Lens.field @"instancePort"
{-# INLINEABLE clbInstancePort #-}
{-# DEPRECATED instancePort "Use generic-lens or generic-optics with 'instancePort' instead"  #-}

-- | The optional alternative domains and subdomains to use with your SSL/TLS certificate (e.g., @www.example.com@ , @example.com@ , @m.example.com@ , @blog.example.com@ ).
--
-- /Note:/ Consider using 'certificateAlternativeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbCertificateAlternativeNames :: Lens.Lens' CreateLoadBalancer (Core.Maybe [Types.DomainName])
clbCertificateAlternativeNames = Lens.field @"certificateAlternativeNames"
{-# INLINEABLE clbCertificateAlternativeNames #-}
{-# DEPRECATED certificateAlternativeNames "Use generic-lens or generic-optics with 'certificateAlternativeNames' instead"  #-}

-- | The domain name with which your certificate is associated (e.g., @example.com@ ).
--
-- If you specify @certificateDomainName@ , then @certificateName@ is required (and vice-versa).
--
-- /Note:/ Consider using 'certificateDomainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbCertificateDomainName :: Lens.Lens' CreateLoadBalancer (Core.Maybe Types.DomainName)
clbCertificateDomainName = Lens.field @"certificateDomainName"
{-# INLINEABLE clbCertificateDomainName #-}
{-# DEPRECATED certificateDomainName "Use generic-lens or generic-optics with 'certificateDomainName' instead"  #-}

-- | The name of the SSL/TLS certificate.
--
-- If you specify @certificateName@ , then @certificateDomainName@ is required (and vice-versa).
--
-- /Note:/ Consider using 'certificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbCertificateName :: Lens.Lens' CreateLoadBalancer (Core.Maybe Types.ResourceName)
clbCertificateName = Lens.field @"certificateName"
{-# INLINEABLE clbCertificateName #-}
{-# DEPRECATED certificateName "Use generic-lens or generic-optics with 'certificateName' instead"  #-}

-- | The path you provided to perform the load balancer health check. If you didn't specify a health check path, Lightsail uses the root path of your website (e.g., @"/"@ ).
--
-- You may want to specify a custom health check path other than the root of your application if your home page loads slowly or has a lot of media or scripting on it.
--
-- /Note:/ Consider using 'healthCheckPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbHealthCheckPath :: Lens.Lens' CreateLoadBalancer (Core.Maybe Core.Text)
clbHealthCheckPath = Lens.field @"healthCheckPath"
{-# INLINEABLE clbHealthCheckPath #-}
{-# DEPRECATED healthCheckPath "Use generic-lens or generic-optics with 'healthCheckPath' instead"  #-}

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbTags :: Lens.Lens' CreateLoadBalancer (Core.Maybe [Types.Tag])
clbTags = Lens.field @"tags"
{-# INLINEABLE clbTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateLoadBalancer where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateLoadBalancer where
        toHeaders CreateLoadBalancer{..}
          = Core.pure
              ("X-Amz-Target", "Lightsail_20161128.CreateLoadBalancer")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateLoadBalancer where
        toJSON CreateLoadBalancer{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("loadBalancerName" Core..= loadBalancerName),
                  Core.Just ("instancePort" Core..= instancePort),
                  ("certificateAlternativeNames" Core..=) Core.<$>
                    certificateAlternativeNames,
                  ("certificateDomainName" Core..=) Core.<$> certificateDomainName,
                  ("certificateName" Core..=) Core.<$> certificateName,
                  ("healthCheckPath" Core..=) Core.<$> healthCheckPath,
                  ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateLoadBalancer where
        type Rs CreateLoadBalancer = CreateLoadBalancerResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateLoadBalancerResponse' Core.<$>
                   (x Core..:? "operations") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateLoadBalancerResponse' smart constructor.
data CreateLoadBalancerResponse = CreateLoadBalancerResponse'
  { operations :: Core.Maybe [Types.Operation]
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateLoadBalancerResponse' value with any optional fields omitted.
mkCreateLoadBalancerResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateLoadBalancerResponse
mkCreateLoadBalancerResponse responseStatus
  = CreateLoadBalancerResponse'{operations = Core.Nothing,
                                responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbrrsOperations :: Lens.Lens' CreateLoadBalancerResponse (Core.Maybe [Types.Operation])
clbrrsOperations = Lens.field @"operations"
{-# INLINEABLE clbrrsOperations #-}
{-# DEPRECATED operations "Use generic-lens or generic-optics with 'operations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbrrsResponseStatus :: Lens.Lens' CreateLoadBalancerResponse Core.Int
clbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE clbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

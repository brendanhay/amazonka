{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.CreateService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a service, which defines the configuration for the following entities:
--
--
--     * For public and private DNS namespaces, one of the following combinations of DNS records in Amazon Route 53:
--
--     * @A@ 
--
--
--     * @AAAA@ 
--
--
--     * @A@ and @AAAA@ 
--
--
--     * @SRV@ 
--
--
--     * @CNAME@ 
--
--
--
--
--     * Optionally, a health check
--
--
-- After you create the service, you can submit a <https://docs.aws.amazon.com/cloud-map/latest/api/API_RegisterInstance.html RegisterInstance> request, and AWS Cloud Map uses the values in the configuration to create the specified entities.
-- For the current quota on the number of instances that you can register using the same namespace and using the same service, see <https://docs.aws.amazon.com/cloud-map/latest/dg/cloud-map-limits.html AWS Cloud Map Limits> in the /AWS Cloud Map Developer Guide/ .
module Network.AWS.Route53AutoNaming.CreateService
    (
    -- * Creating a request
      CreateService (..)
    , mkCreateService
    -- ** Request lenses
    , csName
    , csCreatorRequestId
    , csDescription
    , csDnsConfig
    , csHealthCheckConfig
    , csHealthCheckCustomConfig
    , csNamespaceId
    , csTags

    -- * Destructuring the response
    , CreateServiceResponse (..)
    , mkCreateServiceResponse
    -- ** Response lenses
    , csrrsService
    , csrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53AutoNaming.Types as Types

-- | /See:/ 'mkCreateService' smart constructor.
data CreateService = CreateService'
  { name :: Types.ServiceName
    -- ^ The name that you want to assign to the service.
--
-- If you want AWS Cloud Map to create an @SRV@ record when you register an instance, and if you're using a system that requires a specific @SRV@ format, such as <http://www.haproxy.org/ HAProxy> , specify the following for @Name@ :
--
--     * Start the name with an underscore (_), such as @_exampleservice@ 
--
--
--     * End the name with /._protocol/ , such as @._tcp@ 
--
--
-- When you register an instance, AWS Cloud Map creates an @SRV@ record and assigns a name to the record by concatenating the service name and the namespace name, for example:
-- @_exampleservice._tcp.example.com@ 
  , creatorRequestId :: Core.Maybe Types.ResourceId
    -- ^ A unique string that identifies the request and that allows failed @CreateService@ requests to be retried without the risk of executing the operation twice. @CreatorRequestId@ can be any unique string, for example, a date/time stamp.
  , description :: Core.Maybe Types.ResourceDescription
    -- ^ A description for the service.
  , dnsConfig :: Core.Maybe Types.DnsConfig
    -- ^ A complex type that contains information about the Amazon Route 53 records that you want AWS Cloud Map to create when you register an instance. 
  , healthCheckConfig :: Core.Maybe Types.HealthCheckConfig
    -- ^ /Public DNS and HTTP namespaces only./ A complex type that contains settings for an optional Route 53 health check. If you specify settings for a health check, AWS Cloud Map associates the health check with all the Route 53 DNS records that you specify in @DnsConfig@ .
--
-- /Important:/ If you specify a health check configuration, you can specify either @HealthCheckCustomConfig@ or @HealthCheckConfig@ but not both.
-- For information about the charges for health checks, see <http://aws.amazon.com/cloud-map/pricing/ AWS Cloud Map Pricing> .
  , healthCheckCustomConfig :: Core.Maybe Types.HealthCheckCustomConfig
    -- ^ A complex type that contains information about an optional custom health check.
--
-- /Important:/ If you specify a health check configuration, you can specify either @HealthCheckCustomConfig@ or @HealthCheckConfig@ but not both.
-- You can't add, update, or delete a @HealthCheckCustomConfig@ configuration from an existing service.
  , namespaceId :: Core.Maybe Types.ResourceId
    -- ^ The ID of the namespace that you want to use to create the service.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags to add to the service. Each tag consists of a key and an optional value, both of which you define. Tag keys can have a maximum character length of 128 characters, and tag values can have a maximum length of 256 characters.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateService' value with any optional fields omitted.
mkCreateService
    :: Types.ServiceName -- ^ 'name'
    -> CreateService
mkCreateService name
  = CreateService'{name, creatorRequestId = Core.Nothing,
                   description = Core.Nothing, dnsConfig = Core.Nothing,
                   healthCheckConfig = Core.Nothing,
                   healthCheckCustomConfig = Core.Nothing, namespaceId = Core.Nothing,
                   tags = Core.Nothing}

-- | The name that you want to assign to the service.
--
-- If you want AWS Cloud Map to create an @SRV@ record when you register an instance, and if you're using a system that requires a specific @SRV@ format, such as <http://www.haproxy.org/ HAProxy> , specify the following for @Name@ :
--
--     * Start the name with an underscore (_), such as @_exampleservice@ 
--
--
--     * End the name with /._protocol/ , such as @._tcp@ 
--
--
-- When you register an instance, AWS Cloud Map creates an @SRV@ record and assigns a name to the record by concatenating the service name and the namespace name, for example:
-- @_exampleservice._tcp.example.com@ 
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csName :: Lens.Lens' CreateService Types.ServiceName
csName = Lens.field @"name"
{-# INLINEABLE csName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A unique string that identifies the request and that allows failed @CreateService@ requests to be retried without the risk of executing the operation twice. @CreatorRequestId@ can be any unique string, for example, a date/time stamp.
--
-- /Note:/ Consider using 'creatorRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCreatorRequestId :: Lens.Lens' CreateService (Core.Maybe Types.ResourceId)
csCreatorRequestId = Lens.field @"creatorRequestId"
{-# INLINEABLE csCreatorRequestId #-}
{-# DEPRECATED creatorRequestId "Use generic-lens or generic-optics with 'creatorRequestId' instead"  #-}

-- | A description for the service.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDescription :: Lens.Lens' CreateService (Core.Maybe Types.ResourceDescription)
csDescription = Lens.field @"description"
{-# INLINEABLE csDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | A complex type that contains information about the Amazon Route 53 records that you want AWS Cloud Map to create when you register an instance. 
--
-- /Note:/ Consider using 'dnsConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDnsConfig :: Lens.Lens' CreateService (Core.Maybe Types.DnsConfig)
csDnsConfig = Lens.field @"dnsConfig"
{-# INLINEABLE csDnsConfig #-}
{-# DEPRECATED dnsConfig "Use generic-lens or generic-optics with 'dnsConfig' instead"  #-}

-- | /Public DNS and HTTP namespaces only./ A complex type that contains settings for an optional Route 53 health check. If you specify settings for a health check, AWS Cloud Map associates the health check with all the Route 53 DNS records that you specify in @DnsConfig@ .
--
-- /Important:/ If you specify a health check configuration, you can specify either @HealthCheckCustomConfig@ or @HealthCheckConfig@ but not both.
-- For information about the charges for health checks, see <http://aws.amazon.com/cloud-map/pricing/ AWS Cloud Map Pricing> .
--
-- /Note:/ Consider using 'healthCheckConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csHealthCheckConfig :: Lens.Lens' CreateService (Core.Maybe Types.HealthCheckConfig)
csHealthCheckConfig = Lens.field @"healthCheckConfig"
{-# INLINEABLE csHealthCheckConfig #-}
{-# DEPRECATED healthCheckConfig "Use generic-lens or generic-optics with 'healthCheckConfig' instead"  #-}

-- | A complex type that contains information about an optional custom health check.
--
-- /Important:/ If you specify a health check configuration, you can specify either @HealthCheckCustomConfig@ or @HealthCheckConfig@ but not both.
-- You can't add, update, or delete a @HealthCheckCustomConfig@ configuration from an existing service.
--
-- /Note:/ Consider using 'healthCheckCustomConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csHealthCheckCustomConfig :: Lens.Lens' CreateService (Core.Maybe Types.HealthCheckCustomConfig)
csHealthCheckCustomConfig = Lens.field @"healthCheckCustomConfig"
{-# INLINEABLE csHealthCheckCustomConfig #-}
{-# DEPRECATED healthCheckCustomConfig "Use generic-lens or generic-optics with 'healthCheckCustomConfig' instead"  #-}

-- | The ID of the namespace that you want to use to create the service.
--
-- /Note:/ Consider using 'namespaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csNamespaceId :: Lens.Lens' CreateService (Core.Maybe Types.ResourceId)
csNamespaceId = Lens.field @"namespaceId"
{-# INLINEABLE csNamespaceId #-}
{-# DEPRECATED namespaceId "Use generic-lens or generic-optics with 'namespaceId' instead"  #-}

-- | The tags to add to the service. Each tag consists of a key and an optional value, both of which you define. Tag keys can have a maximum character length of 128 characters, and tag values can have a maximum length of 256 characters.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTags :: Lens.Lens' CreateService (Core.Maybe [Types.Tag])
csTags = Lens.field @"tags"
{-# INLINEABLE csTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateService where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateService where
        toHeaders CreateService{..}
          = Core.pure
              ("X-Amz-Target", "Route53AutoNaming_v20170314.CreateService")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateService where
        toJSON CreateService{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  ("CreatorRequestId" Core..=) Core.<$> creatorRequestId,
                  ("Description" Core..=) Core.<$> description,
                  ("DnsConfig" Core..=) Core.<$> dnsConfig,
                  ("HealthCheckConfig" Core..=) Core.<$> healthCheckConfig,
                  ("HealthCheckCustomConfig" Core..=) Core.<$>
                    healthCheckCustomConfig,
                  ("NamespaceId" Core..=) Core.<$> namespaceId,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateService where
        type Rs CreateService = CreateServiceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateServiceResponse' Core.<$>
                   (x Core..:? "Service") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateServiceResponse' smart constructor.
data CreateServiceResponse = CreateServiceResponse'
  { service :: Core.Maybe Types.ServiceInfo
    -- ^ A complex type that contains information about the new service.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateServiceResponse' value with any optional fields omitted.
mkCreateServiceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateServiceResponse
mkCreateServiceResponse responseStatus
  = CreateServiceResponse'{service = Core.Nothing, responseStatus}

-- | A complex type that contains information about the new service.
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsService :: Lens.Lens' CreateServiceResponse (Core.Maybe Types.ServiceInfo)
csrrsService = Lens.field @"service"
{-# INLINEABLE csrrsService #-}
{-# DEPRECATED service "Use generic-lens or generic-optics with 'service' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrsResponseStatus :: Lens.Lens' CreateServiceResponse Core.Int
csrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE csrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

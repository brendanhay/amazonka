{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateService (..),
    mkCreateService,

    -- ** Request lenses
    csHealthCheckConfig,
    csCreatorRequestId,
    csHealthCheckCustomConfig,
    csNamespaceId,
    csDNSConfig,
    csDescription,
    csTags,
    csName,

    -- * Destructuring the response
    CreateServiceResponse (..),
    mkCreateServiceResponse,

    -- ** Response lenses
    csrsService,
    csrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'mkCreateService' smart constructor.
data CreateService = CreateService'
  { healthCheckConfig ::
      Lude.Maybe HealthCheckConfig,
    creatorRequestId :: Lude.Maybe Lude.Text,
    healthCheckCustomConfig :: Lude.Maybe HealthCheckCustomConfig,
    namespaceId :: Lude.Maybe Lude.Text,
    dnsConfig :: Lude.Maybe DNSConfig,
    description :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateService' with the minimum fields required to make a request.
--
-- * 'creatorRequestId' - A unique string that identifies the request and that allows failed @CreateService@ requests to be retried without the risk of executing the operation twice. @CreatorRequestId@ can be any unique string, for example, a date/time stamp.
-- * 'description' - A description for the service.
-- * 'dnsConfig' - A complex type that contains information about the Amazon Route 53 records that you want AWS Cloud Map to create when you register an instance.
-- * 'healthCheckConfig' - /Public DNS and HTTP namespaces only./ A complex type that contains settings for an optional Route 53 health check. If you specify settings for a health check, AWS Cloud Map associates the health check with all the Route 53 DNS records that you specify in @DnsConfig@ .
--
-- /Important:/ If you specify a health check configuration, you can specify either @HealthCheckCustomConfig@ or @HealthCheckConfig@ but not both.
-- For information about the charges for health checks, see <http://aws.amazon.com/cloud-map/pricing/ AWS Cloud Map Pricing> .
-- * 'healthCheckCustomConfig' - A complex type that contains information about an optional custom health check.
--
-- /Important:/ If you specify a health check configuration, you can specify either @HealthCheckCustomConfig@ or @HealthCheckConfig@ but not both.
-- You can't add, update, or delete a @HealthCheckCustomConfig@ configuration from an existing service.
-- * 'name' - The name that you want to assign to the service.
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
-- * 'namespaceId' - The ID of the namespace that you want to use to create the service.
-- * 'tags' - The tags to add to the service. Each tag consists of a key and an optional value, both of which you define. Tag keys can have a maximum character length of 128 characters, and tag values can have a maximum length of 256 characters.
mkCreateService ::
  -- | 'name'
  Lude.Text ->
  CreateService
mkCreateService pName_ =
  CreateService'
    { healthCheckConfig = Lude.Nothing,
      creatorRequestId = Lude.Nothing,
      healthCheckCustomConfig = Lude.Nothing,
      namespaceId = Lude.Nothing,
      dnsConfig = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing,
      name = pName_
    }

-- | /Public DNS and HTTP namespaces only./ A complex type that contains settings for an optional Route 53 health check. If you specify settings for a health check, AWS Cloud Map associates the health check with all the Route 53 DNS records that you specify in @DnsConfig@ .
--
-- /Important:/ If you specify a health check configuration, you can specify either @HealthCheckCustomConfig@ or @HealthCheckConfig@ but not both.
-- For information about the charges for health checks, see <http://aws.amazon.com/cloud-map/pricing/ AWS Cloud Map Pricing> .
--
-- /Note:/ Consider using 'healthCheckConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csHealthCheckConfig :: Lens.Lens' CreateService (Lude.Maybe HealthCheckConfig)
csHealthCheckConfig = Lens.lens (healthCheckConfig :: CreateService -> Lude.Maybe HealthCheckConfig) (\s a -> s {healthCheckConfig = a} :: CreateService)
{-# DEPRECATED csHealthCheckConfig "Use generic-lens or generic-optics with 'healthCheckConfig' instead." #-}

-- | A unique string that identifies the request and that allows failed @CreateService@ requests to be retried without the risk of executing the operation twice. @CreatorRequestId@ can be any unique string, for example, a date/time stamp.
--
-- /Note:/ Consider using 'creatorRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csCreatorRequestId :: Lens.Lens' CreateService (Lude.Maybe Lude.Text)
csCreatorRequestId = Lens.lens (creatorRequestId :: CreateService -> Lude.Maybe Lude.Text) (\s a -> s {creatorRequestId = a} :: CreateService)
{-# DEPRECATED csCreatorRequestId "Use generic-lens or generic-optics with 'creatorRequestId' instead." #-}

-- | A complex type that contains information about an optional custom health check.
--
-- /Important:/ If you specify a health check configuration, you can specify either @HealthCheckCustomConfig@ or @HealthCheckConfig@ but not both.
-- You can't add, update, or delete a @HealthCheckCustomConfig@ configuration from an existing service.
--
-- /Note:/ Consider using 'healthCheckCustomConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csHealthCheckCustomConfig :: Lens.Lens' CreateService (Lude.Maybe HealthCheckCustomConfig)
csHealthCheckCustomConfig = Lens.lens (healthCheckCustomConfig :: CreateService -> Lude.Maybe HealthCheckCustomConfig) (\s a -> s {healthCheckCustomConfig = a} :: CreateService)
{-# DEPRECATED csHealthCheckCustomConfig "Use generic-lens or generic-optics with 'healthCheckCustomConfig' instead." #-}

-- | The ID of the namespace that you want to use to create the service.
--
-- /Note:/ Consider using 'namespaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csNamespaceId :: Lens.Lens' CreateService (Lude.Maybe Lude.Text)
csNamespaceId = Lens.lens (namespaceId :: CreateService -> Lude.Maybe Lude.Text) (\s a -> s {namespaceId = a} :: CreateService)
{-# DEPRECATED csNamespaceId "Use generic-lens or generic-optics with 'namespaceId' instead." #-}

-- | A complex type that contains information about the Amazon Route 53 records that you want AWS Cloud Map to create when you register an instance.
--
-- /Note:/ Consider using 'dnsConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDNSConfig :: Lens.Lens' CreateService (Lude.Maybe DNSConfig)
csDNSConfig = Lens.lens (dnsConfig :: CreateService -> Lude.Maybe DNSConfig) (\s a -> s {dnsConfig = a} :: CreateService)
{-# DEPRECATED csDNSConfig "Use generic-lens or generic-optics with 'dnsConfig' instead." #-}

-- | A description for the service.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDescription :: Lens.Lens' CreateService (Lude.Maybe Lude.Text)
csDescription = Lens.lens (description :: CreateService -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateService)
{-# DEPRECATED csDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The tags to add to the service. Each tag consists of a key and an optional value, both of which you define. Tag keys can have a maximum character length of 128 characters, and tag values can have a maximum length of 256 characters.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTags :: Lens.Lens' CreateService (Lude.Maybe [Tag])
csTags = Lens.lens (tags :: CreateService -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateService)
{-# DEPRECATED csTags "Use generic-lens or generic-optics with 'tags' instead." #-}

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
csName :: Lens.Lens' CreateService Lude.Text
csName = Lens.lens (name :: CreateService -> Lude.Text) (\s a -> s {name = a} :: CreateService)
{-# DEPRECATED csName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest CreateService where
  type Rs CreateService = CreateServiceResponse
  request = Req.postJSON route53AutoNamingService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateServiceResponse'
            Lude.<$> (x Lude..?> "Service") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateService where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Route53AutoNaming_v20170314.CreateService" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateService where
  toJSON CreateService' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("HealthCheckConfig" Lude..=) Lude.<$> healthCheckConfig,
            ("CreatorRequestId" Lude..=) Lude.<$> creatorRequestId,
            ("HealthCheckCustomConfig" Lude..=)
              Lude.<$> healthCheckCustomConfig,
            ("NamespaceId" Lude..=) Lude.<$> namespaceId,
            ("DnsConfig" Lude..=) Lude.<$> dnsConfig,
            ("Description" Lude..=) Lude.<$> description,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("Name" Lude..= name)
          ]
      )

instance Lude.ToPath CreateService where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateService where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateServiceResponse' smart constructor.
data CreateServiceResponse = CreateServiceResponse'
  { service ::
      Lude.Maybe ServiceInfo,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateServiceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'service' - A complex type that contains information about the new service.
mkCreateServiceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateServiceResponse
mkCreateServiceResponse pResponseStatus_ =
  CreateServiceResponse'
    { service = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A complex type that contains information about the new service.
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsService :: Lens.Lens' CreateServiceResponse (Lude.Maybe ServiceInfo)
csrsService = Lens.lens (service :: CreateServiceResponse -> Lude.Maybe ServiceInfo) (\s a -> s {service = a} :: CreateServiceResponse)
{-# DEPRECATED csrsService "Use generic-lens or generic-optics with 'service' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsResponseStatus :: Lens.Lens' CreateServiceResponse Lude.Int
csrsResponseStatus = Lens.lens (responseStatus :: CreateServiceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateServiceResponse)
{-# DEPRECATED csrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

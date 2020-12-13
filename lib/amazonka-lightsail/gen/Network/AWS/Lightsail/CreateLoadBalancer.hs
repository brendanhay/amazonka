{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateLoadBalancer (..),
    mkCreateLoadBalancer,

    -- ** Request lenses
    clbHealthCheckPath,
    clbCertificateName,
    clbCertificateDomainName,
    clbLoadBalancerName,
    clbInstancePort,
    clbCertificateAlternativeNames,
    clbTags,

    -- * Destructuring the response
    CreateLoadBalancerResponse (..),
    mkCreateLoadBalancerResponse,

    -- ** Response lenses
    clbrsOperations,
    clbrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateLoadBalancer' smart constructor.
data CreateLoadBalancer = CreateLoadBalancer'
  { -- | The path you provided to perform the load balancer health check. If you didn't specify a health check path, Lightsail uses the root path of your website (e.g., @"/"@ ).
    --
    -- You may want to specify a custom health check path other than the root of your application if your home page loads slowly or has a lot of media or scripting on it.
    healthCheckPath :: Lude.Maybe Lude.Text,
    -- | The name of the SSL/TLS certificate.
    --
    -- If you specify @certificateName@ , then @certificateDomainName@ is required (and vice-versa).
    certificateName :: Lude.Maybe Lude.Text,
    -- | The domain name with which your certificate is associated (e.g., @example.com@ ).
    --
    -- If you specify @certificateDomainName@ , then @certificateName@ is required (and vice-versa).
    certificateDomainName :: Lude.Maybe Lude.Text,
    -- | The name of your load balancer.
    loadBalancerName :: Lude.Text,
    -- | The instance port where you're creating your load balancer.
    instancePort :: Lude.Int,
    -- | The optional alternative domains and subdomains to use with your SSL/TLS certificate (e.g., @www.example.com@ , @example.com@ , @m.example.com@ , @blog.example.com@ ).
    certificateAlternativeNames :: Lude.Maybe [Lude.Text],
    -- | The tag keys and optional values to add to the resource during create.
    --
    -- Use the @TagResource@ action to tag a resource after it's created.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateLoadBalancer' with the minimum fields required to make a request.
--
-- * 'healthCheckPath' - The path you provided to perform the load balancer health check. If you didn't specify a health check path, Lightsail uses the root path of your website (e.g., @"/"@ ).
--
-- You may want to specify a custom health check path other than the root of your application if your home page loads slowly or has a lot of media or scripting on it.
-- * 'certificateName' - The name of the SSL/TLS certificate.
--
-- If you specify @certificateName@ , then @certificateDomainName@ is required (and vice-versa).
-- * 'certificateDomainName' - The domain name with which your certificate is associated (e.g., @example.com@ ).
--
-- If you specify @certificateDomainName@ , then @certificateName@ is required (and vice-versa).
-- * 'loadBalancerName' - The name of your load balancer.
-- * 'instancePort' - The instance port where you're creating your load balancer.
-- * 'certificateAlternativeNames' - The optional alternative domains and subdomains to use with your SSL/TLS certificate (e.g., @www.example.com@ , @example.com@ , @m.example.com@ , @blog.example.com@ ).
-- * 'tags' - The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
mkCreateLoadBalancer ::
  -- | 'loadBalancerName'
  Lude.Text ->
  -- | 'instancePort'
  Lude.Int ->
  CreateLoadBalancer
mkCreateLoadBalancer pLoadBalancerName_ pInstancePort_ =
  CreateLoadBalancer'
    { healthCheckPath = Lude.Nothing,
      certificateName = Lude.Nothing,
      certificateDomainName = Lude.Nothing,
      loadBalancerName = pLoadBalancerName_,
      instancePort = pInstancePort_,
      certificateAlternativeNames = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The path you provided to perform the load balancer health check. If you didn't specify a health check path, Lightsail uses the root path of your website (e.g., @"/"@ ).
--
-- You may want to specify a custom health check path other than the root of your application if your home page loads slowly or has a lot of media or scripting on it.
--
-- /Note:/ Consider using 'healthCheckPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbHealthCheckPath :: Lens.Lens' CreateLoadBalancer (Lude.Maybe Lude.Text)
clbHealthCheckPath = Lens.lens (healthCheckPath :: CreateLoadBalancer -> Lude.Maybe Lude.Text) (\s a -> s {healthCheckPath = a} :: CreateLoadBalancer)
{-# DEPRECATED clbHealthCheckPath "Use generic-lens or generic-optics with 'healthCheckPath' instead." #-}

-- | The name of the SSL/TLS certificate.
--
-- If you specify @certificateName@ , then @certificateDomainName@ is required (and vice-versa).
--
-- /Note:/ Consider using 'certificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbCertificateName :: Lens.Lens' CreateLoadBalancer (Lude.Maybe Lude.Text)
clbCertificateName = Lens.lens (certificateName :: CreateLoadBalancer -> Lude.Maybe Lude.Text) (\s a -> s {certificateName = a} :: CreateLoadBalancer)
{-# DEPRECATED clbCertificateName "Use generic-lens or generic-optics with 'certificateName' instead." #-}

-- | The domain name with which your certificate is associated (e.g., @example.com@ ).
--
-- If you specify @certificateDomainName@ , then @certificateName@ is required (and vice-versa).
--
-- /Note:/ Consider using 'certificateDomainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbCertificateDomainName :: Lens.Lens' CreateLoadBalancer (Lude.Maybe Lude.Text)
clbCertificateDomainName = Lens.lens (certificateDomainName :: CreateLoadBalancer -> Lude.Maybe Lude.Text) (\s a -> s {certificateDomainName = a} :: CreateLoadBalancer)
{-# DEPRECATED clbCertificateDomainName "Use generic-lens or generic-optics with 'certificateDomainName' instead." #-}

-- | The name of your load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbLoadBalancerName :: Lens.Lens' CreateLoadBalancer Lude.Text
clbLoadBalancerName = Lens.lens (loadBalancerName :: CreateLoadBalancer -> Lude.Text) (\s a -> s {loadBalancerName = a} :: CreateLoadBalancer)
{-# DEPRECATED clbLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | The instance port where you're creating your load balancer.
--
-- /Note:/ Consider using 'instancePort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbInstancePort :: Lens.Lens' CreateLoadBalancer Lude.Int
clbInstancePort = Lens.lens (instancePort :: CreateLoadBalancer -> Lude.Int) (\s a -> s {instancePort = a} :: CreateLoadBalancer)
{-# DEPRECATED clbInstancePort "Use generic-lens or generic-optics with 'instancePort' instead." #-}

-- | The optional alternative domains and subdomains to use with your SSL/TLS certificate (e.g., @www.example.com@ , @example.com@ , @m.example.com@ , @blog.example.com@ ).
--
-- /Note:/ Consider using 'certificateAlternativeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbCertificateAlternativeNames :: Lens.Lens' CreateLoadBalancer (Lude.Maybe [Lude.Text])
clbCertificateAlternativeNames = Lens.lens (certificateAlternativeNames :: CreateLoadBalancer -> Lude.Maybe [Lude.Text]) (\s a -> s {certificateAlternativeNames = a} :: CreateLoadBalancer)
{-# DEPRECATED clbCertificateAlternativeNames "Use generic-lens or generic-optics with 'certificateAlternativeNames' instead." #-}

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbTags :: Lens.Lens' CreateLoadBalancer (Lude.Maybe [Tag])
clbTags = Lens.lens (tags :: CreateLoadBalancer -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateLoadBalancer)
{-# DEPRECATED clbTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateLoadBalancer where
  type Rs CreateLoadBalancer = CreateLoadBalancerResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateLoadBalancerResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateLoadBalancer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.CreateLoadBalancer" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateLoadBalancer where
  toJSON CreateLoadBalancer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("healthCheckPath" Lude..=) Lude.<$> healthCheckPath,
            ("certificateName" Lude..=) Lude.<$> certificateName,
            ("certificateDomainName" Lude..=) Lude.<$> certificateDomainName,
            Lude.Just ("loadBalancerName" Lude..= loadBalancerName),
            Lude.Just ("instancePort" Lude..= instancePort),
            ("certificateAlternativeNames" Lude..=)
              Lude.<$> certificateAlternativeNames,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateLoadBalancer where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateLoadBalancer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateLoadBalancerResponse' smart constructor.
data CreateLoadBalancerResponse = CreateLoadBalancerResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Lude.Maybe [Operation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateLoadBalancerResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkCreateLoadBalancerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateLoadBalancerResponse
mkCreateLoadBalancerResponse pResponseStatus_ =
  CreateLoadBalancerResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbrsOperations :: Lens.Lens' CreateLoadBalancerResponse (Lude.Maybe [Operation])
clbrsOperations = Lens.lens (operations :: CreateLoadBalancerResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: CreateLoadBalancerResponse)
{-# DEPRECATED clbrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbrsResponseStatus :: Lens.Lens' CreateLoadBalancerResponse Lude.Int
clbrsResponseStatus = Lens.lens (responseStatus :: CreateLoadBalancerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateLoadBalancerResponse)
{-# DEPRECATED clbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

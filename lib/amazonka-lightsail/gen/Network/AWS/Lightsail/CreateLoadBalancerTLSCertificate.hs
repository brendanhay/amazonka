{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateLoadBalancerTLSCertificate
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
module Network.AWS.Lightsail.CreateLoadBalancerTLSCertificate
  ( -- * Creating a request
    CreateLoadBalancerTLSCertificate (..),
    mkCreateLoadBalancerTLSCertificate,

    -- ** Request lenses
    clbtcCertificateName,
    clbtcCertificateDomainName,
    clbtcLoadBalancerName,
    clbtcCertificateAlternativeNames,
    clbtcTags,

    -- * Destructuring the response
    CreateLoadBalancerTLSCertificateResponse (..),
    mkCreateLoadBalancerTLSCertificateResponse,

    -- ** Response lenses
    clbtcrsOperations,
    clbtcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateLoadBalancerTLSCertificate' smart constructor.
data CreateLoadBalancerTLSCertificate = CreateLoadBalancerTLSCertificate'
  { -- | The SSL/TLS certificate name.
    --
    -- You can have up to 10 certificates in your account at one time. Each Lightsail load balancer can have up to 2 certificates associated with it at one time. There is also an overall limit to the number of certificates that can be issue in a 365-day period. For more information, see <http://docs.aws.amazon.com/acm/latest/userguide/acm-limits.html Limits> .
    certificateName :: Lude.Text,
    -- | The domain name (e.g., @example.com@ ) for your SSL/TLS certificate.
    certificateDomainName :: Lude.Text,
    -- | The load balancer name where you want to create the SSL/TLS certificate.
    loadBalancerName :: Lude.Text,
    -- | An array of strings listing alternative domains and subdomains for your SSL/TLS certificate. Lightsail will de-dupe the names for you. You can have a maximum of 9 alternative names (in addition to the 1 primary domain). We do not support wildcards (e.g., @*.example.com@ ).
    certificateAlternativeNames :: Lude.Maybe [Lude.Text],
    -- | The tag keys and optional values to add to the resource during create.
    --
    -- Use the @TagResource@ action to tag a resource after it's created.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateLoadBalancerTLSCertificate' with the minimum fields required to make a request.
--
-- * 'certificateName' - The SSL/TLS certificate name.
--
-- You can have up to 10 certificates in your account at one time. Each Lightsail load balancer can have up to 2 certificates associated with it at one time. There is also an overall limit to the number of certificates that can be issue in a 365-day period. For more information, see <http://docs.aws.amazon.com/acm/latest/userguide/acm-limits.html Limits> .
-- * 'certificateDomainName' - The domain name (e.g., @example.com@ ) for your SSL/TLS certificate.
-- * 'loadBalancerName' - The load balancer name where you want to create the SSL/TLS certificate.
-- * 'certificateAlternativeNames' - An array of strings listing alternative domains and subdomains for your SSL/TLS certificate. Lightsail will de-dupe the names for you. You can have a maximum of 9 alternative names (in addition to the 1 primary domain). We do not support wildcards (e.g., @*.example.com@ ).
-- * 'tags' - The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
mkCreateLoadBalancerTLSCertificate ::
  -- | 'certificateName'
  Lude.Text ->
  -- | 'certificateDomainName'
  Lude.Text ->
  -- | 'loadBalancerName'
  Lude.Text ->
  CreateLoadBalancerTLSCertificate
mkCreateLoadBalancerTLSCertificate
  pCertificateName_
  pCertificateDomainName_
  pLoadBalancerName_ =
    CreateLoadBalancerTLSCertificate'
      { certificateName =
          pCertificateName_,
        certificateDomainName = pCertificateDomainName_,
        loadBalancerName = pLoadBalancerName_,
        certificateAlternativeNames = Lude.Nothing,
        tags = Lude.Nothing
      }

-- | The SSL/TLS certificate name.
--
-- You can have up to 10 certificates in your account at one time. Each Lightsail load balancer can have up to 2 certificates associated with it at one time. There is also an overall limit to the number of certificates that can be issue in a 365-day period. For more information, see <http://docs.aws.amazon.com/acm/latest/userguide/acm-limits.html Limits> .
--
-- /Note:/ Consider using 'certificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbtcCertificateName :: Lens.Lens' CreateLoadBalancerTLSCertificate Lude.Text
clbtcCertificateName = Lens.lens (certificateName :: CreateLoadBalancerTLSCertificate -> Lude.Text) (\s a -> s {certificateName = a} :: CreateLoadBalancerTLSCertificate)
{-# DEPRECATED clbtcCertificateName "Use generic-lens or generic-optics with 'certificateName' instead." #-}

-- | The domain name (e.g., @example.com@ ) for your SSL/TLS certificate.
--
-- /Note:/ Consider using 'certificateDomainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbtcCertificateDomainName :: Lens.Lens' CreateLoadBalancerTLSCertificate Lude.Text
clbtcCertificateDomainName = Lens.lens (certificateDomainName :: CreateLoadBalancerTLSCertificate -> Lude.Text) (\s a -> s {certificateDomainName = a} :: CreateLoadBalancerTLSCertificate)
{-# DEPRECATED clbtcCertificateDomainName "Use generic-lens or generic-optics with 'certificateDomainName' instead." #-}

-- | The load balancer name where you want to create the SSL/TLS certificate.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbtcLoadBalancerName :: Lens.Lens' CreateLoadBalancerTLSCertificate Lude.Text
clbtcLoadBalancerName = Lens.lens (loadBalancerName :: CreateLoadBalancerTLSCertificate -> Lude.Text) (\s a -> s {loadBalancerName = a} :: CreateLoadBalancerTLSCertificate)
{-# DEPRECATED clbtcLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | An array of strings listing alternative domains and subdomains for your SSL/TLS certificate. Lightsail will de-dupe the names for you. You can have a maximum of 9 alternative names (in addition to the 1 primary domain). We do not support wildcards (e.g., @*.example.com@ ).
--
-- /Note:/ Consider using 'certificateAlternativeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbtcCertificateAlternativeNames :: Lens.Lens' CreateLoadBalancerTLSCertificate (Lude.Maybe [Lude.Text])
clbtcCertificateAlternativeNames = Lens.lens (certificateAlternativeNames :: CreateLoadBalancerTLSCertificate -> Lude.Maybe [Lude.Text]) (\s a -> s {certificateAlternativeNames = a} :: CreateLoadBalancerTLSCertificate)
{-# DEPRECATED clbtcCertificateAlternativeNames "Use generic-lens or generic-optics with 'certificateAlternativeNames' instead." #-}

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbtcTags :: Lens.Lens' CreateLoadBalancerTLSCertificate (Lude.Maybe [Tag])
clbtcTags = Lens.lens (tags :: CreateLoadBalancerTLSCertificate -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateLoadBalancerTLSCertificate)
{-# DEPRECATED clbtcTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateLoadBalancerTLSCertificate where
  type
    Rs CreateLoadBalancerTLSCertificate =
      CreateLoadBalancerTLSCertificateResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateLoadBalancerTLSCertificateResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateLoadBalancerTLSCertificate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Lightsail_20161128.CreateLoadBalancerTlsCertificate" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateLoadBalancerTLSCertificate where
  toJSON CreateLoadBalancerTLSCertificate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("certificateName" Lude..= certificateName),
            Lude.Just ("certificateDomainName" Lude..= certificateDomainName),
            Lude.Just ("loadBalancerName" Lude..= loadBalancerName),
            ("certificateAlternativeNames" Lude..=)
              Lude.<$> certificateAlternativeNames,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateLoadBalancerTLSCertificate where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateLoadBalancerTLSCertificate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateLoadBalancerTLSCertificateResponse' smart constructor.
data CreateLoadBalancerTLSCertificateResponse = CreateLoadBalancerTLSCertificateResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Lude.Maybe [Operation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateLoadBalancerTLSCertificateResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkCreateLoadBalancerTLSCertificateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateLoadBalancerTLSCertificateResponse
mkCreateLoadBalancerTLSCertificateResponse pResponseStatus_ =
  CreateLoadBalancerTLSCertificateResponse'
    { operations =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbtcrsOperations :: Lens.Lens' CreateLoadBalancerTLSCertificateResponse (Lude.Maybe [Operation])
clbtcrsOperations = Lens.lens (operations :: CreateLoadBalancerTLSCertificateResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: CreateLoadBalancerTLSCertificateResponse)
{-# DEPRECATED clbtcrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbtcrsResponseStatus :: Lens.Lens' CreateLoadBalancerTLSCertificateResponse Lude.Int
clbtcrsResponseStatus = Lens.lens (responseStatus :: CreateLoadBalancerTLSCertificateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateLoadBalancerTLSCertificateResponse)
{-# DEPRECATED clbtcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

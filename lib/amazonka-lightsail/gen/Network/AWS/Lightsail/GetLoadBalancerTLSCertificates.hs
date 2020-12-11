{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetLoadBalancerTLSCertificates
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
module Network.AWS.Lightsail.GetLoadBalancerTLSCertificates
  ( -- * Creating a request
    GetLoadBalancerTLSCertificates (..),
    mkGetLoadBalancerTLSCertificates,

    -- ** Request lenses
    glbtcLoadBalancerName,

    -- * Destructuring the response
    GetLoadBalancerTLSCertificatesResponse (..),
    mkGetLoadBalancerTLSCertificatesResponse,

    -- ** Response lenses
    glbtcrsTlsCertificates,
    glbtcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetLoadBalancerTLSCertificates' smart constructor.
newtype GetLoadBalancerTLSCertificates = GetLoadBalancerTLSCertificates'
  { loadBalancerName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetLoadBalancerTLSCertificates' with the minimum fields required to make a request.
--
-- * 'loadBalancerName' - The name of the load balancer you associated with your SSL/TLS certificate.
mkGetLoadBalancerTLSCertificates ::
  -- | 'loadBalancerName'
  Lude.Text ->
  GetLoadBalancerTLSCertificates
mkGetLoadBalancerTLSCertificates pLoadBalancerName_ =
  GetLoadBalancerTLSCertificates'
    { loadBalancerName =
        pLoadBalancerName_
    }

-- | The name of the load balancer you associated with your SSL/TLS certificate.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glbtcLoadBalancerName :: Lens.Lens' GetLoadBalancerTLSCertificates Lude.Text
glbtcLoadBalancerName = Lens.lens (loadBalancerName :: GetLoadBalancerTLSCertificates -> Lude.Text) (\s a -> s {loadBalancerName = a} :: GetLoadBalancerTLSCertificates)
{-# DEPRECATED glbtcLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

instance Lude.AWSRequest GetLoadBalancerTLSCertificates where
  type
    Rs GetLoadBalancerTLSCertificates =
      GetLoadBalancerTLSCertificatesResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetLoadBalancerTLSCertificatesResponse'
            Lude.<$> (x Lude..?> "tlsCertificates" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetLoadBalancerTLSCertificates where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Lightsail_20161128.GetLoadBalancerTlsCertificates" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetLoadBalancerTLSCertificates where
  toJSON GetLoadBalancerTLSCertificates' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("loadBalancerName" Lude..= loadBalancerName)]
      )

instance Lude.ToPath GetLoadBalancerTLSCertificates where
  toPath = Lude.const "/"

instance Lude.ToQuery GetLoadBalancerTLSCertificates where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetLoadBalancerTLSCertificatesResponse' smart constructor.
data GetLoadBalancerTLSCertificatesResponse = GetLoadBalancerTLSCertificatesResponse'
  { tlsCertificates ::
      Lude.Maybe
        [LoadBalancerTLSCertificate],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetLoadBalancerTLSCertificatesResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'tlsCertificates' - An array of LoadBalancerTlsCertificate objects describing your SSL/TLS certificates.
mkGetLoadBalancerTLSCertificatesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetLoadBalancerTLSCertificatesResponse
mkGetLoadBalancerTLSCertificatesResponse pResponseStatus_ =
  GetLoadBalancerTLSCertificatesResponse'
    { tlsCertificates =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of LoadBalancerTlsCertificate objects describing your SSL/TLS certificates.
--
-- /Note:/ Consider using 'tlsCertificates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glbtcrsTlsCertificates :: Lens.Lens' GetLoadBalancerTLSCertificatesResponse (Lude.Maybe [LoadBalancerTLSCertificate])
glbtcrsTlsCertificates = Lens.lens (tlsCertificates :: GetLoadBalancerTLSCertificatesResponse -> Lude.Maybe [LoadBalancerTLSCertificate]) (\s a -> s {tlsCertificates = a} :: GetLoadBalancerTLSCertificatesResponse)
{-# DEPRECATED glbtcrsTlsCertificates "Use generic-lens or generic-optics with 'tlsCertificates' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glbtcrsResponseStatus :: Lens.Lens' GetLoadBalancerTLSCertificatesResponse Lude.Int
glbtcrsResponseStatus = Lens.lens (responseStatus :: GetLoadBalancerTLSCertificatesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetLoadBalancerTLSCertificatesResponse)
{-# DEPRECATED glbtcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

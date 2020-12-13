{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.SetLoadBalancerListenerSSLCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the certificate that terminates the specified listener's SSL connections. The specified certificate replaces any prior certificate that was used on the same load balancer and port.
--
-- For more information about updating your SSL certificate, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-update-ssl-cert.html Replace the SSL Certificate for Your Load Balancer> in the /Classic Load Balancers Guide/ .
module Network.AWS.ELB.SetLoadBalancerListenerSSLCertificate
  ( -- * Creating a request
    SetLoadBalancerListenerSSLCertificate (..),
    mkSetLoadBalancerListenerSSLCertificate,

    -- ** Request lenses
    slblscLoadBalancerName,
    slblscLoadBalancerPort,
    slblscSSLCertificateId,

    -- * Destructuring the response
    SetLoadBalancerListenerSSLCertificateResponse (..),
    mkSetLoadBalancerListenerSSLCertificateResponse,

    -- ** Response lenses
    slblscrsResponseStatus,
  )
where

import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for SetLoadBalancerListenerSSLCertificate.
--
-- /See:/ 'mkSetLoadBalancerListenerSSLCertificate' smart constructor.
data SetLoadBalancerListenerSSLCertificate = SetLoadBalancerListenerSSLCertificate'
  { -- | The name of the load balancer.
    loadBalancerName :: Lude.Text,
    -- | The port that uses the specified SSL certificate.
    loadBalancerPort :: Lude.Int,
    -- | The Amazon Resource Name (ARN) of the SSL certificate.
    sslCertificateId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetLoadBalancerListenerSSLCertificate' with the minimum fields required to make a request.
--
-- * 'loadBalancerName' - The name of the load balancer.
-- * 'loadBalancerPort' - The port that uses the specified SSL certificate.
-- * 'sslCertificateId' - The Amazon Resource Name (ARN) of the SSL certificate.
mkSetLoadBalancerListenerSSLCertificate ::
  -- | 'loadBalancerName'
  Lude.Text ->
  -- | 'loadBalancerPort'
  Lude.Int ->
  -- | 'sslCertificateId'
  Lude.Text ->
  SetLoadBalancerListenerSSLCertificate
mkSetLoadBalancerListenerSSLCertificate
  pLoadBalancerName_
  pLoadBalancerPort_
  pSSLCertificateId_ =
    SetLoadBalancerListenerSSLCertificate'
      { loadBalancerName =
          pLoadBalancerName_,
        loadBalancerPort = pLoadBalancerPort_,
        sslCertificateId = pSSLCertificateId_
      }

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slblscLoadBalancerName :: Lens.Lens' SetLoadBalancerListenerSSLCertificate Lude.Text
slblscLoadBalancerName = Lens.lens (loadBalancerName :: SetLoadBalancerListenerSSLCertificate -> Lude.Text) (\s a -> s {loadBalancerName = a} :: SetLoadBalancerListenerSSLCertificate)
{-# DEPRECATED slblscLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | The port that uses the specified SSL certificate.
--
-- /Note:/ Consider using 'loadBalancerPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slblscLoadBalancerPort :: Lens.Lens' SetLoadBalancerListenerSSLCertificate Lude.Int
slblscLoadBalancerPort = Lens.lens (loadBalancerPort :: SetLoadBalancerListenerSSLCertificate -> Lude.Int) (\s a -> s {loadBalancerPort = a} :: SetLoadBalancerListenerSSLCertificate)
{-# DEPRECATED slblscLoadBalancerPort "Use generic-lens or generic-optics with 'loadBalancerPort' instead." #-}

-- | The Amazon Resource Name (ARN) of the SSL certificate.
--
-- /Note:/ Consider using 'sslCertificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slblscSSLCertificateId :: Lens.Lens' SetLoadBalancerListenerSSLCertificate Lude.Text
slblscSSLCertificateId = Lens.lens (sslCertificateId :: SetLoadBalancerListenerSSLCertificate -> Lude.Text) (\s a -> s {sslCertificateId = a} :: SetLoadBalancerListenerSSLCertificate)
{-# DEPRECATED slblscSSLCertificateId "Use generic-lens or generic-optics with 'sslCertificateId' instead." #-}

instance Lude.AWSRequest SetLoadBalancerListenerSSLCertificate where
  type
    Rs SetLoadBalancerListenerSSLCertificate =
      SetLoadBalancerListenerSSLCertificateResponse
  request = Req.postQuery elbService
  response =
    Res.receiveXMLWrapper
      "SetLoadBalancerListenerSSLCertificateResult"
      ( \s h x ->
          SetLoadBalancerListenerSSLCertificateResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SetLoadBalancerListenerSSLCertificate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SetLoadBalancerListenerSSLCertificate where
  toPath = Lude.const "/"

instance Lude.ToQuery SetLoadBalancerListenerSSLCertificate where
  toQuery SetLoadBalancerListenerSSLCertificate' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("SetLoadBalancerListenerSSLCertificate" :: Lude.ByteString),
        "Version" Lude.=: ("2012-06-01" :: Lude.ByteString),
        "LoadBalancerName" Lude.=: loadBalancerName,
        "LoadBalancerPort" Lude.=: loadBalancerPort,
        "SSLCertificateId" Lude.=: sslCertificateId
      ]

-- | Contains the output of SetLoadBalancerListenerSSLCertificate.
--
-- /See:/ 'mkSetLoadBalancerListenerSSLCertificateResponse' smart constructor.
newtype SetLoadBalancerListenerSSLCertificateResponse = SetLoadBalancerListenerSSLCertificateResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetLoadBalancerListenerSSLCertificateResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkSetLoadBalancerListenerSSLCertificateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SetLoadBalancerListenerSSLCertificateResponse
mkSetLoadBalancerListenerSSLCertificateResponse pResponseStatus_ =
  SetLoadBalancerListenerSSLCertificateResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slblscrsResponseStatus :: Lens.Lens' SetLoadBalancerListenerSSLCertificateResponse Lude.Int
slblscrsResponseStatus = Lens.lens (responseStatus :: SetLoadBalancerListenerSSLCertificateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SetLoadBalancerListenerSSLCertificateResponse)
{-# DEPRECATED slblscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

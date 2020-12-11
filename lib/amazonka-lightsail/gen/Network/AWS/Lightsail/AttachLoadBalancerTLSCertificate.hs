{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.AttachLoadBalancerTLSCertificate
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
module Network.AWS.Lightsail.AttachLoadBalancerTLSCertificate
  ( -- * Creating a request
    AttachLoadBalancerTLSCertificate (..),
    mkAttachLoadBalancerTLSCertificate,

    -- ** Request lenses
    albtcLoadBalancerName,
    albtcCertificateName,

    -- * Destructuring the response
    AttachLoadBalancerTLSCertificateResponse (..),
    mkAttachLoadBalancerTLSCertificateResponse,

    -- ** Response lenses
    albtcrsOperations,
    albtcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAttachLoadBalancerTLSCertificate' smart constructor.
data AttachLoadBalancerTLSCertificate = AttachLoadBalancerTLSCertificate'
  { loadBalancerName ::
      Lude.Text,
    certificateName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachLoadBalancerTLSCertificate' with the minimum fields required to make a request.
--
-- * 'certificateName' - The name of your SSL/TLS certificate.
-- * 'loadBalancerName' - The name of the load balancer to which you want to associate the SSL/TLS certificate.
mkAttachLoadBalancerTLSCertificate ::
  -- | 'loadBalancerName'
  Lude.Text ->
  -- | 'certificateName'
  Lude.Text ->
  AttachLoadBalancerTLSCertificate
mkAttachLoadBalancerTLSCertificate
  pLoadBalancerName_
  pCertificateName_ =
    AttachLoadBalancerTLSCertificate'
      { loadBalancerName =
          pLoadBalancerName_,
        certificateName = pCertificateName_
      }

-- | The name of the load balancer to which you want to associate the SSL/TLS certificate.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albtcLoadBalancerName :: Lens.Lens' AttachLoadBalancerTLSCertificate Lude.Text
albtcLoadBalancerName = Lens.lens (loadBalancerName :: AttachLoadBalancerTLSCertificate -> Lude.Text) (\s a -> s {loadBalancerName = a} :: AttachLoadBalancerTLSCertificate)
{-# DEPRECATED albtcLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | The name of your SSL/TLS certificate.
--
-- /Note:/ Consider using 'certificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albtcCertificateName :: Lens.Lens' AttachLoadBalancerTLSCertificate Lude.Text
albtcCertificateName = Lens.lens (certificateName :: AttachLoadBalancerTLSCertificate -> Lude.Text) (\s a -> s {certificateName = a} :: AttachLoadBalancerTLSCertificate)
{-# DEPRECATED albtcCertificateName "Use generic-lens or generic-optics with 'certificateName' instead." #-}

instance Lude.AWSRequest AttachLoadBalancerTLSCertificate where
  type
    Rs AttachLoadBalancerTLSCertificate =
      AttachLoadBalancerTLSCertificateResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          AttachLoadBalancerTLSCertificateResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AttachLoadBalancerTLSCertificate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Lightsail_20161128.AttachLoadBalancerTlsCertificate" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AttachLoadBalancerTLSCertificate where
  toJSON AttachLoadBalancerTLSCertificate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("loadBalancerName" Lude..= loadBalancerName),
            Lude.Just ("certificateName" Lude..= certificateName)
          ]
      )

instance Lude.ToPath AttachLoadBalancerTLSCertificate where
  toPath = Lude.const "/"

instance Lude.ToQuery AttachLoadBalancerTLSCertificate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAttachLoadBalancerTLSCertificateResponse' smart constructor.
data AttachLoadBalancerTLSCertificateResponse = AttachLoadBalancerTLSCertificateResponse'
  { operations ::
      Lude.Maybe
        [Operation],
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

-- | Creates a value of 'AttachLoadBalancerTLSCertificateResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- These SSL/TLS certificates are only usable by Lightsail load balancers. You can't get the certificate and use it for another purpose.
-- * 'responseStatus' - The response status code.
mkAttachLoadBalancerTLSCertificateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AttachLoadBalancerTLSCertificateResponse
mkAttachLoadBalancerTLSCertificateResponse pResponseStatus_ =
  AttachLoadBalancerTLSCertificateResponse'
    { operations =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- These SSL/TLS certificates are only usable by Lightsail load balancers. You can't get the certificate and use it for another purpose.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albtcrsOperations :: Lens.Lens' AttachLoadBalancerTLSCertificateResponse (Lude.Maybe [Operation])
albtcrsOperations = Lens.lens (operations :: AttachLoadBalancerTLSCertificateResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: AttachLoadBalancerTLSCertificateResponse)
{-# DEPRECATED albtcrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
albtcrsResponseStatus :: Lens.Lens' AttachLoadBalancerTLSCertificateResponse Lude.Int
albtcrsResponseStatus = Lens.lens (responseStatus :: AttachLoadBalancerTLSCertificateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AttachLoadBalancerTLSCertificateResponse)
{-# DEPRECATED albtcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteLoadBalancerTLSCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an SSL/TLS certificate associated with a Lightsail load balancer.
--
-- The @DeleteLoadBalancerTlsCertificate@ operation supports tag-based access control via resource tags applied to the resource identified by @load balancer name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.DeleteLoadBalancerTLSCertificate
  ( -- * Creating a request
    DeleteLoadBalancerTLSCertificate (..),
    mkDeleteLoadBalancerTLSCertificate,

    -- ** Request lenses
    dlbtcCertificateName,
    dlbtcForce,
    dlbtcLoadBalancerName,

    -- * Destructuring the response
    DeleteLoadBalancerTLSCertificateResponse (..),
    mkDeleteLoadBalancerTLSCertificateResponse,

    -- ** Response lenses
    dlbtcrsOperations,
    dlbtcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteLoadBalancerTLSCertificate' smart constructor.
data DeleteLoadBalancerTLSCertificate = DeleteLoadBalancerTLSCertificate'
  { -- | The SSL/TLS certificate name.
    certificateName :: Lude.Text,
    -- | When @true@ , forces the deletion of an SSL/TLS certificate.
    --
    -- There can be two certificates associated with a Lightsail load balancer: the primary and the backup. The @force@ parameter is required when the primary SSL/TLS certificate is in use by an instance attached to the load balancer.
    force :: Lude.Maybe Lude.Bool,
    -- | The load balancer name.
    loadBalancerName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteLoadBalancerTLSCertificate' with the minimum fields required to make a request.
--
-- * 'certificateName' - The SSL/TLS certificate name.
-- * 'force' - When @true@ , forces the deletion of an SSL/TLS certificate.
--
-- There can be two certificates associated with a Lightsail load balancer: the primary and the backup. The @force@ parameter is required when the primary SSL/TLS certificate is in use by an instance attached to the load balancer.
-- * 'loadBalancerName' - The load balancer name.
mkDeleteLoadBalancerTLSCertificate ::
  -- | 'certificateName'
  Lude.Text ->
  -- | 'loadBalancerName'
  Lude.Text ->
  DeleteLoadBalancerTLSCertificate
mkDeleteLoadBalancerTLSCertificate
  pCertificateName_
  pLoadBalancerName_ =
    DeleteLoadBalancerTLSCertificate'
      { certificateName =
          pCertificateName_,
        force = Lude.Nothing,
        loadBalancerName = pLoadBalancerName_
      }

-- | The SSL/TLS certificate name.
--
-- /Note:/ Consider using 'certificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbtcCertificateName :: Lens.Lens' DeleteLoadBalancerTLSCertificate Lude.Text
dlbtcCertificateName = Lens.lens (certificateName :: DeleteLoadBalancerTLSCertificate -> Lude.Text) (\s a -> s {certificateName = a} :: DeleteLoadBalancerTLSCertificate)
{-# DEPRECATED dlbtcCertificateName "Use generic-lens or generic-optics with 'certificateName' instead." #-}

-- | When @true@ , forces the deletion of an SSL/TLS certificate.
--
-- There can be two certificates associated with a Lightsail load balancer: the primary and the backup. The @force@ parameter is required when the primary SSL/TLS certificate is in use by an instance attached to the load balancer.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbtcForce :: Lens.Lens' DeleteLoadBalancerTLSCertificate (Lude.Maybe Lude.Bool)
dlbtcForce = Lens.lens (force :: DeleteLoadBalancerTLSCertificate -> Lude.Maybe Lude.Bool) (\s a -> s {force = a} :: DeleteLoadBalancerTLSCertificate)
{-# DEPRECATED dlbtcForce "Use generic-lens or generic-optics with 'force' instead." #-}

-- | The load balancer name.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbtcLoadBalancerName :: Lens.Lens' DeleteLoadBalancerTLSCertificate Lude.Text
dlbtcLoadBalancerName = Lens.lens (loadBalancerName :: DeleteLoadBalancerTLSCertificate -> Lude.Text) (\s a -> s {loadBalancerName = a} :: DeleteLoadBalancerTLSCertificate)
{-# DEPRECATED dlbtcLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

instance Lude.AWSRequest DeleteLoadBalancerTLSCertificate where
  type
    Rs DeleteLoadBalancerTLSCertificate =
      DeleteLoadBalancerTLSCertificateResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteLoadBalancerTLSCertificateResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteLoadBalancerTLSCertificate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Lightsail_20161128.DeleteLoadBalancerTlsCertificate" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteLoadBalancerTLSCertificate where
  toJSON DeleteLoadBalancerTLSCertificate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("certificateName" Lude..= certificateName),
            ("force" Lude..=) Lude.<$> force,
            Lude.Just ("loadBalancerName" Lude..= loadBalancerName)
          ]
      )

instance Lude.ToPath DeleteLoadBalancerTLSCertificate where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteLoadBalancerTLSCertificate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteLoadBalancerTLSCertificateResponse' smart constructor.
data DeleteLoadBalancerTLSCertificateResponse = DeleteLoadBalancerTLSCertificateResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Lude.Maybe [Operation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteLoadBalancerTLSCertificateResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkDeleteLoadBalancerTLSCertificateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteLoadBalancerTLSCertificateResponse
mkDeleteLoadBalancerTLSCertificateResponse pResponseStatus_ =
  DeleteLoadBalancerTLSCertificateResponse'
    { operations =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbtcrsOperations :: Lens.Lens' DeleteLoadBalancerTLSCertificateResponse (Lude.Maybe [Operation])
dlbtcrsOperations = Lens.lens (operations :: DeleteLoadBalancerTLSCertificateResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: DeleteLoadBalancerTLSCertificateResponse)
{-# DEPRECATED dlbtcrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlbtcrsResponseStatus :: Lens.Lens' DeleteLoadBalancerTLSCertificateResponse Lude.Int
dlbtcrsResponseStatus = Lens.lens (responseStatus :: DeleteLoadBalancerTLSCertificateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteLoadBalancerTLSCertificateResponse)
{-# DEPRECATED dlbtcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

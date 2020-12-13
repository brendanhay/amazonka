{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an SSL/TLS certificate for your Amazon Lightsail content delivery network (CDN) distribution.
--
-- Certificates that are currently attached to a distribution cannot be deleted. Use the @DetachCertificateFromDistribution@ action to detach a certificate from a distribution.
module Network.AWS.Lightsail.DeleteCertificate
  ( -- * Creating a request
    DeleteCertificate (..),
    mkDeleteCertificate,

    -- ** Request lenses
    dcCertificateName,

    -- * Destructuring the response
    DeleteCertificateResponse (..),
    mkDeleteCertificateResponse,

    -- ** Response lenses
    dcrsOperations,
    dcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteCertificate' smart constructor.
newtype DeleteCertificate = DeleteCertificate'
  { -- | The name of the certificate to delete.
    --
    -- Use the @GetCertificates@ action to get a list of certificate names that you can specify.
    certificateName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCertificate' with the minimum fields required to make a request.
--
-- * 'certificateName' - The name of the certificate to delete.
--
-- Use the @GetCertificates@ action to get a list of certificate names that you can specify.
mkDeleteCertificate ::
  -- | 'certificateName'
  Lude.Text ->
  DeleteCertificate
mkDeleteCertificate pCertificateName_ =
  DeleteCertificate' {certificateName = pCertificateName_}

-- | The name of the certificate to delete.
--
-- Use the @GetCertificates@ action to get a list of certificate names that you can specify.
--
-- /Note:/ Consider using 'certificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcCertificateName :: Lens.Lens' DeleteCertificate Lude.Text
dcCertificateName = Lens.lens (certificateName :: DeleteCertificate -> Lude.Text) (\s a -> s {certificateName = a} :: DeleteCertificate)
{-# DEPRECATED dcCertificateName "Use generic-lens or generic-optics with 'certificateName' instead." #-}

instance Lude.AWSRequest DeleteCertificate where
  type Rs DeleteCertificate = DeleteCertificateResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteCertificateResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteCertificate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.DeleteCertificate" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteCertificate where
  toJSON DeleteCertificate' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("certificateName" Lude..= certificateName)]
      )

instance Lude.ToPath DeleteCertificate where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteCertificate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteCertificateResponse' smart constructor.
data DeleteCertificateResponse = DeleteCertificateResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Lude.Maybe [Operation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCertificateResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkDeleteCertificateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteCertificateResponse
mkDeleteCertificateResponse pResponseStatus_ =
  DeleteCertificateResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsOperations :: Lens.Lens' DeleteCertificateResponse (Lude.Maybe [Operation])
dcrsOperations = Lens.lens (operations :: DeleteCertificateResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: DeleteCertificateResponse)
{-# DEPRECATED dcrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsResponseStatus :: Lens.Lens' DeleteCertificateResponse Lude.Int
dcrsResponseStatus = Lens.lens (responseStatus :: DeleteCertificateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteCertificateResponse)
{-# DEPRECATED dcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DeleteCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified certificate.
module Network.AWS.DMS.DeleteCertificate
  ( -- * Creating a request
    DeleteCertificate (..),
    mkDeleteCertificate,

    -- ** Request lenses
    dcCertificateARN,

    -- * Destructuring the response
    DeleteCertificateResponse (..),
    mkDeleteCertificateResponse,

    -- ** Response lenses
    dccrsCertificate,
    dccrsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteCertificate' smart constructor.
newtype DeleteCertificate = DeleteCertificate'
  { certificateARN ::
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

-- | Creates a value of 'DeleteCertificate' with the minimum fields required to make a request.
--
-- * 'certificateARN' - The Amazon Resource Name (ARN) of the deleted certificate.
mkDeleteCertificate ::
  -- | 'certificateARN'
  Lude.Text ->
  DeleteCertificate
mkDeleteCertificate pCertificateARN_ =
  DeleteCertificate' {certificateARN = pCertificateARN_}

-- | The Amazon Resource Name (ARN) of the deleted certificate.
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcCertificateARN :: Lens.Lens' DeleteCertificate Lude.Text
dcCertificateARN = Lens.lens (certificateARN :: DeleteCertificate -> Lude.Text) (\s a -> s {certificateARN = a} :: DeleteCertificate)
{-# DEPRECATED dcCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

instance Lude.AWSRequest DeleteCertificate where
  type Rs DeleteCertificate = DeleteCertificateResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteCertificateResponse'
            Lude.<$> (x Lude..?> "Certificate") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteCertificate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonDMSv20160101.DeleteCertificate" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteCertificate where
  toJSON DeleteCertificate' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("CertificateArn" Lude..= certificateARN)]
      )

instance Lude.ToPath DeleteCertificate where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteCertificate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteCertificateResponse' smart constructor.
data DeleteCertificateResponse = DeleteCertificateResponse'
  { certificate ::
      Lude.Maybe Certificate,
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

-- | Creates a value of 'DeleteCertificateResponse' with the minimum fields required to make a request.
--
-- * 'certificate' - The Secure Sockets Layer (SSL) certificate.
-- * 'responseStatus' - The response status code.
mkDeleteCertificateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteCertificateResponse
mkDeleteCertificateResponse pResponseStatus_ =
  DeleteCertificateResponse'
    { certificate = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Secure Sockets Layer (SSL) certificate.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccrsCertificate :: Lens.Lens' DeleteCertificateResponse (Lude.Maybe Certificate)
dccrsCertificate = Lens.lens (certificate :: DeleteCertificateResponse -> Lude.Maybe Certificate) (\s a -> s {certificate = a} :: DeleteCertificateResponse)
{-# DEPRECATED dccrsCertificate "Use generic-lens or generic-optics with 'certificate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccrsResponseStatus :: Lens.Lens' DeleteCertificateResponse Lude.Int
dccrsResponseStatus = Lens.lens (responseStatus :: DeleteCertificateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteCertificateResponse)
{-# DEPRECATED dccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

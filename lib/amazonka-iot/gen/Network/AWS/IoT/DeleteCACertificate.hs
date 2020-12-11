{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteCACertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a registered CA certificate.
module Network.AWS.IoT.DeleteCACertificate
  ( -- * Creating a request
    DeleteCACertificate (..),
    mkDeleteCACertificate,

    -- ** Request lenses
    dcacCertificateId,

    -- * Destructuring the response
    DeleteCACertificateResponse (..),
    mkDeleteCACertificateResponse,

    -- ** Response lenses
    dcacrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Input for the DeleteCACertificate operation.
--
-- /See:/ 'mkDeleteCACertificate' smart constructor.
newtype DeleteCACertificate = DeleteCACertificate'
  { certificateId ::
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

-- | Creates a value of 'DeleteCACertificate' with the minimum fields required to make a request.
--
-- * 'certificateId' - The ID of the certificate to delete. (The last part of the certificate ARN contains the certificate ID.)
mkDeleteCACertificate ::
  -- | 'certificateId'
  Lude.Text ->
  DeleteCACertificate
mkDeleteCACertificate pCertificateId_ =
  DeleteCACertificate' {certificateId = pCertificateId_}

-- | The ID of the certificate to delete. (The last part of the certificate ARN contains the certificate ID.)
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcacCertificateId :: Lens.Lens' DeleteCACertificate Lude.Text
dcacCertificateId = Lens.lens (certificateId :: DeleteCACertificate -> Lude.Text) (\s a -> s {certificateId = a} :: DeleteCACertificate)
{-# DEPRECATED dcacCertificateId "Use generic-lens or generic-optics with 'certificateId' instead." #-}

instance Lude.AWSRequest DeleteCACertificate where
  type Rs DeleteCACertificate = DeleteCACertificateResponse
  request = Req.delete ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteCACertificateResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteCACertificate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteCACertificate where
  toPath DeleteCACertificate' {..} =
    Lude.mconcat ["/cacertificate/", Lude.toBS certificateId]

instance Lude.ToQuery DeleteCACertificate where
  toQuery = Lude.const Lude.mempty

-- | The output for the DeleteCACertificate operation.
--
-- /See:/ 'mkDeleteCACertificateResponse' smart constructor.
newtype DeleteCACertificateResponse = DeleteCACertificateResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCACertificateResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteCACertificateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteCACertificateResponse
mkDeleteCACertificateResponse pResponseStatus_ =
  DeleteCACertificateResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcacrsResponseStatus :: Lens.Lens' DeleteCACertificateResponse Lude.Int
dcacrsResponseStatus = Lens.lens (responseStatus :: DeleteCACertificateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteCACertificateResponse)
{-# DEPRECATED dcacrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

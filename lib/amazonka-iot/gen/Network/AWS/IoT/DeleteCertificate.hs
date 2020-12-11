{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified certificate.
--
-- A certificate cannot be deleted if it has a policy or IoT thing attached to it or if its status is set to ACTIVE. To delete a certificate, first use the 'DetachPrincipalPolicy' API to detach all policies. Next, use the 'UpdateCertificate' API to set the certificate to the INACTIVE status.
module Network.AWS.IoT.DeleteCertificate
  ( -- * Creating a request
    DeleteCertificate (..),
    mkDeleteCertificate,

    -- ** Request lenses
    dcForceDelete,
    dcCertificateId,

    -- * Destructuring the response
    DeleteCertificateResponse (..),
    mkDeleteCertificateResponse,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the DeleteCertificate operation.
--
-- /See:/ 'mkDeleteCertificate' smart constructor.
data DeleteCertificate = DeleteCertificate'
  { forceDelete ::
      Lude.Maybe Lude.Bool,
    certificateId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCertificate' with the minimum fields required to make a request.
--
-- * 'certificateId' - The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
-- * 'forceDelete' - Forces the deletion of a certificate if it is inactive and is not attached to an IoT thing.
mkDeleteCertificate ::
  -- | 'certificateId'
  Lude.Text ->
  DeleteCertificate
mkDeleteCertificate pCertificateId_ =
  DeleteCertificate'
    { forceDelete = Lude.Nothing,
      certificateId = pCertificateId_
    }

-- | Forces the deletion of a certificate if it is inactive and is not attached to an IoT thing.
--
-- /Note:/ Consider using 'forceDelete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcForceDelete :: Lens.Lens' DeleteCertificate (Lude.Maybe Lude.Bool)
dcForceDelete = Lens.lens (forceDelete :: DeleteCertificate -> Lude.Maybe Lude.Bool) (\s a -> s {forceDelete = a} :: DeleteCertificate)
{-# DEPRECATED dcForceDelete "Use generic-lens or generic-optics with 'forceDelete' instead." #-}

-- | The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcCertificateId :: Lens.Lens' DeleteCertificate Lude.Text
dcCertificateId = Lens.lens (certificateId :: DeleteCertificate -> Lude.Text) (\s a -> s {certificateId = a} :: DeleteCertificate)
{-# DEPRECATED dcCertificateId "Use generic-lens or generic-optics with 'certificateId' instead." #-}

instance Lude.AWSRequest DeleteCertificate where
  type Rs DeleteCertificate = DeleteCertificateResponse
  request = Req.delete ioTService
  response = Res.receiveNull DeleteCertificateResponse'

instance Lude.ToHeaders DeleteCertificate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteCertificate where
  toPath DeleteCertificate' {..} =
    Lude.mconcat ["/certificates/", Lude.toBS certificateId]

instance Lude.ToQuery DeleteCertificate where
  toQuery DeleteCertificate' {..} =
    Lude.mconcat ["forceDelete" Lude.=: forceDelete]

-- | /See:/ 'mkDeleteCertificateResponse' smart constructor.
data DeleteCertificateResponse = DeleteCertificateResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCertificateResponse' with the minimum fields required to make a request.
mkDeleteCertificateResponse ::
  DeleteCertificateResponse
mkDeleteCertificateResponse = DeleteCertificateResponse'

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CancelCertificateTransfer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a pending transfer for the specified certificate.
--
-- __Note__ Only the transfer source account can use this operation to cancel a transfer. (Transfer destinations can use 'RejectCertificateTransfer' instead.) After transfer, AWS IoT returns the certificate to the source account in the INACTIVE state. After the destination account has accepted the transfer, the transfer cannot be cancelled.
-- After a certificate transfer is cancelled, the status of the certificate changes from PENDING_TRANSFER to INACTIVE.
module Network.AWS.IoT.CancelCertificateTransfer
  ( -- * Creating a request
    CancelCertificateTransfer (..),
    mkCancelCertificateTransfer,

    -- ** Request lenses
    cctCertificateId,

    -- * Destructuring the response
    CancelCertificateTransferResponse (..),
    mkCancelCertificateTransferResponse,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the CancelCertificateTransfer operation.
--
-- /See:/ 'mkCancelCertificateTransfer' smart constructor.
newtype CancelCertificateTransfer = CancelCertificateTransfer'
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

-- | Creates a value of 'CancelCertificateTransfer' with the minimum fields required to make a request.
--
-- * 'certificateId' - The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
mkCancelCertificateTransfer ::
  -- | 'certificateId'
  Lude.Text ->
  CancelCertificateTransfer
mkCancelCertificateTransfer pCertificateId_ =
  CancelCertificateTransfer' {certificateId = pCertificateId_}

-- | The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cctCertificateId :: Lens.Lens' CancelCertificateTransfer Lude.Text
cctCertificateId = Lens.lens (certificateId :: CancelCertificateTransfer -> Lude.Text) (\s a -> s {certificateId = a} :: CancelCertificateTransfer)
{-# DEPRECATED cctCertificateId "Use generic-lens or generic-optics with 'certificateId' instead." #-}

instance Lude.AWSRequest CancelCertificateTransfer where
  type
    Rs CancelCertificateTransfer =
      CancelCertificateTransferResponse
  request = Req.patchJSON ioTService
  response = Res.receiveNull CancelCertificateTransferResponse'

instance Lude.ToHeaders CancelCertificateTransfer where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CancelCertificateTransfer where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath CancelCertificateTransfer where
  toPath CancelCertificateTransfer' {..} =
    Lude.mconcat
      ["/cancel-certificate-transfer/", Lude.toBS certificateId]

instance Lude.ToQuery CancelCertificateTransfer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCancelCertificateTransferResponse' smart constructor.
data CancelCertificateTransferResponse = CancelCertificateTransferResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelCertificateTransferResponse' with the minimum fields required to make a request.
mkCancelCertificateTransferResponse ::
  CancelCertificateTransferResponse
mkCancelCertificateTransferResponse =
  CancelCertificateTransferResponse'

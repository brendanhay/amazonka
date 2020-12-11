{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.RejectCertificateTransfer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects a pending certificate transfer. After AWS IoT rejects a certificate transfer, the certificate status changes from __PENDING_TRANSFER__ to __INACTIVE__ .
--
-- To check for pending certificate transfers, call 'ListCertificates' to enumerate your certificates.
-- This operation can only be called by the transfer destination. After it is called, the certificate will be returned to the source's account in the INACTIVE state.
module Network.AWS.IoT.RejectCertificateTransfer
  ( -- * Creating a request
    RejectCertificateTransfer (..),
    mkRejectCertificateTransfer,

    -- ** Request lenses
    rctRejectReason,
    rctCertificateId,

    -- * Destructuring the response
    RejectCertificateTransferResponse (..),
    mkRejectCertificateTransferResponse,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the RejectCertificateTransfer operation.
--
-- /See:/ 'mkRejectCertificateTransfer' smart constructor.
data RejectCertificateTransfer = RejectCertificateTransfer'
  { rejectReason ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'RejectCertificateTransfer' with the minimum fields required to make a request.
--
-- * 'certificateId' - The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
-- * 'rejectReason' - The reason the certificate transfer was rejected.
mkRejectCertificateTransfer ::
  -- | 'certificateId'
  Lude.Text ->
  RejectCertificateTransfer
mkRejectCertificateTransfer pCertificateId_ =
  RejectCertificateTransfer'
    { rejectReason = Lude.Nothing,
      certificateId = pCertificateId_
    }

-- | The reason the certificate transfer was rejected.
--
-- /Note:/ Consider using 'rejectReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rctRejectReason :: Lens.Lens' RejectCertificateTransfer (Lude.Maybe Lude.Text)
rctRejectReason = Lens.lens (rejectReason :: RejectCertificateTransfer -> Lude.Maybe Lude.Text) (\s a -> s {rejectReason = a} :: RejectCertificateTransfer)
{-# DEPRECATED rctRejectReason "Use generic-lens or generic-optics with 'rejectReason' instead." #-}

-- | The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rctCertificateId :: Lens.Lens' RejectCertificateTransfer Lude.Text
rctCertificateId = Lens.lens (certificateId :: RejectCertificateTransfer -> Lude.Text) (\s a -> s {certificateId = a} :: RejectCertificateTransfer)
{-# DEPRECATED rctCertificateId "Use generic-lens or generic-optics with 'certificateId' instead." #-}

instance Lude.AWSRequest RejectCertificateTransfer where
  type
    Rs RejectCertificateTransfer =
      RejectCertificateTransferResponse
  request = Req.patchJSON ioTService
  response = Res.receiveNull RejectCertificateTransferResponse'

instance Lude.ToHeaders RejectCertificateTransfer where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON RejectCertificateTransfer where
  toJSON RejectCertificateTransfer' {..} =
    Lude.object
      (Lude.catMaybes [("rejectReason" Lude..=) Lude.<$> rejectReason])

instance Lude.ToPath RejectCertificateTransfer where
  toPath RejectCertificateTransfer' {..} =
    Lude.mconcat
      ["/reject-certificate-transfer/", Lude.toBS certificateId]

instance Lude.ToQuery RejectCertificateTransfer where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRejectCertificateTransferResponse' smart constructor.
data RejectCertificateTransferResponse = RejectCertificateTransferResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RejectCertificateTransferResponse' with the minimum fields required to make a request.
mkRejectCertificateTransferResponse ::
  RejectCertificateTransferResponse
mkRejectCertificateTransferResponse =
  RejectCertificateTransferResponse'

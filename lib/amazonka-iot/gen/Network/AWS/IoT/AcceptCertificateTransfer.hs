{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.AcceptCertificateTransfer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a pending certificate transfer. The default state of the certificate is INACTIVE.
--
-- To check for pending certificate transfers, call 'ListCertificates' to enumerate your certificates.
module Network.AWS.IoT.AcceptCertificateTransfer
  ( -- * Creating a request
    AcceptCertificateTransfer (..),
    mkAcceptCertificateTransfer,

    -- ** Request lenses
    actSetAsActive,
    actCertificateId,

    -- * Destructuring the response
    AcceptCertificateTransferResponse (..),
    mkAcceptCertificateTransferResponse,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the AcceptCertificateTransfer operation.
--
-- /See:/ 'mkAcceptCertificateTransfer' smart constructor.
data AcceptCertificateTransfer = AcceptCertificateTransfer'
  { setAsActive ::
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

-- | Creates a value of 'AcceptCertificateTransfer' with the minimum fields required to make a request.
--
-- * 'certificateId' - The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
-- * 'setAsActive' - Specifies whether the certificate is active.
mkAcceptCertificateTransfer ::
  -- | 'certificateId'
  Lude.Text ->
  AcceptCertificateTransfer
mkAcceptCertificateTransfer pCertificateId_ =
  AcceptCertificateTransfer'
    { setAsActive = Lude.Nothing,
      certificateId = pCertificateId_
    }

-- | Specifies whether the certificate is active.
--
-- /Note:/ Consider using 'setAsActive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
actSetAsActive :: Lens.Lens' AcceptCertificateTransfer (Lude.Maybe Lude.Bool)
actSetAsActive = Lens.lens (setAsActive :: AcceptCertificateTransfer -> Lude.Maybe Lude.Bool) (\s a -> s {setAsActive = a} :: AcceptCertificateTransfer)
{-# DEPRECATED actSetAsActive "Use generic-lens or generic-optics with 'setAsActive' instead." #-}

-- | The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
actCertificateId :: Lens.Lens' AcceptCertificateTransfer Lude.Text
actCertificateId = Lens.lens (certificateId :: AcceptCertificateTransfer -> Lude.Text) (\s a -> s {certificateId = a} :: AcceptCertificateTransfer)
{-# DEPRECATED actCertificateId "Use generic-lens or generic-optics with 'certificateId' instead." #-}

instance Lude.AWSRequest AcceptCertificateTransfer where
  type
    Rs AcceptCertificateTransfer =
      AcceptCertificateTransferResponse
  request = Req.patchJSON ioTService
  response = Res.receiveNull AcceptCertificateTransferResponse'

instance Lude.ToHeaders AcceptCertificateTransfer where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON AcceptCertificateTransfer where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath AcceptCertificateTransfer where
  toPath AcceptCertificateTransfer' {..} =
    Lude.mconcat
      ["/accept-certificate-transfer/", Lude.toBS certificateId]

instance Lude.ToQuery AcceptCertificateTransfer where
  toQuery AcceptCertificateTransfer' {..} =
    Lude.mconcat ["setAsActive" Lude.=: setAsActive]

-- | /See:/ 'mkAcceptCertificateTransferResponse' smart constructor.
data AcceptCertificateTransferResponse = AcceptCertificateTransferResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AcceptCertificateTransferResponse' with the minimum fields required to make a request.
mkAcceptCertificateTransferResponse ::
  AcceptCertificateTransferResponse
mkAcceptCertificateTransferResponse =
  AcceptCertificateTransferResponse'

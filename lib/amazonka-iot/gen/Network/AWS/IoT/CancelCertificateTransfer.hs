{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the CancelCertificateTransfer operation.
--
-- /See:/ 'mkCancelCertificateTransfer' smart constructor.
newtype CancelCertificateTransfer = CancelCertificateTransfer'
  { -- | The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
    certificateId :: Types.CertificateId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CancelCertificateTransfer' value with any optional fields omitted.
mkCancelCertificateTransfer ::
  -- | 'certificateId'
  Types.CertificateId ->
  CancelCertificateTransfer
mkCancelCertificateTransfer certificateId =
  CancelCertificateTransfer' {certificateId}

-- | The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cctCertificateId :: Lens.Lens' CancelCertificateTransfer Types.CertificateId
cctCertificateId = Lens.field @"certificateId"
{-# DEPRECATED cctCertificateId "Use generic-lens or generic-optics with 'certificateId' instead." #-}

instance Core.FromJSON CancelCertificateTransfer where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest CancelCertificateTransfer where
  type
    Rs CancelCertificateTransfer =
      CancelCertificateTransferResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PATCH,
        Core._rqPath =
          Core.rawPath
            ( "/cancel-certificate-transfer/"
                Core.<> (Core.toText certificateId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull CancelCertificateTransferResponse'

-- | /See:/ 'mkCancelCertificateTransferResponse' smart constructor.
data CancelCertificateTransferResponse = CancelCertificateTransferResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelCertificateTransferResponse' value with any optional fields omitted.
mkCancelCertificateTransferResponse ::
  CancelCertificateTransferResponse
mkCancelCertificateTransferResponse =
  CancelCertificateTransferResponse'

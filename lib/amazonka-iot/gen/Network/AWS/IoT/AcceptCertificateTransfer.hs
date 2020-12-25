{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    actCertificateId,
    actSetAsActive,

    -- * Destructuring the response
    AcceptCertificateTransferResponse (..),
    mkAcceptCertificateTransferResponse,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the AcceptCertificateTransfer operation.
--
-- /See:/ 'mkAcceptCertificateTransfer' smart constructor.
data AcceptCertificateTransfer = AcceptCertificateTransfer'
  { -- | The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
    certificateId :: Types.CertificateId,
    -- | Specifies whether the certificate is active.
    setAsActive :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AcceptCertificateTransfer' value with any optional fields omitted.
mkAcceptCertificateTransfer ::
  -- | 'certificateId'
  Types.CertificateId ->
  AcceptCertificateTransfer
mkAcceptCertificateTransfer certificateId =
  AcceptCertificateTransfer'
    { certificateId,
      setAsActive = Core.Nothing
    }

-- | The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
actCertificateId :: Lens.Lens' AcceptCertificateTransfer Types.CertificateId
actCertificateId = Lens.field @"certificateId"
{-# DEPRECATED actCertificateId "Use generic-lens or generic-optics with 'certificateId' instead." #-}

-- | Specifies whether the certificate is active.
--
-- /Note:/ Consider using 'setAsActive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
actSetAsActive :: Lens.Lens' AcceptCertificateTransfer (Core.Maybe Core.Bool)
actSetAsActive = Lens.field @"setAsActive"
{-# DEPRECATED actSetAsActive "Use generic-lens or generic-optics with 'setAsActive' instead." #-}

instance Core.FromJSON AcceptCertificateTransfer where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest AcceptCertificateTransfer where
  type
    Rs AcceptCertificateTransfer =
      AcceptCertificateTransferResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PATCH,
        Core._rqPath =
          Core.rawPath
            ( "/accept-certificate-transfer/"
                Core.<> (Core.toText certificateId)
            ),
        Core._rqQuery =
          Core.toQueryValue "setAsActive" Core.<$> setAsActive,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull AcceptCertificateTransferResponse'

-- | /See:/ 'mkAcceptCertificateTransferResponse' smart constructor.
data AcceptCertificateTransferResponse = AcceptCertificateTransferResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AcceptCertificateTransferResponse' value with any optional fields omitted.
mkAcceptCertificateTransferResponse ::
  AcceptCertificateTransferResponse
mkAcceptCertificateTransferResponse =
  AcceptCertificateTransferResponse'

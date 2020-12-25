{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the status of the specified certificate. This operation is idempotent.
--
-- Certificates must be in the ACTIVE state to authenticate devices that use a certificate to connect to AWS IoT.
-- Within a few minutes of updating a certificate from the ACTIVE state to any other state, AWS IoT disconnects all devices that used that certificate to connect. Devices cannot use a certificate that is not in the ACTIVE state to reconnect.
module Network.AWS.IoT.UpdateCertificate
  ( -- * Creating a request
    UpdateCertificate (..),
    mkUpdateCertificate,

    -- ** Request lenses
    ucCertificateId,
    ucNewStatus,

    -- * Destructuring the response
    UpdateCertificateResponse (..),
    mkUpdateCertificateResponse,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the UpdateCertificate operation.
--
-- /See:/ 'mkUpdateCertificate' smart constructor.
data UpdateCertificate = UpdateCertificate'
  { -- | The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
    certificateId :: Types.CertificateId,
    -- | The new status.
    --
    -- __Note:__ Setting the status to PENDING_TRANSFER or PENDING_ACTIVATION will result in an exception being thrown. PENDING_TRANSFER and PENDING_ACTIVATION are statuses used internally by AWS IoT. They are not intended for developer use.
    -- __Note:__ The status value REGISTER_INACTIVE is deprecated and should not be used.
    newStatus :: Types.CertificateStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCertificate' value with any optional fields omitted.
mkUpdateCertificate ::
  -- | 'certificateId'
  Types.CertificateId ->
  -- | 'newStatus'
  Types.CertificateStatus ->
  UpdateCertificate
mkUpdateCertificate certificateId newStatus =
  UpdateCertificate' {certificateId, newStatus}

-- | The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucCertificateId :: Lens.Lens' UpdateCertificate Types.CertificateId
ucCertificateId = Lens.field @"certificateId"
{-# DEPRECATED ucCertificateId "Use generic-lens or generic-optics with 'certificateId' instead." #-}

-- | The new status.
--
-- __Note:__ Setting the status to PENDING_TRANSFER or PENDING_ACTIVATION will result in an exception being thrown. PENDING_TRANSFER and PENDING_ACTIVATION are statuses used internally by AWS IoT. They are not intended for developer use.
-- __Note:__ The status value REGISTER_INACTIVE is deprecated and should not be used.
--
-- /Note:/ Consider using 'newStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucNewStatus :: Lens.Lens' UpdateCertificate Types.CertificateStatus
ucNewStatus = Lens.field @"newStatus"
{-# DEPRECATED ucNewStatus "Use generic-lens or generic-optics with 'newStatus' instead." #-}

instance Core.FromJSON UpdateCertificate where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest UpdateCertificate where
  type Rs UpdateCertificate = UpdateCertificateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ("/certificates/" Core.<> (Core.toText certificateId)),
        Core._rqQuery = Core.toQueryValue "newStatus" newStatus,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull UpdateCertificateResponse'

-- | /See:/ 'mkUpdateCertificateResponse' smart constructor.
data UpdateCertificateResponse = UpdateCertificateResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCertificateResponse' value with any optional fields omitted.
mkUpdateCertificateResponse ::
  UpdateCertificateResponse
mkUpdateCertificateResponse = UpdateCertificateResponse'

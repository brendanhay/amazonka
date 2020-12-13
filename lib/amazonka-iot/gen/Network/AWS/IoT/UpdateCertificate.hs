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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the UpdateCertificate operation.
--
-- /See:/ 'mkUpdateCertificate' smart constructor.
data UpdateCertificate = UpdateCertificate'
  { -- | The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
    certificateId :: Lude.Text,
    -- | The new status.
    --
    -- __Note:__ Setting the status to PENDING_TRANSFER or PENDING_ACTIVATION will result in an exception being thrown. PENDING_TRANSFER and PENDING_ACTIVATION are statuses used internally by AWS IoT. They are not intended for developer use.
    -- __Note:__ The status value REGISTER_INACTIVE is deprecated and should not be used.
    newStatus :: CertificateStatus
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateCertificate' with the minimum fields required to make a request.
--
-- * 'certificateId' - The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
-- * 'newStatus' - The new status.
--
-- __Note:__ Setting the status to PENDING_TRANSFER or PENDING_ACTIVATION will result in an exception being thrown. PENDING_TRANSFER and PENDING_ACTIVATION are statuses used internally by AWS IoT. They are not intended for developer use.
-- __Note:__ The status value REGISTER_INACTIVE is deprecated and should not be used.
mkUpdateCertificate ::
  -- | 'certificateId'
  Lude.Text ->
  -- | 'newStatus'
  CertificateStatus ->
  UpdateCertificate
mkUpdateCertificate pCertificateId_ pNewStatus_ =
  UpdateCertificate'
    { certificateId = pCertificateId_,
      newStatus = pNewStatus_
    }

-- | The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucCertificateId :: Lens.Lens' UpdateCertificate Lude.Text
ucCertificateId = Lens.lens (certificateId :: UpdateCertificate -> Lude.Text) (\s a -> s {certificateId = a} :: UpdateCertificate)
{-# DEPRECATED ucCertificateId "Use generic-lens or generic-optics with 'certificateId' instead." #-}

-- | The new status.
--
-- __Note:__ Setting the status to PENDING_TRANSFER or PENDING_ACTIVATION will result in an exception being thrown. PENDING_TRANSFER and PENDING_ACTIVATION are statuses used internally by AWS IoT. They are not intended for developer use.
-- __Note:__ The status value REGISTER_INACTIVE is deprecated and should not be used.
--
-- /Note:/ Consider using 'newStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucNewStatus :: Lens.Lens' UpdateCertificate CertificateStatus
ucNewStatus = Lens.lens (newStatus :: UpdateCertificate -> CertificateStatus) (\s a -> s {newStatus = a} :: UpdateCertificate)
{-# DEPRECATED ucNewStatus "Use generic-lens or generic-optics with 'newStatus' instead." #-}

instance Lude.AWSRequest UpdateCertificate where
  type Rs UpdateCertificate = UpdateCertificateResponse
  request = Req.putJSON ioTService
  response = Res.receiveNull UpdateCertificateResponse'

instance Lude.ToHeaders UpdateCertificate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdateCertificate where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath UpdateCertificate where
  toPath UpdateCertificate' {..} =
    Lude.mconcat ["/certificates/", Lude.toBS certificateId]

instance Lude.ToQuery UpdateCertificate where
  toQuery UpdateCertificate' {..} =
    Lude.mconcat ["newStatus" Lude.=: newStatus]

-- | /See:/ 'mkUpdateCertificateResponse' smart constructor.
data UpdateCertificateResponse = UpdateCertificateResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateCertificateResponse' with the minimum fields required to make a request.
mkUpdateCertificateResponse ::
  UpdateCertificateResponse
mkUpdateCertificateResponse = UpdateCertificateResponse'

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.RegisterCertificateWithoutCA
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Register a certificate that does not have a certificate authority (CA).
module Network.AWS.IoT.RegisterCertificateWithoutCA
  ( -- * Creating a request
    RegisterCertificateWithoutCA (..),
    mkRegisterCertificateWithoutCA,

    -- ** Request lenses
    rcwcaCertificatePem,
    rcwcaStatus,

    -- * Destructuring the response
    RegisterCertificateWithoutCAResponse (..),
    mkRegisterCertificateWithoutCAResponse,

    -- ** Response lenses
    rcwcarrsCertificateArn,
    rcwcarrsCertificateId,
    rcwcarrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRegisterCertificateWithoutCA' smart constructor.
data RegisterCertificateWithoutCA = RegisterCertificateWithoutCA'
  { -- | The certificate data, in PEM format.
    certificatePem :: Types.CertificatePem,
    -- | The status of the register certificate request.
    status :: Core.Maybe Types.CertificateStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterCertificateWithoutCA' value with any optional fields omitted.
mkRegisterCertificateWithoutCA ::
  -- | 'certificatePem'
  Types.CertificatePem ->
  RegisterCertificateWithoutCA
mkRegisterCertificateWithoutCA certificatePem =
  RegisterCertificateWithoutCA'
    { certificatePem,
      status = Core.Nothing
    }

-- | The certificate data, in PEM format.
--
-- /Note:/ Consider using 'certificatePem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcwcaCertificatePem :: Lens.Lens' RegisterCertificateWithoutCA Types.CertificatePem
rcwcaCertificatePem = Lens.field @"certificatePem"
{-# DEPRECATED rcwcaCertificatePem "Use generic-lens or generic-optics with 'certificatePem' instead." #-}

-- | The status of the register certificate request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcwcaStatus :: Lens.Lens' RegisterCertificateWithoutCA (Core.Maybe Types.CertificateStatus)
rcwcaStatus = Lens.field @"status"
{-# DEPRECATED rcwcaStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON RegisterCertificateWithoutCA where
  toJSON RegisterCertificateWithoutCA {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("certificatePem" Core..= certificatePem),
            ("status" Core..=) Core.<$> status
          ]
      )

instance Core.AWSRequest RegisterCertificateWithoutCA where
  type
    Rs RegisterCertificateWithoutCA =
      RegisterCertificateWithoutCAResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/certificate/register-no-ca",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterCertificateWithoutCAResponse'
            Core.<$> (x Core..:? "certificateArn")
            Core.<*> (x Core..:? "certificateId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRegisterCertificateWithoutCAResponse' smart constructor.
data RegisterCertificateWithoutCAResponse = RegisterCertificateWithoutCAResponse'
  { -- | The Amazon Resource Name (ARN) of the registered certificate.
    certificateArn :: Core.Maybe Types.CertificateArn,
    -- | The ID of the registered certificate. (The last part of the certificate ARN contains the certificate ID.
    certificateId :: Core.Maybe Types.CertificateId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterCertificateWithoutCAResponse' value with any optional fields omitted.
mkRegisterCertificateWithoutCAResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RegisterCertificateWithoutCAResponse
mkRegisterCertificateWithoutCAResponse responseStatus =
  RegisterCertificateWithoutCAResponse'
    { certificateArn =
        Core.Nothing,
      certificateId = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the registered certificate.
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcwcarrsCertificateArn :: Lens.Lens' RegisterCertificateWithoutCAResponse (Core.Maybe Types.CertificateArn)
rcwcarrsCertificateArn = Lens.field @"certificateArn"
{-# DEPRECATED rcwcarrsCertificateArn "Use generic-lens or generic-optics with 'certificateArn' instead." #-}

-- | The ID of the registered certificate. (The last part of the certificate ARN contains the certificate ID.
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcwcarrsCertificateId :: Lens.Lens' RegisterCertificateWithoutCAResponse (Core.Maybe Types.CertificateId)
rcwcarrsCertificateId = Lens.field @"certificateId"
{-# DEPRECATED rcwcarrsCertificateId "Use generic-lens or generic-optics with 'certificateId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcwcarrsResponseStatus :: Lens.Lens' RegisterCertificateWithoutCAResponse Core.Int
rcwcarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rcwcarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

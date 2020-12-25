{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateKeysAndCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a 2048-bit RSA key pair and issues an X.509 certificate using the issued public key. You can also call @CreateKeysAndCertificate@ over MQTT from a device, for more information, see <https://docs.aws.amazon.com/iot/latest/developerguide/provision-wo-cert.html#provision-mqtt-api Provisioning MQTT API> .
--
-- __Note__ This is the only time AWS IoT issues the private key for this certificate, so it is important to keep it in a secure location.
module Network.AWS.IoT.CreateKeysAndCertificate
  ( -- * Creating a request
    CreateKeysAndCertificate (..),
    mkCreateKeysAndCertificate,

    -- ** Request lenses
    ckacSetAsActive,

    -- * Destructuring the response
    CreateKeysAndCertificateResponse (..),
    mkCreateKeysAndCertificateResponse,

    -- ** Response lenses
    ckacrrsCertificateArn,
    ckacrrsCertificateId,
    ckacrrsCertificatePem,
    ckacrrsKeyPair,
    ckacrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the CreateKeysAndCertificate operation.
--
-- /See:/ 'mkCreateKeysAndCertificate' smart constructor.
newtype CreateKeysAndCertificate = CreateKeysAndCertificate'
  { -- | Specifies whether the certificate is active.
    setAsActive :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateKeysAndCertificate' value with any optional fields omitted.
mkCreateKeysAndCertificate ::
  CreateKeysAndCertificate
mkCreateKeysAndCertificate =
  CreateKeysAndCertificate' {setAsActive = Core.Nothing}

-- | Specifies whether the certificate is active.
--
-- /Note:/ Consider using 'setAsActive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckacSetAsActive :: Lens.Lens' CreateKeysAndCertificate (Core.Maybe Core.Bool)
ckacSetAsActive = Lens.field @"setAsActive"
{-# DEPRECATED ckacSetAsActive "Use generic-lens or generic-optics with 'setAsActive' instead." #-}

instance Core.FromJSON CreateKeysAndCertificate where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest CreateKeysAndCertificate where
  type Rs CreateKeysAndCertificate = CreateKeysAndCertificateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/keys-and-certificate",
        Core._rqQuery =
          Core.toQueryValue "setAsActive" Core.<$> setAsActive,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateKeysAndCertificateResponse'
            Core.<$> (x Core..:? "certificateArn")
            Core.<*> (x Core..:? "certificateId")
            Core.<*> (x Core..:? "certificatePem")
            Core.<*> (x Core..:? "keyPair")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The output of the CreateKeysAndCertificate operation.
--
-- /See:/ 'mkCreateKeysAndCertificateResponse' smart constructor.
data CreateKeysAndCertificateResponse = CreateKeysAndCertificateResponse'
  { -- | The ARN of the certificate.
    certificateArn :: Core.Maybe Types.CertificateArn,
    -- | The ID of the certificate. AWS IoT issues a default subject name for the certificate (for example, AWS IoT Certificate).
    certificateId :: Core.Maybe Types.CertificateId,
    -- | The certificate data, in PEM format.
    certificatePem :: Core.Maybe Types.CertificatePem,
    -- | The generated key pair.
    keyPair :: Core.Maybe Types.KeyPair,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateKeysAndCertificateResponse' value with any optional fields omitted.
mkCreateKeysAndCertificateResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateKeysAndCertificateResponse
mkCreateKeysAndCertificateResponse responseStatus =
  CreateKeysAndCertificateResponse'
    { certificateArn = Core.Nothing,
      certificateId = Core.Nothing,
      certificatePem = Core.Nothing,
      keyPair = Core.Nothing,
      responseStatus
    }

-- | The ARN of the certificate.
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckacrrsCertificateArn :: Lens.Lens' CreateKeysAndCertificateResponse (Core.Maybe Types.CertificateArn)
ckacrrsCertificateArn = Lens.field @"certificateArn"
{-# DEPRECATED ckacrrsCertificateArn "Use generic-lens or generic-optics with 'certificateArn' instead." #-}

-- | The ID of the certificate. AWS IoT issues a default subject name for the certificate (for example, AWS IoT Certificate).
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckacrrsCertificateId :: Lens.Lens' CreateKeysAndCertificateResponse (Core.Maybe Types.CertificateId)
ckacrrsCertificateId = Lens.field @"certificateId"
{-# DEPRECATED ckacrrsCertificateId "Use generic-lens or generic-optics with 'certificateId' instead." #-}

-- | The certificate data, in PEM format.
--
-- /Note:/ Consider using 'certificatePem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckacrrsCertificatePem :: Lens.Lens' CreateKeysAndCertificateResponse (Core.Maybe Types.CertificatePem)
ckacrrsCertificatePem = Lens.field @"certificatePem"
{-# DEPRECATED ckacrrsCertificatePem "Use generic-lens or generic-optics with 'certificatePem' instead." #-}

-- | The generated key pair.
--
-- /Note:/ Consider using 'keyPair' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckacrrsKeyPair :: Lens.Lens' CreateKeysAndCertificateResponse (Core.Maybe Types.KeyPair)
ckacrrsKeyPair = Lens.field @"keyPair"
{-# DEPRECATED ckacrrsKeyPair "Use generic-lens or generic-optics with 'keyPair' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ckacrrsResponseStatus :: Lens.Lens' CreateKeysAndCertificateResponse Core.Int
ckacrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ckacrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

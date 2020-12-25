{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateProvisioningClaim
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a provisioning claim.
module Network.AWS.IoT.CreateProvisioningClaim
  ( -- * Creating a request
    CreateProvisioningClaim (..),
    mkCreateProvisioningClaim,

    -- ** Request lenses
    cpcTemplateName,

    -- * Destructuring the response
    CreateProvisioningClaimResponse (..),
    mkCreateProvisioningClaimResponse,

    -- ** Response lenses
    cpcrrsCertificateId,
    cpcrrsCertificatePem,
    cpcrrsExpiration,
    cpcrrsKeyPair,
    cpcrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateProvisioningClaim' smart constructor.
newtype CreateProvisioningClaim = CreateProvisioningClaim'
  { -- | The name of the provisioning template to use.
    templateName :: Types.TemplateName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateProvisioningClaim' value with any optional fields omitted.
mkCreateProvisioningClaim ::
  -- | 'templateName'
  Types.TemplateName ->
  CreateProvisioningClaim
mkCreateProvisioningClaim templateName =
  CreateProvisioningClaim' {templateName}

-- | The name of the provisioning template to use.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcTemplateName :: Lens.Lens' CreateProvisioningClaim Types.TemplateName
cpcTemplateName = Lens.field @"templateName"
{-# DEPRECATED cpcTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

instance Core.FromJSON CreateProvisioningClaim where
  toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest CreateProvisioningClaim where
  type Rs CreateProvisioningClaim = CreateProvisioningClaimResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/provisioning-templates/" Core.<> (Core.toText templateName)
                Core.<> ("/provisioning-claim")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProvisioningClaimResponse'
            Core.<$> (x Core..:? "certificateId")
            Core.<*> (x Core..:? "certificatePem")
            Core.<*> (x Core..:? "expiration")
            Core.<*> (x Core..:? "keyPair")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateProvisioningClaimResponse' smart constructor.
data CreateProvisioningClaimResponse = CreateProvisioningClaimResponse'
  { -- | The ID of the certificate.
    certificateId :: Core.Maybe Types.CertificateId,
    -- | The provisioning claim certificate.
    certificatePem :: Core.Maybe Types.CertificatePem,
    -- | The provisioning claim expiration time.
    expiration :: Core.Maybe Core.NominalDiffTime,
    -- | The provisioning claim key pair.
    keyPair :: Core.Maybe Types.KeyPair,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateProvisioningClaimResponse' value with any optional fields omitted.
mkCreateProvisioningClaimResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateProvisioningClaimResponse
mkCreateProvisioningClaimResponse responseStatus =
  CreateProvisioningClaimResponse'
    { certificateId = Core.Nothing,
      certificatePem = Core.Nothing,
      expiration = Core.Nothing,
      keyPair = Core.Nothing,
      responseStatus
    }

-- | The ID of the certificate.
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcrrsCertificateId :: Lens.Lens' CreateProvisioningClaimResponse (Core.Maybe Types.CertificateId)
cpcrrsCertificateId = Lens.field @"certificateId"
{-# DEPRECATED cpcrrsCertificateId "Use generic-lens or generic-optics with 'certificateId' instead." #-}

-- | The provisioning claim certificate.
--
-- /Note:/ Consider using 'certificatePem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcrrsCertificatePem :: Lens.Lens' CreateProvisioningClaimResponse (Core.Maybe Types.CertificatePem)
cpcrrsCertificatePem = Lens.field @"certificatePem"
{-# DEPRECATED cpcrrsCertificatePem "Use generic-lens or generic-optics with 'certificatePem' instead." #-}

-- | The provisioning claim expiration time.
--
-- /Note:/ Consider using 'expiration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcrrsExpiration :: Lens.Lens' CreateProvisioningClaimResponse (Core.Maybe Core.NominalDiffTime)
cpcrrsExpiration = Lens.field @"expiration"
{-# DEPRECATED cpcrrsExpiration "Use generic-lens or generic-optics with 'expiration' instead." #-}

-- | The provisioning claim key pair.
--
-- /Note:/ Consider using 'keyPair' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcrrsKeyPair :: Lens.Lens' CreateProvisioningClaimResponse (Core.Maybe Types.KeyPair)
cpcrrsKeyPair = Lens.field @"keyPair"
{-# DEPRECATED cpcrrsKeyPair "Use generic-lens or generic-optics with 'keyPair' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcrrsResponseStatus :: Lens.Lens' CreateProvisioningClaimResponse Core.Int
cpcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cpcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

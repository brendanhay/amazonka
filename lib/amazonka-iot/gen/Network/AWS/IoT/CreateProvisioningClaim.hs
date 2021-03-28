{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateProvisioningClaim (..)
    , mkCreateProvisioningClaim
    -- ** Request lenses
    , cpcTemplateName

    -- * Destructuring the response
    , CreateProvisioningClaimResponse (..)
    , mkCreateProvisioningClaimResponse
    -- ** Response lenses
    , cpcrrsCertificateId
    , cpcrrsCertificatePem
    , cpcrrsExpiration
    , cpcrrsKeyPair
    , cpcrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateProvisioningClaim' smart constructor.
newtype CreateProvisioningClaim = CreateProvisioningClaim'
  { templateName :: Types.TemplateName
    -- ^ The name of the provisioning template to use.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateProvisioningClaim' value with any optional fields omitted.
mkCreateProvisioningClaim
    :: Types.TemplateName -- ^ 'templateName'
    -> CreateProvisioningClaim
mkCreateProvisioningClaim templateName
  = CreateProvisioningClaim'{templateName}

-- | The name of the provisioning template to use.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcTemplateName :: Lens.Lens' CreateProvisioningClaim Types.TemplateName
cpcTemplateName = Lens.field @"templateName"
{-# INLINEABLE cpcTemplateName #-}
{-# DEPRECATED templateName "Use generic-lens or generic-optics with 'templateName' instead"  #-}

instance Core.ToQuery CreateProvisioningClaim where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateProvisioningClaim where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON CreateProvisioningClaim where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest CreateProvisioningClaim where
        type Rs CreateProvisioningClaim = CreateProvisioningClaimResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/provisioning-templates/" Core.<> Core.toText templateName Core.<>
                             "/provisioning-claim",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateProvisioningClaimResponse' Core.<$>
                   (x Core..:? "certificateId") Core.<*> x Core..:? "certificatePem"
                     Core.<*> x Core..:? "expiration"
                     Core.<*> x Core..:? "keyPair"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateProvisioningClaimResponse' smart constructor.
data CreateProvisioningClaimResponse = CreateProvisioningClaimResponse'
  { certificateId :: Core.Maybe Types.CertificateId
    -- ^ The ID of the certificate.
  , certificatePem :: Core.Maybe Types.CertificatePem
    -- ^ The provisioning claim certificate.
  , expiration :: Core.Maybe Core.NominalDiffTime
    -- ^ The provisioning claim expiration time.
  , keyPair :: Core.Maybe Types.KeyPair
    -- ^ The provisioning claim key pair.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateProvisioningClaimResponse' value with any optional fields omitted.
mkCreateProvisioningClaimResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateProvisioningClaimResponse
mkCreateProvisioningClaimResponse responseStatus
  = CreateProvisioningClaimResponse'{certificateId = Core.Nothing,
                                     certificatePem = Core.Nothing, expiration = Core.Nothing,
                                     keyPair = Core.Nothing, responseStatus}

-- | The ID of the certificate.
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcrrsCertificateId :: Lens.Lens' CreateProvisioningClaimResponse (Core.Maybe Types.CertificateId)
cpcrrsCertificateId = Lens.field @"certificateId"
{-# INLINEABLE cpcrrsCertificateId #-}
{-# DEPRECATED certificateId "Use generic-lens or generic-optics with 'certificateId' instead"  #-}

-- | The provisioning claim certificate.
--
-- /Note:/ Consider using 'certificatePem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcrrsCertificatePem :: Lens.Lens' CreateProvisioningClaimResponse (Core.Maybe Types.CertificatePem)
cpcrrsCertificatePem = Lens.field @"certificatePem"
{-# INLINEABLE cpcrrsCertificatePem #-}
{-# DEPRECATED certificatePem "Use generic-lens or generic-optics with 'certificatePem' instead"  #-}

-- | The provisioning claim expiration time.
--
-- /Note:/ Consider using 'expiration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcrrsExpiration :: Lens.Lens' CreateProvisioningClaimResponse (Core.Maybe Core.NominalDiffTime)
cpcrrsExpiration = Lens.field @"expiration"
{-# INLINEABLE cpcrrsExpiration #-}
{-# DEPRECATED expiration "Use generic-lens or generic-optics with 'expiration' instead"  #-}

-- | The provisioning claim key pair.
--
-- /Note:/ Consider using 'keyPair' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcrrsKeyPair :: Lens.Lens' CreateProvisioningClaimResponse (Core.Maybe Types.KeyPair)
cpcrrsKeyPair = Lens.field @"keyPair"
{-# INLINEABLE cpcrrsKeyPair #-}
{-# DEPRECATED keyPair "Use generic-lens or generic-optics with 'keyPair' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpcrrsResponseStatus :: Lens.Lens' CreateProvisioningClaimResponse Core.Int
cpcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cpcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

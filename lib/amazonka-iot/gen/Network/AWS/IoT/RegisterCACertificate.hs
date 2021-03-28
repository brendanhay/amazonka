{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.RegisterCACertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a CA certificate with AWS IoT. This CA certificate can then be used to sign device certificates, which can be then registered with AWS IoT. You can register up to 10 CA certificates per AWS account that have the same subject field. This enables you to have up to 10 certificate authorities sign your device certificates. If you have more than one CA certificate registered, make sure you pass the CA certificate when you register your device certificates with the RegisterCertificate API.
module Network.AWS.IoT.RegisterCACertificate
    (
    -- * Creating a request
      RegisterCACertificate (..)
    , mkRegisterCACertificate
    -- ** Request lenses
    , rcacCaCertificate
    , rcacVerificationCertificate
    , rcacAllowAutoRegistration
    , rcacRegistrationConfig
    , rcacSetAsActive
    , rcacTags

    -- * Destructuring the response
    , RegisterCACertificateResponse (..)
    , mkRegisterCACertificateResponse
    -- ** Response lenses
    , rcacrrsCertificateArn
    , rcacrrsCertificateId
    , rcacrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input to the RegisterCACertificate operation.
--
-- /See:/ 'mkRegisterCACertificate' smart constructor.
data RegisterCACertificate = RegisterCACertificate'
  { caCertificate :: Types.CertificatePem
    -- ^ The CA certificate.
  , verificationCertificate :: Types.CertificatePem
    -- ^ The private key verification certificate.
  , allowAutoRegistration :: Core.Maybe Core.Bool
    -- ^ Allows this CA certificate to be used for auto registration of device certificates.
  , registrationConfig :: Core.Maybe Types.RegistrationConfig
    -- ^ Information about the registration configuration.
  , setAsActive :: Core.Maybe Core.Bool
    -- ^ A boolean value that specifies if the CA certificate is set to active.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Metadata which can be used to manage the CA certificate.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterCACertificate' value with any optional fields omitted.
mkRegisterCACertificate
    :: Types.CertificatePem -- ^ 'caCertificate'
    -> Types.CertificatePem -- ^ 'verificationCertificate'
    -> RegisterCACertificate
mkRegisterCACertificate caCertificate verificationCertificate
  = RegisterCACertificate'{caCertificate, verificationCertificate,
                           allowAutoRegistration = Core.Nothing,
                           registrationConfig = Core.Nothing, setAsActive = Core.Nothing,
                           tags = Core.Nothing}

-- | The CA certificate.
--
-- /Note:/ Consider using 'caCertificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcacCaCertificate :: Lens.Lens' RegisterCACertificate Types.CertificatePem
rcacCaCertificate = Lens.field @"caCertificate"
{-# INLINEABLE rcacCaCertificate #-}
{-# DEPRECATED caCertificate "Use generic-lens or generic-optics with 'caCertificate' instead"  #-}

-- | The private key verification certificate.
--
-- /Note:/ Consider using 'verificationCertificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcacVerificationCertificate :: Lens.Lens' RegisterCACertificate Types.CertificatePem
rcacVerificationCertificate = Lens.field @"verificationCertificate"
{-# INLINEABLE rcacVerificationCertificate #-}
{-# DEPRECATED verificationCertificate "Use generic-lens or generic-optics with 'verificationCertificate' instead"  #-}

-- | Allows this CA certificate to be used for auto registration of device certificates.
--
-- /Note:/ Consider using 'allowAutoRegistration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcacAllowAutoRegistration :: Lens.Lens' RegisterCACertificate (Core.Maybe Core.Bool)
rcacAllowAutoRegistration = Lens.field @"allowAutoRegistration"
{-# INLINEABLE rcacAllowAutoRegistration #-}
{-# DEPRECATED allowAutoRegistration "Use generic-lens or generic-optics with 'allowAutoRegistration' instead"  #-}

-- | Information about the registration configuration.
--
-- /Note:/ Consider using 'registrationConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcacRegistrationConfig :: Lens.Lens' RegisterCACertificate (Core.Maybe Types.RegistrationConfig)
rcacRegistrationConfig = Lens.field @"registrationConfig"
{-# INLINEABLE rcacRegistrationConfig #-}
{-# DEPRECATED registrationConfig "Use generic-lens or generic-optics with 'registrationConfig' instead"  #-}

-- | A boolean value that specifies if the CA certificate is set to active.
--
-- /Note:/ Consider using 'setAsActive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcacSetAsActive :: Lens.Lens' RegisterCACertificate (Core.Maybe Core.Bool)
rcacSetAsActive = Lens.field @"setAsActive"
{-# INLINEABLE rcacSetAsActive #-}
{-# DEPRECATED setAsActive "Use generic-lens or generic-optics with 'setAsActive' instead"  #-}

-- | Metadata which can be used to manage the CA certificate.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcacTags :: Lens.Lens' RegisterCACertificate (Core.Maybe [Types.Tag])
rcacTags = Lens.field @"tags"
{-# INLINEABLE rcacTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery RegisterCACertificate where
        toQuery RegisterCACertificate{..}
          = Core.maybe Core.mempty (Core.toQueryPair "allowAutoRegistration")
              allowAutoRegistration
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "setAsActive") setAsActive

instance Core.ToHeaders RegisterCACertificate where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON RegisterCACertificate where
        toJSON RegisterCACertificate{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("caCertificate" Core..= caCertificate),
                  Core.Just
                    ("verificationCertificate" Core..= verificationCertificate),
                  ("registrationConfig" Core..=) Core.<$> registrationConfig,
                  ("tags" Core..=) Core.<$> tags])

instance Core.AWSRequest RegisterCACertificate where
        type Rs RegisterCACertificate = RegisterCACertificateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/cacertificate",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 RegisterCACertificateResponse' Core.<$>
                   (x Core..:? "certificateArn") Core.<*> x Core..:? "certificateId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The output from the RegisterCACertificateResponse operation.
--
-- /See:/ 'mkRegisterCACertificateResponse' smart constructor.
data RegisterCACertificateResponse = RegisterCACertificateResponse'
  { certificateArn :: Core.Maybe Types.CertificateArn
    -- ^ The CA certificate ARN.
  , certificateId :: Core.Maybe Types.CertificateId
    -- ^ The CA certificate identifier.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterCACertificateResponse' value with any optional fields omitted.
mkRegisterCACertificateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RegisterCACertificateResponse
mkRegisterCACertificateResponse responseStatus
  = RegisterCACertificateResponse'{certificateArn = Core.Nothing,
                                   certificateId = Core.Nothing, responseStatus}

-- | The CA certificate ARN.
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcacrrsCertificateArn :: Lens.Lens' RegisterCACertificateResponse (Core.Maybe Types.CertificateArn)
rcacrrsCertificateArn = Lens.field @"certificateArn"
{-# INLINEABLE rcacrrsCertificateArn #-}
{-# DEPRECATED certificateArn "Use generic-lens or generic-optics with 'certificateArn' instead"  #-}

-- | The CA certificate identifier.
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcacrrsCertificateId :: Lens.Lens' RegisterCACertificateResponse (Core.Maybe Types.CertificateId)
rcacrrsCertificateId = Lens.field @"certificateId"
{-# INLINEABLE rcacrrsCertificateId #-}
{-# DEPRECATED certificateId "Use generic-lens or generic-optics with 'certificateId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcacrrsResponseStatus :: Lens.Lens' RegisterCACertificateResponse Core.Int
rcacrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rcacrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

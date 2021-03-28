{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.RegisterCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a device certificate with AWS IoT. If you have more than one CA certificate that has the same subject field, you must specify the CA certificate that was used to sign the device certificate being registered.
module Network.AWS.IoT.RegisterCertificate
    (
    -- * Creating a request
      RegisterCertificate (..)
    , mkRegisterCertificate
    -- ** Request lenses
    , rcCertificatePem
    , rcCaCertificatePem
    , rcSetAsActive
    , rcStatus

    -- * Destructuring the response
    , RegisterCertificateResponse (..)
    , mkRegisterCertificateResponse
    -- ** Response lenses
    , rcrrsCertificateArn
    , rcrrsCertificateId
    , rcrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input to the RegisterCertificate operation.
--
-- /See:/ 'mkRegisterCertificate' smart constructor.
data RegisterCertificate = RegisterCertificate'
  { certificatePem :: Types.CertificatePem
    -- ^ The certificate data, in PEM format.
  , caCertificatePem :: Core.Maybe Types.CertificatePem
    -- ^ The CA certificate used to sign the device certificate being registered.
  , setAsActive :: Core.Maybe Core.Bool
    -- ^ A boolean value that specifies if the certificate is set to active.
  , status :: Core.Maybe Types.CertificateStatus
    -- ^ The status of the register certificate request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterCertificate' value with any optional fields omitted.
mkRegisterCertificate
    :: Types.CertificatePem -- ^ 'certificatePem'
    -> RegisterCertificate
mkRegisterCertificate certificatePem
  = RegisterCertificate'{certificatePem,
                         caCertificatePem = Core.Nothing, setAsActive = Core.Nothing,
                         status = Core.Nothing}

-- | The certificate data, in PEM format.
--
-- /Note:/ Consider using 'certificatePem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcCertificatePem :: Lens.Lens' RegisterCertificate Types.CertificatePem
rcCertificatePem = Lens.field @"certificatePem"
{-# INLINEABLE rcCertificatePem #-}
{-# DEPRECATED certificatePem "Use generic-lens or generic-optics with 'certificatePem' instead"  #-}

-- | The CA certificate used to sign the device certificate being registered.
--
-- /Note:/ Consider using 'caCertificatePem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcCaCertificatePem :: Lens.Lens' RegisterCertificate (Core.Maybe Types.CertificatePem)
rcCaCertificatePem = Lens.field @"caCertificatePem"
{-# INLINEABLE rcCaCertificatePem #-}
{-# DEPRECATED caCertificatePem "Use generic-lens or generic-optics with 'caCertificatePem' instead"  #-}

-- | A boolean value that specifies if the certificate is set to active.
--
-- /Note:/ Consider using 'setAsActive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcSetAsActive :: Lens.Lens' RegisterCertificate (Core.Maybe Core.Bool)
rcSetAsActive = Lens.field @"setAsActive"
{-# INLINEABLE rcSetAsActive #-}
{-# DEPRECATED setAsActive "Use generic-lens or generic-optics with 'setAsActive' instead"  #-}

-- | The status of the register certificate request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcStatus :: Lens.Lens' RegisterCertificate (Core.Maybe Types.CertificateStatus)
rcStatus = Lens.field @"status"
{-# INLINEABLE rcStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.ToQuery RegisterCertificate where
        toQuery RegisterCertificate{..}
          = Core.maybe Core.mempty (Core.toQueryPair "setAsActive")
              setAsActive

instance Core.ToHeaders RegisterCertificate where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON RegisterCertificate where
        toJSON RegisterCertificate{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("certificatePem" Core..= certificatePem),
                  ("caCertificatePem" Core..=) Core.<$> caCertificatePem,
                  ("status" Core..=) Core.<$> status])

instance Core.AWSRequest RegisterCertificate where
        type Rs RegisterCertificate = RegisterCertificateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/certificate/register",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 RegisterCertificateResponse' Core.<$>
                   (x Core..:? "certificateArn") Core.<*> x Core..:? "certificateId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The output from the RegisterCertificate operation.
--
-- /See:/ 'mkRegisterCertificateResponse' smart constructor.
data RegisterCertificateResponse = RegisterCertificateResponse'
  { certificateArn :: Core.Maybe Types.CertificateArn
    -- ^ The certificate ARN.
  , certificateId :: Core.Maybe Types.CertificateId
    -- ^ The certificate identifier.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterCertificateResponse' value with any optional fields omitted.
mkRegisterCertificateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RegisterCertificateResponse
mkRegisterCertificateResponse responseStatus
  = RegisterCertificateResponse'{certificateArn = Core.Nothing,
                                 certificateId = Core.Nothing, responseStatus}

-- | The certificate ARN.
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrrsCertificateArn :: Lens.Lens' RegisterCertificateResponse (Core.Maybe Types.CertificateArn)
rcrrsCertificateArn = Lens.field @"certificateArn"
{-# INLINEABLE rcrrsCertificateArn #-}
{-# DEPRECATED certificateArn "Use generic-lens or generic-optics with 'certificateArn' instead"  #-}

-- | The certificate identifier.
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrrsCertificateId :: Lens.Lens' RegisterCertificateResponse (Core.Maybe Types.CertificateId)
rcrrsCertificateId = Lens.field @"certificateId"
{-# INLINEABLE rcrrsCertificateId #-}
{-# DEPRECATED certificateId "Use generic-lens or generic-optics with 'certificateId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrrsResponseStatus :: Lens.Lens' RegisterCertificateResponse Core.Int
rcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

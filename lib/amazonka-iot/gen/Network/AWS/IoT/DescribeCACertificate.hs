{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeCACertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a registered CA certificate.
module Network.AWS.IoT.DescribeCACertificate
    (
    -- * Creating a request
      DescribeCACertificate (..)
    , mkDescribeCACertificate
    -- ** Request lenses
    , dCertificateId

    -- * Destructuring the response
    , DescribeCACertificateResponse (..)
    , mkDescribeCACertificateResponse
    -- ** Response lenses
    , dcacrfrsCertificateDescription
    , dcacrfrsRegistrationConfig
    , dcacrfrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DescribeCACertificate operation.
--
-- /See:/ 'mkDescribeCACertificate' smart constructor.
newtype DescribeCACertificate = DescribeCACertificate'
  { certificateId :: Types.CertificateId
    -- ^ The CA certificate identifier.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCACertificate' value with any optional fields omitted.
mkDescribeCACertificate
    :: Types.CertificateId -- ^ 'certificateId'
    -> DescribeCACertificate
mkDescribeCACertificate certificateId
  = DescribeCACertificate'{certificateId}

-- | The CA certificate identifier.
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCertificateId :: Lens.Lens' DescribeCACertificate Types.CertificateId
dCertificateId = Lens.field @"certificateId"
{-# INLINEABLE dCertificateId #-}
{-# DEPRECATED certificateId "Use generic-lens or generic-optics with 'certificateId' instead"  #-}

instance Core.ToQuery DescribeCACertificate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeCACertificate where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeCACertificate where
        type Rs DescribeCACertificate = DescribeCACertificateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/cacertificate/" Core.<> Core.toText certificateId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeCACertificateResponse' Core.<$>
                   (x Core..:? "certificateDescription") Core.<*>
                     x Core..:? "registrationConfig"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The output from the DescribeCACertificate operation.
--
-- /See:/ 'mkDescribeCACertificateResponse' smart constructor.
data DescribeCACertificateResponse = DescribeCACertificateResponse'
  { certificateDescription :: Core.Maybe Types.CACertificateDescription
    -- ^ The CA certificate description.
  , registrationConfig :: Core.Maybe Types.RegistrationConfig
    -- ^ Information about the registration configuration.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeCACertificateResponse' value with any optional fields omitted.
mkDescribeCACertificateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeCACertificateResponse
mkDescribeCACertificateResponse responseStatus
  = DescribeCACertificateResponse'{certificateDescription =
                                     Core.Nothing,
                                   registrationConfig = Core.Nothing, responseStatus}

-- | The CA certificate description.
--
-- /Note:/ Consider using 'certificateDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcacrfrsCertificateDescription :: Lens.Lens' DescribeCACertificateResponse (Core.Maybe Types.CACertificateDescription)
dcacrfrsCertificateDescription = Lens.field @"certificateDescription"
{-# INLINEABLE dcacrfrsCertificateDescription #-}
{-# DEPRECATED certificateDescription "Use generic-lens or generic-optics with 'certificateDescription' instead"  #-}

-- | Information about the registration configuration.
--
-- /Note:/ Consider using 'registrationConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcacrfrsRegistrationConfig :: Lens.Lens' DescribeCACertificateResponse (Core.Maybe Types.RegistrationConfig)
dcacrfrsRegistrationConfig = Lens.field @"registrationConfig"
{-# INLINEABLE dcacrfrsRegistrationConfig #-}
{-# DEPRECATED registrationConfig "Use generic-lens or generic-optics with 'registrationConfig' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcacrfrsResponseStatus :: Lens.Lens' DescribeCACertificateResponse Core.Int
dcacrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcacrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

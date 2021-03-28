{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified certificate.
module Network.AWS.IoT.DescribeCertificate
    (
    -- * Creating a request
      DescribeCertificate (..)
    , mkDescribeCertificate
    -- ** Request lenses
    , dcfCertificateId

    -- * Destructuring the response
    , DescribeCertificateResponse (..)
    , mkDescribeCertificateResponse
    -- ** Response lenses
    , dcrrsCertificateDescription
    , dcrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DescribeCertificate operation.
--
-- /See:/ 'mkDescribeCertificate' smart constructor.
newtype DescribeCertificate = DescribeCertificate'
  { certificateId :: Types.CertificateId
    -- ^ The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCertificate' value with any optional fields omitted.
mkDescribeCertificate
    :: Types.CertificateId -- ^ 'certificateId'
    -> DescribeCertificate
mkDescribeCertificate certificateId
  = DescribeCertificate'{certificateId}

-- | The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfCertificateId :: Lens.Lens' DescribeCertificate Types.CertificateId
dcfCertificateId = Lens.field @"certificateId"
{-# INLINEABLE dcfCertificateId #-}
{-# DEPRECATED certificateId "Use generic-lens or generic-optics with 'certificateId' instead"  #-}

instance Core.ToQuery DescribeCertificate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeCertificate where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeCertificate where
        type Rs DescribeCertificate = DescribeCertificateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/certificates/" Core.<> Core.toText certificateId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeCertificateResponse' Core.<$>
                   (x Core..:? "certificateDescription") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The output of the DescribeCertificate operation.
--
-- /See:/ 'mkDescribeCertificateResponse' smart constructor.
data DescribeCertificateResponse = DescribeCertificateResponse'
  { certificateDescription :: Core.Maybe Types.CertificateDescription
    -- ^ The description of the certificate.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeCertificateResponse' value with any optional fields omitted.
mkDescribeCertificateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeCertificateResponse
mkDescribeCertificateResponse responseStatus
  = DescribeCertificateResponse'{certificateDescription =
                                   Core.Nothing,
                                 responseStatus}

-- | The description of the certificate.
--
-- /Note:/ Consider using 'certificateDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsCertificateDescription :: Lens.Lens' DescribeCertificateResponse (Core.Maybe Types.CertificateDescription)
dcrrsCertificateDescription = Lens.field @"certificateDescription"
{-# INLINEABLE dcrrsCertificateDescription #-}
{-# DEPRECATED certificateDescription "Use generic-lens or generic-optics with 'certificateDescription' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsResponseStatus :: Lens.Lens' DescribeCertificateResponse Core.Int
dcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

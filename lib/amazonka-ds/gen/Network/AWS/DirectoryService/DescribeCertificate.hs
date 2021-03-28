{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DescribeCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays information about the certificate registered for a secured LDAP connection.
module Network.AWS.DirectoryService.DescribeCertificate
    (
    -- * Creating a request
      DescribeCertificate (..)
    , mkDescribeCertificate
    -- ** Request lenses
    , dchDirectoryId
    , dchCertificateId

    -- * Destructuring the response
    , DescribeCertificateResponse (..)
    , mkDescribeCertificateResponse
    -- ** Response lenses
    , dcrfrsCertificate
    , dcrfrsResponseStatus
    ) where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeCertificate' smart constructor.
data DescribeCertificate = DescribeCertificate'
  { directoryId :: Types.DirectoryId
    -- ^ The identifier of the directory.
  , certificateId :: Types.CertificateId
    -- ^ The identifier of the certificate.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCertificate' value with any optional fields omitted.
mkDescribeCertificate
    :: Types.DirectoryId -- ^ 'directoryId'
    -> Types.CertificateId -- ^ 'certificateId'
    -> DescribeCertificate
mkDescribeCertificate directoryId certificateId
  = DescribeCertificate'{directoryId, certificateId}

-- | The identifier of the directory.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dchDirectoryId :: Lens.Lens' DescribeCertificate Types.DirectoryId
dchDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE dchDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | The identifier of the certificate.
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dchCertificateId :: Lens.Lens' DescribeCertificate Types.CertificateId
dchCertificateId = Lens.field @"certificateId"
{-# INLINEABLE dchCertificateId #-}
{-# DEPRECATED certificateId "Use generic-lens or generic-optics with 'certificateId' instead"  #-}

instance Core.ToQuery DescribeCertificate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeCertificate where
        toHeaders DescribeCertificate{..}
          = Core.pure
              ("X-Amz-Target", "DirectoryService_20150416.DescribeCertificate")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeCertificate where
        toJSON DescribeCertificate{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DirectoryId" Core..= directoryId),
                  Core.Just ("CertificateId" Core..= certificateId)])

instance Core.AWSRequest DescribeCertificate where
        type Rs DescribeCertificate = DescribeCertificateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeCertificateResponse' Core.<$>
                   (x Core..:? "Certificate") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeCertificateResponse' smart constructor.
data DescribeCertificateResponse = DescribeCertificateResponse'
  { certificate :: Core.Maybe Types.Certificate
    -- ^ Information about the certificate, including registered date time, certificate state, the reason for the state, expiration date time, and certificate common name.
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
  = DescribeCertificateResponse'{certificate = Core.Nothing,
                                 responseStatus}

-- | Information about the certificate, including registered date time, certificate state, the reason for the state, expiration date time, and certificate common name.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrfrsCertificate :: Lens.Lens' DescribeCertificateResponse (Core.Maybe Types.Certificate)
dcrfrsCertificate = Lens.field @"certificate"
{-# INLINEABLE dcrfrsCertificate #-}
{-# DEPRECATED certificate "Use generic-lens or generic-optics with 'certificate' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrfrsResponseStatus :: Lens.Lens' DescribeCertificateResponse Core.Int
dcrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

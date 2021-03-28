{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.ImportCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads the specified certificate.
module Network.AWS.DMS.ImportCertificate
    (
    -- * Creating a request
      ImportCertificate (..)
    , mkImportCertificate
    -- ** Request lenses
    , icCertificateIdentifier
    , icCertificatePem
    , icCertificateWallet
    , icTags

    -- * Destructuring the response
    , ImportCertificateResponse (..)
    , mkImportCertificateResponse
    -- ** Response lenses
    , icrrsCertificate
    , icrrsResponseStatus
    ) where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkImportCertificate' smart constructor.
data ImportCertificate = ImportCertificate'
  { certificateIdentifier :: Core.Text
    -- ^ A customer-assigned name for the certificate. Identifiers must begin with a letter and must contain only ASCII letters, digits, and hyphens. They can't end with a hyphen or contain two consecutive hyphens.
  , certificatePem :: Core.Maybe Core.Text
    -- ^ The contents of a @.pem@ file, which contains an X.509 certificate.
  , certificateWallet :: Core.Maybe Core.Base64
    -- ^ The location of an imported Oracle Wallet certificate for use with SSL.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags associated with the certificate.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportCertificate' value with any optional fields omitted.
mkImportCertificate
    :: Core.Text -- ^ 'certificateIdentifier'
    -> ImportCertificate
mkImportCertificate certificateIdentifier
  = ImportCertificate'{certificateIdentifier,
                       certificatePem = Core.Nothing, certificateWallet = Core.Nothing,
                       tags = Core.Nothing}

-- | A customer-assigned name for the certificate. Identifiers must begin with a letter and must contain only ASCII letters, digits, and hyphens. They can't end with a hyphen or contain two consecutive hyphens.
--
-- /Note:/ Consider using 'certificateIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icCertificateIdentifier :: Lens.Lens' ImportCertificate Core.Text
icCertificateIdentifier = Lens.field @"certificateIdentifier"
{-# INLINEABLE icCertificateIdentifier #-}
{-# DEPRECATED certificateIdentifier "Use generic-lens or generic-optics with 'certificateIdentifier' instead"  #-}

-- | The contents of a @.pem@ file, which contains an X.509 certificate.
--
-- /Note:/ Consider using 'certificatePem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icCertificatePem :: Lens.Lens' ImportCertificate (Core.Maybe Core.Text)
icCertificatePem = Lens.field @"certificatePem"
{-# INLINEABLE icCertificatePem #-}
{-# DEPRECATED certificatePem "Use generic-lens or generic-optics with 'certificatePem' instead"  #-}

-- | The location of an imported Oracle Wallet certificate for use with SSL.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'certificateWallet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icCertificateWallet :: Lens.Lens' ImportCertificate (Core.Maybe Core.Base64)
icCertificateWallet = Lens.field @"certificateWallet"
{-# INLINEABLE icCertificateWallet #-}
{-# DEPRECATED certificateWallet "Use generic-lens or generic-optics with 'certificateWallet' instead"  #-}

-- | The tags associated with the certificate.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icTags :: Lens.Lens' ImportCertificate (Core.Maybe [Types.Tag])
icTags = Lens.field @"tags"
{-# INLINEABLE icTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery ImportCertificate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ImportCertificate where
        toHeaders ImportCertificate{..}
          = Core.pure
              ("X-Amz-Target", "AmazonDMSv20160101.ImportCertificate")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ImportCertificate where
        toJSON ImportCertificate{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("CertificateIdentifier" Core..= certificateIdentifier),
                  ("CertificatePem" Core..=) Core.<$> certificatePem,
                  ("CertificateWallet" Core..=) Core.<$> certificateWallet,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest ImportCertificate where
        type Rs ImportCertificate = ImportCertificateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ImportCertificateResponse' Core.<$>
                   (x Core..:? "Certificate") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkImportCertificateResponse' smart constructor.
data ImportCertificateResponse = ImportCertificateResponse'
  { certificate :: Core.Maybe Types.Certificate
    -- ^ The certificate to be uploaded.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ImportCertificateResponse' value with any optional fields omitted.
mkImportCertificateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ImportCertificateResponse
mkImportCertificateResponse responseStatus
  = ImportCertificateResponse'{certificate = Core.Nothing,
                               responseStatus}

-- | The certificate to be uploaded.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icrrsCertificate :: Lens.Lens' ImportCertificateResponse (Core.Maybe Types.Certificate)
icrrsCertificate = Lens.field @"certificate"
{-# INLINEABLE icrrsCertificate #-}
{-# DEPRECATED certificate "Use generic-lens or generic-optics with 'certificate' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icrrsResponseStatus :: Lens.Lens' ImportCertificateResponse Core.Int
icrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE icrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

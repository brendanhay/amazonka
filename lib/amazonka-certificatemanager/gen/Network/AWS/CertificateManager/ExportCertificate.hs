{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.ExportCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports a private certificate issued by a private certificate authority (CA) for use anywhere. The exported file contains the certificate, the certificate chain, and the encrypted private 2048-bit RSA key associated with the public key that is embedded in the certificate. For security, you must assign a passphrase for the private key when exporting it. 
--
-- For information about exporting and formatting a certificate using the ACM console or CLI, see <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-export-private.html Export a Private Certificate> .
module Network.AWS.CertificateManager.ExportCertificate
    (
    -- * Creating a request
      ExportCertificate (..)
    , mkExportCertificate
    -- ** Request lenses
    , ecCertificateArn
    , ecPassphrase

    -- * Destructuring the response
    , ExportCertificateResponse (..)
    , mkExportCertificateResponse
    -- ** Response lenses
    , ecrrsCertificate
    , ecrrsCertificateChain
    , ecrrsPrivateKey
    , ecrrsResponseStatus
    ) where

import qualified Network.AWS.CertificateManager.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkExportCertificate' smart constructor.
data ExportCertificate = ExportCertificate'
  { certificateArn :: Types.Arn
    -- ^ An Amazon Resource Name (ARN) of the issued certificate. This must be of the form:
--
-- @arn:aws:acm:region:account:certificate/12345678-1234-1234-1234-123456789012@ 
  , passphrase :: Core.Sensitive Core.Base64
    -- ^ Passphrase to associate with the encrypted exported private key. If you want to later decrypt the private key, you must have the passphrase. You can use the following OpenSSL command to decrypt a private key: 
--
-- @openssl rsa -in encrypted_key.pem -out decrypted_key.pem@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExportCertificate' value with any optional fields omitted.
mkExportCertificate
    :: Types.Arn -- ^ 'certificateArn'
    -> Core.Sensitive Core.Base64 -- ^ 'passphrase'
    -> ExportCertificate
mkExportCertificate certificateArn passphrase
  = ExportCertificate'{certificateArn, passphrase}

-- | An Amazon Resource Name (ARN) of the issued certificate. This must be of the form:
--
-- @arn:aws:acm:region:account:certificate/12345678-1234-1234-1234-123456789012@ 
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecCertificateArn :: Lens.Lens' ExportCertificate Types.Arn
ecCertificateArn = Lens.field @"certificateArn"
{-# INLINEABLE ecCertificateArn #-}
{-# DEPRECATED certificateArn "Use generic-lens or generic-optics with 'certificateArn' instead"  #-}

-- | Passphrase to associate with the encrypted exported private key. If you want to later decrypt the private key, you must have the passphrase. You can use the following OpenSSL command to decrypt a private key: 
--
-- @openssl rsa -in encrypted_key.pem -out decrypted_key.pem@ --
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'passphrase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecPassphrase :: Lens.Lens' ExportCertificate (Core.Sensitive Core.Base64)
ecPassphrase = Lens.field @"passphrase"
{-# INLINEABLE ecPassphrase #-}
{-# DEPRECATED passphrase "Use generic-lens or generic-optics with 'passphrase' instead"  #-}

instance Core.ToQuery ExportCertificate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ExportCertificate where
        toHeaders ExportCertificate{..}
          = Core.pure
              ("X-Amz-Target", "CertificateManager.ExportCertificate")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ExportCertificate where
        toJSON ExportCertificate{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("CertificateArn" Core..= certificateArn),
                  Core.Just ("Passphrase" Core..= passphrase)])

instance Core.AWSRequest ExportCertificate where
        type Rs ExportCertificate = ExportCertificateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ExportCertificateResponse' Core.<$>
                   (x Core..:? "Certificate") Core.<*> x Core..:? "CertificateChain"
                     Core.<*> x Core..:? "PrivateKey"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkExportCertificateResponse' smart constructor.
data ExportCertificateResponse = ExportCertificateResponse'
  { certificate :: Core.Maybe Types.Certificate
    -- ^ The base64 PEM-encoded certificate.
  , certificateChain :: Core.Maybe Types.CertificateChain
    -- ^ The base64 PEM-encoded certificate chain. This does not include the certificate that you are exporting.
  , privateKey :: Core.Maybe Types.PrivateKey
    -- ^ The encrypted private key associated with the public key in the certificate. The key is output in PKCS #8 format and is base64 PEM-encoded. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExportCertificateResponse' value with any optional fields omitted.
mkExportCertificateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ExportCertificateResponse
mkExportCertificateResponse responseStatus
  = ExportCertificateResponse'{certificate = Core.Nothing,
                               certificateChain = Core.Nothing, privateKey = Core.Nothing,
                               responseStatus}

-- | The base64 PEM-encoded certificate.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrrsCertificate :: Lens.Lens' ExportCertificateResponse (Core.Maybe Types.Certificate)
ecrrsCertificate = Lens.field @"certificate"
{-# INLINEABLE ecrrsCertificate #-}
{-# DEPRECATED certificate "Use generic-lens or generic-optics with 'certificate' instead"  #-}

-- | The base64 PEM-encoded certificate chain. This does not include the certificate that you are exporting.
--
-- /Note:/ Consider using 'certificateChain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrrsCertificateChain :: Lens.Lens' ExportCertificateResponse (Core.Maybe Types.CertificateChain)
ecrrsCertificateChain = Lens.field @"certificateChain"
{-# INLINEABLE ecrrsCertificateChain #-}
{-# DEPRECATED certificateChain "Use generic-lens or generic-optics with 'certificateChain' instead"  #-}

-- | The encrypted private key associated with the public key in the certificate. The key is output in PKCS #8 format and is base64 PEM-encoded. 
--
-- /Note:/ Consider using 'privateKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrrsPrivateKey :: Lens.Lens' ExportCertificateResponse (Core.Maybe Types.PrivateKey)
ecrrsPrivateKey = Lens.field @"privateKey"
{-# INLINEABLE ecrrsPrivateKey #-}
{-# DEPRECATED privateKey "Use generic-lens or generic-optics with 'privateKey' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrrsResponseStatus :: Lens.Lens' ExportCertificateResponse Core.Int
ecrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ecrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

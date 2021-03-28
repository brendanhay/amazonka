{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.ImportCertificateAuthorityCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports a signed private CA certificate into ACM Private CA. This action is used when you are using a chain of trust whose root is located outside ACM Private CA. Before you can call this action, the following preparations must in place:
--
--
--     * In ACM Private CA, call the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> action to create the private CA that that you plan to back with the imported certificate.
--
--
--     * Call the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_GetCertificateAuthorityCsr.html GetCertificateAuthorityCsr> action to generate a certificate signing request (CSR).
--
--
--     * Sign the CSR using a root or intermediate CA hosted by either an on-premises PKI hierarchy or by a commercial CA.
--
--
--     * Create a certificate chain and copy the signed certificate and the certificate chain to your working directory.
--
--
-- The following requirements apply when you import a CA certificate.
--
--     * You cannot import a non-self-signed certificate for use as a root CA.
--
--
--     * You cannot import a self-signed certificate for use as a subordinate CA.
--
--
--     * Your certificate chain must not include the private CA certificate that you are importing.
--
--
--     * Your ACM Private CA-hosted or on-premises CA certificate must be the last certificate in your chain. The subordinate certificate, if any, that your root CA signed must be next to last. The subordinate certificate signed by the preceding subordinate CA must come next, and so on until your chain is built. 
--
--
--     * The chain must be PEM-encoded.
--
--
--     * The maximum allowed size of a certificate is 32 KB.
--
--
--     * The maximum allowed size of a certificate chain is 2 MB.
--
--
-- /Enforcement of Critical Constraints/ 
-- ACM Private CA allows the following extensions to be marked critical in the imported CA certificate or chain.
--
--     * Basic constraints (/must/ be marked critical)
--
--
--     * Subject alternative names
--
--
--     * Key usage
--
--
--     * Extended key usage
--
--
--     * Authority key identifier
--
--
--     * Subject key identifier
--
--
--     * Issuer alternative name
--
--
--     * Subject directory attributes
--
--
--     * Subject information access
--
--
--     * Certificate policies
--
--
--     * Policy mappings
--
--
--     * Inhibit anyPolicy
--
--
-- ACM Private CA rejects the following extensions when they are marked critical in an imported CA certificate or chain.
--
--     * Name constraints
--
--
--     * Policy constraints
--
--
--     * CRL distribution points
--
--
--     * Authority information access
--
--
--     * Freshest CRL
--
--
--     * Any other extension
--
--
module Network.AWS.CertificateManagerPCA.ImportCertificateAuthorityCertificate
    (
    -- * Creating a request
      ImportCertificateAuthorityCertificate (..)
    , mkImportCertificateAuthorityCertificate
    -- ** Request lenses
    , icacCertificateAuthorityArn
    , icacCertificate
    , icacCertificateChain

    -- * Destructuring the response
    , ImportCertificateAuthorityCertificateResponse (..)
    , mkImportCertificateAuthorityCertificateResponse
    ) where

import qualified Network.AWS.CertificateManagerPCA.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkImportCertificateAuthorityCertificate' smart constructor.
data ImportCertificateAuthorityCertificate = ImportCertificateAuthorityCertificate'
  { certificateAuthorityArn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) that was returned when you called <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> . This must be of the form: 
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ 
  , certificate :: Core.Base64
    -- ^ The PEM-encoded certificate for a private CA. This may be a self-signed certificate in the case of a root CA, or it may be signed by another CA that you control.
  , certificateChain :: Core.Maybe Core.Base64
    -- ^ A PEM-encoded file that contains all of your certificates, other than the certificate you're importing, chaining up to your root CA. Your ACM Private CA-hosted or on-premises root certificate is the last in the chain, and each certificate in the chain signs the one preceding. 
--
-- This parameter must be supplied when you import a subordinate CA. When you import a root CA, there is no chain.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportCertificateAuthorityCertificate' value with any optional fields omitted.
mkImportCertificateAuthorityCertificate
    :: Types.Arn -- ^ 'certificateAuthorityArn'
    -> Core.Base64 -- ^ 'certificate'
    -> ImportCertificateAuthorityCertificate
mkImportCertificateAuthorityCertificate certificateAuthorityArn
  certificate
  = ImportCertificateAuthorityCertificate'{certificateAuthorityArn,
                                           certificate, certificateChain = Core.Nothing}

-- | The Amazon Resource Name (ARN) that was returned when you called <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> . This must be of the form: 
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ 
--
-- /Note:/ Consider using 'certificateAuthorityArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icacCertificateAuthorityArn :: Lens.Lens' ImportCertificateAuthorityCertificate Types.Arn
icacCertificateAuthorityArn = Lens.field @"certificateAuthorityArn"
{-# INLINEABLE icacCertificateAuthorityArn #-}
{-# DEPRECATED certificateAuthorityArn "Use generic-lens or generic-optics with 'certificateAuthorityArn' instead"  #-}

-- | The PEM-encoded certificate for a private CA. This may be a self-signed certificate in the case of a root CA, or it may be signed by another CA that you control.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icacCertificate :: Lens.Lens' ImportCertificateAuthorityCertificate Core.Base64
icacCertificate = Lens.field @"certificate"
{-# INLINEABLE icacCertificate #-}
{-# DEPRECATED certificate "Use generic-lens or generic-optics with 'certificate' instead"  #-}

-- | A PEM-encoded file that contains all of your certificates, other than the certificate you're importing, chaining up to your root CA. Your ACM Private CA-hosted or on-premises root certificate is the last in the chain, and each certificate in the chain signs the one preceding. 
--
-- This parameter must be supplied when you import a subordinate CA. When you import a root CA, there is no chain.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'certificateChain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icacCertificateChain :: Lens.Lens' ImportCertificateAuthorityCertificate (Core.Maybe Core.Base64)
icacCertificateChain = Lens.field @"certificateChain"
{-# INLINEABLE icacCertificateChain #-}
{-# DEPRECATED certificateChain "Use generic-lens or generic-optics with 'certificateChain' instead"  #-}

instance Core.ToQuery ImportCertificateAuthorityCertificate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ImportCertificateAuthorityCertificate where
        toHeaders ImportCertificateAuthorityCertificate{..}
          = Core.pure
              ("X-Amz-Target",
               "ACMPrivateCA.ImportCertificateAuthorityCertificate")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ImportCertificateAuthorityCertificate where
        toJSON ImportCertificateAuthorityCertificate{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("CertificateAuthorityArn" Core..= certificateAuthorityArn),
                  Core.Just ("Certificate" Core..= certificate),
                  ("CertificateChain" Core..=) Core.<$> certificateChain])

instance Core.AWSRequest ImportCertificateAuthorityCertificate
         where
        type Rs ImportCertificateAuthorityCertificate =
             ImportCertificateAuthorityCertificateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull
              ImportCertificateAuthorityCertificateResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkImportCertificateAuthorityCertificateResponse' smart constructor.
data ImportCertificateAuthorityCertificateResponse = ImportCertificateAuthorityCertificateResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportCertificateAuthorityCertificateResponse' value with any optional fields omitted.
mkImportCertificateAuthorityCertificateResponse
    :: ImportCertificateAuthorityCertificateResponse
mkImportCertificateAuthorityCertificateResponse
  = ImportCertificateAuthorityCertificateResponse'

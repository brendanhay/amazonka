{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.IssueCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uses your private certificate authority (CA), or one that has been shared with you, to issue a client certificate. This action returns the Amazon Resource Name (ARN) of the certificate. You can retrieve the certificate by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_GetCertificate.html GetCertificate> action and specifying the ARN. 
module Network.AWS.CertificateManagerPCA.IssueCertificate
    (
    -- * Creating a request
      IssueCertificate (..)
    , mkIssueCertificate
    -- ** Request lenses
    , icCertificateAuthorityArn
    , icCsr
    , icSigningAlgorithm
    , icValidity
    , icIdempotencyToken
    , icTemplateArn

    -- * Destructuring the response
    , IssueCertificateResponse (..)
    , mkIssueCertificateResponse
    -- ** Response lenses
    , icrrsCertificateArn
    , icrrsResponseStatus
    ) where

import qualified Network.AWS.CertificateManagerPCA.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkIssueCertificate' smart constructor.
data IssueCertificate = IssueCertificate'
  { certificateAuthorityArn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) that was returned when you called <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> . This must be of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ 
  , csr :: Core.Base64
    -- ^ The certificate signing request (CSR) for the certificate you want to issue. You can use the following OpenSSL command to create the CSR and a 2048 bit RSA private key. 
--
-- @openssl req -new -newkey rsa:2048 -days 365 -keyout private/test_cert_priv_key.pem -out csr/test_cert_.csr@ 
-- If you have a configuration file, you can use the following OpenSSL command. The @usr_cert@ block in the configuration file contains your X509 version 3 extensions. 
-- @openssl req -new -config openssl_rsa.cnf -extensions usr_cert -newkey rsa:2048 -days -365 -keyout private/test_cert_priv_key.pem -out csr/test_cert_.csr@ 
-- Note: A CSR must provide either a /subject name/ or a /subject alternative name/ or the request will be rejected. 
  , signingAlgorithm :: Types.SigningAlgorithm
    -- ^ The name of the algorithm that will be used to sign the certificate to be issued. 
--
-- This parameter should not be confused with the @SigningAlgorithm@ parameter used to sign a CSR.
  , validity :: Types.Validity
    -- ^ Information describing the validity period of the certificate.
--
-- When issuing a certificate, ACM Private CA sets the "Not Before" date in the validity field to date and time minus 60 minutes. This is intended to compensate for time inconsistencies across systems of 60 minutes or less. 
-- The validity period configured on a certificate must not exceed the limit set by its parents in the CA hierarchy.
  , idempotencyToken :: Core.Maybe Types.IdempotencyToken
    -- ^ Custom string that can be used to distinguish between calls to the __IssueCertificate__ action. Idempotency tokens time out after one hour. Therefore, if you call __IssueCertificate__ multiple times with the same idempotency token within 5 minutes, ACM Private CA recognizes that you are requesting only one certificate and will issue only one. If you change the idempotency token for each call, PCA recognizes that you are requesting multiple certificates.
  , templateArn :: Core.Maybe Types.Arn
    -- ^ Specifies a custom configuration template to use when issuing a certificate. If this parameter is not provided, ACM Private CA defaults to the @EndEntityCertificate/V1@ template. For CA certificates, you should choose the shortest path length that meets your needs. The path length is indicated by the PathLen/N/ portion of the ARN, where /N/ is the <https://docs.aws.amazon.com/acm-pca/latest/userguide/PcaTerms.html#terms-cadepth CA depth> .
--
-- Note: The CA depth configured on a subordinate CA certificate must not exceed the limit set by its parents in the CA hierarchy.
-- The following service-owned @TemplateArn@ values are supported by ACM Private CA: 
--
--     * arn:aws:acm-pca:::template/CodeSigningCertificate/V1
--
--
--     * arn:aws:acm-pca:::template/CodeSigningCertificate_CSRPassthrough/V1
--
--
--     * arn:aws:acm-pca:::template/EndEntityCertificate/V1
--
--
--     * arn:aws:acm-pca:::template/EndEntityCertificate_CSRPassthrough/V1
--
--
--     * arn:aws:acm-pca:::template/EndEntityClientAuthCertificate/V1
--
--
--     * arn:aws:acm-pca:::template/EndEntityClientAuthCertificate_CSRPassthrough/V1
--
--
--     * arn:aws:acm-pca:::template/EndEntityServerAuthCertificate/V1
--
--
--     * arn:aws:acm-pca:::template/EndEntityServerAuthCertificate_CSRPassthrough/V1
--
--
--     * arn:aws:acm-pca:::template/OCSPSigningCertificate/V1
--
--
--     * arn:aws:acm-pca:::template/OCSPSigningCertificate_CSRPassthrough/V1
--
--
--     * arn:aws:acm-pca:::template/RootCACertificate/V1
--
--
--     * arn:aws:acm-pca:::template/SubordinateCACertificate_PathLen0/V1
--
--
--     * arn:aws:acm-pca:::template/SubordinateCACertificate_PathLen1/V1
--
--
--     * arn:aws:acm-pca:::template/SubordinateCACertificate_PathLen2/V1
--
--
--     * arn:aws:acm-pca:::template/SubordinateCACertificate_PathLen3/V1
--
--
-- For more information, see <https://docs.aws.amazon.com/acm-pca/latest/userguide/UsingTemplates.html Using Templates> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IssueCertificate' value with any optional fields omitted.
mkIssueCertificate
    :: Types.Arn -- ^ 'certificateAuthorityArn'
    -> Core.Base64 -- ^ 'csr'
    -> Types.SigningAlgorithm -- ^ 'signingAlgorithm'
    -> Types.Validity -- ^ 'validity'
    -> IssueCertificate
mkIssueCertificate certificateAuthorityArn csr signingAlgorithm
  validity
  = IssueCertificate'{certificateAuthorityArn, csr, signingAlgorithm,
                      validity, idempotencyToken = Core.Nothing,
                      templateArn = Core.Nothing}

-- | The Amazon Resource Name (ARN) that was returned when you called <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> . This must be of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ 
--
-- /Note:/ Consider using 'certificateAuthorityArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icCertificateAuthorityArn :: Lens.Lens' IssueCertificate Types.Arn
icCertificateAuthorityArn = Lens.field @"certificateAuthorityArn"
{-# INLINEABLE icCertificateAuthorityArn #-}
{-# DEPRECATED certificateAuthorityArn "Use generic-lens or generic-optics with 'certificateAuthorityArn' instead"  #-}

-- | The certificate signing request (CSR) for the certificate you want to issue. You can use the following OpenSSL command to create the CSR and a 2048 bit RSA private key. 
--
-- @openssl req -new -newkey rsa:2048 -days 365 -keyout private/test_cert_priv_key.pem -out csr/test_cert_.csr@ 
-- If you have a configuration file, you can use the following OpenSSL command. The @usr_cert@ block in the configuration file contains your X509 version 3 extensions. 
-- @openssl req -new -config openssl_rsa.cnf -extensions usr_cert -newkey rsa:2048 -days -365 -keyout private/test_cert_priv_key.pem -out csr/test_cert_.csr@ 
-- Note: A CSR must provide either a /subject name/ or a /subject alternative name/ or the request will be rejected. --
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'csr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icCsr :: Lens.Lens' IssueCertificate Core.Base64
icCsr = Lens.field @"csr"
{-# INLINEABLE icCsr #-}
{-# DEPRECATED csr "Use generic-lens or generic-optics with 'csr' instead"  #-}

-- | The name of the algorithm that will be used to sign the certificate to be issued. 
--
-- This parameter should not be confused with the @SigningAlgorithm@ parameter used to sign a CSR.
--
-- /Note:/ Consider using 'signingAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icSigningAlgorithm :: Lens.Lens' IssueCertificate Types.SigningAlgorithm
icSigningAlgorithm = Lens.field @"signingAlgorithm"
{-# INLINEABLE icSigningAlgorithm #-}
{-# DEPRECATED signingAlgorithm "Use generic-lens or generic-optics with 'signingAlgorithm' instead"  #-}

-- | Information describing the validity period of the certificate.
--
-- When issuing a certificate, ACM Private CA sets the "Not Before" date in the validity field to date and time minus 60 minutes. This is intended to compensate for time inconsistencies across systems of 60 minutes or less. 
-- The validity period configured on a certificate must not exceed the limit set by its parents in the CA hierarchy.
--
-- /Note:/ Consider using 'validity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icValidity :: Lens.Lens' IssueCertificate Types.Validity
icValidity = Lens.field @"validity"
{-# INLINEABLE icValidity #-}
{-# DEPRECATED validity "Use generic-lens or generic-optics with 'validity' instead"  #-}

-- | Custom string that can be used to distinguish between calls to the __IssueCertificate__ action. Idempotency tokens time out after one hour. Therefore, if you call __IssueCertificate__ multiple times with the same idempotency token within 5 minutes, ACM Private CA recognizes that you are requesting only one certificate and will issue only one. If you change the idempotency token for each call, PCA recognizes that you are requesting multiple certificates.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icIdempotencyToken :: Lens.Lens' IssueCertificate (Core.Maybe Types.IdempotencyToken)
icIdempotencyToken = Lens.field @"idempotencyToken"
{-# INLINEABLE icIdempotencyToken #-}
{-# DEPRECATED idempotencyToken "Use generic-lens or generic-optics with 'idempotencyToken' instead"  #-}

-- | Specifies a custom configuration template to use when issuing a certificate. If this parameter is not provided, ACM Private CA defaults to the @EndEntityCertificate/V1@ template. For CA certificates, you should choose the shortest path length that meets your needs. The path length is indicated by the PathLen/N/ portion of the ARN, where /N/ is the <https://docs.aws.amazon.com/acm-pca/latest/userguide/PcaTerms.html#terms-cadepth CA depth> .
--
-- Note: The CA depth configured on a subordinate CA certificate must not exceed the limit set by its parents in the CA hierarchy.
-- The following service-owned @TemplateArn@ values are supported by ACM Private CA: 
--
--     * arn:aws:acm-pca:::template/CodeSigningCertificate/V1
--
--
--     * arn:aws:acm-pca:::template/CodeSigningCertificate_CSRPassthrough/V1
--
--
--     * arn:aws:acm-pca:::template/EndEntityCertificate/V1
--
--
--     * arn:aws:acm-pca:::template/EndEntityCertificate_CSRPassthrough/V1
--
--
--     * arn:aws:acm-pca:::template/EndEntityClientAuthCertificate/V1
--
--
--     * arn:aws:acm-pca:::template/EndEntityClientAuthCertificate_CSRPassthrough/V1
--
--
--     * arn:aws:acm-pca:::template/EndEntityServerAuthCertificate/V1
--
--
--     * arn:aws:acm-pca:::template/EndEntityServerAuthCertificate_CSRPassthrough/V1
--
--
--     * arn:aws:acm-pca:::template/OCSPSigningCertificate/V1
--
--
--     * arn:aws:acm-pca:::template/OCSPSigningCertificate_CSRPassthrough/V1
--
--
--     * arn:aws:acm-pca:::template/RootCACertificate/V1
--
--
--     * arn:aws:acm-pca:::template/SubordinateCACertificate_PathLen0/V1
--
--
--     * arn:aws:acm-pca:::template/SubordinateCACertificate_PathLen1/V1
--
--
--     * arn:aws:acm-pca:::template/SubordinateCACertificate_PathLen2/V1
--
--
--     * arn:aws:acm-pca:::template/SubordinateCACertificate_PathLen3/V1
--
--
-- For more information, see <https://docs.aws.amazon.com/acm-pca/latest/userguide/UsingTemplates.html Using Templates> .
--
-- /Note:/ Consider using 'templateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icTemplateArn :: Lens.Lens' IssueCertificate (Core.Maybe Types.Arn)
icTemplateArn = Lens.field @"templateArn"
{-# INLINEABLE icTemplateArn #-}
{-# DEPRECATED templateArn "Use generic-lens or generic-optics with 'templateArn' instead"  #-}

instance Core.ToQuery IssueCertificate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders IssueCertificate where
        toHeaders IssueCertificate{..}
          = Core.pure ("X-Amz-Target", "ACMPrivateCA.IssueCertificate")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON IssueCertificate where
        toJSON IssueCertificate{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("CertificateAuthorityArn" Core..= certificateAuthorityArn),
                  Core.Just ("Csr" Core..= csr),
                  Core.Just ("SigningAlgorithm" Core..= signingAlgorithm),
                  Core.Just ("Validity" Core..= validity),
                  ("IdempotencyToken" Core..=) Core.<$> idempotencyToken,
                  ("TemplateArn" Core..=) Core.<$> templateArn])

instance Core.AWSRequest IssueCertificate where
        type Rs IssueCertificate = IssueCertificateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 IssueCertificateResponse' Core.<$>
                   (x Core..:? "CertificateArn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkIssueCertificateResponse' smart constructor.
data IssueCertificateResponse = IssueCertificateResponse'
  { certificateArn :: Core.Maybe Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the issued certificate and the certificate serial number. This is of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ /certificate//286535153982981100925020015808220737245/ @ 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IssueCertificateResponse' value with any optional fields omitted.
mkIssueCertificateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> IssueCertificateResponse
mkIssueCertificateResponse responseStatus
  = IssueCertificateResponse'{certificateArn = Core.Nothing,
                              responseStatus}

-- | The Amazon Resource Name (ARN) of the issued certificate and the certificate serial number. This is of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ /certificate//286535153982981100925020015808220737245/ @ 
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icrrsCertificateArn :: Lens.Lens' IssueCertificateResponse (Core.Maybe Types.Arn)
icrrsCertificateArn = Lens.field @"certificateArn"
{-# INLINEABLE icrrsCertificateArn #-}
{-# DEPRECATED certificateArn "Use generic-lens or generic-optics with 'certificateArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icrrsResponseStatus :: Lens.Lens' IssueCertificateResponse Core.Int
icrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE icrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}

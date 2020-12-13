{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    IssueCertificate (..),
    mkIssueCertificate,

    -- ** Request lenses
    icSigningAlgorithm,
    icIdempotencyToken,
    icCSR,
    icValidity,
    icTemplateARN,
    icCertificateAuthorityARN,

    -- * Destructuring the response
    IssueCertificateResponse (..),
    mkIssueCertificateResponse,

    -- ** Response lenses
    icrsCertificateARN,
    icrsResponseStatus,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkIssueCertificate' smart constructor.
data IssueCertificate = IssueCertificate'
  { -- | The name of the algorithm that will be used to sign the certificate to be issued.
    --
    -- This parameter should not be confused with the @SigningAlgorithm@ parameter used to sign a CSR.
    signingAlgorithm :: SigningAlgorithm,
    -- | Custom string that can be used to distinguish between calls to the __IssueCertificate__ action. Idempotency tokens time out after one hour. Therefore, if you call __IssueCertificate__ multiple times with the same idempotency token within 5 minutes, ACM Private CA recognizes that you are requesting only one certificate and will issue only one. If you change the idempotency token for each call, PCA recognizes that you are requesting multiple certificates.
    idempotencyToken :: Lude.Maybe Lude.Text,
    -- | The certificate signing request (CSR) for the certificate you want to issue. You can use the following OpenSSL command to create the CSR and a 2048 bit RSA private key.
    --
    -- @openssl req -new -newkey rsa:2048 -days 365 -keyout private/test_cert_priv_key.pem -out csr/test_cert_.csr@
    -- If you have a configuration file, you can use the following OpenSSL command. The @usr_cert@ block in the configuration file contains your X509 version 3 extensions.
    -- @openssl req -new -config openssl_rsa.cnf -extensions usr_cert -newkey rsa:2048 -days -365 -keyout private/test_cert_priv_key.pem -out csr/test_cert_.csr@
    -- Note: A CSR must provide either a /subject name/ or a /subject alternative name/ or the request will be rejected.
    cSR :: Lude.Base64,
    -- | Information describing the validity period of the certificate.
    --
    -- When issuing a certificate, ACM Private CA sets the "Not Before" date in the validity field to date and time minus 60 minutes. This is intended to compensate for time inconsistencies across systems of 60 minutes or less.
    -- The validity period configured on a certificate must not exceed the limit set by its parents in the CA hierarchy.
    validity :: Validity,
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
    templateARN :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) that was returned when you called <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> . This must be of the form:
    --
    -- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
    certificateAuthorityARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IssueCertificate' with the minimum fields required to make a request.
--
-- * 'signingAlgorithm' - The name of the algorithm that will be used to sign the certificate to be issued.
--
-- This parameter should not be confused with the @SigningAlgorithm@ parameter used to sign a CSR.
-- * 'idempotencyToken' - Custom string that can be used to distinguish between calls to the __IssueCertificate__ action. Idempotency tokens time out after one hour. Therefore, if you call __IssueCertificate__ multiple times with the same idempotency token within 5 minutes, ACM Private CA recognizes that you are requesting only one certificate and will issue only one. If you change the idempotency token for each call, PCA recognizes that you are requesting multiple certificates.
-- * 'cSR' - The certificate signing request (CSR) for the certificate you want to issue. You can use the following OpenSSL command to create the CSR and a 2048 bit RSA private key.
--
-- @openssl req -new -newkey rsa:2048 -days 365 -keyout private/test_cert_priv_key.pem -out csr/test_cert_.csr@
-- If you have a configuration file, you can use the following OpenSSL command. The @usr_cert@ block in the configuration file contains your X509 version 3 extensions.
-- @openssl req -new -config openssl_rsa.cnf -extensions usr_cert -newkey rsa:2048 -days -365 -keyout private/test_cert_priv_key.pem -out csr/test_cert_.csr@
-- Note: A CSR must provide either a /subject name/ or a /subject alternative name/ or the request will be rejected.
-- * 'validity' - Information describing the validity period of the certificate.
--
-- When issuing a certificate, ACM Private CA sets the "Not Before" date in the validity field to date and time minus 60 minutes. This is intended to compensate for time inconsistencies across systems of 60 minutes or less.
-- The validity period configured on a certificate must not exceed the limit set by its parents in the CA hierarchy.
-- * 'templateARN' - Specifies a custom configuration template to use when issuing a certificate. If this parameter is not provided, ACM Private CA defaults to the @EndEntityCertificate/V1@ template. For CA certificates, you should choose the shortest path length that meets your needs. The path length is indicated by the PathLen/N/ portion of the ARN, where /N/ is the <https://docs.aws.amazon.com/acm-pca/latest/userguide/PcaTerms.html#terms-cadepth CA depth> .
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
-- * 'certificateAuthorityARN' - The Amazon Resource Name (ARN) that was returned when you called <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> . This must be of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
mkIssueCertificate ::
  -- | 'signingAlgorithm'
  SigningAlgorithm ->
  -- | 'cSR'
  Lude.Base64 ->
  -- | 'validity'
  Validity ->
  -- | 'certificateAuthorityARN'
  Lude.Text ->
  IssueCertificate
mkIssueCertificate
  pSigningAlgorithm_
  pCSR_
  pValidity_
  pCertificateAuthorityARN_ =
    IssueCertificate'
      { signingAlgorithm = pSigningAlgorithm_,
        idempotencyToken = Lude.Nothing,
        cSR = pCSR_,
        validity = pValidity_,
        templateARN = Lude.Nothing,
        certificateAuthorityARN = pCertificateAuthorityARN_
      }

-- | The name of the algorithm that will be used to sign the certificate to be issued.
--
-- This parameter should not be confused with the @SigningAlgorithm@ parameter used to sign a CSR.
--
-- /Note:/ Consider using 'signingAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icSigningAlgorithm :: Lens.Lens' IssueCertificate SigningAlgorithm
icSigningAlgorithm = Lens.lens (signingAlgorithm :: IssueCertificate -> SigningAlgorithm) (\s a -> s {signingAlgorithm = a} :: IssueCertificate)
{-# DEPRECATED icSigningAlgorithm "Use generic-lens or generic-optics with 'signingAlgorithm' instead." #-}

-- | Custom string that can be used to distinguish between calls to the __IssueCertificate__ action. Idempotency tokens time out after one hour. Therefore, if you call __IssueCertificate__ multiple times with the same idempotency token within 5 minutes, ACM Private CA recognizes that you are requesting only one certificate and will issue only one. If you change the idempotency token for each call, PCA recognizes that you are requesting multiple certificates.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icIdempotencyToken :: Lens.Lens' IssueCertificate (Lude.Maybe Lude.Text)
icIdempotencyToken = Lens.lens (idempotencyToken :: IssueCertificate -> Lude.Maybe Lude.Text) (\s a -> s {idempotencyToken = a} :: IssueCertificate)
{-# DEPRECATED icIdempotencyToken "Use generic-lens or generic-optics with 'idempotencyToken' instead." #-}

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
-- /Note:/ Consider using 'cSR' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icCSR :: Lens.Lens' IssueCertificate Lude.Base64
icCSR = Lens.lens (cSR :: IssueCertificate -> Lude.Base64) (\s a -> s {cSR = a} :: IssueCertificate)
{-# DEPRECATED icCSR "Use generic-lens or generic-optics with 'cSR' instead." #-}

-- | Information describing the validity period of the certificate.
--
-- When issuing a certificate, ACM Private CA sets the "Not Before" date in the validity field to date and time minus 60 minutes. This is intended to compensate for time inconsistencies across systems of 60 minutes or less.
-- The validity period configured on a certificate must not exceed the limit set by its parents in the CA hierarchy.
--
-- /Note:/ Consider using 'validity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icValidity :: Lens.Lens' IssueCertificate Validity
icValidity = Lens.lens (validity :: IssueCertificate -> Validity) (\s a -> s {validity = a} :: IssueCertificate)
{-# DEPRECATED icValidity "Use generic-lens or generic-optics with 'validity' instead." #-}

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
-- /Note:/ Consider using 'templateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icTemplateARN :: Lens.Lens' IssueCertificate (Lude.Maybe Lude.Text)
icTemplateARN = Lens.lens (templateARN :: IssueCertificate -> Lude.Maybe Lude.Text) (\s a -> s {templateARN = a} :: IssueCertificate)
{-# DEPRECATED icTemplateARN "Use generic-lens or generic-optics with 'templateARN' instead." #-}

-- | The Amazon Resource Name (ARN) that was returned when you called <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> . This must be of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
--
-- /Note:/ Consider using 'certificateAuthorityARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icCertificateAuthorityARN :: Lens.Lens' IssueCertificate Lude.Text
icCertificateAuthorityARN = Lens.lens (certificateAuthorityARN :: IssueCertificate -> Lude.Text) (\s a -> s {certificateAuthorityARN = a} :: IssueCertificate)
{-# DEPRECATED icCertificateAuthorityARN "Use generic-lens or generic-optics with 'certificateAuthorityARN' instead." #-}

instance Lude.AWSRequest IssueCertificate where
  type Rs IssueCertificate = IssueCertificateResponse
  request = Req.postJSON certificateManagerPCAService
  response =
    Res.receiveJSON
      ( \s h x ->
          IssueCertificateResponse'
            Lude.<$> (x Lude..?> "CertificateArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders IssueCertificate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ACMPrivateCA.IssueCertificate" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON IssueCertificate where
  toJSON IssueCertificate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("SigningAlgorithm" Lude..= signingAlgorithm),
            ("IdempotencyToken" Lude..=) Lude.<$> idempotencyToken,
            Lude.Just ("Csr" Lude..= cSR),
            Lude.Just ("Validity" Lude..= validity),
            ("TemplateArn" Lude..=) Lude.<$> templateARN,
            Lude.Just
              ("CertificateAuthorityArn" Lude..= certificateAuthorityARN)
          ]
      )

instance Lude.ToPath IssueCertificate where
  toPath = Lude.const "/"

instance Lude.ToQuery IssueCertificate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkIssueCertificateResponse' smart constructor.
data IssueCertificateResponse = IssueCertificateResponse'
  { -- | The Amazon Resource Name (ARN) of the issued certificate and the certificate serial number. This is of the form:
    --
    -- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ /certificate//286535153982981100925020015808220737245/ @
    certificateARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IssueCertificateResponse' with the minimum fields required to make a request.
--
-- * 'certificateARN' - The Amazon Resource Name (ARN) of the issued certificate and the certificate serial number. This is of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ /certificate//286535153982981100925020015808220737245/ @
-- * 'responseStatus' - The response status code.
mkIssueCertificateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  IssueCertificateResponse
mkIssueCertificateResponse pResponseStatus_ =
  IssueCertificateResponse'
    { certificateARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the issued certificate and the certificate serial number. This is of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ /certificate//286535153982981100925020015808220737245/ @
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icrsCertificateARN :: Lens.Lens' IssueCertificateResponse (Lude.Maybe Lude.Text)
icrsCertificateARN = Lens.lens (certificateARN :: IssueCertificateResponse -> Lude.Maybe Lude.Text) (\s a -> s {certificateARN = a} :: IssueCertificateResponse)
{-# DEPRECATED icrsCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icrsResponseStatus :: Lens.Lens' IssueCertificateResponse Lude.Int
icrsResponseStatus = Lens.lens (responseStatus :: IssueCertificateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: IssueCertificateResponse)
{-# DEPRECATED icrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

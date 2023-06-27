{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CertificateManagerPCA.IssueCertificate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uses your private certificate authority (CA), or one that has been
-- shared with you, to issue a client certificate. This action returns the
-- Amazon Resource Name (ARN) of the certificate. You can retrieve the
-- certificate by calling the
-- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_GetCertificate.html GetCertificate>
-- action and specifying the ARN.
--
-- You cannot use the ACM __ListCertificateAuthorities__ action to retrieve
-- the ARNs of the certificates that you issue by using Amazon Web Services
-- Private CA.
module Amazonka.CertificateManagerPCA.IssueCertificate
  ( -- * Creating a Request
    IssueCertificate (..),
    newIssueCertificate,

    -- * Request Lenses
    issueCertificate_apiPassthrough,
    issueCertificate_idempotencyToken,
    issueCertificate_templateArn,
    issueCertificate_validityNotBefore,
    issueCertificate_certificateAuthorityArn,
    issueCertificate_csr,
    issueCertificate_signingAlgorithm,
    issueCertificate_validity,

    -- * Destructuring the Response
    IssueCertificateResponse (..),
    newIssueCertificateResponse,

    -- * Response Lenses
    issueCertificateResponse_certificateArn,
    issueCertificateResponse_httpStatus,
  )
where

import Amazonka.CertificateManagerPCA.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newIssueCertificate' smart constructor.
data IssueCertificate = IssueCertificate'
  { -- | Specifies X.509 certificate information to be included in the issued
    -- certificate. An @APIPassthrough@ or @APICSRPassthrough@ template variant
    -- must be selected, or else this parameter is ignored. For more
    -- information about using these templates, see
    -- <https://docs.aws.amazon.com/privateca/latest/userguide/UsingTemplates.html Understanding Certificate Templates>.
    --
    -- If conflicting or duplicate certificate information is supplied during
    -- certificate issuance, Amazon Web Services Private CA applies
    -- <https://docs.aws.amazon.com/privateca/latest/userguide/UsingTemplates.html#template-order-of-operations order of operation rules>
    -- to determine what information is used.
    apiPassthrough :: Prelude.Maybe ApiPassthrough,
    -- | Alphanumeric string that can be used to distinguish between calls to the
    -- __IssueCertificate__ action. Idempotency tokens for __IssueCertificate__
    -- time out after one minute. Therefore, if you call __IssueCertificate__
    -- multiple times with the same idempotency token within one minute, Amazon
    -- Web Services Private CA recognizes that you are requesting only one
    -- certificate and will issue only one. If you change the idempotency token
    -- for each call, Amazon Web Services Private CA recognizes that you are
    -- requesting multiple certificates.
    idempotencyToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies a custom configuration template to use when issuing a
    -- certificate. If this parameter is not provided, Amazon Web Services
    -- Private CA defaults to the @EndEntityCertificate\/V1@ template. For CA
    -- certificates, you should choose the shortest path length that meets your
    -- needs. The path length is indicated by the PathLen/N/ portion of the
    -- ARN, where /N/ is the
    -- <https://docs.aws.amazon.com/privateca/latest/userguide/PcaTerms.html#terms-cadepth CA depth>.
    --
    -- Note: The CA depth configured on a subordinate CA certificate must not
    -- exceed the limit set by its parents in the CA hierarchy.
    --
    -- For a list of @TemplateArn@ values supported by Amazon Web Services
    -- Private CA, see
    -- <https://docs.aws.amazon.com/privateca/latest/userguide/UsingTemplates.html Understanding Certificate Templates>.
    templateArn :: Prelude.Maybe Prelude.Text,
    -- | Information describing the start of the validity period of the
    -- certificate. This parameter sets the “Not Before\" date for the
    -- certificate.
    --
    -- By default, when issuing a certificate, Amazon Web Services Private CA
    -- sets the \"Not Before\" date to the issuance time minus 60 minutes. This
    -- compensates for clock inconsistencies across computer systems. The
    -- @ValidityNotBefore@ parameter can be used to customize the “Not Before”
    -- value.
    --
    -- Unlike the @Validity@ parameter, the @ValidityNotBefore@ parameter is
    -- optional.
    --
    -- The @ValidityNotBefore@ value is expressed as an explicit date and time,
    -- using the @Validity@ type value @ABSOLUTE@. For more information, see
    -- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_Validity.html Validity>
    -- in this API reference and
    -- <https://datatracker.ietf.org/doc/html/rfc5280#section-4.1.2.5 Validity>
    -- in RFC 5280.
    validityNotBefore :: Prelude.Maybe Validity,
    -- | The Amazon Resource Name (ARN) that was returned when you called
    -- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority>.
    -- This must be of the form:
    --
    -- @arn:aws:acm-pca:@/@region@/@:@/@account@/@:certificate-authority\/@/@12345678-1234-1234-1234-123456789012@/@ @
    certificateAuthorityArn :: Prelude.Text,
    -- | The certificate signing request (CSR) for the certificate you want to
    -- issue. As an example, you can use the following OpenSSL command to
    -- create the CSR and a 2048 bit RSA private key.
    --
    -- @openssl req -new -newkey rsa:2048 -days 365 -keyout private\/test_cert_priv_key.pem -out csr\/test_cert_.csr@
    --
    -- If you have a configuration file, you can then use the following OpenSSL
    -- command. The @usr_cert@ block in the configuration file contains your
    -- X509 version 3 extensions.
    --
    -- @openssl req -new -config openssl_rsa.cnf -extensions usr_cert -newkey rsa:2048 -days 365 -keyout private\/test_cert_priv_key.pem -out csr\/test_cert_.csr@
    --
    -- Note: A CSR must provide either a /subject name/ or a /subject
    -- alternative name/ or the request will be rejected.
    csr :: Data.Base64,
    -- | The name of the algorithm that will be used to sign the certificate to
    -- be issued.
    --
    -- This parameter should not be confused with the @SigningAlgorithm@
    -- parameter used to sign a CSR in the @CreateCertificateAuthority@ action.
    --
    -- The specified signing algorithm family (RSA or ECDSA) must match the
    -- algorithm family of the CA\'s secret key.
    signingAlgorithm :: SigningAlgorithm,
    -- | Information describing the end of the validity period of the
    -- certificate. This parameter sets the “Not After” date for the
    -- certificate.
    --
    -- Certificate validity is the period of time during which a certificate is
    -- valid. Validity can be expressed as an explicit date and time when the
    -- certificate expires, or as a span of time after issuance, stated in
    -- days, months, or years. For more information, see
    -- <https://datatracker.ietf.org/doc/html/rfc5280#section-4.1.2.5 Validity>
    -- in RFC 5280.
    --
    -- This value is unaffected when @ValidityNotBefore@ is also specified. For
    -- example, if @Validity@ is set to 20 days in the future, the certificate
    -- will expire 20 days from issuance time regardless of the
    -- @ValidityNotBefore@ value.
    --
    -- The end of the validity period configured on a certificate must not
    -- exceed the limit set on its parents in the CA hierarchy.
    validity :: Validity
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IssueCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiPassthrough', 'issueCertificate_apiPassthrough' - Specifies X.509 certificate information to be included in the issued
-- certificate. An @APIPassthrough@ or @APICSRPassthrough@ template variant
-- must be selected, or else this parameter is ignored. For more
-- information about using these templates, see
-- <https://docs.aws.amazon.com/privateca/latest/userguide/UsingTemplates.html Understanding Certificate Templates>.
--
-- If conflicting or duplicate certificate information is supplied during
-- certificate issuance, Amazon Web Services Private CA applies
-- <https://docs.aws.amazon.com/privateca/latest/userguide/UsingTemplates.html#template-order-of-operations order of operation rules>
-- to determine what information is used.
--
-- 'idempotencyToken', 'issueCertificate_idempotencyToken' - Alphanumeric string that can be used to distinguish between calls to the
-- __IssueCertificate__ action. Idempotency tokens for __IssueCertificate__
-- time out after one minute. Therefore, if you call __IssueCertificate__
-- multiple times with the same idempotency token within one minute, Amazon
-- Web Services Private CA recognizes that you are requesting only one
-- certificate and will issue only one. If you change the idempotency token
-- for each call, Amazon Web Services Private CA recognizes that you are
-- requesting multiple certificates.
--
-- 'templateArn', 'issueCertificate_templateArn' - Specifies a custom configuration template to use when issuing a
-- certificate. If this parameter is not provided, Amazon Web Services
-- Private CA defaults to the @EndEntityCertificate\/V1@ template. For CA
-- certificates, you should choose the shortest path length that meets your
-- needs. The path length is indicated by the PathLen/N/ portion of the
-- ARN, where /N/ is the
-- <https://docs.aws.amazon.com/privateca/latest/userguide/PcaTerms.html#terms-cadepth CA depth>.
--
-- Note: The CA depth configured on a subordinate CA certificate must not
-- exceed the limit set by its parents in the CA hierarchy.
--
-- For a list of @TemplateArn@ values supported by Amazon Web Services
-- Private CA, see
-- <https://docs.aws.amazon.com/privateca/latest/userguide/UsingTemplates.html Understanding Certificate Templates>.
--
-- 'validityNotBefore', 'issueCertificate_validityNotBefore' - Information describing the start of the validity period of the
-- certificate. This parameter sets the “Not Before\" date for the
-- certificate.
--
-- By default, when issuing a certificate, Amazon Web Services Private CA
-- sets the \"Not Before\" date to the issuance time minus 60 minutes. This
-- compensates for clock inconsistencies across computer systems. The
-- @ValidityNotBefore@ parameter can be used to customize the “Not Before”
-- value.
--
-- Unlike the @Validity@ parameter, the @ValidityNotBefore@ parameter is
-- optional.
--
-- The @ValidityNotBefore@ value is expressed as an explicit date and time,
-- using the @Validity@ type value @ABSOLUTE@. For more information, see
-- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_Validity.html Validity>
-- in this API reference and
-- <https://datatracker.ietf.org/doc/html/rfc5280#section-4.1.2.5 Validity>
-- in RFC 5280.
--
-- 'certificateAuthorityArn', 'issueCertificate_certificateAuthorityArn' - The Amazon Resource Name (ARN) that was returned when you called
-- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority>.
-- This must be of the form:
--
-- @arn:aws:acm-pca:@/@region@/@:@/@account@/@:certificate-authority\/@/@12345678-1234-1234-1234-123456789012@/@ @
--
-- 'csr', 'issueCertificate_csr' - The certificate signing request (CSR) for the certificate you want to
-- issue. As an example, you can use the following OpenSSL command to
-- create the CSR and a 2048 bit RSA private key.
--
-- @openssl req -new -newkey rsa:2048 -days 365 -keyout private\/test_cert_priv_key.pem -out csr\/test_cert_.csr@
--
-- If you have a configuration file, you can then use the following OpenSSL
-- command. The @usr_cert@ block in the configuration file contains your
-- X509 version 3 extensions.
--
-- @openssl req -new -config openssl_rsa.cnf -extensions usr_cert -newkey rsa:2048 -days 365 -keyout private\/test_cert_priv_key.pem -out csr\/test_cert_.csr@
--
-- Note: A CSR must provide either a /subject name/ or a /subject
-- alternative name/ or the request will be rejected.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'signingAlgorithm', 'issueCertificate_signingAlgorithm' - The name of the algorithm that will be used to sign the certificate to
-- be issued.
--
-- This parameter should not be confused with the @SigningAlgorithm@
-- parameter used to sign a CSR in the @CreateCertificateAuthority@ action.
--
-- The specified signing algorithm family (RSA or ECDSA) must match the
-- algorithm family of the CA\'s secret key.
--
-- 'validity', 'issueCertificate_validity' - Information describing the end of the validity period of the
-- certificate. This parameter sets the “Not After” date for the
-- certificate.
--
-- Certificate validity is the period of time during which a certificate is
-- valid. Validity can be expressed as an explicit date and time when the
-- certificate expires, or as a span of time after issuance, stated in
-- days, months, or years. For more information, see
-- <https://datatracker.ietf.org/doc/html/rfc5280#section-4.1.2.5 Validity>
-- in RFC 5280.
--
-- This value is unaffected when @ValidityNotBefore@ is also specified. For
-- example, if @Validity@ is set to 20 days in the future, the certificate
-- will expire 20 days from issuance time regardless of the
-- @ValidityNotBefore@ value.
--
-- The end of the validity period configured on a certificate must not
-- exceed the limit set on its parents in the CA hierarchy.
newIssueCertificate ::
  -- | 'certificateAuthorityArn'
  Prelude.Text ->
  -- | 'csr'
  Prelude.ByteString ->
  -- | 'signingAlgorithm'
  SigningAlgorithm ->
  -- | 'validity'
  Validity ->
  IssueCertificate
newIssueCertificate
  pCertificateAuthorityArn_
  pCsr_
  pSigningAlgorithm_
  pValidity_ =
    IssueCertificate'
      { apiPassthrough = Prelude.Nothing,
        idempotencyToken = Prelude.Nothing,
        templateArn = Prelude.Nothing,
        validityNotBefore = Prelude.Nothing,
        certificateAuthorityArn = pCertificateAuthorityArn_,
        csr = Data._Base64 Lens.# pCsr_,
        signingAlgorithm = pSigningAlgorithm_,
        validity = pValidity_
      }

-- | Specifies X.509 certificate information to be included in the issued
-- certificate. An @APIPassthrough@ or @APICSRPassthrough@ template variant
-- must be selected, or else this parameter is ignored. For more
-- information about using these templates, see
-- <https://docs.aws.amazon.com/privateca/latest/userguide/UsingTemplates.html Understanding Certificate Templates>.
--
-- If conflicting or duplicate certificate information is supplied during
-- certificate issuance, Amazon Web Services Private CA applies
-- <https://docs.aws.amazon.com/privateca/latest/userguide/UsingTemplates.html#template-order-of-operations order of operation rules>
-- to determine what information is used.
issueCertificate_apiPassthrough :: Lens.Lens' IssueCertificate (Prelude.Maybe ApiPassthrough)
issueCertificate_apiPassthrough = Lens.lens (\IssueCertificate' {apiPassthrough} -> apiPassthrough) (\s@IssueCertificate' {} a -> s {apiPassthrough = a} :: IssueCertificate)

-- | Alphanumeric string that can be used to distinguish between calls to the
-- __IssueCertificate__ action. Idempotency tokens for __IssueCertificate__
-- time out after one minute. Therefore, if you call __IssueCertificate__
-- multiple times with the same idempotency token within one minute, Amazon
-- Web Services Private CA recognizes that you are requesting only one
-- certificate and will issue only one. If you change the idempotency token
-- for each call, Amazon Web Services Private CA recognizes that you are
-- requesting multiple certificates.
issueCertificate_idempotencyToken :: Lens.Lens' IssueCertificate (Prelude.Maybe Prelude.Text)
issueCertificate_idempotencyToken = Lens.lens (\IssueCertificate' {idempotencyToken} -> idempotencyToken) (\s@IssueCertificate' {} a -> s {idempotencyToken = a} :: IssueCertificate)

-- | Specifies a custom configuration template to use when issuing a
-- certificate. If this parameter is not provided, Amazon Web Services
-- Private CA defaults to the @EndEntityCertificate\/V1@ template. For CA
-- certificates, you should choose the shortest path length that meets your
-- needs. The path length is indicated by the PathLen/N/ portion of the
-- ARN, where /N/ is the
-- <https://docs.aws.amazon.com/privateca/latest/userguide/PcaTerms.html#terms-cadepth CA depth>.
--
-- Note: The CA depth configured on a subordinate CA certificate must not
-- exceed the limit set by its parents in the CA hierarchy.
--
-- For a list of @TemplateArn@ values supported by Amazon Web Services
-- Private CA, see
-- <https://docs.aws.amazon.com/privateca/latest/userguide/UsingTemplates.html Understanding Certificate Templates>.
issueCertificate_templateArn :: Lens.Lens' IssueCertificate (Prelude.Maybe Prelude.Text)
issueCertificate_templateArn = Lens.lens (\IssueCertificate' {templateArn} -> templateArn) (\s@IssueCertificate' {} a -> s {templateArn = a} :: IssueCertificate)

-- | Information describing the start of the validity period of the
-- certificate. This parameter sets the “Not Before\" date for the
-- certificate.
--
-- By default, when issuing a certificate, Amazon Web Services Private CA
-- sets the \"Not Before\" date to the issuance time minus 60 minutes. This
-- compensates for clock inconsistencies across computer systems. The
-- @ValidityNotBefore@ parameter can be used to customize the “Not Before”
-- value.
--
-- Unlike the @Validity@ parameter, the @ValidityNotBefore@ parameter is
-- optional.
--
-- The @ValidityNotBefore@ value is expressed as an explicit date and time,
-- using the @Validity@ type value @ABSOLUTE@. For more information, see
-- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_Validity.html Validity>
-- in this API reference and
-- <https://datatracker.ietf.org/doc/html/rfc5280#section-4.1.2.5 Validity>
-- in RFC 5280.
issueCertificate_validityNotBefore :: Lens.Lens' IssueCertificate (Prelude.Maybe Validity)
issueCertificate_validityNotBefore = Lens.lens (\IssueCertificate' {validityNotBefore} -> validityNotBefore) (\s@IssueCertificate' {} a -> s {validityNotBefore = a} :: IssueCertificate)

-- | The Amazon Resource Name (ARN) that was returned when you called
-- <https://docs.aws.amazon.com/privateca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority>.
-- This must be of the form:
--
-- @arn:aws:acm-pca:@/@region@/@:@/@account@/@:certificate-authority\/@/@12345678-1234-1234-1234-123456789012@/@ @
issueCertificate_certificateAuthorityArn :: Lens.Lens' IssueCertificate Prelude.Text
issueCertificate_certificateAuthorityArn = Lens.lens (\IssueCertificate' {certificateAuthorityArn} -> certificateAuthorityArn) (\s@IssueCertificate' {} a -> s {certificateAuthorityArn = a} :: IssueCertificate)

-- | The certificate signing request (CSR) for the certificate you want to
-- issue. As an example, you can use the following OpenSSL command to
-- create the CSR and a 2048 bit RSA private key.
--
-- @openssl req -new -newkey rsa:2048 -days 365 -keyout private\/test_cert_priv_key.pem -out csr\/test_cert_.csr@
--
-- If you have a configuration file, you can then use the following OpenSSL
-- command. The @usr_cert@ block in the configuration file contains your
-- X509 version 3 extensions.
--
-- @openssl req -new -config openssl_rsa.cnf -extensions usr_cert -newkey rsa:2048 -days 365 -keyout private\/test_cert_priv_key.pem -out csr\/test_cert_.csr@
--
-- Note: A CSR must provide either a /subject name/ or a /subject
-- alternative name/ or the request will be rejected.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
issueCertificate_csr :: Lens.Lens' IssueCertificate Prelude.ByteString
issueCertificate_csr = Lens.lens (\IssueCertificate' {csr} -> csr) (\s@IssueCertificate' {} a -> s {csr = a} :: IssueCertificate) Prelude.. Data._Base64

-- | The name of the algorithm that will be used to sign the certificate to
-- be issued.
--
-- This parameter should not be confused with the @SigningAlgorithm@
-- parameter used to sign a CSR in the @CreateCertificateAuthority@ action.
--
-- The specified signing algorithm family (RSA or ECDSA) must match the
-- algorithm family of the CA\'s secret key.
issueCertificate_signingAlgorithm :: Lens.Lens' IssueCertificate SigningAlgorithm
issueCertificate_signingAlgorithm = Lens.lens (\IssueCertificate' {signingAlgorithm} -> signingAlgorithm) (\s@IssueCertificate' {} a -> s {signingAlgorithm = a} :: IssueCertificate)

-- | Information describing the end of the validity period of the
-- certificate. This parameter sets the “Not After” date for the
-- certificate.
--
-- Certificate validity is the period of time during which a certificate is
-- valid. Validity can be expressed as an explicit date and time when the
-- certificate expires, or as a span of time after issuance, stated in
-- days, months, or years. For more information, see
-- <https://datatracker.ietf.org/doc/html/rfc5280#section-4.1.2.5 Validity>
-- in RFC 5280.
--
-- This value is unaffected when @ValidityNotBefore@ is also specified. For
-- example, if @Validity@ is set to 20 days in the future, the certificate
-- will expire 20 days from issuance time regardless of the
-- @ValidityNotBefore@ value.
--
-- The end of the validity period configured on a certificate must not
-- exceed the limit set on its parents in the CA hierarchy.
issueCertificate_validity :: Lens.Lens' IssueCertificate Validity
issueCertificate_validity = Lens.lens (\IssueCertificate' {validity} -> validity) (\s@IssueCertificate' {} a -> s {validity = a} :: IssueCertificate)

instance Core.AWSRequest IssueCertificate where
  type
    AWSResponse IssueCertificate =
      IssueCertificateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          IssueCertificateResponse'
            Prelude.<$> (x Data..?> "CertificateArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable IssueCertificate where
  hashWithSalt _salt IssueCertificate' {..} =
    _salt
      `Prelude.hashWithSalt` apiPassthrough
      `Prelude.hashWithSalt` idempotencyToken
      `Prelude.hashWithSalt` templateArn
      `Prelude.hashWithSalt` validityNotBefore
      `Prelude.hashWithSalt` certificateAuthorityArn
      `Prelude.hashWithSalt` csr
      `Prelude.hashWithSalt` signingAlgorithm
      `Prelude.hashWithSalt` validity

instance Prelude.NFData IssueCertificate where
  rnf IssueCertificate' {..} =
    Prelude.rnf apiPassthrough
      `Prelude.seq` Prelude.rnf idempotencyToken
      `Prelude.seq` Prelude.rnf templateArn
      `Prelude.seq` Prelude.rnf validityNotBefore
      `Prelude.seq` Prelude.rnf certificateAuthorityArn
      `Prelude.seq` Prelude.rnf csr
      `Prelude.seq` Prelude.rnf signingAlgorithm
      `Prelude.seq` Prelude.rnf validity

instance Data.ToHeaders IssueCertificate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ACMPrivateCA.IssueCertificate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON IssueCertificate where
  toJSON IssueCertificate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ApiPassthrough" Data..=)
              Prelude.<$> apiPassthrough,
            ("IdempotencyToken" Data..=)
              Prelude.<$> idempotencyToken,
            ("TemplateArn" Data..=) Prelude.<$> templateArn,
            ("ValidityNotBefore" Data..=)
              Prelude.<$> validityNotBefore,
            Prelude.Just
              ( "CertificateAuthorityArn"
                  Data..= certificateAuthorityArn
              ),
            Prelude.Just ("Csr" Data..= csr),
            Prelude.Just
              ("SigningAlgorithm" Data..= signingAlgorithm),
            Prelude.Just ("Validity" Data..= validity)
          ]
      )

instance Data.ToPath IssueCertificate where
  toPath = Prelude.const "/"

instance Data.ToQuery IssueCertificate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newIssueCertificateResponse' smart constructor.
data IssueCertificateResponse = IssueCertificateResponse'
  { -- | The Amazon Resource Name (ARN) of the issued certificate and the
    -- certificate serial number. This is of the form:
    --
    -- @arn:aws:acm-pca:@/@region@/@:@/@account@/@:certificate-authority\/@/@12345678-1234-1234-1234-123456789012@/@\/certificate\/@/@286535153982981100925020015808220737245@/@ @
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IssueCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateArn', 'issueCertificateResponse_certificateArn' - The Amazon Resource Name (ARN) of the issued certificate and the
-- certificate serial number. This is of the form:
--
-- @arn:aws:acm-pca:@/@region@/@:@/@account@/@:certificate-authority\/@/@12345678-1234-1234-1234-123456789012@/@\/certificate\/@/@286535153982981100925020015808220737245@/@ @
--
-- 'httpStatus', 'issueCertificateResponse_httpStatus' - The response's http status code.
newIssueCertificateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  IssueCertificateResponse
newIssueCertificateResponse pHttpStatus_ =
  IssueCertificateResponse'
    { certificateArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the issued certificate and the
-- certificate serial number. This is of the form:
--
-- @arn:aws:acm-pca:@/@region@/@:@/@account@/@:certificate-authority\/@/@12345678-1234-1234-1234-123456789012@/@\/certificate\/@/@286535153982981100925020015808220737245@/@ @
issueCertificateResponse_certificateArn :: Lens.Lens' IssueCertificateResponse (Prelude.Maybe Prelude.Text)
issueCertificateResponse_certificateArn = Lens.lens (\IssueCertificateResponse' {certificateArn} -> certificateArn) (\s@IssueCertificateResponse' {} a -> s {certificateArn = a} :: IssueCertificateResponse)

-- | The response's http status code.
issueCertificateResponse_httpStatus :: Lens.Lens' IssueCertificateResponse Prelude.Int
issueCertificateResponse_httpStatus = Lens.lens (\IssueCertificateResponse' {httpStatus} -> httpStatus) (\s@IssueCertificateResponse' {} a -> s {httpStatus = a} :: IssueCertificateResponse)

instance Prelude.NFData IssueCertificateResponse where
  rnf IssueCertificateResponse' {..} =
    Prelude.rnf certificateArn
      `Prelude.seq` Prelude.rnf httpStatus

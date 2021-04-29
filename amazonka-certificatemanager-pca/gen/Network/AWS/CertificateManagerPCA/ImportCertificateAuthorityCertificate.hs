{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CertificateManagerPCA.ImportCertificateAuthorityCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports a signed private CA certificate into ACM Private CA. This action
-- is used when you are using a chain of trust whose root is located
-- outside ACM Private CA. Before you can call this action, the following
-- preparations must in place:
--
-- 1.  In ACM Private CA, call the
--     <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority>
--     action to create the private CA that you plan to back with the
--     imported certificate.
--
-- 2.  Call the
--     <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_GetCertificateAuthorityCsr.html GetCertificateAuthorityCsr>
--     action to generate a certificate signing request (CSR).
--
-- 3.  Sign the CSR using a root or intermediate CA hosted by either an
--     on-premises PKI hierarchy or by a commercial CA.
--
-- 4.  Create a certificate chain and copy the signed certificate and the
--     certificate chain to your working directory.
--
-- ACM Private CA supports three scenarios for installing a CA certificate:
--
-- -   Installing a certificate for a root CA hosted by ACM Private CA.
--
-- -   Installing a subordinate CA certificate whose parent authority is
--     hosted by ACM Private CA.
--
-- -   Installing a subordinate CA certificate whose parent authority is
--     externally hosted.
--
-- The following additional requirements apply when you import a CA
-- certificate.
--
-- -   Only a self-signed certificate can be imported as a root CA.
--
-- -   A self-signed certificate cannot be imported as a subordinate CA.
--
-- -   Your certificate chain must not include the private CA certificate
--     that you are importing.
--
-- -   Your root CA must be the last certificate in your chain. The
--     subordinate certificate, if any, that your root CA signed must be
--     next to last. The subordinate certificate signed by the preceding
--     subordinate CA must come next, and so on until your chain is built.
--
-- -   The chain must be PEM-encoded.
--
-- -   The maximum allowed size of a certificate is 32 KB.
--
-- -   The maximum allowed size of a certificate chain is 2 MB.
--
-- /Enforcement of Critical Constraints/
--
-- ACM Private CA allows the following extensions to be marked critical in
-- the imported CA certificate or chain.
--
-- -   Basic constraints (/must/ be marked critical)
--
-- -   Subject alternative names
--
-- -   Key usage
--
-- -   Extended key usage
--
-- -   Authority key identifier
--
-- -   Subject key identifier
--
-- -   Issuer alternative name
--
-- -   Subject directory attributes
--
-- -   Subject information access
--
-- -   Certificate policies
--
-- -   Policy mappings
--
-- -   Inhibit anyPolicy
--
-- ACM Private CA rejects the following extensions when they are marked
-- critical in an imported CA certificate or chain.
--
-- -   Name constraints
--
-- -   Policy constraints
--
-- -   CRL distribution points
--
-- -   Authority information access
--
-- -   Freshest CRL
--
-- -   Any other extension
module Network.AWS.CertificateManagerPCA.ImportCertificateAuthorityCertificate
  ( -- * Creating a Request
    ImportCertificateAuthorityCertificate (..),
    newImportCertificateAuthorityCertificate,

    -- * Request Lenses
    importCertificateAuthorityCertificate_certificateChain,
    importCertificateAuthorityCertificate_certificateAuthorityArn,
    importCertificateAuthorityCertificate_certificate,

    -- * Destructuring the Response
    ImportCertificateAuthorityCertificateResponse (..),
    newImportCertificateAuthorityCertificateResponse,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newImportCertificateAuthorityCertificate' smart constructor.
data ImportCertificateAuthorityCertificate = ImportCertificateAuthorityCertificate'
  { -- | A PEM-encoded file that contains all of your certificates, other than
    -- the certificate you\'re importing, chaining up to your root CA. Your ACM
    -- Private CA-hosted or on-premises root certificate is the last in the
    -- chain, and each certificate in the chain signs the one preceding.
    --
    -- This parameter must be supplied when you import a subordinate CA. When
    -- you import a root CA, there is no chain.
    certificateChain :: Prelude.Maybe Prelude.Base64,
    -- | The Amazon Resource Name (ARN) that was returned when you called
    -- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority>.
    -- This must be of the form:
    --
    -- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012 @
    certificateAuthorityArn :: Prelude.Text,
    -- | The PEM-encoded certificate for a private CA. This may be a self-signed
    -- certificate in the case of a root CA, or it may be signed by another CA
    -- that you control.
    certificate :: Prelude.Base64
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ImportCertificateAuthorityCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateChain', 'importCertificateAuthorityCertificate_certificateChain' - A PEM-encoded file that contains all of your certificates, other than
-- the certificate you\'re importing, chaining up to your root CA. Your ACM
-- Private CA-hosted or on-premises root certificate is the last in the
-- chain, and each certificate in the chain signs the one preceding.
--
-- This parameter must be supplied when you import a subordinate CA. When
-- you import a root CA, there is no chain.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'certificateAuthorityArn', 'importCertificateAuthorityCertificate_certificateAuthorityArn' - The Amazon Resource Name (ARN) that was returned when you called
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority>.
-- This must be of the form:
--
-- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012 @
--
-- 'certificate', 'importCertificateAuthorityCertificate_certificate' - The PEM-encoded certificate for a private CA. This may be a self-signed
-- certificate in the case of a root CA, or it may be signed by another CA
-- that you control.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
newImportCertificateAuthorityCertificate ::
  -- | 'certificateAuthorityArn'
  Prelude.Text ->
  -- | 'certificate'
  Prelude.ByteString ->
  ImportCertificateAuthorityCertificate
newImportCertificateAuthorityCertificate
  pCertificateAuthorityArn_
  pCertificate_ =
    ImportCertificateAuthorityCertificate'
      { certificateChain =
          Prelude.Nothing,
        certificateAuthorityArn =
          pCertificateAuthorityArn_,
        certificate =
          Prelude._Base64
            Lens.# pCertificate_
      }

-- | A PEM-encoded file that contains all of your certificates, other than
-- the certificate you\'re importing, chaining up to your root CA. Your ACM
-- Private CA-hosted or on-premises root certificate is the last in the
-- chain, and each certificate in the chain signs the one preceding.
--
-- This parameter must be supplied when you import a subordinate CA. When
-- you import a root CA, there is no chain.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
importCertificateAuthorityCertificate_certificateChain :: Lens.Lens' ImportCertificateAuthorityCertificate (Prelude.Maybe Prelude.ByteString)
importCertificateAuthorityCertificate_certificateChain = Lens.lens (\ImportCertificateAuthorityCertificate' {certificateChain} -> certificateChain) (\s@ImportCertificateAuthorityCertificate' {} a -> s {certificateChain = a} :: ImportCertificateAuthorityCertificate) Prelude.. Lens.mapping Prelude._Base64

-- | The Amazon Resource Name (ARN) that was returned when you called
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority>.
-- This must be of the form:
--
-- @arn:aws:acm-pca:region:account:certificate-authority\/12345678-1234-1234-1234-123456789012 @
importCertificateAuthorityCertificate_certificateAuthorityArn :: Lens.Lens' ImportCertificateAuthorityCertificate Prelude.Text
importCertificateAuthorityCertificate_certificateAuthorityArn = Lens.lens (\ImportCertificateAuthorityCertificate' {certificateAuthorityArn} -> certificateAuthorityArn) (\s@ImportCertificateAuthorityCertificate' {} a -> s {certificateAuthorityArn = a} :: ImportCertificateAuthorityCertificate)

-- | The PEM-encoded certificate for a private CA. This may be a self-signed
-- certificate in the case of a root CA, or it may be signed by another CA
-- that you control.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
importCertificateAuthorityCertificate_certificate :: Lens.Lens' ImportCertificateAuthorityCertificate Prelude.ByteString
importCertificateAuthorityCertificate_certificate = Lens.lens (\ImportCertificateAuthorityCertificate' {certificate} -> certificate) (\s@ImportCertificateAuthorityCertificate' {} a -> s {certificate = a} :: ImportCertificateAuthorityCertificate) Prelude.. Prelude._Base64

instance
  Prelude.AWSRequest
    ImportCertificateAuthorityCertificate
  where
  type
    Rs ImportCertificateAuthorityCertificate =
      ImportCertificateAuthorityCertificateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      ImportCertificateAuthorityCertificateResponse'

instance
  Prelude.Hashable
    ImportCertificateAuthorityCertificate

instance
  Prelude.NFData
    ImportCertificateAuthorityCertificate

instance
  Prelude.ToHeaders
    ImportCertificateAuthorityCertificate
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "ACMPrivateCA.ImportCertificateAuthorityCertificate" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    ImportCertificateAuthorityCertificate
  where
  toJSON ImportCertificateAuthorityCertificate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CertificateChain" Prelude..=)
              Prelude.<$> certificateChain,
            Prelude.Just
              ( "CertificateAuthorityArn"
                  Prelude..= certificateAuthorityArn
              ),
            Prelude.Just ("Certificate" Prelude..= certificate)
          ]
      )

instance
  Prelude.ToPath
    ImportCertificateAuthorityCertificate
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    ImportCertificateAuthorityCertificate
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newImportCertificateAuthorityCertificateResponse' smart constructor.
data ImportCertificateAuthorityCertificateResponse = ImportCertificateAuthorityCertificateResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ImportCertificateAuthorityCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newImportCertificateAuthorityCertificateResponse ::
  ImportCertificateAuthorityCertificateResponse
newImportCertificateAuthorityCertificateResponse =
  ImportCertificateAuthorityCertificateResponse'

instance
  Prelude.NFData
    ImportCertificateAuthorityCertificateResponse

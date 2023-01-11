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
-- Module      : Amazonka.CertificateManager.ImportCertificate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports a certificate into Certificate Manager (ACM) to use with
-- services that are integrated with ACM. Note that
-- <https://docs.aws.amazon.com/acm/latest/userguide/acm-services.html integrated services>
-- allow only certificate types and keys they support to be associated with
-- their resources. Further, their support differs depending on whether the
-- certificate is imported into IAM or into ACM. For more information, see
-- the documentation for each service. For more information about importing
-- certificates into ACM, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/import-certificate.html Importing Certificates>
-- in the /Certificate Manager User Guide/.
--
-- ACM does not provide
-- <https://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal>
-- for certificates that you import.
--
-- Note the following guidelines when importing third party certificates:
--
-- -   You must enter the private key that matches the certificate you are
--     importing.
--
-- -   The private key must be unencrypted. You cannot import a private key
--     that is protected by a password or a passphrase.
--
-- -   The private key must be no larger than 5 KB (5,120 bytes).
--
-- -   If the certificate you are importing is not self-signed, you must
--     enter its certificate chain.
--
-- -   If a certificate chain is included, the issuer must be the subject
--     of one of the certificates in the chain.
--
-- -   The certificate, private key, and certificate chain must be
--     PEM-encoded.
--
-- -   The current time must be between the @Not Before@ and @Not After@
--     certificate fields.
--
-- -   The @Issuer@ field must not be empty.
--
-- -   The OCSP authority URL, if present, must not exceed 1000 characters.
--
-- -   To import a new certificate, omit the @CertificateArn@ argument.
--     Include this argument only when you want to replace a previously
--     imported certificate.
--
-- -   When you import a certificate by using the CLI, you must specify the
--     certificate, the certificate chain, and the private key by their
--     file names preceded by @fileb:\/\/@. For example, you can specify a
--     certificate saved in the @C:\\temp@ folder as
--     @fileb:\/\/C:\\temp\\certificate_to_import.pem@. If you are making
--     an HTTP or HTTPS Query request, include these arguments as BLOBs.
--
-- -   When you import a certificate by using an SDK, you must specify the
--     certificate, the certificate chain, and the private key files in the
--     manner required by the programming language you\'re using.
--
-- -   The cryptographic algorithm of an imported certificate must match
--     the algorithm of the signing CA. For example, if the signing CA key
--     type is RSA, then the certificate key type must also be RSA.
--
-- This operation returns the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of the imported certificate.
module Amazonka.CertificateManager.ImportCertificate
  ( -- * Creating a Request
    ImportCertificate (..),
    newImportCertificate,

    -- * Request Lenses
    importCertificate_certificateArn,
    importCertificate_certificateChain,
    importCertificate_tags,
    importCertificate_certificate,
    importCertificate_privateKey,

    -- * Destructuring the Response
    ImportCertificateResponse (..),
    newImportCertificateResponse,

    -- * Response Lenses
    importCertificateResponse_certificateArn,
    importCertificateResponse_httpStatus,
  )
where

import Amazonka.CertificateManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newImportCertificate' smart constructor.
data ImportCertificate = ImportCertificate'
  { -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
    -- of an imported certificate to replace. To import a new certificate, omit
    -- this field.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The PEM encoded certificate chain.
    certificateChain :: Prelude.Maybe Data.Base64,
    -- | One or more resource tags to associate with the imported certificate.
    --
    -- Note: You cannot apply tags when reimporting a certificate.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The certificate to import.
    certificate :: Data.Base64,
    -- | The private key that matches the public key in the certificate.
    privateKey :: Data.Sensitive Data.Base64
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateArn', 'importCertificate_certificateArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of an imported certificate to replace. To import a new certificate, omit
-- this field.
--
-- 'certificateChain', 'importCertificate_certificateChain' - The PEM encoded certificate chain.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'tags', 'importCertificate_tags' - One or more resource tags to associate with the imported certificate.
--
-- Note: You cannot apply tags when reimporting a certificate.
--
-- 'certificate', 'importCertificate_certificate' - The certificate to import.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'privateKey', 'importCertificate_privateKey' - The private key that matches the public key in the certificate.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
newImportCertificate ::
  -- | 'certificate'
  Prelude.ByteString ->
  -- | 'privateKey'
  Prelude.ByteString ->
  ImportCertificate
newImportCertificate pCertificate_ pPrivateKey_ =
  ImportCertificate'
    { certificateArn =
        Prelude.Nothing,
      certificateChain = Prelude.Nothing,
      tags = Prelude.Nothing,
      certificate = Data._Base64 Lens.# pCertificate_,
      privateKey =
        Data._Sensitive Prelude.. Data._Base64
          Lens.# pPrivateKey_
    }

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of an imported certificate to replace. To import a new certificate, omit
-- this field.
importCertificate_certificateArn :: Lens.Lens' ImportCertificate (Prelude.Maybe Prelude.Text)
importCertificate_certificateArn = Lens.lens (\ImportCertificate' {certificateArn} -> certificateArn) (\s@ImportCertificate' {} a -> s {certificateArn = a} :: ImportCertificate)

-- | The PEM encoded certificate chain.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
importCertificate_certificateChain :: Lens.Lens' ImportCertificate (Prelude.Maybe Prelude.ByteString)
importCertificate_certificateChain = Lens.lens (\ImportCertificate' {certificateChain} -> certificateChain) (\s@ImportCertificate' {} a -> s {certificateChain = a} :: ImportCertificate) Prelude.. Lens.mapping Data._Base64

-- | One or more resource tags to associate with the imported certificate.
--
-- Note: You cannot apply tags when reimporting a certificate.
importCertificate_tags :: Lens.Lens' ImportCertificate (Prelude.Maybe (Prelude.NonEmpty Tag))
importCertificate_tags = Lens.lens (\ImportCertificate' {tags} -> tags) (\s@ImportCertificate' {} a -> s {tags = a} :: ImportCertificate) Prelude.. Lens.mapping Lens.coerced

-- | The certificate to import.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
importCertificate_certificate :: Lens.Lens' ImportCertificate Prelude.ByteString
importCertificate_certificate = Lens.lens (\ImportCertificate' {certificate} -> certificate) (\s@ImportCertificate' {} a -> s {certificate = a} :: ImportCertificate) Prelude.. Data._Base64

-- | The private key that matches the public key in the certificate.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
importCertificate_privateKey :: Lens.Lens' ImportCertificate Prelude.ByteString
importCertificate_privateKey = Lens.lens (\ImportCertificate' {privateKey} -> privateKey) (\s@ImportCertificate' {} a -> s {privateKey = a} :: ImportCertificate) Prelude.. Data._Sensitive Prelude.. Data._Base64

instance Core.AWSRequest ImportCertificate where
  type
    AWSResponse ImportCertificate =
      ImportCertificateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ImportCertificateResponse'
            Prelude.<$> (x Data..?> "CertificateArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ImportCertificate where
  hashWithSalt _salt ImportCertificate' {..} =
    _salt `Prelude.hashWithSalt` certificateArn
      `Prelude.hashWithSalt` certificateChain
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` certificate
      `Prelude.hashWithSalt` privateKey

instance Prelude.NFData ImportCertificate where
  rnf ImportCertificate' {..} =
    Prelude.rnf certificateArn
      `Prelude.seq` Prelude.rnf certificateChain
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf certificate
      `Prelude.seq` Prelude.rnf privateKey

instance Data.ToHeaders ImportCertificate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CertificateManager.ImportCertificate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ImportCertificate where
  toJSON ImportCertificate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CertificateArn" Data..=)
              Prelude.<$> certificateArn,
            ("CertificateChain" Data..=)
              Prelude.<$> certificateChain,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Certificate" Data..= certificate),
            Prelude.Just ("PrivateKey" Data..= privateKey)
          ]
      )

instance Data.ToPath ImportCertificate where
  toPath = Prelude.const "/"

instance Data.ToQuery ImportCertificate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newImportCertificateResponse' smart constructor.
data ImportCertificateResponse = ImportCertificateResponse'
  { -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
    -- of the imported certificate.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateArn', 'importCertificateResponse_certificateArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of the imported certificate.
--
-- 'httpStatus', 'importCertificateResponse_httpStatus' - The response's http status code.
newImportCertificateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ImportCertificateResponse
newImportCertificateResponse pHttpStatus_ =
  ImportCertificateResponse'
    { certificateArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of the imported certificate.
importCertificateResponse_certificateArn :: Lens.Lens' ImportCertificateResponse (Prelude.Maybe Prelude.Text)
importCertificateResponse_certificateArn = Lens.lens (\ImportCertificateResponse' {certificateArn} -> certificateArn) (\s@ImportCertificateResponse' {} a -> s {certificateArn = a} :: ImportCertificateResponse)

-- | The response's http status code.
importCertificateResponse_httpStatus :: Lens.Lens' ImportCertificateResponse Prelude.Int
importCertificateResponse_httpStatus = Lens.lens (\ImportCertificateResponse' {httpStatus} -> httpStatus) (\s@ImportCertificateResponse' {} a -> s {httpStatus = a} :: ImportCertificateResponse)

instance Prelude.NFData ImportCertificateResponse where
  rnf ImportCertificateResponse' {..} =
    Prelude.rnf certificateArn
      `Prelude.seq` Prelude.rnf httpStatus

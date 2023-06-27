{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DMS.Types.Certificate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.Certificate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The SSL certificate that can be used to encrypt connections between the
-- endpoints and the replication instance.
--
-- /See:/ 'newCertificate' smart constructor.
data Certificate = Certificate'
  { -- | The Amazon Resource Name (ARN) for the certificate.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The date that the certificate was created.
    certificateCreationDate :: Prelude.Maybe Data.POSIX,
    -- | A customer-assigned name for the certificate. Identifiers must begin
    -- with a letter and must contain only ASCII letters, digits, and hyphens.
    -- They can\'t end with a hyphen or contain two consecutive hyphens.
    certificateIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The owner of the certificate.
    certificateOwner :: Prelude.Maybe Prelude.Text,
    -- | The contents of a @.pem@ file, which contains an X.509 certificate.
    certificatePem :: Prelude.Maybe Prelude.Text,
    -- | The location of an imported Oracle Wallet certificate for use with SSL.
    -- Example: @filebase64(\"${path.root}\/rds-ca-2019-root.sso\")@
    certificateWallet :: Prelude.Maybe Data.Base64,
    -- | The key length of the cryptographic algorithm being used.
    keyLength :: Prelude.Maybe Prelude.Int,
    -- | The signing algorithm for the certificate.
    signingAlgorithm :: Prelude.Maybe Prelude.Text,
    -- | The beginning date that the certificate is valid.
    validFromDate :: Prelude.Maybe Data.POSIX,
    -- | The final date that the certificate is valid.
    validToDate :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Certificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateArn', 'certificate_certificateArn' - The Amazon Resource Name (ARN) for the certificate.
--
-- 'certificateCreationDate', 'certificate_certificateCreationDate' - The date that the certificate was created.
--
-- 'certificateIdentifier', 'certificate_certificateIdentifier' - A customer-assigned name for the certificate. Identifiers must begin
-- with a letter and must contain only ASCII letters, digits, and hyphens.
-- They can\'t end with a hyphen or contain two consecutive hyphens.
--
-- 'certificateOwner', 'certificate_certificateOwner' - The owner of the certificate.
--
-- 'certificatePem', 'certificate_certificatePem' - The contents of a @.pem@ file, which contains an X.509 certificate.
--
-- 'certificateWallet', 'certificate_certificateWallet' - The location of an imported Oracle Wallet certificate for use with SSL.
-- Example: @filebase64(\"${path.root}\/rds-ca-2019-root.sso\")@--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'keyLength', 'certificate_keyLength' - The key length of the cryptographic algorithm being used.
--
-- 'signingAlgorithm', 'certificate_signingAlgorithm' - The signing algorithm for the certificate.
--
-- 'validFromDate', 'certificate_validFromDate' - The beginning date that the certificate is valid.
--
-- 'validToDate', 'certificate_validToDate' - The final date that the certificate is valid.
newCertificate ::
  Certificate
newCertificate =
  Certificate'
    { certificateArn = Prelude.Nothing,
      certificateCreationDate = Prelude.Nothing,
      certificateIdentifier = Prelude.Nothing,
      certificateOwner = Prelude.Nothing,
      certificatePem = Prelude.Nothing,
      certificateWallet = Prelude.Nothing,
      keyLength = Prelude.Nothing,
      signingAlgorithm = Prelude.Nothing,
      validFromDate = Prelude.Nothing,
      validToDate = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) for the certificate.
certificate_certificateArn :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_certificateArn = Lens.lens (\Certificate' {certificateArn} -> certificateArn) (\s@Certificate' {} a -> s {certificateArn = a} :: Certificate)

-- | The date that the certificate was created.
certificate_certificateCreationDate :: Lens.Lens' Certificate (Prelude.Maybe Prelude.UTCTime)
certificate_certificateCreationDate = Lens.lens (\Certificate' {certificateCreationDate} -> certificateCreationDate) (\s@Certificate' {} a -> s {certificateCreationDate = a} :: Certificate) Prelude.. Lens.mapping Data._Time

-- | A customer-assigned name for the certificate. Identifiers must begin
-- with a letter and must contain only ASCII letters, digits, and hyphens.
-- They can\'t end with a hyphen or contain two consecutive hyphens.
certificate_certificateIdentifier :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_certificateIdentifier = Lens.lens (\Certificate' {certificateIdentifier} -> certificateIdentifier) (\s@Certificate' {} a -> s {certificateIdentifier = a} :: Certificate)

-- | The owner of the certificate.
certificate_certificateOwner :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_certificateOwner = Lens.lens (\Certificate' {certificateOwner} -> certificateOwner) (\s@Certificate' {} a -> s {certificateOwner = a} :: Certificate)

-- | The contents of a @.pem@ file, which contains an X.509 certificate.
certificate_certificatePem :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_certificatePem = Lens.lens (\Certificate' {certificatePem} -> certificatePem) (\s@Certificate' {} a -> s {certificatePem = a} :: Certificate)

-- | The location of an imported Oracle Wallet certificate for use with SSL.
-- Example: @filebase64(\"${path.root}\/rds-ca-2019-root.sso\")@--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
certificate_certificateWallet :: Lens.Lens' Certificate (Prelude.Maybe Prelude.ByteString)
certificate_certificateWallet = Lens.lens (\Certificate' {certificateWallet} -> certificateWallet) (\s@Certificate' {} a -> s {certificateWallet = a} :: Certificate) Prelude.. Lens.mapping Data._Base64

-- | The key length of the cryptographic algorithm being used.
certificate_keyLength :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Int)
certificate_keyLength = Lens.lens (\Certificate' {keyLength} -> keyLength) (\s@Certificate' {} a -> s {keyLength = a} :: Certificate)

-- | The signing algorithm for the certificate.
certificate_signingAlgorithm :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_signingAlgorithm = Lens.lens (\Certificate' {signingAlgorithm} -> signingAlgorithm) (\s@Certificate' {} a -> s {signingAlgorithm = a} :: Certificate)

-- | The beginning date that the certificate is valid.
certificate_validFromDate :: Lens.Lens' Certificate (Prelude.Maybe Prelude.UTCTime)
certificate_validFromDate = Lens.lens (\Certificate' {validFromDate} -> validFromDate) (\s@Certificate' {} a -> s {validFromDate = a} :: Certificate) Prelude.. Lens.mapping Data._Time

-- | The final date that the certificate is valid.
certificate_validToDate :: Lens.Lens' Certificate (Prelude.Maybe Prelude.UTCTime)
certificate_validToDate = Lens.lens (\Certificate' {validToDate} -> validToDate) (\s@Certificate' {} a -> s {validToDate = a} :: Certificate) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON Certificate where
  parseJSON =
    Data.withObject
      "Certificate"
      ( \x ->
          Certificate'
            Prelude.<$> (x Data..:? "CertificateArn")
            Prelude.<*> (x Data..:? "CertificateCreationDate")
            Prelude.<*> (x Data..:? "CertificateIdentifier")
            Prelude.<*> (x Data..:? "CertificateOwner")
            Prelude.<*> (x Data..:? "CertificatePem")
            Prelude.<*> (x Data..:? "CertificateWallet")
            Prelude.<*> (x Data..:? "KeyLength")
            Prelude.<*> (x Data..:? "SigningAlgorithm")
            Prelude.<*> (x Data..:? "ValidFromDate")
            Prelude.<*> (x Data..:? "ValidToDate")
      )

instance Prelude.Hashable Certificate where
  hashWithSalt _salt Certificate' {..} =
    _salt
      `Prelude.hashWithSalt` certificateArn
      `Prelude.hashWithSalt` certificateCreationDate
      `Prelude.hashWithSalt` certificateIdentifier
      `Prelude.hashWithSalt` certificateOwner
      `Prelude.hashWithSalt` certificatePem
      `Prelude.hashWithSalt` certificateWallet
      `Prelude.hashWithSalt` keyLength
      `Prelude.hashWithSalt` signingAlgorithm
      `Prelude.hashWithSalt` validFromDate
      `Prelude.hashWithSalt` validToDate

instance Prelude.NFData Certificate where
  rnf Certificate' {..} =
    Prelude.rnf certificateArn
      `Prelude.seq` Prelude.rnf certificateCreationDate
      `Prelude.seq` Prelude.rnf certificateIdentifier
      `Prelude.seq` Prelude.rnf certificateOwner
      `Prelude.seq` Prelude.rnf certificatePem
      `Prelude.seq` Prelude.rnf certificateWallet
      `Prelude.seq` Prelude.rnf keyLength
      `Prelude.seq` Prelude.rnf signingAlgorithm
      `Prelude.seq` Prelude.rnf validFromDate
      `Prelude.seq` Prelude.rnf validToDate

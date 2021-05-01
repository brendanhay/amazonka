{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.DMS.Types.Certificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.Certificate where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The SSL certificate that can be used to encrypt connections between the
-- endpoints and the replication instance.
--
-- /See:/ 'newCertificate' smart constructor.
data Certificate = Certificate'
  { -- | The owner of the certificate.
    certificateOwner :: Prelude.Maybe Prelude.Text,
    -- | The signing algorithm for the certificate.
    signingAlgorithm :: Prelude.Maybe Prelude.Text,
    -- | The final date that the certificate is valid.
    validToDate :: Prelude.Maybe Prelude.POSIX,
    -- | A customer-assigned name for the certificate. Identifiers must begin
    -- with a letter and must contain only ASCII letters, digits, and hyphens.
    -- They can\'t end with a hyphen or contain two consecutive hyphens.
    certificateIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The key length of the cryptographic algorithm being used.
    keyLength :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) for the certificate.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The location of an imported Oracle Wallet certificate for use with SSL.
    certificateWallet :: Prelude.Maybe Prelude.Base64,
    -- | The beginning date that the certificate is valid.
    validFromDate :: Prelude.Maybe Prelude.POSIX,
    -- | The date that the certificate was created.
    certificateCreationDate :: Prelude.Maybe Prelude.POSIX,
    -- | The contents of a @.pem@ file, which contains an X.509 certificate.
    certificatePem :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Certificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateOwner', 'certificate_certificateOwner' - The owner of the certificate.
--
-- 'signingAlgorithm', 'certificate_signingAlgorithm' - The signing algorithm for the certificate.
--
-- 'validToDate', 'certificate_validToDate' - The final date that the certificate is valid.
--
-- 'certificateIdentifier', 'certificate_certificateIdentifier' - A customer-assigned name for the certificate. Identifiers must begin
-- with a letter and must contain only ASCII letters, digits, and hyphens.
-- They can\'t end with a hyphen or contain two consecutive hyphens.
--
-- 'keyLength', 'certificate_keyLength' - The key length of the cryptographic algorithm being used.
--
-- 'certificateArn', 'certificate_certificateArn' - The Amazon Resource Name (ARN) for the certificate.
--
-- 'certificateWallet', 'certificate_certificateWallet' - The location of an imported Oracle Wallet certificate for use with SSL.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'validFromDate', 'certificate_validFromDate' - The beginning date that the certificate is valid.
--
-- 'certificateCreationDate', 'certificate_certificateCreationDate' - The date that the certificate was created.
--
-- 'certificatePem', 'certificate_certificatePem' - The contents of a @.pem@ file, which contains an X.509 certificate.
newCertificate ::
  Certificate
newCertificate =
  Certificate'
    { certificateOwner = Prelude.Nothing,
      signingAlgorithm = Prelude.Nothing,
      validToDate = Prelude.Nothing,
      certificateIdentifier = Prelude.Nothing,
      keyLength = Prelude.Nothing,
      certificateArn = Prelude.Nothing,
      certificateWallet = Prelude.Nothing,
      validFromDate = Prelude.Nothing,
      certificateCreationDate = Prelude.Nothing,
      certificatePem = Prelude.Nothing
    }

-- | The owner of the certificate.
certificate_certificateOwner :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_certificateOwner = Lens.lens (\Certificate' {certificateOwner} -> certificateOwner) (\s@Certificate' {} a -> s {certificateOwner = a} :: Certificate)

-- | The signing algorithm for the certificate.
certificate_signingAlgorithm :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_signingAlgorithm = Lens.lens (\Certificate' {signingAlgorithm} -> signingAlgorithm) (\s@Certificate' {} a -> s {signingAlgorithm = a} :: Certificate)

-- | The final date that the certificate is valid.
certificate_validToDate :: Lens.Lens' Certificate (Prelude.Maybe Prelude.UTCTime)
certificate_validToDate = Lens.lens (\Certificate' {validToDate} -> validToDate) (\s@Certificate' {} a -> s {validToDate = a} :: Certificate) Prelude.. Lens.mapping Prelude._Time

-- | A customer-assigned name for the certificate. Identifiers must begin
-- with a letter and must contain only ASCII letters, digits, and hyphens.
-- They can\'t end with a hyphen or contain two consecutive hyphens.
certificate_certificateIdentifier :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_certificateIdentifier = Lens.lens (\Certificate' {certificateIdentifier} -> certificateIdentifier) (\s@Certificate' {} a -> s {certificateIdentifier = a} :: Certificate)

-- | The key length of the cryptographic algorithm being used.
certificate_keyLength :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Int)
certificate_keyLength = Lens.lens (\Certificate' {keyLength} -> keyLength) (\s@Certificate' {} a -> s {keyLength = a} :: Certificate)

-- | The Amazon Resource Name (ARN) for the certificate.
certificate_certificateArn :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_certificateArn = Lens.lens (\Certificate' {certificateArn} -> certificateArn) (\s@Certificate' {} a -> s {certificateArn = a} :: Certificate)

-- | The location of an imported Oracle Wallet certificate for use with SSL.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
certificate_certificateWallet :: Lens.Lens' Certificate (Prelude.Maybe Prelude.ByteString)
certificate_certificateWallet = Lens.lens (\Certificate' {certificateWallet} -> certificateWallet) (\s@Certificate' {} a -> s {certificateWallet = a} :: Certificate) Prelude.. Lens.mapping Prelude._Base64

-- | The beginning date that the certificate is valid.
certificate_validFromDate :: Lens.Lens' Certificate (Prelude.Maybe Prelude.UTCTime)
certificate_validFromDate = Lens.lens (\Certificate' {validFromDate} -> validFromDate) (\s@Certificate' {} a -> s {validFromDate = a} :: Certificate) Prelude.. Lens.mapping Prelude._Time

-- | The date that the certificate was created.
certificate_certificateCreationDate :: Lens.Lens' Certificate (Prelude.Maybe Prelude.UTCTime)
certificate_certificateCreationDate = Lens.lens (\Certificate' {certificateCreationDate} -> certificateCreationDate) (\s@Certificate' {} a -> s {certificateCreationDate = a} :: Certificate) Prelude.. Lens.mapping Prelude._Time

-- | The contents of a @.pem@ file, which contains an X.509 certificate.
certificate_certificatePem :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_certificatePem = Lens.lens (\Certificate' {certificatePem} -> certificatePem) (\s@Certificate' {} a -> s {certificatePem = a} :: Certificate)

instance Prelude.FromJSON Certificate where
  parseJSON =
    Prelude.withObject
      "Certificate"
      ( \x ->
          Certificate'
            Prelude.<$> (x Prelude..:? "CertificateOwner")
            Prelude.<*> (x Prelude..:? "SigningAlgorithm")
            Prelude.<*> (x Prelude..:? "ValidToDate")
            Prelude.<*> (x Prelude..:? "CertificateIdentifier")
            Prelude.<*> (x Prelude..:? "KeyLength")
            Prelude.<*> (x Prelude..:? "CertificateArn")
            Prelude.<*> (x Prelude..:? "CertificateWallet")
            Prelude.<*> (x Prelude..:? "ValidFromDate")
            Prelude.<*> (x Prelude..:? "CertificateCreationDate")
            Prelude.<*> (x Prelude..:? "CertificatePem")
      )

instance Prelude.Hashable Certificate

instance Prelude.NFData Certificate

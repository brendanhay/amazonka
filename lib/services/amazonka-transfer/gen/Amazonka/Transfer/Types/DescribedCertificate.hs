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
-- Module      : Amazonka.Transfer.Types.DescribedCertificate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.DescribedCertificate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transfer.Types.CertificateStatusType
import Amazonka.Transfer.Types.CertificateType
import Amazonka.Transfer.Types.CertificateUsageType
import Amazonka.Transfer.Types.Tag

-- | Describes the properties of a certificate.
--
-- /See:/ 'newDescribedCertificate' smart constructor.
data DescribedCertificate = DescribedCertificate'
  { -- | An optional date that specifies when the certificate becomes active.
    activeDate :: Prelude.Maybe Data.POSIX,
    -- | The file name for the certificate.
    certificate :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The list of certificates that make up the chain for the certificate.
    certificateChain :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | An array of identifiers for the imported certificates. You use this
    -- identifier for working with profiles and partner profiles.
    certificateId :: Prelude.Maybe Prelude.Text,
    -- | The name or description that\'s used to identity the certificate.
    description :: Prelude.Maybe Prelude.Text,
    -- | An optional date that specifies when the certificate becomes inactive.
    inactiveDate :: Prelude.Maybe Data.POSIX,
    -- | The final date that the certificate is valid.
    notAfterDate :: Prelude.Maybe Data.POSIX,
    -- | The earliest date that the certificate is valid.
    notBeforeDate :: Prelude.Maybe Data.POSIX,
    -- | The serial number for the certificate.
    serial :: Prelude.Maybe Prelude.Text,
    -- | The certificate can be either @ACTIVE@, @PENDING_ROTATION@, or
    -- @INACTIVE@. @PENDING_ROTATION@ means that this certificate will replace
    -- the current certificate when it expires.
    status :: Prelude.Maybe CertificateStatusType,
    -- | Key-value pairs that can be used to group and search for certificates.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | If a private key has been specified for the certificate, its type is
    -- @CERTIFICATE_WITH_PRIVATE_KEY@. If there is no private key, the type is
    -- @CERTIFICATE@.
    type' :: Prelude.Maybe CertificateType,
    -- | Specifies whether this certificate is used for signing or encryption.
    usage :: Prelude.Maybe CertificateUsageType,
    -- | The unique Amazon Resource Name (ARN) for the certificate.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribedCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeDate', 'describedCertificate_activeDate' - An optional date that specifies when the certificate becomes active.
--
-- 'certificate', 'describedCertificate_certificate' - The file name for the certificate.
--
-- 'certificateChain', 'describedCertificate_certificateChain' - The list of certificates that make up the chain for the certificate.
--
-- 'certificateId', 'describedCertificate_certificateId' - An array of identifiers for the imported certificates. You use this
-- identifier for working with profiles and partner profiles.
--
-- 'description', 'describedCertificate_description' - The name or description that\'s used to identity the certificate.
--
-- 'inactiveDate', 'describedCertificate_inactiveDate' - An optional date that specifies when the certificate becomes inactive.
--
-- 'notAfterDate', 'describedCertificate_notAfterDate' - The final date that the certificate is valid.
--
-- 'notBeforeDate', 'describedCertificate_notBeforeDate' - The earliest date that the certificate is valid.
--
-- 'serial', 'describedCertificate_serial' - The serial number for the certificate.
--
-- 'status', 'describedCertificate_status' - The certificate can be either @ACTIVE@, @PENDING_ROTATION@, or
-- @INACTIVE@. @PENDING_ROTATION@ means that this certificate will replace
-- the current certificate when it expires.
--
-- 'tags', 'describedCertificate_tags' - Key-value pairs that can be used to group and search for certificates.
--
-- 'type'', 'describedCertificate_type' - If a private key has been specified for the certificate, its type is
-- @CERTIFICATE_WITH_PRIVATE_KEY@. If there is no private key, the type is
-- @CERTIFICATE@.
--
-- 'usage', 'describedCertificate_usage' - Specifies whether this certificate is used for signing or encryption.
--
-- 'arn', 'describedCertificate_arn' - The unique Amazon Resource Name (ARN) for the certificate.
newDescribedCertificate ::
  -- | 'arn'
  Prelude.Text ->
  DescribedCertificate
newDescribedCertificate pArn_ =
  DescribedCertificate'
    { activeDate = Prelude.Nothing,
      certificate = Prelude.Nothing,
      certificateChain = Prelude.Nothing,
      certificateId = Prelude.Nothing,
      description = Prelude.Nothing,
      inactiveDate = Prelude.Nothing,
      notAfterDate = Prelude.Nothing,
      notBeforeDate = Prelude.Nothing,
      serial = Prelude.Nothing,
      status = Prelude.Nothing,
      tags = Prelude.Nothing,
      type' = Prelude.Nothing,
      usage = Prelude.Nothing,
      arn = pArn_
    }

-- | An optional date that specifies when the certificate becomes active.
describedCertificate_activeDate :: Lens.Lens' DescribedCertificate (Prelude.Maybe Prelude.UTCTime)
describedCertificate_activeDate = Lens.lens (\DescribedCertificate' {activeDate} -> activeDate) (\s@DescribedCertificate' {} a -> s {activeDate = a} :: DescribedCertificate) Prelude.. Lens.mapping Data._Time

-- | The file name for the certificate.
describedCertificate_certificate :: Lens.Lens' DescribedCertificate (Prelude.Maybe Prelude.Text)
describedCertificate_certificate = Lens.lens (\DescribedCertificate' {certificate} -> certificate) (\s@DescribedCertificate' {} a -> s {certificate = a} :: DescribedCertificate) Prelude.. Lens.mapping Data._Sensitive

-- | The list of certificates that make up the chain for the certificate.
describedCertificate_certificateChain :: Lens.Lens' DescribedCertificate (Prelude.Maybe Prelude.Text)
describedCertificate_certificateChain = Lens.lens (\DescribedCertificate' {certificateChain} -> certificateChain) (\s@DescribedCertificate' {} a -> s {certificateChain = a} :: DescribedCertificate) Prelude.. Lens.mapping Data._Sensitive

-- | An array of identifiers for the imported certificates. You use this
-- identifier for working with profiles and partner profiles.
describedCertificate_certificateId :: Lens.Lens' DescribedCertificate (Prelude.Maybe Prelude.Text)
describedCertificate_certificateId = Lens.lens (\DescribedCertificate' {certificateId} -> certificateId) (\s@DescribedCertificate' {} a -> s {certificateId = a} :: DescribedCertificate)

-- | The name or description that\'s used to identity the certificate.
describedCertificate_description :: Lens.Lens' DescribedCertificate (Prelude.Maybe Prelude.Text)
describedCertificate_description = Lens.lens (\DescribedCertificate' {description} -> description) (\s@DescribedCertificate' {} a -> s {description = a} :: DescribedCertificate)

-- | An optional date that specifies when the certificate becomes inactive.
describedCertificate_inactiveDate :: Lens.Lens' DescribedCertificate (Prelude.Maybe Prelude.UTCTime)
describedCertificate_inactiveDate = Lens.lens (\DescribedCertificate' {inactiveDate} -> inactiveDate) (\s@DescribedCertificate' {} a -> s {inactiveDate = a} :: DescribedCertificate) Prelude.. Lens.mapping Data._Time

-- | The final date that the certificate is valid.
describedCertificate_notAfterDate :: Lens.Lens' DescribedCertificate (Prelude.Maybe Prelude.UTCTime)
describedCertificate_notAfterDate = Lens.lens (\DescribedCertificate' {notAfterDate} -> notAfterDate) (\s@DescribedCertificate' {} a -> s {notAfterDate = a} :: DescribedCertificate) Prelude.. Lens.mapping Data._Time

-- | The earliest date that the certificate is valid.
describedCertificate_notBeforeDate :: Lens.Lens' DescribedCertificate (Prelude.Maybe Prelude.UTCTime)
describedCertificate_notBeforeDate = Lens.lens (\DescribedCertificate' {notBeforeDate} -> notBeforeDate) (\s@DescribedCertificate' {} a -> s {notBeforeDate = a} :: DescribedCertificate) Prelude.. Lens.mapping Data._Time

-- | The serial number for the certificate.
describedCertificate_serial :: Lens.Lens' DescribedCertificate (Prelude.Maybe Prelude.Text)
describedCertificate_serial = Lens.lens (\DescribedCertificate' {serial} -> serial) (\s@DescribedCertificate' {} a -> s {serial = a} :: DescribedCertificate)

-- | The certificate can be either @ACTIVE@, @PENDING_ROTATION@, or
-- @INACTIVE@. @PENDING_ROTATION@ means that this certificate will replace
-- the current certificate when it expires.
describedCertificate_status :: Lens.Lens' DescribedCertificate (Prelude.Maybe CertificateStatusType)
describedCertificate_status = Lens.lens (\DescribedCertificate' {status} -> status) (\s@DescribedCertificate' {} a -> s {status = a} :: DescribedCertificate)

-- | Key-value pairs that can be used to group and search for certificates.
describedCertificate_tags :: Lens.Lens' DescribedCertificate (Prelude.Maybe (Prelude.NonEmpty Tag))
describedCertificate_tags = Lens.lens (\DescribedCertificate' {tags} -> tags) (\s@DescribedCertificate' {} a -> s {tags = a} :: DescribedCertificate) Prelude.. Lens.mapping Lens.coerced

-- | If a private key has been specified for the certificate, its type is
-- @CERTIFICATE_WITH_PRIVATE_KEY@. If there is no private key, the type is
-- @CERTIFICATE@.
describedCertificate_type :: Lens.Lens' DescribedCertificate (Prelude.Maybe CertificateType)
describedCertificate_type = Lens.lens (\DescribedCertificate' {type'} -> type') (\s@DescribedCertificate' {} a -> s {type' = a} :: DescribedCertificate)

-- | Specifies whether this certificate is used for signing or encryption.
describedCertificate_usage :: Lens.Lens' DescribedCertificate (Prelude.Maybe CertificateUsageType)
describedCertificate_usage = Lens.lens (\DescribedCertificate' {usage} -> usage) (\s@DescribedCertificate' {} a -> s {usage = a} :: DescribedCertificate)

-- | The unique Amazon Resource Name (ARN) for the certificate.
describedCertificate_arn :: Lens.Lens' DescribedCertificate Prelude.Text
describedCertificate_arn = Lens.lens (\DescribedCertificate' {arn} -> arn) (\s@DescribedCertificate' {} a -> s {arn = a} :: DescribedCertificate)

instance Data.FromJSON DescribedCertificate where
  parseJSON =
    Data.withObject
      "DescribedCertificate"
      ( \x ->
          DescribedCertificate'
            Prelude.<$> (x Data..:? "ActiveDate")
            Prelude.<*> (x Data..:? "Certificate")
            Prelude.<*> (x Data..:? "CertificateChain")
            Prelude.<*> (x Data..:? "CertificateId")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "InactiveDate")
            Prelude.<*> (x Data..:? "NotAfterDate")
            Prelude.<*> (x Data..:? "NotBeforeDate")
            Prelude.<*> (x Data..:? "Serial")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Tags")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "Usage")
            Prelude.<*> (x Data..: "Arn")
      )

instance Prelude.Hashable DescribedCertificate where
  hashWithSalt _salt DescribedCertificate' {..} =
    _salt
      `Prelude.hashWithSalt` activeDate
      `Prelude.hashWithSalt` certificate
      `Prelude.hashWithSalt` certificateChain
      `Prelude.hashWithSalt` certificateId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` inactiveDate
      `Prelude.hashWithSalt` notAfterDate
      `Prelude.hashWithSalt` notBeforeDate
      `Prelude.hashWithSalt` serial
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` usage
      `Prelude.hashWithSalt` arn

instance Prelude.NFData DescribedCertificate where
  rnf DescribedCertificate' {..} =
    Prelude.rnf activeDate
      `Prelude.seq` Prelude.rnf certificate
      `Prelude.seq` Prelude.rnf certificateChain
      `Prelude.seq` Prelude.rnf certificateId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf inactiveDate
      `Prelude.seq` Prelude.rnf notAfterDate
      `Prelude.seq` Prelude.rnf notBeforeDate
      `Prelude.seq` Prelude.rnf serial
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf usage
      `Prelude.seq` Prelude.rnf arn

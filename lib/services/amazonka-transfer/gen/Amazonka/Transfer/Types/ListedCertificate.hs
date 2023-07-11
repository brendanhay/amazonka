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
-- Module      : Amazonka.Transfer.Types.ListedCertificate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.ListedCertificate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transfer.Types.CertificateStatusType
import Amazonka.Transfer.Types.CertificateType
import Amazonka.Transfer.Types.CertificateUsageType

-- | Describes the properties of a certificate.
--
-- /See:/ 'newListedCertificate' smart constructor.
data ListedCertificate = ListedCertificate'
  { -- | An optional date that specifies when the certificate becomes active.
    activeDate :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the specified certificate.
    arn :: Prelude.Maybe Prelude.Text,
    -- | An array of identifiers for the imported certificates. You use this
    -- identifier for working with profiles and partner profiles.
    certificateId :: Prelude.Maybe Prelude.Text,
    -- | The name or short description that\'s used to identify the certificate.
    description :: Prelude.Maybe Prelude.Text,
    -- | An optional date that specifies when the certificate becomes inactive.
    inactiveDate :: Prelude.Maybe Data.POSIX,
    -- | The certificate can be either @ACTIVE@, @PENDING_ROTATION@, or
    -- @INACTIVE@. @PENDING_ROTATION@ means that this certificate will replace
    -- the current certificate when it expires.
    status :: Prelude.Maybe CertificateStatusType,
    -- | The type for the certificate. If a private key has been specified for
    -- the certificate, its type is @CERTIFICATE_WITH_PRIVATE_KEY@. If there is
    -- no private key, the type is @CERTIFICATE@.
    type' :: Prelude.Maybe CertificateType,
    -- | Specifies whether this certificate is used for signing or encryption.
    usage :: Prelude.Maybe CertificateUsageType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListedCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeDate', 'listedCertificate_activeDate' - An optional date that specifies when the certificate becomes active.
--
-- 'arn', 'listedCertificate_arn' - The Amazon Resource Name (ARN) of the specified certificate.
--
-- 'certificateId', 'listedCertificate_certificateId' - An array of identifiers for the imported certificates. You use this
-- identifier for working with profiles and partner profiles.
--
-- 'description', 'listedCertificate_description' - The name or short description that\'s used to identify the certificate.
--
-- 'inactiveDate', 'listedCertificate_inactiveDate' - An optional date that specifies when the certificate becomes inactive.
--
-- 'status', 'listedCertificate_status' - The certificate can be either @ACTIVE@, @PENDING_ROTATION@, or
-- @INACTIVE@. @PENDING_ROTATION@ means that this certificate will replace
-- the current certificate when it expires.
--
-- 'type'', 'listedCertificate_type' - The type for the certificate. If a private key has been specified for
-- the certificate, its type is @CERTIFICATE_WITH_PRIVATE_KEY@. If there is
-- no private key, the type is @CERTIFICATE@.
--
-- 'usage', 'listedCertificate_usage' - Specifies whether this certificate is used for signing or encryption.
newListedCertificate ::
  ListedCertificate
newListedCertificate =
  ListedCertificate'
    { activeDate = Prelude.Nothing,
      arn = Prelude.Nothing,
      certificateId = Prelude.Nothing,
      description = Prelude.Nothing,
      inactiveDate = Prelude.Nothing,
      status = Prelude.Nothing,
      type' = Prelude.Nothing,
      usage = Prelude.Nothing
    }

-- | An optional date that specifies when the certificate becomes active.
listedCertificate_activeDate :: Lens.Lens' ListedCertificate (Prelude.Maybe Prelude.UTCTime)
listedCertificate_activeDate = Lens.lens (\ListedCertificate' {activeDate} -> activeDate) (\s@ListedCertificate' {} a -> s {activeDate = a} :: ListedCertificate) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the specified certificate.
listedCertificate_arn :: Lens.Lens' ListedCertificate (Prelude.Maybe Prelude.Text)
listedCertificate_arn = Lens.lens (\ListedCertificate' {arn} -> arn) (\s@ListedCertificate' {} a -> s {arn = a} :: ListedCertificate)

-- | An array of identifiers for the imported certificates. You use this
-- identifier for working with profiles and partner profiles.
listedCertificate_certificateId :: Lens.Lens' ListedCertificate (Prelude.Maybe Prelude.Text)
listedCertificate_certificateId = Lens.lens (\ListedCertificate' {certificateId} -> certificateId) (\s@ListedCertificate' {} a -> s {certificateId = a} :: ListedCertificate)

-- | The name or short description that\'s used to identify the certificate.
listedCertificate_description :: Lens.Lens' ListedCertificate (Prelude.Maybe Prelude.Text)
listedCertificate_description = Lens.lens (\ListedCertificate' {description} -> description) (\s@ListedCertificate' {} a -> s {description = a} :: ListedCertificate)

-- | An optional date that specifies when the certificate becomes inactive.
listedCertificate_inactiveDate :: Lens.Lens' ListedCertificate (Prelude.Maybe Prelude.UTCTime)
listedCertificate_inactiveDate = Lens.lens (\ListedCertificate' {inactiveDate} -> inactiveDate) (\s@ListedCertificate' {} a -> s {inactiveDate = a} :: ListedCertificate) Prelude.. Lens.mapping Data._Time

-- | The certificate can be either @ACTIVE@, @PENDING_ROTATION@, or
-- @INACTIVE@. @PENDING_ROTATION@ means that this certificate will replace
-- the current certificate when it expires.
listedCertificate_status :: Lens.Lens' ListedCertificate (Prelude.Maybe CertificateStatusType)
listedCertificate_status = Lens.lens (\ListedCertificate' {status} -> status) (\s@ListedCertificate' {} a -> s {status = a} :: ListedCertificate)

-- | The type for the certificate. If a private key has been specified for
-- the certificate, its type is @CERTIFICATE_WITH_PRIVATE_KEY@. If there is
-- no private key, the type is @CERTIFICATE@.
listedCertificate_type :: Lens.Lens' ListedCertificate (Prelude.Maybe CertificateType)
listedCertificate_type = Lens.lens (\ListedCertificate' {type'} -> type') (\s@ListedCertificate' {} a -> s {type' = a} :: ListedCertificate)

-- | Specifies whether this certificate is used for signing or encryption.
listedCertificate_usage :: Lens.Lens' ListedCertificate (Prelude.Maybe CertificateUsageType)
listedCertificate_usage = Lens.lens (\ListedCertificate' {usage} -> usage) (\s@ListedCertificate' {} a -> s {usage = a} :: ListedCertificate)

instance Data.FromJSON ListedCertificate where
  parseJSON =
    Data.withObject
      "ListedCertificate"
      ( \x ->
          ListedCertificate'
            Prelude.<$> (x Data..:? "ActiveDate")
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CertificateId")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "InactiveDate")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "Usage")
      )

instance Prelude.Hashable ListedCertificate where
  hashWithSalt _salt ListedCertificate' {..} =
    _salt
      `Prelude.hashWithSalt` activeDate
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` certificateId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` inactiveDate
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` usage

instance Prelude.NFData ListedCertificate where
  rnf ListedCertificate' {..} =
    Prelude.rnf activeDate
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf certificateId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf inactiveDate
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf usage

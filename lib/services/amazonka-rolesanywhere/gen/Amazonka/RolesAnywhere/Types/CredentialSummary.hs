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
-- Module      : Amazonka.RolesAnywhere.Types.CredentialSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RolesAnywhere.Types.CredentialSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A record of a presented X509 credential to
-- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>.
--
-- /See:/ 'newCredentialSummary' smart constructor.
data CredentialSummary = CredentialSummary'
  { -- | The fully qualified domain name of the issuing certificate for the
    -- presented end-entity certificate.
    issuer :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the
    -- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
    -- operation was successful.
    failed :: Prelude.Maybe Prelude.Bool,
    -- | The ISO-8601 time stamp of when the certificate was last used in a
    -- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
    -- operation.
    seenAt :: Prelude.Maybe Core.POSIX,
    -- | Indicates whether the credential is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The serial number of the certificate.
    serialNumber :: Prelude.Maybe Prelude.Text,
    -- | The PEM-encoded data of the certificate.
    x509CertificateData :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CredentialSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'issuer', 'credentialSummary_issuer' - The fully qualified domain name of the issuing certificate for the
-- presented end-entity certificate.
--
-- 'failed', 'credentialSummary_failed' - Indicates whether the
-- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
-- operation was successful.
--
-- 'seenAt', 'credentialSummary_seenAt' - The ISO-8601 time stamp of when the certificate was last used in a
-- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
-- operation.
--
-- 'enabled', 'credentialSummary_enabled' - Indicates whether the credential is enabled.
--
-- 'serialNumber', 'credentialSummary_serialNumber' - The serial number of the certificate.
--
-- 'x509CertificateData', 'credentialSummary_x509CertificateData' - The PEM-encoded data of the certificate.
newCredentialSummary ::
  CredentialSummary
newCredentialSummary =
  CredentialSummary'
    { issuer = Prelude.Nothing,
      failed = Prelude.Nothing,
      seenAt = Prelude.Nothing,
      enabled = Prelude.Nothing,
      serialNumber = Prelude.Nothing,
      x509CertificateData = Prelude.Nothing
    }

-- | The fully qualified domain name of the issuing certificate for the
-- presented end-entity certificate.
credentialSummary_issuer :: Lens.Lens' CredentialSummary (Prelude.Maybe Prelude.Text)
credentialSummary_issuer = Lens.lens (\CredentialSummary' {issuer} -> issuer) (\s@CredentialSummary' {} a -> s {issuer = a} :: CredentialSummary)

-- | Indicates whether the
-- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
-- operation was successful.
credentialSummary_failed :: Lens.Lens' CredentialSummary (Prelude.Maybe Prelude.Bool)
credentialSummary_failed = Lens.lens (\CredentialSummary' {failed} -> failed) (\s@CredentialSummary' {} a -> s {failed = a} :: CredentialSummary)

-- | The ISO-8601 time stamp of when the certificate was last used in a
-- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
-- operation.
credentialSummary_seenAt :: Lens.Lens' CredentialSummary (Prelude.Maybe Prelude.UTCTime)
credentialSummary_seenAt = Lens.lens (\CredentialSummary' {seenAt} -> seenAt) (\s@CredentialSummary' {} a -> s {seenAt = a} :: CredentialSummary) Prelude.. Lens.mapping Core._Time

-- | Indicates whether the credential is enabled.
credentialSummary_enabled :: Lens.Lens' CredentialSummary (Prelude.Maybe Prelude.Bool)
credentialSummary_enabled = Lens.lens (\CredentialSummary' {enabled} -> enabled) (\s@CredentialSummary' {} a -> s {enabled = a} :: CredentialSummary)

-- | The serial number of the certificate.
credentialSummary_serialNumber :: Lens.Lens' CredentialSummary (Prelude.Maybe Prelude.Text)
credentialSummary_serialNumber = Lens.lens (\CredentialSummary' {serialNumber} -> serialNumber) (\s@CredentialSummary' {} a -> s {serialNumber = a} :: CredentialSummary)

-- | The PEM-encoded data of the certificate.
credentialSummary_x509CertificateData :: Lens.Lens' CredentialSummary (Prelude.Maybe Prelude.Text)
credentialSummary_x509CertificateData = Lens.lens (\CredentialSummary' {x509CertificateData} -> x509CertificateData) (\s@CredentialSummary' {} a -> s {x509CertificateData = a} :: CredentialSummary)

instance Core.FromJSON CredentialSummary where
  parseJSON =
    Core.withObject
      "CredentialSummary"
      ( \x ->
          CredentialSummary'
            Prelude.<$> (x Core..:? "issuer")
            Prelude.<*> (x Core..:? "failed")
            Prelude.<*> (x Core..:? "seenAt")
            Prelude.<*> (x Core..:? "enabled")
            Prelude.<*> (x Core..:? "serialNumber")
            Prelude.<*> (x Core..:? "x509CertificateData")
      )

instance Prelude.Hashable CredentialSummary where
  hashWithSalt _salt CredentialSummary' {..} =
    _salt `Prelude.hashWithSalt` issuer
      `Prelude.hashWithSalt` failed
      `Prelude.hashWithSalt` seenAt
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` serialNumber
      `Prelude.hashWithSalt` x509CertificateData

instance Prelude.NFData CredentialSummary where
  rnf CredentialSummary' {..} =
    Prelude.rnf issuer
      `Prelude.seq` Prelude.rnf failed
      `Prelude.seq` Prelude.rnf seenAt
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf serialNumber
      `Prelude.seq` Prelude.rnf x509CertificateData

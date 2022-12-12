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
-- Module      : Amazonka.Lightsail.Types.RenewalSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.RenewalSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.DomainValidationRecord
import Amazonka.Lightsail.Types.RenewalStatus
import qualified Amazonka.Prelude as Prelude

-- | Describes the status of a SSL\/TLS certificate renewal managed by Amazon
-- Lightsail.
--
-- /See:/ 'newRenewalSummary' smart constructor.
data RenewalSummary = RenewalSummary'
  { -- | An array of objects that describe the domain validation records of the
    -- certificate.
    domainValidationRecords :: Prelude.Maybe [DomainValidationRecord],
    -- | The renewal status of the certificate.
    --
    -- The following renewal status are possible:
    --
    -- -   __@PendingAutoRenewal@__ - Lightsail is attempting to automatically
    --     validate the domain names of the certificate. No further action is
    --     required.
    --
    -- -   __@PendingValidation@__ - Lightsail couldn\'t automatically validate
    --     one or more domain names of the certificate. You must take action to
    --     validate these domain names or the certificate won\'t be renewed.
    --     Check to make sure your certificate\'s domain validation records
    --     exist in your domain\'s DNS, and that your certificate remains in
    --     use.
    --
    -- -   __@Success@__ - All domain names in the certificate are validated,
    --     and Lightsail renewed the certificate. No further action is
    --     required.
    --
    -- -   __@Failed@__ - One or more domain names were not validated before
    --     the certificate expired, and Lightsail did not renew the
    --     certificate. You can request a new certificate using the
    --     @CreateCertificate@ action.
    renewalStatus :: Prelude.Maybe RenewalStatus,
    -- | The reason for the renewal status of the certificate.
    renewalStatusReason :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the certificate was last updated.
    updatedAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RenewalSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainValidationRecords', 'renewalSummary_domainValidationRecords' - An array of objects that describe the domain validation records of the
-- certificate.
--
-- 'renewalStatus', 'renewalSummary_renewalStatus' - The renewal status of the certificate.
--
-- The following renewal status are possible:
--
-- -   __@PendingAutoRenewal@__ - Lightsail is attempting to automatically
--     validate the domain names of the certificate. No further action is
--     required.
--
-- -   __@PendingValidation@__ - Lightsail couldn\'t automatically validate
--     one or more domain names of the certificate. You must take action to
--     validate these domain names or the certificate won\'t be renewed.
--     Check to make sure your certificate\'s domain validation records
--     exist in your domain\'s DNS, and that your certificate remains in
--     use.
--
-- -   __@Success@__ - All domain names in the certificate are validated,
--     and Lightsail renewed the certificate. No further action is
--     required.
--
-- -   __@Failed@__ - One or more domain names were not validated before
--     the certificate expired, and Lightsail did not renew the
--     certificate. You can request a new certificate using the
--     @CreateCertificate@ action.
--
-- 'renewalStatusReason', 'renewalSummary_renewalStatusReason' - The reason for the renewal status of the certificate.
--
-- 'updatedAt', 'renewalSummary_updatedAt' - The timestamp when the certificate was last updated.
newRenewalSummary ::
  RenewalSummary
newRenewalSummary =
  RenewalSummary'
    { domainValidationRecords =
        Prelude.Nothing,
      renewalStatus = Prelude.Nothing,
      renewalStatusReason = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | An array of objects that describe the domain validation records of the
-- certificate.
renewalSummary_domainValidationRecords :: Lens.Lens' RenewalSummary (Prelude.Maybe [DomainValidationRecord])
renewalSummary_domainValidationRecords = Lens.lens (\RenewalSummary' {domainValidationRecords} -> domainValidationRecords) (\s@RenewalSummary' {} a -> s {domainValidationRecords = a} :: RenewalSummary) Prelude.. Lens.mapping Lens.coerced

-- | The renewal status of the certificate.
--
-- The following renewal status are possible:
--
-- -   __@PendingAutoRenewal@__ - Lightsail is attempting to automatically
--     validate the domain names of the certificate. No further action is
--     required.
--
-- -   __@PendingValidation@__ - Lightsail couldn\'t automatically validate
--     one or more domain names of the certificate. You must take action to
--     validate these domain names or the certificate won\'t be renewed.
--     Check to make sure your certificate\'s domain validation records
--     exist in your domain\'s DNS, and that your certificate remains in
--     use.
--
-- -   __@Success@__ - All domain names in the certificate are validated,
--     and Lightsail renewed the certificate. No further action is
--     required.
--
-- -   __@Failed@__ - One or more domain names were not validated before
--     the certificate expired, and Lightsail did not renew the
--     certificate. You can request a new certificate using the
--     @CreateCertificate@ action.
renewalSummary_renewalStatus :: Lens.Lens' RenewalSummary (Prelude.Maybe RenewalStatus)
renewalSummary_renewalStatus = Lens.lens (\RenewalSummary' {renewalStatus} -> renewalStatus) (\s@RenewalSummary' {} a -> s {renewalStatus = a} :: RenewalSummary)

-- | The reason for the renewal status of the certificate.
renewalSummary_renewalStatusReason :: Lens.Lens' RenewalSummary (Prelude.Maybe Prelude.Text)
renewalSummary_renewalStatusReason = Lens.lens (\RenewalSummary' {renewalStatusReason} -> renewalStatusReason) (\s@RenewalSummary' {} a -> s {renewalStatusReason = a} :: RenewalSummary)

-- | The timestamp when the certificate was last updated.
renewalSummary_updatedAt :: Lens.Lens' RenewalSummary (Prelude.Maybe Prelude.UTCTime)
renewalSummary_updatedAt = Lens.lens (\RenewalSummary' {updatedAt} -> updatedAt) (\s@RenewalSummary' {} a -> s {updatedAt = a} :: RenewalSummary) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON RenewalSummary where
  parseJSON =
    Data.withObject
      "RenewalSummary"
      ( \x ->
          RenewalSummary'
            Prelude.<$> ( x Data..:? "domainValidationRecords"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "renewalStatus")
            Prelude.<*> (x Data..:? "renewalStatusReason")
            Prelude.<*> (x Data..:? "updatedAt")
      )

instance Prelude.Hashable RenewalSummary where
  hashWithSalt _salt RenewalSummary' {..} =
    _salt
      `Prelude.hashWithSalt` domainValidationRecords
      `Prelude.hashWithSalt` renewalStatus
      `Prelude.hashWithSalt` renewalStatusReason
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData RenewalSummary where
  rnf RenewalSummary' {..} =
    Prelude.rnf domainValidationRecords
      `Prelude.seq` Prelude.rnf renewalStatus
      `Prelude.seq` Prelude.rnf renewalStatusReason
      `Prelude.seq` Prelude.rnf updatedAt

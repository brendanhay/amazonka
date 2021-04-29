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
-- Module      : Network.AWS.Lightsail.Types.RenewalSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RenewalSummary where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.DomainValidationRecord
import Network.AWS.Lightsail.Types.RenewalStatus
import qualified Network.AWS.Prelude as Prelude

-- | Describes the status of a SSL\/TLS certificate renewal managed by Amazon
-- Lightsail.
--
-- /See:/ 'newRenewalSummary' smart constructor.
data RenewalSummary = RenewalSummary'
  { -- | The timestamp when the certificate was last updated.
    updatedAt :: Prelude.Maybe Prelude.POSIX,
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
    -- | An array of objects that describe the domain validation records of the
    -- certificate.
    domainValidationRecords :: Prelude.Maybe [DomainValidationRecord]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RenewalSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'updatedAt', 'renewalSummary_updatedAt' - The timestamp when the certificate was last updated.
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
-- 'domainValidationRecords', 'renewalSummary_domainValidationRecords' - An array of objects that describe the domain validation records of the
-- certificate.
newRenewalSummary ::
  RenewalSummary
newRenewalSummary =
  RenewalSummary'
    { updatedAt = Prelude.Nothing,
      renewalStatus = Prelude.Nothing,
      renewalStatusReason = Prelude.Nothing,
      domainValidationRecords = Prelude.Nothing
    }

-- | The timestamp when the certificate was last updated.
renewalSummary_updatedAt :: Lens.Lens' RenewalSummary (Prelude.Maybe Prelude.UTCTime)
renewalSummary_updatedAt = Lens.lens (\RenewalSummary' {updatedAt} -> updatedAt) (\s@RenewalSummary' {} a -> s {updatedAt = a} :: RenewalSummary) Prelude.. Lens.mapping Prelude._Time

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

-- | An array of objects that describe the domain validation records of the
-- certificate.
renewalSummary_domainValidationRecords :: Lens.Lens' RenewalSummary (Prelude.Maybe [DomainValidationRecord])
renewalSummary_domainValidationRecords = Lens.lens (\RenewalSummary' {domainValidationRecords} -> domainValidationRecords) (\s@RenewalSummary' {} a -> s {domainValidationRecords = a} :: RenewalSummary) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON RenewalSummary where
  parseJSON =
    Prelude.withObject
      "RenewalSummary"
      ( \x ->
          RenewalSummary'
            Prelude.<$> (x Prelude..:? "updatedAt")
            Prelude.<*> (x Prelude..:? "renewalStatus")
            Prelude.<*> (x Prelude..:? "renewalStatusReason")
            Prelude.<*> ( x Prelude..:? "domainValidationRecords"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable RenewalSummary

instance Prelude.NFData RenewalSummary

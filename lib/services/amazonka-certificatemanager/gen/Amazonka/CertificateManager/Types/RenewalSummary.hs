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
-- Module      : Amazonka.CertificateManager.Types.RenewalSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManager.Types.RenewalSummary where

import Amazonka.CertificateManager.Types.DomainValidation
import Amazonka.CertificateManager.Types.FailureReason
import Amazonka.CertificateManager.Types.RenewalStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the status of ACM\'s
-- <https://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal>
-- for the certificate. This structure exists only when the certificate
-- type is @AMAZON_ISSUED@.
--
-- /See:/ 'newRenewalSummary' smart constructor.
data RenewalSummary = RenewalSummary'
  { -- | The reason that a renewal request was unsuccessful.
    renewalStatusReason :: Prelude.Maybe FailureReason,
    -- | The status of ACM\'s
    -- <https://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal>
    -- of the certificate.
    renewalStatus :: RenewalStatus,
    -- | Contains information about the validation of each domain name in the
    -- certificate, as it pertains to ACM\'s
    -- <https://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal>.
    -- This is different from the initial validation that occurs as a result of
    -- the RequestCertificate request. This field exists only when the
    -- certificate type is @AMAZON_ISSUED@.
    domainValidationOptions :: Prelude.NonEmpty DomainValidation,
    -- | The time at which the renewal summary was last updated.
    updatedAt :: Core.POSIX
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
-- 'renewalStatusReason', 'renewalSummary_renewalStatusReason' - The reason that a renewal request was unsuccessful.
--
-- 'renewalStatus', 'renewalSummary_renewalStatus' - The status of ACM\'s
-- <https://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal>
-- of the certificate.
--
-- 'domainValidationOptions', 'renewalSummary_domainValidationOptions' - Contains information about the validation of each domain name in the
-- certificate, as it pertains to ACM\'s
-- <https://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal>.
-- This is different from the initial validation that occurs as a result of
-- the RequestCertificate request. This field exists only when the
-- certificate type is @AMAZON_ISSUED@.
--
-- 'updatedAt', 'renewalSummary_updatedAt' - The time at which the renewal summary was last updated.
newRenewalSummary ::
  -- | 'renewalStatus'
  RenewalStatus ->
  -- | 'domainValidationOptions'
  Prelude.NonEmpty DomainValidation ->
  -- | 'updatedAt'
  Prelude.UTCTime ->
  RenewalSummary
newRenewalSummary
  pRenewalStatus_
  pDomainValidationOptions_
  pUpdatedAt_ =
    RenewalSummary'
      { renewalStatusReason =
          Prelude.Nothing,
        renewalStatus = pRenewalStatus_,
        domainValidationOptions =
          Lens.coerced Lens.# pDomainValidationOptions_,
        updatedAt = Core._Time Lens.# pUpdatedAt_
      }

-- | The reason that a renewal request was unsuccessful.
renewalSummary_renewalStatusReason :: Lens.Lens' RenewalSummary (Prelude.Maybe FailureReason)
renewalSummary_renewalStatusReason = Lens.lens (\RenewalSummary' {renewalStatusReason} -> renewalStatusReason) (\s@RenewalSummary' {} a -> s {renewalStatusReason = a} :: RenewalSummary)

-- | The status of ACM\'s
-- <https://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal>
-- of the certificate.
renewalSummary_renewalStatus :: Lens.Lens' RenewalSummary RenewalStatus
renewalSummary_renewalStatus = Lens.lens (\RenewalSummary' {renewalStatus} -> renewalStatus) (\s@RenewalSummary' {} a -> s {renewalStatus = a} :: RenewalSummary)

-- | Contains information about the validation of each domain name in the
-- certificate, as it pertains to ACM\'s
-- <https://docs.aws.amazon.com/acm/latest/userguide/acm-renewal.html managed renewal>.
-- This is different from the initial validation that occurs as a result of
-- the RequestCertificate request. This field exists only when the
-- certificate type is @AMAZON_ISSUED@.
renewalSummary_domainValidationOptions :: Lens.Lens' RenewalSummary (Prelude.NonEmpty DomainValidation)
renewalSummary_domainValidationOptions = Lens.lens (\RenewalSummary' {domainValidationOptions} -> domainValidationOptions) (\s@RenewalSummary' {} a -> s {domainValidationOptions = a} :: RenewalSummary) Prelude.. Lens.coerced

-- | The time at which the renewal summary was last updated.
renewalSummary_updatedAt :: Lens.Lens' RenewalSummary Prelude.UTCTime
renewalSummary_updatedAt = Lens.lens (\RenewalSummary' {updatedAt} -> updatedAt) (\s@RenewalSummary' {} a -> s {updatedAt = a} :: RenewalSummary) Prelude.. Core._Time

instance Core.FromJSON RenewalSummary where
  parseJSON =
    Core.withObject
      "RenewalSummary"
      ( \x ->
          RenewalSummary'
            Prelude.<$> (x Core..:? "RenewalStatusReason")
            Prelude.<*> (x Core..: "RenewalStatus")
            Prelude.<*> (x Core..: "DomainValidationOptions")
            Prelude.<*> (x Core..: "UpdatedAt")
      )

instance Prelude.Hashable RenewalSummary where
  hashWithSalt _salt RenewalSummary' {..} =
    _salt `Prelude.hashWithSalt` renewalStatusReason
      `Prelude.hashWithSalt` renewalStatus
      `Prelude.hashWithSalt` domainValidationOptions
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData RenewalSummary where
  rnf RenewalSummary' {..} =
    Prelude.rnf renewalStatusReason
      `Prelude.seq` Prelude.rnf renewalStatus
      `Prelude.seq` Prelude.rnf domainValidationOptions
      `Prelude.seq` Prelude.rnf updatedAt

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
-- Module      : Amazonka.Lightsail.Types.LoadBalancerTlsCertificateRenewalSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.LoadBalancerTlsCertificateRenewalSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificateDomainValidationOption
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificateRenewalStatus
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the status of Lightsail\'s managed renewal
-- for the certificate.
--
-- The renewal status of the certificate.
--
-- The following renewal status are possible:
--
-- -   __@PendingAutoRenewal@__ - Lightsail is attempting to automatically
--     validate the domain names in the certificate. No further action is
--     required.
--
-- -   __@PendingValidation@__ - Lightsail couldn\'t automatically validate
--     one or more domain names in the certificate. You must take action to
--     validate these domain names or the certificate won\'t be renewed. If
--     you used DNS validation, check to make sure your certificate\'s
--     domain validation records exist in your domain\'s DNS, and that your
--     certificate remains in use.
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
-- /See:/ 'newLoadBalancerTlsCertificateRenewalSummary' smart constructor.
data LoadBalancerTlsCertificateRenewalSummary = LoadBalancerTlsCertificateRenewalSummary'
  { -- | Contains information about the validation of each domain name in the
    -- certificate, as it pertains to Lightsail\'s managed renewal. This is
    -- different from the initial validation that occurs as a result of the
    -- RequestCertificate request.
    domainValidationOptions :: Prelude.Maybe [LoadBalancerTlsCertificateDomainValidationOption],
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
    renewalStatus :: Prelude.Maybe LoadBalancerTlsCertificateRenewalStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoadBalancerTlsCertificateRenewalSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainValidationOptions', 'loadBalancerTlsCertificateRenewalSummary_domainValidationOptions' - Contains information about the validation of each domain name in the
-- certificate, as it pertains to Lightsail\'s managed renewal. This is
-- different from the initial validation that occurs as a result of the
-- RequestCertificate request.
--
-- 'renewalStatus', 'loadBalancerTlsCertificateRenewalSummary_renewalStatus' - The renewal status of the certificate.
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
newLoadBalancerTlsCertificateRenewalSummary ::
  LoadBalancerTlsCertificateRenewalSummary
newLoadBalancerTlsCertificateRenewalSummary =
  LoadBalancerTlsCertificateRenewalSummary'
    { domainValidationOptions =
        Prelude.Nothing,
      renewalStatus = Prelude.Nothing
    }

-- | Contains information about the validation of each domain name in the
-- certificate, as it pertains to Lightsail\'s managed renewal. This is
-- different from the initial validation that occurs as a result of the
-- RequestCertificate request.
loadBalancerTlsCertificateRenewalSummary_domainValidationOptions :: Lens.Lens' LoadBalancerTlsCertificateRenewalSummary (Prelude.Maybe [LoadBalancerTlsCertificateDomainValidationOption])
loadBalancerTlsCertificateRenewalSummary_domainValidationOptions = Lens.lens (\LoadBalancerTlsCertificateRenewalSummary' {domainValidationOptions} -> domainValidationOptions) (\s@LoadBalancerTlsCertificateRenewalSummary' {} a -> s {domainValidationOptions = a} :: LoadBalancerTlsCertificateRenewalSummary) Prelude.. Lens.mapping Lens.coerced

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
loadBalancerTlsCertificateRenewalSummary_renewalStatus :: Lens.Lens' LoadBalancerTlsCertificateRenewalSummary (Prelude.Maybe LoadBalancerTlsCertificateRenewalStatus)
loadBalancerTlsCertificateRenewalSummary_renewalStatus = Lens.lens (\LoadBalancerTlsCertificateRenewalSummary' {renewalStatus} -> renewalStatus) (\s@LoadBalancerTlsCertificateRenewalSummary' {} a -> s {renewalStatus = a} :: LoadBalancerTlsCertificateRenewalSummary)

instance
  Data.FromJSON
    LoadBalancerTlsCertificateRenewalSummary
  where
  parseJSON =
    Data.withObject
      "LoadBalancerTlsCertificateRenewalSummary"
      ( \x ->
          LoadBalancerTlsCertificateRenewalSummary'
            Prelude.<$> ( x Data..:? "domainValidationOptions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "renewalStatus")
      )

instance
  Prelude.Hashable
    LoadBalancerTlsCertificateRenewalSummary
  where
  hashWithSalt
    _salt
    LoadBalancerTlsCertificateRenewalSummary' {..} =
      _salt
        `Prelude.hashWithSalt` domainValidationOptions
        `Prelude.hashWithSalt` renewalStatus

instance
  Prelude.NFData
    LoadBalancerTlsCertificateRenewalSummary
  where
  rnf LoadBalancerTlsCertificateRenewalSummary' {..} =
    Prelude.rnf domainValidationOptions
      `Prelude.seq` Prelude.rnf renewalStatus

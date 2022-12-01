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
-- Module      : Amazonka.Lightsail.Types.LoadBalancerTlsCertificateDomainValidationRecord
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.LoadBalancerTlsCertificateDomainValidationRecord where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificateDnsRecordCreationState
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificateDomainStatus
import qualified Amazonka.Prelude as Prelude

-- | Describes the validation record of each domain name in the SSL\/TLS
-- certificate.
--
-- /See:/ 'newLoadBalancerTlsCertificateDomainValidationRecord' smart constructor.
data LoadBalancerTlsCertificateDomainValidationRecord = LoadBalancerTlsCertificateDomainValidationRecord'
  { -- | A fully qualified domain name in the certificate. For example,
    -- @example.com@.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of validation record. For example, @CNAME@ for domain
    -- validation.
    type' :: Prelude.Maybe Prelude.Text,
    -- | An object that describes the state of the canonical name (CNAME) records
    -- that are automatically added by Lightsail to the DNS of a domain to
    -- validate domain ownership.
    dnsRecordCreationState :: Prelude.Maybe LoadBalancerTlsCertificateDnsRecordCreationState,
    -- | The domain name against which your SSL\/TLS certificate was validated.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | The validation status. Valid values are listed below.
    validationStatus :: Prelude.Maybe LoadBalancerTlsCertificateDomainStatus,
    -- | The value for that type.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoadBalancerTlsCertificateDomainValidationRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'loadBalancerTlsCertificateDomainValidationRecord_name' - A fully qualified domain name in the certificate. For example,
-- @example.com@.
--
-- 'type'', 'loadBalancerTlsCertificateDomainValidationRecord_type' - The type of validation record. For example, @CNAME@ for domain
-- validation.
--
-- 'dnsRecordCreationState', 'loadBalancerTlsCertificateDomainValidationRecord_dnsRecordCreationState' - An object that describes the state of the canonical name (CNAME) records
-- that are automatically added by Lightsail to the DNS of a domain to
-- validate domain ownership.
--
-- 'domainName', 'loadBalancerTlsCertificateDomainValidationRecord_domainName' - The domain name against which your SSL\/TLS certificate was validated.
--
-- 'validationStatus', 'loadBalancerTlsCertificateDomainValidationRecord_validationStatus' - The validation status. Valid values are listed below.
--
-- 'value', 'loadBalancerTlsCertificateDomainValidationRecord_value' - The value for that type.
newLoadBalancerTlsCertificateDomainValidationRecord ::
  LoadBalancerTlsCertificateDomainValidationRecord
newLoadBalancerTlsCertificateDomainValidationRecord =
  LoadBalancerTlsCertificateDomainValidationRecord'
    { name =
        Prelude.Nothing,
      type' = Prelude.Nothing,
      dnsRecordCreationState =
        Prelude.Nothing,
      domainName =
        Prelude.Nothing,
      validationStatus =
        Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | A fully qualified domain name in the certificate. For example,
-- @example.com@.
loadBalancerTlsCertificateDomainValidationRecord_name :: Lens.Lens' LoadBalancerTlsCertificateDomainValidationRecord (Prelude.Maybe Prelude.Text)
loadBalancerTlsCertificateDomainValidationRecord_name = Lens.lens (\LoadBalancerTlsCertificateDomainValidationRecord' {name} -> name) (\s@LoadBalancerTlsCertificateDomainValidationRecord' {} a -> s {name = a} :: LoadBalancerTlsCertificateDomainValidationRecord)

-- | The type of validation record. For example, @CNAME@ for domain
-- validation.
loadBalancerTlsCertificateDomainValidationRecord_type :: Lens.Lens' LoadBalancerTlsCertificateDomainValidationRecord (Prelude.Maybe Prelude.Text)
loadBalancerTlsCertificateDomainValidationRecord_type = Lens.lens (\LoadBalancerTlsCertificateDomainValidationRecord' {type'} -> type') (\s@LoadBalancerTlsCertificateDomainValidationRecord' {} a -> s {type' = a} :: LoadBalancerTlsCertificateDomainValidationRecord)

-- | An object that describes the state of the canonical name (CNAME) records
-- that are automatically added by Lightsail to the DNS of a domain to
-- validate domain ownership.
loadBalancerTlsCertificateDomainValidationRecord_dnsRecordCreationState :: Lens.Lens' LoadBalancerTlsCertificateDomainValidationRecord (Prelude.Maybe LoadBalancerTlsCertificateDnsRecordCreationState)
loadBalancerTlsCertificateDomainValidationRecord_dnsRecordCreationState = Lens.lens (\LoadBalancerTlsCertificateDomainValidationRecord' {dnsRecordCreationState} -> dnsRecordCreationState) (\s@LoadBalancerTlsCertificateDomainValidationRecord' {} a -> s {dnsRecordCreationState = a} :: LoadBalancerTlsCertificateDomainValidationRecord)

-- | The domain name against which your SSL\/TLS certificate was validated.
loadBalancerTlsCertificateDomainValidationRecord_domainName :: Lens.Lens' LoadBalancerTlsCertificateDomainValidationRecord (Prelude.Maybe Prelude.Text)
loadBalancerTlsCertificateDomainValidationRecord_domainName = Lens.lens (\LoadBalancerTlsCertificateDomainValidationRecord' {domainName} -> domainName) (\s@LoadBalancerTlsCertificateDomainValidationRecord' {} a -> s {domainName = a} :: LoadBalancerTlsCertificateDomainValidationRecord)

-- | The validation status. Valid values are listed below.
loadBalancerTlsCertificateDomainValidationRecord_validationStatus :: Lens.Lens' LoadBalancerTlsCertificateDomainValidationRecord (Prelude.Maybe LoadBalancerTlsCertificateDomainStatus)
loadBalancerTlsCertificateDomainValidationRecord_validationStatus = Lens.lens (\LoadBalancerTlsCertificateDomainValidationRecord' {validationStatus} -> validationStatus) (\s@LoadBalancerTlsCertificateDomainValidationRecord' {} a -> s {validationStatus = a} :: LoadBalancerTlsCertificateDomainValidationRecord)

-- | The value for that type.
loadBalancerTlsCertificateDomainValidationRecord_value :: Lens.Lens' LoadBalancerTlsCertificateDomainValidationRecord (Prelude.Maybe Prelude.Text)
loadBalancerTlsCertificateDomainValidationRecord_value = Lens.lens (\LoadBalancerTlsCertificateDomainValidationRecord' {value} -> value) (\s@LoadBalancerTlsCertificateDomainValidationRecord' {} a -> s {value = a} :: LoadBalancerTlsCertificateDomainValidationRecord)

instance
  Core.FromJSON
    LoadBalancerTlsCertificateDomainValidationRecord
  where
  parseJSON =
    Core.withObject
      "LoadBalancerTlsCertificateDomainValidationRecord"
      ( \x ->
          LoadBalancerTlsCertificateDomainValidationRecord'
            Prelude.<$> (x Core..:? "name") Prelude.<*> (x Core..:? "type")
              Prelude.<*> (x Core..:? "dnsRecordCreationState")
              Prelude.<*> (x Core..:? "domainName")
              Prelude.<*> (x Core..:? "validationStatus")
              Prelude.<*> (x Core..:? "value")
      )

instance
  Prelude.Hashable
    LoadBalancerTlsCertificateDomainValidationRecord
  where
  hashWithSalt
    _salt
    LoadBalancerTlsCertificateDomainValidationRecord' {..} =
      _salt `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` type'
        `Prelude.hashWithSalt` dnsRecordCreationState
        `Prelude.hashWithSalt` domainName
        `Prelude.hashWithSalt` validationStatus
        `Prelude.hashWithSalt` value

instance
  Prelude.NFData
    LoadBalancerTlsCertificateDomainValidationRecord
  where
  rnf
    LoadBalancerTlsCertificateDomainValidationRecord' {..} =
      Prelude.rnf name
        `Prelude.seq` Prelude.rnf type'
        `Prelude.seq` Prelude.rnf dnsRecordCreationState
        `Prelude.seq` Prelude.rnf domainName
        `Prelude.seq` Prelude.rnf validationStatus
        `Prelude.seq` Prelude.rnf value

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
-- Module      : Amazonka.Lightsail.Types.DomainValidationRecord
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.DomainValidationRecord where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.CertificateDomainValidationStatus
import Amazonka.Lightsail.Types.DnsRecordCreationState
import Amazonka.Lightsail.Types.ResourceRecord
import qualified Amazonka.Prelude as Prelude

-- | Describes the domain name system (DNS) records that you must add to the
-- DNS of your registered domain to validate ownership for an Amazon
-- Lightsail SSL\/TLS certificate.
--
-- /See:/ 'newDomainValidationRecord' smart constructor.
data DomainValidationRecord = DomainValidationRecord'
  { -- | An object that describes the state of the canonical name (CNAME) records
    -- that are automatically added by Lightsail to the DNS of the domain to
    -- validate domain ownership.
    dnsRecordCreationState :: Prelude.Maybe DnsRecordCreationState,
    -- | The domain name of the certificate validation record. For example,
    -- @example.com@ or @www.example.com@.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | An object that describes the DNS records to add to your domain\'s DNS to
    -- validate it for the certificate.
    resourceRecord :: Prelude.Maybe ResourceRecord,
    -- | The validation status of the record.
    validationStatus :: Prelude.Maybe CertificateDomainValidationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DomainValidationRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dnsRecordCreationState', 'domainValidationRecord_dnsRecordCreationState' - An object that describes the state of the canonical name (CNAME) records
-- that are automatically added by Lightsail to the DNS of the domain to
-- validate domain ownership.
--
-- 'domainName', 'domainValidationRecord_domainName' - The domain name of the certificate validation record. For example,
-- @example.com@ or @www.example.com@.
--
-- 'resourceRecord', 'domainValidationRecord_resourceRecord' - An object that describes the DNS records to add to your domain\'s DNS to
-- validate it for the certificate.
--
-- 'validationStatus', 'domainValidationRecord_validationStatus' - The validation status of the record.
newDomainValidationRecord ::
  DomainValidationRecord
newDomainValidationRecord =
  DomainValidationRecord'
    { dnsRecordCreationState =
        Prelude.Nothing,
      domainName = Prelude.Nothing,
      resourceRecord = Prelude.Nothing,
      validationStatus = Prelude.Nothing
    }

-- | An object that describes the state of the canonical name (CNAME) records
-- that are automatically added by Lightsail to the DNS of the domain to
-- validate domain ownership.
domainValidationRecord_dnsRecordCreationState :: Lens.Lens' DomainValidationRecord (Prelude.Maybe DnsRecordCreationState)
domainValidationRecord_dnsRecordCreationState = Lens.lens (\DomainValidationRecord' {dnsRecordCreationState} -> dnsRecordCreationState) (\s@DomainValidationRecord' {} a -> s {dnsRecordCreationState = a} :: DomainValidationRecord)

-- | The domain name of the certificate validation record. For example,
-- @example.com@ or @www.example.com@.
domainValidationRecord_domainName :: Lens.Lens' DomainValidationRecord (Prelude.Maybe Prelude.Text)
domainValidationRecord_domainName = Lens.lens (\DomainValidationRecord' {domainName} -> domainName) (\s@DomainValidationRecord' {} a -> s {domainName = a} :: DomainValidationRecord)

-- | An object that describes the DNS records to add to your domain\'s DNS to
-- validate it for the certificate.
domainValidationRecord_resourceRecord :: Lens.Lens' DomainValidationRecord (Prelude.Maybe ResourceRecord)
domainValidationRecord_resourceRecord = Lens.lens (\DomainValidationRecord' {resourceRecord} -> resourceRecord) (\s@DomainValidationRecord' {} a -> s {resourceRecord = a} :: DomainValidationRecord)

-- | The validation status of the record.
domainValidationRecord_validationStatus :: Lens.Lens' DomainValidationRecord (Prelude.Maybe CertificateDomainValidationStatus)
domainValidationRecord_validationStatus = Lens.lens (\DomainValidationRecord' {validationStatus} -> validationStatus) (\s@DomainValidationRecord' {} a -> s {validationStatus = a} :: DomainValidationRecord)

instance Data.FromJSON DomainValidationRecord where
  parseJSON =
    Data.withObject
      "DomainValidationRecord"
      ( \x ->
          DomainValidationRecord'
            Prelude.<$> (x Data..:? "dnsRecordCreationState")
            Prelude.<*> (x Data..:? "domainName")
            Prelude.<*> (x Data..:? "resourceRecord")
            Prelude.<*> (x Data..:? "validationStatus")
      )

instance Prelude.Hashable DomainValidationRecord where
  hashWithSalt _salt DomainValidationRecord' {..} =
    _salt
      `Prelude.hashWithSalt` dnsRecordCreationState
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` resourceRecord
      `Prelude.hashWithSalt` validationStatus

instance Prelude.NFData DomainValidationRecord where
  rnf DomainValidationRecord' {..} =
    Prelude.rnf dnsRecordCreationState
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf resourceRecord
      `Prelude.seq` Prelude.rnf validationStatus

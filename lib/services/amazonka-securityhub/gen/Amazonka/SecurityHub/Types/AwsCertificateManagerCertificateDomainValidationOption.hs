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
-- Module      : Amazonka.SecurityHub.Types.AwsCertificateManagerCertificateDomainValidationOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCertificateManagerCertificateDomainValidationOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsCertificateManagerCertificateResourceRecord

-- | Contains information about one of the following:
--
-- -   The initial validation of each domain name that occurs as a result
--     of the @RequestCertificate@ request
--
-- -   The validation of each domain name in the certificate, as it
--     pertains to Certificate Manager managed renewal
--
-- /See:/ 'newAwsCertificateManagerCertificateDomainValidationOption' smart constructor.
data AwsCertificateManagerCertificateDomainValidationOption = AwsCertificateManagerCertificateDomainValidationOption'
  { -- | A fully qualified domain name (FQDN) in the certificate.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | The CNAME record that is added to the DNS database for domain
    -- validation.
    resourceRecord :: Prelude.Maybe AwsCertificateManagerCertificateResourceRecord,
    -- | The domain name that Certificate Manager uses to send domain validation
    -- emails.
    validationDomain :: Prelude.Maybe Prelude.Text,
    -- | A list of email addresses that Certificate Manager uses to send domain
    -- validation emails.
    validationEmails :: Prelude.Maybe [Prelude.Text],
    -- | The method used to validate the domain name.
    validationMethod :: Prelude.Maybe Prelude.Text,
    -- | The validation status of the domain name.
    validationStatus :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsCertificateManagerCertificateDomainValidationOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'awsCertificateManagerCertificateDomainValidationOption_domainName' - A fully qualified domain name (FQDN) in the certificate.
--
-- 'resourceRecord', 'awsCertificateManagerCertificateDomainValidationOption_resourceRecord' - The CNAME record that is added to the DNS database for domain
-- validation.
--
-- 'validationDomain', 'awsCertificateManagerCertificateDomainValidationOption_validationDomain' - The domain name that Certificate Manager uses to send domain validation
-- emails.
--
-- 'validationEmails', 'awsCertificateManagerCertificateDomainValidationOption_validationEmails' - A list of email addresses that Certificate Manager uses to send domain
-- validation emails.
--
-- 'validationMethod', 'awsCertificateManagerCertificateDomainValidationOption_validationMethod' - The method used to validate the domain name.
--
-- 'validationStatus', 'awsCertificateManagerCertificateDomainValidationOption_validationStatus' - The validation status of the domain name.
newAwsCertificateManagerCertificateDomainValidationOption ::
  AwsCertificateManagerCertificateDomainValidationOption
newAwsCertificateManagerCertificateDomainValidationOption =
  AwsCertificateManagerCertificateDomainValidationOption'
    { domainName =
        Prelude.Nothing,
      resourceRecord =
        Prelude.Nothing,
      validationDomain =
        Prelude.Nothing,
      validationEmails =
        Prelude.Nothing,
      validationMethod =
        Prelude.Nothing,
      validationStatus =
        Prelude.Nothing
    }

-- | A fully qualified domain name (FQDN) in the certificate.
awsCertificateManagerCertificateDomainValidationOption_domainName :: Lens.Lens' AwsCertificateManagerCertificateDomainValidationOption (Prelude.Maybe Prelude.Text)
awsCertificateManagerCertificateDomainValidationOption_domainName = Lens.lens (\AwsCertificateManagerCertificateDomainValidationOption' {domainName} -> domainName) (\s@AwsCertificateManagerCertificateDomainValidationOption' {} a -> s {domainName = a} :: AwsCertificateManagerCertificateDomainValidationOption)

-- | The CNAME record that is added to the DNS database for domain
-- validation.
awsCertificateManagerCertificateDomainValidationOption_resourceRecord :: Lens.Lens' AwsCertificateManagerCertificateDomainValidationOption (Prelude.Maybe AwsCertificateManagerCertificateResourceRecord)
awsCertificateManagerCertificateDomainValidationOption_resourceRecord = Lens.lens (\AwsCertificateManagerCertificateDomainValidationOption' {resourceRecord} -> resourceRecord) (\s@AwsCertificateManagerCertificateDomainValidationOption' {} a -> s {resourceRecord = a} :: AwsCertificateManagerCertificateDomainValidationOption)

-- | The domain name that Certificate Manager uses to send domain validation
-- emails.
awsCertificateManagerCertificateDomainValidationOption_validationDomain :: Lens.Lens' AwsCertificateManagerCertificateDomainValidationOption (Prelude.Maybe Prelude.Text)
awsCertificateManagerCertificateDomainValidationOption_validationDomain = Lens.lens (\AwsCertificateManagerCertificateDomainValidationOption' {validationDomain} -> validationDomain) (\s@AwsCertificateManagerCertificateDomainValidationOption' {} a -> s {validationDomain = a} :: AwsCertificateManagerCertificateDomainValidationOption)

-- | A list of email addresses that Certificate Manager uses to send domain
-- validation emails.
awsCertificateManagerCertificateDomainValidationOption_validationEmails :: Lens.Lens' AwsCertificateManagerCertificateDomainValidationOption (Prelude.Maybe [Prelude.Text])
awsCertificateManagerCertificateDomainValidationOption_validationEmails = Lens.lens (\AwsCertificateManagerCertificateDomainValidationOption' {validationEmails} -> validationEmails) (\s@AwsCertificateManagerCertificateDomainValidationOption' {} a -> s {validationEmails = a} :: AwsCertificateManagerCertificateDomainValidationOption) Prelude.. Lens.mapping Lens.coerced

-- | The method used to validate the domain name.
awsCertificateManagerCertificateDomainValidationOption_validationMethod :: Lens.Lens' AwsCertificateManagerCertificateDomainValidationOption (Prelude.Maybe Prelude.Text)
awsCertificateManagerCertificateDomainValidationOption_validationMethod = Lens.lens (\AwsCertificateManagerCertificateDomainValidationOption' {validationMethod} -> validationMethod) (\s@AwsCertificateManagerCertificateDomainValidationOption' {} a -> s {validationMethod = a} :: AwsCertificateManagerCertificateDomainValidationOption)

-- | The validation status of the domain name.
awsCertificateManagerCertificateDomainValidationOption_validationStatus :: Lens.Lens' AwsCertificateManagerCertificateDomainValidationOption (Prelude.Maybe Prelude.Text)
awsCertificateManagerCertificateDomainValidationOption_validationStatus = Lens.lens (\AwsCertificateManagerCertificateDomainValidationOption' {validationStatus} -> validationStatus) (\s@AwsCertificateManagerCertificateDomainValidationOption' {} a -> s {validationStatus = a} :: AwsCertificateManagerCertificateDomainValidationOption)

instance
  Data.FromJSON
    AwsCertificateManagerCertificateDomainValidationOption
  where
  parseJSON =
    Data.withObject
      "AwsCertificateManagerCertificateDomainValidationOption"
      ( \x ->
          AwsCertificateManagerCertificateDomainValidationOption'
            Prelude.<$> (x Data..:? "DomainName")
            Prelude.<*> (x Data..:? "ResourceRecord")
            Prelude.<*> (x Data..:? "ValidationDomain")
            Prelude.<*> ( x
                            Data..:? "ValidationEmails"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ValidationMethod")
            Prelude.<*> (x Data..:? "ValidationStatus")
      )

instance
  Prelude.Hashable
    AwsCertificateManagerCertificateDomainValidationOption
  where
  hashWithSalt
    _salt
    AwsCertificateManagerCertificateDomainValidationOption' {..} =
      _salt
        `Prelude.hashWithSalt` domainName
        `Prelude.hashWithSalt` resourceRecord
        `Prelude.hashWithSalt` validationDomain
        `Prelude.hashWithSalt` validationEmails
        `Prelude.hashWithSalt` validationMethod
        `Prelude.hashWithSalt` validationStatus

instance
  Prelude.NFData
    AwsCertificateManagerCertificateDomainValidationOption
  where
  rnf
    AwsCertificateManagerCertificateDomainValidationOption' {..} =
      Prelude.rnf domainName
        `Prelude.seq` Prelude.rnf resourceRecord
        `Prelude.seq` Prelude.rnf validationDomain
        `Prelude.seq` Prelude.rnf validationEmails
        `Prelude.seq` Prelude.rnf validationMethod
        `Prelude.seq` Prelude.rnf validationStatus

instance
  Data.ToJSON
    AwsCertificateManagerCertificateDomainValidationOption
  where
  toJSON
    AwsCertificateManagerCertificateDomainValidationOption' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("DomainName" Data..=) Prelude.<$> domainName,
              ("ResourceRecord" Data..=)
                Prelude.<$> resourceRecord,
              ("ValidationDomain" Data..=)
                Prelude.<$> validationDomain,
              ("ValidationEmails" Data..=)
                Prelude.<$> validationEmails,
              ("ValidationMethod" Data..=)
                Prelude.<$> validationMethod,
              ("ValidationStatus" Data..=)
                Prelude.<$> validationStatus
            ]
        )

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
-- Module      : Network.AWS.SecurityHub.Types.AwsCertificateManagerCertificateDomainValidationOption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsCertificateManagerCertificateDomainValidationOption where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SecurityHub.Types.AwsCertificateManagerCertificateResourceRecord

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
  { -- | A list of email addresses that Certificate Manager uses to send domain
    -- validation emails.
    validationEmails :: Prelude.Maybe [Prelude.Text],
    -- | The method used to validate the domain name.
    validationMethod :: Prelude.Maybe Prelude.Text,
    -- | The CNAME record that is added to the DNS database for domain
    -- validation.
    resourceRecord :: Prelude.Maybe AwsCertificateManagerCertificateResourceRecord,
    -- | The validation status of the domain name.
    validationStatus :: Prelude.Maybe Prelude.Text,
    -- | A fully qualified domain name (FQDN) in the certificate.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | The domain name that Certificate Manager uses to send domain validation
    -- emails.
    validationDomain :: Prelude.Maybe Prelude.Text
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
-- 'validationEmails', 'awsCertificateManagerCertificateDomainValidationOption_validationEmails' - A list of email addresses that Certificate Manager uses to send domain
-- validation emails.
--
-- 'validationMethod', 'awsCertificateManagerCertificateDomainValidationOption_validationMethod' - The method used to validate the domain name.
--
-- 'resourceRecord', 'awsCertificateManagerCertificateDomainValidationOption_resourceRecord' - The CNAME record that is added to the DNS database for domain
-- validation.
--
-- 'validationStatus', 'awsCertificateManagerCertificateDomainValidationOption_validationStatus' - The validation status of the domain name.
--
-- 'domainName', 'awsCertificateManagerCertificateDomainValidationOption_domainName' - A fully qualified domain name (FQDN) in the certificate.
--
-- 'validationDomain', 'awsCertificateManagerCertificateDomainValidationOption_validationDomain' - The domain name that Certificate Manager uses to send domain validation
-- emails.
newAwsCertificateManagerCertificateDomainValidationOption ::
  AwsCertificateManagerCertificateDomainValidationOption
newAwsCertificateManagerCertificateDomainValidationOption =
  AwsCertificateManagerCertificateDomainValidationOption'
    { validationEmails =
        Prelude.Nothing,
      validationMethod =
        Prelude.Nothing,
      resourceRecord =
        Prelude.Nothing,
      validationStatus =
        Prelude.Nothing,
      domainName =
        Prelude.Nothing,
      validationDomain =
        Prelude.Nothing
    }

-- | A list of email addresses that Certificate Manager uses to send domain
-- validation emails.
awsCertificateManagerCertificateDomainValidationOption_validationEmails :: Lens.Lens' AwsCertificateManagerCertificateDomainValidationOption (Prelude.Maybe [Prelude.Text])
awsCertificateManagerCertificateDomainValidationOption_validationEmails = Lens.lens (\AwsCertificateManagerCertificateDomainValidationOption' {validationEmails} -> validationEmails) (\s@AwsCertificateManagerCertificateDomainValidationOption' {} a -> s {validationEmails = a} :: AwsCertificateManagerCertificateDomainValidationOption) Prelude.. Lens.mapping Lens.coerced

-- | The method used to validate the domain name.
awsCertificateManagerCertificateDomainValidationOption_validationMethod :: Lens.Lens' AwsCertificateManagerCertificateDomainValidationOption (Prelude.Maybe Prelude.Text)
awsCertificateManagerCertificateDomainValidationOption_validationMethod = Lens.lens (\AwsCertificateManagerCertificateDomainValidationOption' {validationMethod} -> validationMethod) (\s@AwsCertificateManagerCertificateDomainValidationOption' {} a -> s {validationMethod = a} :: AwsCertificateManagerCertificateDomainValidationOption)

-- | The CNAME record that is added to the DNS database for domain
-- validation.
awsCertificateManagerCertificateDomainValidationOption_resourceRecord :: Lens.Lens' AwsCertificateManagerCertificateDomainValidationOption (Prelude.Maybe AwsCertificateManagerCertificateResourceRecord)
awsCertificateManagerCertificateDomainValidationOption_resourceRecord = Lens.lens (\AwsCertificateManagerCertificateDomainValidationOption' {resourceRecord} -> resourceRecord) (\s@AwsCertificateManagerCertificateDomainValidationOption' {} a -> s {resourceRecord = a} :: AwsCertificateManagerCertificateDomainValidationOption)

-- | The validation status of the domain name.
awsCertificateManagerCertificateDomainValidationOption_validationStatus :: Lens.Lens' AwsCertificateManagerCertificateDomainValidationOption (Prelude.Maybe Prelude.Text)
awsCertificateManagerCertificateDomainValidationOption_validationStatus = Lens.lens (\AwsCertificateManagerCertificateDomainValidationOption' {validationStatus} -> validationStatus) (\s@AwsCertificateManagerCertificateDomainValidationOption' {} a -> s {validationStatus = a} :: AwsCertificateManagerCertificateDomainValidationOption)

-- | A fully qualified domain name (FQDN) in the certificate.
awsCertificateManagerCertificateDomainValidationOption_domainName :: Lens.Lens' AwsCertificateManagerCertificateDomainValidationOption (Prelude.Maybe Prelude.Text)
awsCertificateManagerCertificateDomainValidationOption_domainName = Lens.lens (\AwsCertificateManagerCertificateDomainValidationOption' {domainName} -> domainName) (\s@AwsCertificateManagerCertificateDomainValidationOption' {} a -> s {domainName = a} :: AwsCertificateManagerCertificateDomainValidationOption)

-- | The domain name that Certificate Manager uses to send domain validation
-- emails.
awsCertificateManagerCertificateDomainValidationOption_validationDomain :: Lens.Lens' AwsCertificateManagerCertificateDomainValidationOption (Prelude.Maybe Prelude.Text)
awsCertificateManagerCertificateDomainValidationOption_validationDomain = Lens.lens (\AwsCertificateManagerCertificateDomainValidationOption' {validationDomain} -> validationDomain) (\s@AwsCertificateManagerCertificateDomainValidationOption' {} a -> s {validationDomain = a} :: AwsCertificateManagerCertificateDomainValidationOption)

instance
  Core.FromJSON
    AwsCertificateManagerCertificateDomainValidationOption
  where
  parseJSON =
    Core.withObject
      "AwsCertificateManagerCertificateDomainValidationOption"
      ( \x ->
          AwsCertificateManagerCertificateDomainValidationOption'
            Prelude.<$> ( x Core..:? "ValidationEmails"
                            Core..!= Prelude.mempty
                        )
              Prelude.<*> (x Core..:? "ValidationMethod")
              Prelude.<*> (x Core..:? "ResourceRecord")
              Prelude.<*> (x Core..:? "ValidationStatus")
              Prelude.<*> (x Core..:? "DomainName")
              Prelude.<*> (x Core..:? "ValidationDomain")
      )

instance
  Prelude.Hashable
    AwsCertificateManagerCertificateDomainValidationOption

instance
  Prelude.NFData
    AwsCertificateManagerCertificateDomainValidationOption

instance
  Core.ToJSON
    AwsCertificateManagerCertificateDomainValidationOption
  where
  toJSON
    AwsCertificateManagerCertificateDomainValidationOption' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("ValidationEmails" Core..=)
                Prelude.<$> validationEmails,
              ("ValidationMethod" Core..=)
                Prelude.<$> validationMethod,
              ("ResourceRecord" Core..=)
                Prelude.<$> resourceRecord,
              ("ValidationStatus" Core..=)
                Prelude.<$> validationStatus,
              ("DomainName" Core..=) Prelude.<$> domainName,
              ("ValidationDomain" Core..=)
                Prelude.<$> validationDomain
            ]
        )

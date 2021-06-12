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
-- Module      : Network.AWS.CertificateManager.Types.DomainValidation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.DomainValidation where

import Network.AWS.CertificateManager.Types.DomainStatus
import Network.AWS.CertificateManager.Types.ResourceRecord
import Network.AWS.CertificateManager.Types.ValidationMethod
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about the validation of each domain name in the
-- certificate.
--
-- /See:/ 'newDomainValidation' smart constructor.
data DomainValidation = DomainValidation'
  { -- | Contains the CNAME record that you add to your DNS database for domain
    -- validation. For more information, see
    -- <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-validate-dns.html Use DNS to Validate Domain Ownership>.
    --
    -- Note: The CNAME information that you need does not include the name of
    -- your domain. If you include  your domain name in the DNS database CNAME
    -- record, validation fails.  For example, if the name is
    -- \"_a79865eb4cd1a6ab990a45779b4e0b96.yourdomain.com\", only
    -- \"_a79865eb4cd1a6ab990a45779b4e0b96\" must be used.
    resourceRecord :: Core.Maybe ResourceRecord,
    -- | A list of email addresses that ACM used to send domain validation
    -- emails.
    validationEmails :: Core.Maybe [Core.Text],
    -- | Specifies the domain validation method.
    validationMethod :: Core.Maybe ValidationMethod,
    -- | The validation status of the domain name. This can be one of the
    -- following values:
    --
    -- -   @PENDING_VALIDATION@
    --
    -- -   @@SUCCESS
    --
    -- -   @@FAILED
    validationStatus :: Core.Maybe DomainStatus,
    -- | The domain name that ACM used to send domain validation emails.
    validationDomain :: Core.Maybe Core.Text,
    -- | A fully qualified domain name (FQDN) in the certificate. For example,
    -- @www.example.com@ or @example.com@.
    domainName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DomainValidation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceRecord', 'domainValidation_resourceRecord' - Contains the CNAME record that you add to your DNS database for domain
-- validation. For more information, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-validate-dns.html Use DNS to Validate Domain Ownership>.
--
-- Note: The CNAME information that you need does not include the name of
-- your domain. If you include  your domain name in the DNS database CNAME
-- record, validation fails.  For example, if the name is
-- \"_a79865eb4cd1a6ab990a45779b4e0b96.yourdomain.com\", only
-- \"_a79865eb4cd1a6ab990a45779b4e0b96\" must be used.
--
-- 'validationEmails', 'domainValidation_validationEmails' - A list of email addresses that ACM used to send domain validation
-- emails.
--
-- 'validationMethod', 'domainValidation_validationMethod' - Specifies the domain validation method.
--
-- 'validationStatus', 'domainValidation_validationStatus' - The validation status of the domain name. This can be one of the
-- following values:
--
-- -   @PENDING_VALIDATION@
--
-- -   @@SUCCESS
--
-- -   @@FAILED
--
-- 'validationDomain', 'domainValidation_validationDomain' - The domain name that ACM used to send domain validation emails.
--
-- 'domainName', 'domainValidation_domainName' - A fully qualified domain name (FQDN) in the certificate. For example,
-- @www.example.com@ or @example.com@.
newDomainValidation ::
  -- | 'domainName'
  Core.Text ->
  DomainValidation
newDomainValidation pDomainName_ =
  DomainValidation'
    { resourceRecord = Core.Nothing,
      validationEmails = Core.Nothing,
      validationMethod = Core.Nothing,
      validationStatus = Core.Nothing,
      validationDomain = Core.Nothing,
      domainName = pDomainName_
    }

-- | Contains the CNAME record that you add to your DNS database for domain
-- validation. For more information, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-validate-dns.html Use DNS to Validate Domain Ownership>.
--
-- Note: The CNAME information that you need does not include the name of
-- your domain. If you include  your domain name in the DNS database CNAME
-- record, validation fails.  For example, if the name is
-- \"_a79865eb4cd1a6ab990a45779b4e0b96.yourdomain.com\", only
-- \"_a79865eb4cd1a6ab990a45779b4e0b96\" must be used.
domainValidation_resourceRecord :: Lens.Lens' DomainValidation (Core.Maybe ResourceRecord)
domainValidation_resourceRecord = Lens.lens (\DomainValidation' {resourceRecord} -> resourceRecord) (\s@DomainValidation' {} a -> s {resourceRecord = a} :: DomainValidation)

-- | A list of email addresses that ACM used to send domain validation
-- emails.
domainValidation_validationEmails :: Lens.Lens' DomainValidation (Core.Maybe [Core.Text])
domainValidation_validationEmails = Lens.lens (\DomainValidation' {validationEmails} -> validationEmails) (\s@DomainValidation' {} a -> s {validationEmails = a} :: DomainValidation) Core.. Lens.mapping Lens._Coerce

-- | Specifies the domain validation method.
domainValidation_validationMethod :: Lens.Lens' DomainValidation (Core.Maybe ValidationMethod)
domainValidation_validationMethod = Lens.lens (\DomainValidation' {validationMethod} -> validationMethod) (\s@DomainValidation' {} a -> s {validationMethod = a} :: DomainValidation)

-- | The validation status of the domain name. This can be one of the
-- following values:
--
-- -   @PENDING_VALIDATION@
--
-- -   @@SUCCESS
--
-- -   @@FAILED
domainValidation_validationStatus :: Lens.Lens' DomainValidation (Core.Maybe DomainStatus)
domainValidation_validationStatus = Lens.lens (\DomainValidation' {validationStatus} -> validationStatus) (\s@DomainValidation' {} a -> s {validationStatus = a} :: DomainValidation)

-- | The domain name that ACM used to send domain validation emails.
domainValidation_validationDomain :: Lens.Lens' DomainValidation (Core.Maybe Core.Text)
domainValidation_validationDomain = Lens.lens (\DomainValidation' {validationDomain} -> validationDomain) (\s@DomainValidation' {} a -> s {validationDomain = a} :: DomainValidation)

-- | A fully qualified domain name (FQDN) in the certificate. For example,
-- @www.example.com@ or @example.com@.
domainValidation_domainName :: Lens.Lens' DomainValidation Core.Text
domainValidation_domainName = Lens.lens (\DomainValidation' {domainName} -> domainName) (\s@DomainValidation' {} a -> s {domainName = a} :: DomainValidation)

instance Core.FromJSON DomainValidation where
  parseJSON =
    Core.withObject
      "DomainValidation"
      ( \x ->
          DomainValidation'
            Core.<$> (x Core..:? "ResourceRecord")
            Core.<*> (x Core..:? "ValidationEmails" Core..!= Core.mempty)
            Core.<*> (x Core..:? "ValidationMethod")
            Core.<*> (x Core..:? "ValidationStatus")
            Core.<*> (x Core..:? "ValidationDomain")
            Core.<*> (x Core..: "DomainName")
      )

instance Core.Hashable DomainValidation

instance Core.NFData DomainValidation

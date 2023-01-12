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
-- Module      : Amazonka.Amplify.Types.DomainAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Amplify.Types.DomainAssociation where

import Amazonka.Amplify.Types.DomainStatus
import Amazonka.Amplify.Types.SubDomain
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a domain association that associates a custom domain with an
-- Amplify app.
--
-- /See:/ 'newDomainAssociation' smart constructor.
data DomainAssociation = DomainAssociation'
  { -- | Sets branch patterns for automatic subdomain creation.
    autoSubDomainCreationPatterns :: Prelude.Maybe [Prelude.Text],
    -- | The required AWS Identity and Access Management (IAM) service role for
    -- the Amazon Resource Name (ARN) for automatically creating subdomains.
    autoSubDomainIAMRole :: Prelude.Maybe Prelude.Text,
    -- | The DNS record for certificate verification.
    certificateVerificationDNSRecord :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the domain association.
    domainAssociationArn :: Prelude.Text,
    -- | The name of the domain.
    domainName :: Prelude.Text,
    -- | Enables the automated creation of subdomains for branches.
    enableAutoSubDomain :: Prelude.Bool,
    -- | The current status of the domain association.
    domainStatus :: DomainStatus,
    -- | The reason for the current status of the domain association.
    statusReason :: Prelude.Text,
    -- | The subdomains for the domain association.
    subDomains :: [SubDomain]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DomainAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoSubDomainCreationPatterns', 'domainAssociation_autoSubDomainCreationPatterns' - Sets branch patterns for automatic subdomain creation.
--
-- 'autoSubDomainIAMRole', 'domainAssociation_autoSubDomainIAMRole' - The required AWS Identity and Access Management (IAM) service role for
-- the Amazon Resource Name (ARN) for automatically creating subdomains.
--
-- 'certificateVerificationDNSRecord', 'domainAssociation_certificateVerificationDNSRecord' - The DNS record for certificate verification.
--
-- 'domainAssociationArn', 'domainAssociation_domainAssociationArn' - The Amazon Resource Name (ARN) for the domain association.
--
-- 'domainName', 'domainAssociation_domainName' - The name of the domain.
--
-- 'enableAutoSubDomain', 'domainAssociation_enableAutoSubDomain' - Enables the automated creation of subdomains for branches.
--
-- 'domainStatus', 'domainAssociation_domainStatus' - The current status of the domain association.
--
-- 'statusReason', 'domainAssociation_statusReason' - The reason for the current status of the domain association.
--
-- 'subDomains', 'domainAssociation_subDomains' - The subdomains for the domain association.
newDomainAssociation ::
  -- | 'domainAssociationArn'
  Prelude.Text ->
  -- | 'domainName'
  Prelude.Text ->
  -- | 'enableAutoSubDomain'
  Prelude.Bool ->
  -- | 'domainStatus'
  DomainStatus ->
  -- | 'statusReason'
  Prelude.Text ->
  DomainAssociation
newDomainAssociation
  pDomainAssociationArn_
  pDomainName_
  pEnableAutoSubDomain_
  pDomainStatus_
  pStatusReason_ =
    DomainAssociation'
      { autoSubDomainCreationPatterns =
          Prelude.Nothing,
        autoSubDomainIAMRole = Prelude.Nothing,
        certificateVerificationDNSRecord = Prelude.Nothing,
        domainAssociationArn = pDomainAssociationArn_,
        domainName = pDomainName_,
        enableAutoSubDomain = pEnableAutoSubDomain_,
        domainStatus = pDomainStatus_,
        statusReason = pStatusReason_,
        subDomains = Prelude.mempty
      }

-- | Sets branch patterns for automatic subdomain creation.
domainAssociation_autoSubDomainCreationPatterns :: Lens.Lens' DomainAssociation (Prelude.Maybe [Prelude.Text])
domainAssociation_autoSubDomainCreationPatterns = Lens.lens (\DomainAssociation' {autoSubDomainCreationPatterns} -> autoSubDomainCreationPatterns) (\s@DomainAssociation' {} a -> s {autoSubDomainCreationPatterns = a} :: DomainAssociation) Prelude.. Lens.mapping Lens.coerced

-- | The required AWS Identity and Access Management (IAM) service role for
-- the Amazon Resource Name (ARN) for automatically creating subdomains.
domainAssociation_autoSubDomainIAMRole :: Lens.Lens' DomainAssociation (Prelude.Maybe Prelude.Text)
domainAssociation_autoSubDomainIAMRole = Lens.lens (\DomainAssociation' {autoSubDomainIAMRole} -> autoSubDomainIAMRole) (\s@DomainAssociation' {} a -> s {autoSubDomainIAMRole = a} :: DomainAssociation)

-- | The DNS record for certificate verification.
domainAssociation_certificateVerificationDNSRecord :: Lens.Lens' DomainAssociation (Prelude.Maybe Prelude.Text)
domainAssociation_certificateVerificationDNSRecord = Lens.lens (\DomainAssociation' {certificateVerificationDNSRecord} -> certificateVerificationDNSRecord) (\s@DomainAssociation' {} a -> s {certificateVerificationDNSRecord = a} :: DomainAssociation)

-- | The Amazon Resource Name (ARN) for the domain association.
domainAssociation_domainAssociationArn :: Lens.Lens' DomainAssociation Prelude.Text
domainAssociation_domainAssociationArn = Lens.lens (\DomainAssociation' {domainAssociationArn} -> domainAssociationArn) (\s@DomainAssociation' {} a -> s {domainAssociationArn = a} :: DomainAssociation)

-- | The name of the domain.
domainAssociation_domainName :: Lens.Lens' DomainAssociation Prelude.Text
domainAssociation_domainName = Lens.lens (\DomainAssociation' {domainName} -> domainName) (\s@DomainAssociation' {} a -> s {domainName = a} :: DomainAssociation)

-- | Enables the automated creation of subdomains for branches.
domainAssociation_enableAutoSubDomain :: Lens.Lens' DomainAssociation Prelude.Bool
domainAssociation_enableAutoSubDomain = Lens.lens (\DomainAssociation' {enableAutoSubDomain} -> enableAutoSubDomain) (\s@DomainAssociation' {} a -> s {enableAutoSubDomain = a} :: DomainAssociation)

-- | The current status of the domain association.
domainAssociation_domainStatus :: Lens.Lens' DomainAssociation DomainStatus
domainAssociation_domainStatus = Lens.lens (\DomainAssociation' {domainStatus} -> domainStatus) (\s@DomainAssociation' {} a -> s {domainStatus = a} :: DomainAssociation)

-- | The reason for the current status of the domain association.
domainAssociation_statusReason :: Lens.Lens' DomainAssociation Prelude.Text
domainAssociation_statusReason = Lens.lens (\DomainAssociation' {statusReason} -> statusReason) (\s@DomainAssociation' {} a -> s {statusReason = a} :: DomainAssociation)

-- | The subdomains for the domain association.
domainAssociation_subDomains :: Lens.Lens' DomainAssociation [SubDomain]
domainAssociation_subDomains = Lens.lens (\DomainAssociation' {subDomains} -> subDomains) (\s@DomainAssociation' {} a -> s {subDomains = a} :: DomainAssociation) Prelude.. Lens.coerced

instance Data.FromJSON DomainAssociation where
  parseJSON =
    Data.withObject
      "DomainAssociation"
      ( \x ->
          DomainAssociation'
            Prelude.<$> ( x Data..:? "autoSubDomainCreationPatterns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "autoSubDomainIAMRole")
            Prelude.<*> (x Data..:? "certificateVerificationDNSRecord")
            Prelude.<*> (x Data..: "domainAssociationArn")
            Prelude.<*> (x Data..: "domainName")
            Prelude.<*> (x Data..: "enableAutoSubDomain")
            Prelude.<*> (x Data..: "domainStatus")
            Prelude.<*> (x Data..: "statusReason")
            Prelude.<*> (x Data..:? "subDomains" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable DomainAssociation where
  hashWithSalt _salt DomainAssociation' {..} =
    _salt
      `Prelude.hashWithSalt` autoSubDomainCreationPatterns
      `Prelude.hashWithSalt` autoSubDomainIAMRole
      `Prelude.hashWithSalt` certificateVerificationDNSRecord
      `Prelude.hashWithSalt` domainAssociationArn
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` enableAutoSubDomain
      `Prelude.hashWithSalt` domainStatus
      `Prelude.hashWithSalt` statusReason
      `Prelude.hashWithSalt` subDomains

instance Prelude.NFData DomainAssociation where
  rnf DomainAssociation' {..} =
    Prelude.rnf autoSubDomainCreationPatterns
      `Prelude.seq` Prelude.rnf autoSubDomainIAMRole
      `Prelude.seq` Prelude.rnf certificateVerificationDNSRecord
      `Prelude.seq` Prelude.rnf domainAssociationArn
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf enableAutoSubDomain
      `Prelude.seq` Prelude.rnf domainStatus
      `Prelude.seq` Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf subDomains

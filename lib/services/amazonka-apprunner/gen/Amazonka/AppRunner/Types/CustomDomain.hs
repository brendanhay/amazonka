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
-- Module      : Amazonka.AppRunner.Types.CustomDomain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppRunner.Types.CustomDomain where

import Amazonka.AppRunner.Types.CertificateValidationRecord
import Amazonka.AppRunner.Types.CustomDomainAssociationStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a custom domain that\'s associated with an App Runner service.
--
-- /See:/ 'newCustomDomain' smart constructor.
data CustomDomain = CustomDomain'
  { -- | A list of certificate CNAME records that\'s used for this domain name.
    certificateValidationRecords :: Prelude.Maybe [CertificateValidationRecord],
    -- | An associated custom domain endpoint. It can be a root domain (for
    -- example, @example.com@), a subdomain (for example, @login.example.com@
    -- or @admin.login.example.com@), or a wildcard (for example,
    -- @*.example.com@).
    domainName :: Prelude.Text,
    -- | When @true@, the subdomain @www.DomainName @ is associated with the App
    -- Runner service in addition to the base domain.
    enableWWWSubdomain :: Prelude.Bool,
    -- | The current state of the domain name association.
    status :: CustomDomainAssociationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateValidationRecords', 'customDomain_certificateValidationRecords' - A list of certificate CNAME records that\'s used for this domain name.
--
-- 'domainName', 'customDomain_domainName' - An associated custom domain endpoint. It can be a root domain (for
-- example, @example.com@), a subdomain (for example, @login.example.com@
-- or @admin.login.example.com@), or a wildcard (for example,
-- @*.example.com@).
--
-- 'enableWWWSubdomain', 'customDomain_enableWWWSubdomain' - When @true@, the subdomain @www.DomainName @ is associated with the App
-- Runner service in addition to the base domain.
--
-- 'status', 'customDomain_status' - The current state of the domain name association.
newCustomDomain ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'enableWWWSubdomain'
  Prelude.Bool ->
  -- | 'status'
  CustomDomainAssociationStatus ->
  CustomDomain
newCustomDomain
  pDomainName_
  pEnableWWWSubdomain_
  pStatus_ =
    CustomDomain'
      { certificateValidationRecords =
          Prelude.Nothing,
        domainName = pDomainName_,
        enableWWWSubdomain = pEnableWWWSubdomain_,
        status = pStatus_
      }

-- | A list of certificate CNAME records that\'s used for this domain name.
customDomain_certificateValidationRecords :: Lens.Lens' CustomDomain (Prelude.Maybe [CertificateValidationRecord])
customDomain_certificateValidationRecords = Lens.lens (\CustomDomain' {certificateValidationRecords} -> certificateValidationRecords) (\s@CustomDomain' {} a -> s {certificateValidationRecords = a} :: CustomDomain) Prelude.. Lens.mapping Lens.coerced

-- | An associated custom domain endpoint. It can be a root domain (for
-- example, @example.com@), a subdomain (for example, @login.example.com@
-- or @admin.login.example.com@), or a wildcard (for example,
-- @*.example.com@).
customDomain_domainName :: Lens.Lens' CustomDomain Prelude.Text
customDomain_domainName = Lens.lens (\CustomDomain' {domainName} -> domainName) (\s@CustomDomain' {} a -> s {domainName = a} :: CustomDomain)

-- | When @true@, the subdomain @www.DomainName @ is associated with the App
-- Runner service in addition to the base domain.
customDomain_enableWWWSubdomain :: Lens.Lens' CustomDomain Prelude.Bool
customDomain_enableWWWSubdomain = Lens.lens (\CustomDomain' {enableWWWSubdomain} -> enableWWWSubdomain) (\s@CustomDomain' {} a -> s {enableWWWSubdomain = a} :: CustomDomain)

-- | The current state of the domain name association.
customDomain_status :: Lens.Lens' CustomDomain CustomDomainAssociationStatus
customDomain_status = Lens.lens (\CustomDomain' {status} -> status) (\s@CustomDomain' {} a -> s {status = a} :: CustomDomain)

instance Data.FromJSON CustomDomain where
  parseJSON =
    Data.withObject
      "CustomDomain"
      ( \x ->
          CustomDomain'
            Prelude.<$> ( x Data..:? "CertificateValidationRecords"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "DomainName")
            Prelude.<*> (x Data..: "EnableWWWSubdomain")
            Prelude.<*> (x Data..: "Status")
      )

instance Prelude.Hashable CustomDomain where
  hashWithSalt _salt CustomDomain' {..} =
    _salt
      `Prelude.hashWithSalt` certificateValidationRecords
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` enableWWWSubdomain
      `Prelude.hashWithSalt` status

instance Prelude.NFData CustomDomain where
  rnf CustomDomain' {..} =
    Prelude.rnf certificateValidationRecords
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf enableWWWSubdomain
      `Prelude.seq` Prelude.rnf status

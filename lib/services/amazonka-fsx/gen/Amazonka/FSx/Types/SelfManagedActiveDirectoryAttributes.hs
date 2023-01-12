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
-- Module      : Amazonka.FSx.Types.SelfManagedActiveDirectoryAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.SelfManagedActiveDirectoryAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration of the self-managed Microsoft Active Directory (AD)
-- directory to which the Windows File Server or ONTAP storage virtual
-- machine (SVM) instance is joined.
--
-- /See:/ 'newSelfManagedActiveDirectoryAttributes' smart constructor.
data SelfManagedActiveDirectoryAttributes = SelfManagedActiveDirectoryAttributes'
  { -- | A list of up to three IP addresses of DNS servers or domain controllers
    -- in the self-managed AD directory.
    dnsIps :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The fully qualified domain name of the self-managed AD directory.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain group whose members have administrative
    -- privileges for the FSx file system.
    fileSystemAdministratorsGroup :: Prelude.Maybe Prelude.Text,
    -- | The fully qualified distinguished name of the organizational unit within
    -- the self-managed AD directory to which the Windows File Server or ONTAP
    -- storage virtual machine (SVM) instance is joined.
    organizationalUnitDistinguishedName :: Prelude.Maybe Prelude.Text,
    -- | The user name for the service account on your self-managed AD domain
    -- that FSx uses to join to your AD domain.
    userName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SelfManagedActiveDirectoryAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dnsIps', 'selfManagedActiveDirectoryAttributes_dnsIps' - A list of up to three IP addresses of DNS servers or domain controllers
-- in the self-managed AD directory.
--
-- 'domainName', 'selfManagedActiveDirectoryAttributes_domainName' - The fully qualified domain name of the self-managed AD directory.
--
-- 'fileSystemAdministratorsGroup', 'selfManagedActiveDirectoryAttributes_fileSystemAdministratorsGroup' - The name of the domain group whose members have administrative
-- privileges for the FSx file system.
--
-- 'organizationalUnitDistinguishedName', 'selfManagedActiveDirectoryAttributes_organizationalUnitDistinguishedName' - The fully qualified distinguished name of the organizational unit within
-- the self-managed AD directory to which the Windows File Server or ONTAP
-- storage virtual machine (SVM) instance is joined.
--
-- 'userName', 'selfManagedActiveDirectoryAttributes_userName' - The user name for the service account on your self-managed AD domain
-- that FSx uses to join to your AD domain.
newSelfManagedActiveDirectoryAttributes ::
  SelfManagedActiveDirectoryAttributes
newSelfManagedActiveDirectoryAttributes =
  SelfManagedActiveDirectoryAttributes'
    { dnsIps =
        Prelude.Nothing,
      domainName = Prelude.Nothing,
      fileSystemAdministratorsGroup =
        Prelude.Nothing,
      organizationalUnitDistinguishedName =
        Prelude.Nothing,
      userName = Prelude.Nothing
    }

-- | A list of up to three IP addresses of DNS servers or domain controllers
-- in the self-managed AD directory.
selfManagedActiveDirectoryAttributes_dnsIps :: Lens.Lens' SelfManagedActiveDirectoryAttributes (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
selfManagedActiveDirectoryAttributes_dnsIps = Lens.lens (\SelfManagedActiveDirectoryAttributes' {dnsIps} -> dnsIps) (\s@SelfManagedActiveDirectoryAttributes' {} a -> s {dnsIps = a} :: SelfManagedActiveDirectoryAttributes) Prelude.. Lens.mapping Lens.coerced

-- | The fully qualified domain name of the self-managed AD directory.
selfManagedActiveDirectoryAttributes_domainName :: Lens.Lens' SelfManagedActiveDirectoryAttributes (Prelude.Maybe Prelude.Text)
selfManagedActiveDirectoryAttributes_domainName = Lens.lens (\SelfManagedActiveDirectoryAttributes' {domainName} -> domainName) (\s@SelfManagedActiveDirectoryAttributes' {} a -> s {domainName = a} :: SelfManagedActiveDirectoryAttributes)

-- | The name of the domain group whose members have administrative
-- privileges for the FSx file system.
selfManagedActiveDirectoryAttributes_fileSystemAdministratorsGroup :: Lens.Lens' SelfManagedActiveDirectoryAttributes (Prelude.Maybe Prelude.Text)
selfManagedActiveDirectoryAttributes_fileSystemAdministratorsGroup = Lens.lens (\SelfManagedActiveDirectoryAttributes' {fileSystemAdministratorsGroup} -> fileSystemAdministratorsGroup) (\s@SelfManagedActiveDirectoryAttributes' {} a -> s {fileSystemAdministratorsGroup = a} :: SelfManagedActiveDirectoryAttributes)

-- | The fully qualified distinguished name of the organizational unit within
-- the self-managed AD directory to which the Windows File Server or ONTAP
-- storage virtual machine (SVM) instance is joined.
selfManagedActiveDirectoryAttributes_organizationalUnitDistinguishedName :: Lens.Lens' SelfManagedActiveDirectoryAttributes (Prelude.Maybe Prelude.Text)
selfManagedActiveDirectoryAttributes_organizationalUnitDistinguishedName = Lens.lens (\SelfManagedActiveDirectoryAttributes' {organizationalUnitDistinguishedName} -> organizationalUnitDistinguishedName) (\s@SelfManagedActiveDirectoryAttributes' {} a -> s {organizationalUnitDistinguishedName = a} :: SelfManagedActiveDirectoryAttributes)

-- | The user name for the service account on your self-managed AD domain
-- that FSx uses to join to your AD domain.
selfManagedActiveDirectoryAttributes_userName :: Lens.Lens' SelfManagedActiveDirectoryAttributes (Prelude.Maybe Prelude.Text)
selfManagedActiveDirectoryAttributes_userName = Lens.lens (\SelfManagedActiveDirectoryAttributes' {userName} -> userName) (\s@SelfManagedActiveDirectoryAttributes' {} a -> s {userName = a} :: SelfManagedActiveDirectoryAttributes)

instance
  Data.FromJSON
    SelfManagedActiveDirectoryAttributes
  where
  parseJSON =
    Data.withObject
      "SelfManagedActiveDirectoryAttributes"
      ( \x ->
          SelfManagedActiveDirectoryAttributes'
            Prelude.<$> (x Data..:? "DnsIps")
            Prelude.<*> (x Data..:? "DomainName")
            Prelude.<*> (x Data..:? "FileSystemAdministratorsGroup")
            Prelude.<*> (x Data..:? "OrganizationalUnitDistinguishedName")
            Prelude.<*> (x Data..:? "UserName")
      )

instance
  Prelude.Hashable
    SelfManagedActiveDirectoryAttributes
  where
  hashWithSalt
    _salt
    SelfManagedActiveDirectoryAttributes' {..} =
      _salt `Prelude.hashWithSalt` dnsIps
        `Prelude.hashWithSalt` domainName
        `Prelude.hashWithSalt` fileSystemAdministratorsGroup
        `Prelude.hashWithSalt` organizationalUnitDistinguishedName
        `Prelude.hashWithSalt` userName

instance
  Prelude.NFData
    SelfManagedActiveDirectoryAttributes
  where
  rnf SelfManagedActiveDirectoryAttributes' {..} =
    Prelude.rnf dnsIps
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf fileSystemAdministratorsGroup
      `Prelude.seq` Prelude.rnf organizationalUnitDistinguishedName
      `Prelude.seq` Prelude.rnf userName

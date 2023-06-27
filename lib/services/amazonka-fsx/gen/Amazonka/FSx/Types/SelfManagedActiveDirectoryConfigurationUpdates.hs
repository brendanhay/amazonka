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
-- Module      : Amazonka.FSx.Types.SelfManagedActiveDirectoryConfigurationUpdates
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.SelfManagedActiveDirectoryConfigurationUpdates where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies changes you are making to the self-managed Microsoft Active
-- Directory (AD) configuration to which an FSx for Windows File Server
-- file system or an FSx for ONTAP SVM is joined.
--
-- /See:/ 'newSelfManagedActiveDirectoryConfigurationUpdates' smart constructor.
data SelfManagedActiveDirectoryConfigurationUpdates = SelfManagedActiveDirectoryConfigurationUpdates'
  { -- | A list of up to three DNS server or domain controller IP addresses in
    -- your self-managed AD domain.
    dnsIps :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Specifies an updated fully qualified domain name of your self-managed AD
    -- configuration.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the updated name of the self-managed AD domain group whose
    -- members are granted administrative privileges for the Amazon FSx
    -- resource.
    fileSystemAdministratorsGroup :: Prelude.Maybe Prelude.Text,
    -- | Specifies an updated fully qualified distinguished name of the
    -- organization unit within your self-managed AD.
    organizationalUnitDistinguishedName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the updated password for the service account on your
    -- self-managed AD domain. Amazon FSx uses this account to join to your
    -- self-managed AD domain.
    password :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Specifies the updated user name for the service account on your
    -- self-managed AD domain. Amazon FSx uses this account to join to your
    -- self-managed AD domain.
    --
    -- This account must have the permissions required to join computers to the
    -- domain in the organizational unit provided in
    -- @OrganizationalUnitDistinguishedName@.
    userName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SelfManagedActiveDirectoryConfigurationUpdates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dnsIps', 'selfManagedActiveDirectoryConfigurationUpdates_dnsIps' - A list of up to three DNS server or domain controller IP addresses in
-- your self-managed AD domain.
--
-- 'domainName', 'selfManagedActiveDirectoryConfigurationUpdates_domainName' - Specifies an updated fully qualified domain name of your self-managed AD
-- configuration.
--
-- 'fileSystemAdministratorsGroup', 'selfManagedActiveDirectoryConfigurationUpdates_fileSystemAdministratorsGroup' - Specifies the updated name of the self-managed AD domain group whose
-- members are granted administrative privileges for the Amazon FSx
-- resource.
--
-- 'organizationalUnitDistinguishedName', 'selfManagedActiveDirectoryConfigurationUpdates_organizationalUnitDistinguishedName' - Specifies an updated fully qualified distinguished name of the
-- organization unit within your self-managed AD.
--
-- 'password', 'selfManagedActiveDirectoryConfigurationUpdates_password' - Specifies the updated password for the service account on your
-- self-managed AD domain. Amazon FSx uses this account to join to your
-- self-managed AD domain.
--
-- 'userName', 'selfManagedActiveDirectoryConfigurationUpdates_userName' - Specifies the updated user name for the service account on your
-- self-managed AD domain. Amazon FSx uses this account to join to your
-- self-managed AD domain.
--
-- This account must have the permissions required to join computers to the
-- domain in the organizational unit provided in
-- @OrganizationalUnitDistinguishedName@.
newSelfManagedActiveDirectoryConfigurationUpdates ::
  SelfManagedActiveDirectoryConfigurationUpdates
newSelfManagedActiveDirectoryConfigurationUpdates =
  SelfManagedActiveDirectoryConfigurationUpdates'
    { dnsIps =
        Prelude.Nothing,
      domainName =
        Prelude.Nothing,
      fileSystemAdministratorsGroup =
        Prelude.Nothing,
      organizationalUnitDistinguishedName =
        Prelude.Nothing,
      password = Prelude.Nothing,
      userName = Prelude.Nothing
    }

-- | A list of up to three DNS server or domain controller IP addresses in
-- your self-managed AD domain.
selfManagedActiveDirectoryConfigurationUpdates_dnsIps :: Lens.Lens' SelfManagedActiveDirectoryConfigurationUpdates (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
selfManagedActiveDirectoryConfigurationUpdates_dnsIps = Lens.lens (\SelfManagedActiveDirectoryConfigurationUpdates' {dnsIps} -> dnsIps) (\s@SelfManagedActiveDirectoryConfigurationUpdates' {} a -> s {dnsIps = a} :: SelfManagedActiveDirectoryConfigurationUpdates) Prelude.. Lens.mapping Lens.coerced

-- | Specifies an updated fully qualified domain name of your self-managed AD
-- configuration.
selfManagedActiveDirectoryConfigurationUpdates_domainName :: Lens.Lens' SelfManagedActiveDirectoryConfigurationUpdates (Prelude.Maybe Prelude.Text)
selfManagedActiveDirectoryConfigurationUpdates_domainName = Lens.lens (\SelfManagedActiveDirectoryConfigurationUpdates' {domainName} -> domainName) (\s@SelfManagedActiveDirectoryConfigurationUpdates' {} a -> s {domainName = a} :: SelfManagedActiveDirectoryConfigurationUpdates)

-- | Specifies the updated name of the self-managed AD domain group whose
-- members are granted administrative privileges for the Amazon FSx
-- resource.
selfManagedActiveDirectoryConfigurationUpdates_fileSystemAdministratorsGroup :: Lens.Lens' SelfManagedActiveDirectoryConfigurationUpdates (Prelude.Maybe Prelude.Text)
selfManagedActiveDirectoryConfigurationUpdates_fileSystemAdministratorsGroup = Lens.lens (\SelfManagedActiveDirectoryConfigurationUpdates' {fileSystemAdministratorsGroup} -> fileSystemAdministratorsGroup) (\s@SelfManagedActiveDirectoryConfigurationUpdates' {} a -> s {fileSystemAdministratorsGroup = a} :: SelfManagedActiveDirectoryConfigurationUpdates)

-- | Specifies an updated fully qualified distinguished name of the
-- organization unit within your self-managed AD.
selfManagedActiveDirectoryConfigurationUpdates_organizationalUnitDistinguishedName :: Lens.Lens' SelfManagedActiveDirectoryConfigurationUpdates (Prelude.Maybe Prelude.Text)
selfManagedActiveDirectoryConfigurationUpdates_organizationalUnitDistinguishedName = Lens.lens (\SelfManagedActiveDirectoryConfigurationUpdates' {organizationalUnitDistinguishedName} -> organizationalUnitDistinguishedName) (\s@SelfManagedActiveDirectoryConfigurationUpdates' {} a -> s {organizationalUnitDistinguishedName = a} :: SelfManagedActiveDirectoryConfigurationUpdates)

-- | Specifies the updated password for the service account on your
-- self-managed AD domain. Amazon FSx uses this account to join to your
-- self-managed AD domain.
selfManagedActiveDirectoryConfigurationUpdates_password :: Lens.Lens' SelfManagedActiveDirectoryConfigurationUpdates (Prelude.Maybe Prelude.Text)
selfManagedActiveDirectoryConfigurationUpdates_password = Lens.lens (\SelfManagedActiveDirectoryConfigurationUpdates' {password} -> password) (\s@SelfManagedActiveDirectoryConfigurationUpdates' {} a -> s {password = a} :: SelfManagedActiveDirectoryConfigurationUpdates) Prelude.. Lens.mapping Data._Sensitive

-- | Specifies the updated user name for the service account on your
-- self-managed AD domain. Amazon FSx uses this account to join to your
-- self-managed AD domain.
--
-- This account must have the permissions required to join computers to the
-- domain in the organizational unit provided in
-- @OrganizationalUnitDistinguishedName@.
selfManagedActiveDirectoryConfigurationUpdates_userName :: Lens.Lens' SelfManagedActiveDirectoryConfigurationUpdates (Prelude.Maybe Prelude.Text)
selfManagedActiveDirectoryConfigurationUpdates_userName = Lens.lens (\SelfManagedActiveDirectoryConfigurationUpdates' {userName} -> userName) (\s@SelfManagedActiveDirectoryConfigurationUpdates' {} a -> s {userName = a} :: SelfManagedActiveDirectoryConfigurationUpdates)

instance
  Prelude.Hashable
    SelfManagedActiveDirectoryConfigurationUpdates
  where
  hashWithSalt
    _salt
    SelfManagedActiveDirectoryConfigurationUpdates' {..} =
      _salt
        `Prelude.hashWithSalt` dnsIps
        `Prelude.hashWithSalt` domainName
        `Prelude.hashWithSalt` fileSystemAdministratorsGroup
        `Prelude.hashWithSalt` organizationalUnitDistinguishedName
        `Prelude.hashWithSalt` password
        `Prelude.hashWithSalt` userName

instance
  Prelude.NFData
    SelfManagedActiveDirectoryConfigurationUpdates
  where
  rnf
    SelfManagedActiveDirectoryConfigurationUpdates' {..} =
      Prelude.rnf dnsIps
        `Prelude.seq` Prelude.rnf domainName
        `Prelude.seq` Prelude.rnf fileSystemAdministratorsGroup
        `Prelude.seq` Prelude.rnf organizationalUnitDistinguishedName
        `Prelude.seq` Prelude.rnf password
        `Prelude.seq` Prelude.rnf userName

instance
  Data.ToJSON
    SelfManagedActiveDirectoryConfigurationUpdates
  where
  toJSON
    SelfManagedActiveDirectoryConfigurationUpdates' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("DnsIps" Data..=) Prelude.<$> dnsIps,
              ("DomainName" Data..=) Prelude.<$> domainName,
              ("FileSystemAdministratorsGroup" Data..=)
                Prelude.<$> fileSystemAdministratorsGroup,
              ("OrganizationalUnitDistinguishedName" Data..=)
                Prelude.<$> organizationalUnitDistinguishedName,
              ("Password" Data..=) Prelude.<$> password,
              ("UserName" Data..=) Prelude.<$> userName
            ]
        )

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
-- Module      : Amazonka.FSx.Types.SelfManagedActiveDirectoryConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.SelfManagedActiveDirectoryConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration that Amazon FSx uses to join a FSx for Windows File
-- Server file system or an ONTAP storage virtual machine (SVM) to a
-- self-managed (including on-premises) Microsoft Active Directory (AD)
-- directory. For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/self-managed-AD.html Using Amazon FSx with your self-managed Microsoft Active Directory>
-- or
-- <https://docs.aws.amazon.com/fsx/latest/ONTAPGuide/managing-svms.html Managing SVMs>.
--
-- /See:/ 'newSelfManagedActiveDirectoryConfiguration' smart constructor.
data SelfManagedActiveDirectoryConfiguration = SelfManagedActiveDirectoryConfiguration'
  { -- | (Optional) The name of the domain group whose members are granted
    -- administrative privileges for the file system. Administrative privileges
    -- include taking ownership of files and folders, setting audit controls
    -- (audit ACLs) on files and folders, and administering the file system
    -- remotely by using the FSx Remote PowerShell. The group that you specify
    -- must already exist in your domain. If you don\'t provide one, your AD
    -- domain\'s Domain Admins group is used.
    fileSystemAdministratorsGroup :: Prelude.Maybe Prelude.Text,
    -- | (Optional) The fully qualified distinguished name of the organizational
    -- unit within your self-managed AD directory. Amazon FSx only accepts OU
    -- as the direct parent of the file system. An example is
    -- @OU=FSx,DC=yourdomain,DC=corp,DC=com@. To learn more, see
    -- <https://tools.ietf.org/html/rfc2253 RFC 2253>. If none is provided, the
    -- FSx file system is created in the default location of your self-managed
    -- AD directory.
    --
    -- Only Organizational Unit (OU) objects can be the direct parent of the
    -- file system that you\'re creating.
    organizationalUnitDistinguishedName :: Prelude.Maybe Prelude.Text,
    -- | The fully qualified domain name of the self-managed AD directory, such
    -- as @corp.example.com@.
    domainName :: Prelude.Text,
    -- | The user name for the service account on your self-managed AD domain
    -- that Amazon FSx will use to join to your AD domain. This account must
    -- have the permission to join computers to the domain in the
    -- organizational unit provided in @OrganizationalUnitDistinguishedName@,
    -- or in the default location of your AD domain.
    userName :: Prelude.Text,
    -- | The password for the service account on your self-managed AD domain that
    -- Amazon FSx will use to join to your AD domain.
    password :: Data.Sensitive Prelude.Text,
    -- | A list of up to three IP addresses of DNS servers or domain controllers
    -- in the self-managed AD directory.
    dnsIps :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SelfManagedActiveDirectoryConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileSystemAdministratorsGroup', 'selfManagedActiveDirectoryConfiguration_fileSystemAdministratorsGroup' - (Optional) The name of the domain group whose members are granted
-- administrative privileges for the file system. Administrative privileges
-- include taking ownership of files and folders, setting audit controls
-- (audit ACLs) on files and folders, and administering the file system
-- remotely by using the FSx Remote PowerShell. The group that you specify
-- must already exist in your domain. If you don\'t provide one, your AD
-- domain\'s Domain Admins group is used.
--
-- 'organizationalUnitDistinguishedName', 'selfManagedActiveDirectoryConfiguration_organizationalUnitDistinguishedName' - (Optional) The fully qualified distinguished name of the organizational
-- unit within your self-managed AD directory. Amazon FSx only accepts OU
-- as the direct parent of the file system. An example is
-- @OU=FSx,DC=yourdomain,DC=corp,DC=com@. To learn more, see
-- <https://tools.ietf.org/html/rfc2253 RFC 2253>. If none is provided, the
-- FSx file system is created in the default location of your self-managed
-- AD directory.
--
-- Only Organizational Unit (OU) objects can be the direct parent of the
-- file system that you\'re creating.
--
-- 'domainName', 'selfManagedActiveDirectoryConfiguration_domainName' - The fully qualified domain name of the self-managed AD directory, such
-- as @corp.example.com@.
--
-- 'userName', 'selfManagedActiveDirectoryConfiguration_userName' - The user name for the service account on your self-managed AD domain
-- that Amazon FSx will use to join to your AD domain. This account must
-- have the permission to join computers to the domain in the
-- organizational unit provided in @OrganizationalUnitDistinguishedName@,
-- or in the default location of your AD domain.
--
-- 'password', 'selfManagedActiveDirectoryConfiguration_password' - The password for the service account on your self-managed AD domain that
-- Amazon FSx will use to join to your AD domain.
--
-- 'dnsIps', 'selfManagedActiveDirectoryConfiguration_dnsIps' - A list of up to three IP addresses of DNS servers or domain controllers
-- in the self-managed AD directory.
newSelfManagedActiveDirectoryConfiguration ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'userName'
  Prelude.Text ->
  -- | 'password'
  Prelude.Text ->
  -- | 'dnsIps'
  Prelude.NonEmpty Prelude.Text ->
  SelfManagedActiveDirectoryConfiguration
newSelfManagedActiveDirectoryConfiguration
  pDomainName_
  pUserName_
  pPassword_
  pDnsIps_ =
    SelfManagedActiveDirectoryConfiguration'
      { fileSystemAdministratorsGroup =
          Prelude.Nothing,
        organizationalUnitDistinguishedName =
          Prelude.Nothing,
        domainName = pDomainName_,
        userName = pUserName_,
        password =
          Data._Sensitive
            Lens.# pPassword_,
        dnsIps =
          Lens.coerced Lens.# pDnsIps_
      }

-- | (Optional) The name of the domain group whose members are granted
-- administrative privileges for the file system. Administrative privileges
-- include taking ownership of files and folders, setting audit controls
-- (audit ACLs) on files and folders, and administering the file system
-- remotely by using the FSx Remote PowerShell. The group that you specify
-- must already exist in your domain. If you don\'t provide one, your AD
-- domain\'s Domain Admins group is used.
selfManagedActiveDirectoryConfiguration_fileSystemAdministratorsGroup :: Lens.Lens' SelfManagedActiveDirectoryConfiguration (Prelude.Maybe Prelude.Text)
selfManagedActiveDirectoryConfiguration_fileSystemAdministratorsGroup = Lens.lens (\SelfManagedActiveDirectoryConfiguration' {fileSystemAdministratorsGroup} -> fileSystemAdministratorsGroup) (\s@SelfManagedActiveDirectoryConfiguration' {} a -> s {fileSystemAdministratorsGroup = a} :: SelfManagedActiveDirectoryConfiguration)

-- | (Optional) The fully qualified distinguished name of the organizational
-- unit within your self-managed AD directory. Amazon FSx only accepts OU
-- as the direct parent of the file system. An example is
-- @OU=FSx,DC=yourdomain,DC=corp,DC=com@. To learn more, see
-- <https://tools.ietf.org/html/rfc2253 RFC 2253>. If none is provided, the
-- FSx file system is created in the default location of your self-managed
-- AD directory.
--
-- Only Organizational Unit (OU) objects can be the direct parent of the
-- file system that you\'re creating.
selfManagedActiveDirectoryConfiguration_organizationalUnitDistinguishedName :: Lens.Lens' SelfManagedActiveDirectoryConfiguration (Prelude.Maybe Prelude.Text)
selfManagedActiveDirectoryConfiguration_organizationalUnitDistinguishedName = Lens.lens (\SelfManagedActiveDirectoryConfiguration' {organizationalUnitDistinguishedName} -> organizationalUnitDistinguishedName) (\s@SelfManagedActiveDirectoryConfiguration' {} a -> s {organizationalUnitDistinguishedName = a} :: SelfManagedActiveDirectoryConfiguration)

-- | The fully qualified domain name of the self-managed AD directory, such
-- as @corp.example.com@.
selfManagedActiveDirectoryConfiguration_domainName :: Lens.Lens' SelfManagedActiveDirectoryConfiguration Prelude.Text
selfManagedActiveDirectoryConfiguration_domainName = Lens.lens (\SelfManagedActiveDirectoryConfiguration' {domainName} -> domainName) (\s@SelfManagedActiveDirectoryConfiguration' {} a -> s {domainName = a} :: SelfManagedActiveDirectoryConfiguration)

-- | The user name for the service account on your self-managed AD domain
-- that Amazon FSx will use to join to your AD domain. This account must
-- have the permission to join computers to the domain in the
-- organizational unit provided in @OrganizationalUnitDistinguishedName@,
-- or in the default location of your AD domain.
selfManagedActiveDirectoryConfiguration_userName :: Lens.Lens' SelfManagedActiveDirectoryConfiguration Prelude.Text
selfManagedActiveDirectoryConfiguration_userName = Lens.lens (\SelfManagedActiveDirectoryConfiguration' {userName} -> userName) (\s@SelfManagedActiveDirectoryConfiguration' {} a -> s {userName = a} :: SelfManagedActiveDirectoryConfiguration)

-- | The password for the service account on your self-managed AD domain that
-- Amazon FSx will use to join to your AD domain.
selfManagedActiveDirectoryConfiguration_password :: Lens.Lens' SelfManagedActiveDirectoryConfiguration Prelude.Text
selfManagedActiveDirectoryConfiguration_password = Lens.lens (\SelfManagedActiveDirectoryConfiguration' {password} -> password) (\s@SelfManagedActiveDirectoryConfiguration' {} a -> s {password = a} :: SelfManagedActiveDirectoryConfiguration) Prelude.. Data._Sensitive

-- | A list of up to three IP addresses of DNS servers or domain controllers
-- in the self-managed AD directory.
selfManagedActiveDirectoryConfiguration_dnsIps :: Lens.Lens' SelfManagedActiveDirectoryConfiguration (Prelude.NonEmpty Prelude.Text)
selfManagedActiveDirectoryConfiguration_dnsIps = Lens.lens (\SelfManagedActiveDirectoryConfiguration' {dnsIps} -> dnsIps) (\s@SelfManagedActiveDirectoryConfiguration' {} a -> s {dnsIps = a} :: SelfManagedActiveDirectoryConfiguration) Prelude.. Lens.coerced

instance
  Prelude.Hashable
    SelfManagedActiveDirectoryConfiguration
  where
  hashWithSalt
    _salt
    SelfManagedActiveDirectoryConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` fileSystemAdministratorsGroup
        `Prelude.hashWithSalt` organizationalUnitDistinguishedName
        `Prelude.hashWithSalt` domainName
        `Prelude.hashWithSalt` userName
        `Prelude.hashWithSalt` password
        `Prelude.hashWithSalt` dnsIps

instance
  Prelude.NFData
    SelfManagedActiveDirectoryConfiguration
  where
  rnf SelfManagedActiveDirectoryConfiguration' {..} =
    Prelude.rnf fileSystemAdministratorsGroup
      `Prelude.seq` Prelude.rnf organizationalUnitDistinguishedName
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf userName
      `Prelude.seq` Prelude.rnf password
      `Prelude.seq` Prelude.rnf dnsIps

instance
  Data.ToJSON
    SelfManagedActiveDirectoryConfiguration
  where
  toJSON SelfManagedActiveDirectoryConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FileSystemAdministratorsGroup" Data..=)
              Prelude.<$> fileSystemAdministratorsGroup,
            ("OrganizationalUnitDistinguishedName" Data..=)
              Prelude.<$> organizationalUnitDistinguishedName,
            Prelude.Just ("DomainName" Data..= domainName),
            Prelude.Just ("UserName" Data..= userName),
            Prelude.Just ("Password" Data..= password),
            Prelude.Just ("DnsIps" Data..= dnsIps)
          ]
      )

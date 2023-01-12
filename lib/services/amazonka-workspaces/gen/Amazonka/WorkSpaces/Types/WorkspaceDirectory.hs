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
-- Module      : Amazonka.WorkSpaces.Types.WorkspaceDirectory
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.WorkspaceDirectory where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkSpaces.Types.CertificateBasedAuthProperties
import Amazonka.WorkSpaces.Types.DefaultWorkspaceCreationProperties
import Amazonka.WorkSpaces.Types.SamlProperties
import Amazonka.WorkSpaces.Types.SelfservicePermissions
import Amazonka.WorkSpaces.Types.Tenancy
import Amazonka.WorkSpaces.Types.WorkspaceAccessProperties
import Amazonka.WorkSpaces.Types.WorkspaceDirectoryState
import Amazonka.WorkSpaces.Types.WorkspaceDirectoryType

-- | Describes a directory that is used with Amazon WorkSpaces.
--
-- /See:/ 'newWorkspaceDirectory' smart constructor.
data WorkspaceDirectory = WorkspaceDirectory'
  { -- | The directory alias.
    alias :: Prelude.Maybe Prelude.Text,
    -- | The certificate-based authentication properties used to authenticate
    -- SAML 2.0 Identity Provider (IdP) user identities to Active Directory for
    -- WorkSpaces login.
    certificateBasedAuthProperties :: Prelude.Maybe CertificateBasedAuthProperties,
    -- | The user name for the service account.
    customerUserName :: Prelude.Maybe Prelude.Text,
    -- | The directory identifier.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | The name of the directory.
    directoryName :: Prelude.Maybe Prelude.Text,
    -- | The directory type.
    directoryType :: Prelude.Maybe WorkspaceDirectoryType,
    -- | The IP addresses of the DNS servers for the directory.
    dnsIpAddresses :: Prelude.Maybe [Prelude.Text],
    -- | The identifier of the IAM role. This is the role that allows Amazon
    -- WorkSpaces to make calls to other services, such as Amazon EC2, on your
    -- behalf.
    iamRoleId :: Prelude.Maybe Prelude.Text,
    -- | The registration code for the directory. This is the code that users
    -- enter in their Amazon WorkSpaces client application to connect to the
    -- directory.
    registrationCode :: Prelude.Maybe Prelude.Text,
    -- | Describes the enablement status, user access URL, and relay state
    -- parameter name that are used for configuring federation with an SAML 2.0
    -- identity provider.
    samlProperties :: Prelude.Maybe SamlProperties,
    -- | The default self-service permissions for WorkSpaces in the directory.
    selfservicePermissions :: Prelude.Maybe SelfservicePermissions,
    -- | The state of the directory\'s registration with Amazon WorkSpaces. After
    -- a directory is deregistered, the @DEREGISTERED@ state is returned very
    -- briefly before the directory metadata is cleaned up, so this state is
    -- rarely returned. To confirm that a directory is deregistered, check for
    -- the directory ID by using
    -- <https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeWorkspaceDirectories.html DescribeWorkspaceDirectories>.
    -- If the directory ID isn\'t returned, then the directory has been
    -- successfully deregistered.
    state :: Prelude.Maybe WorkspaceDirectoryState,
    -- | The identifiers of the subnets used with the directory.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | Specifies whether the directory is dedicated or shared. To use Bring
    -- Your Own License (BYOL), this value must be set to @DEDICATED@. For more
    -- information, see
    -- <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Images>.
    tenancy :: Prelude.Maybe Tenancy,
    -- | The devices and operating systems that users can use to access
    -- WorkSpaces.
    workspaceAccessProperties :: Prelude.Maybe WorkspaceAccessProperties,
    -- | The default creation properties for all WorkSpaces in the directory.
    workspaceCreationProperties :: Prelude.Maybe DefaultWorkspaceCreationProperties,
    -- | The identifier of the security group that is assigned to new WorkSpaces.
    workspaceSecurityGroupId :: Prelude.Maybe Prelude.Text,
    -- | The identifiers of the IP access control groups associated with the
    -- directory.
    ipGroupIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkspaceDirectory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alias', 'workspaceDirectory_alias' - The directory alias.
--
-- 'certificateBasedAuthProperties', 'workspaceDirectory_certificateBasedAuthProperties' - The certificate-based authentication properties used to authenticate
-- SAML 2.0 Identity Provider (IdP) user identities to Active Directory for
-- WorkSpaces login.
--
-- 'customerUserName', 'workspaceDirectory_customerUserName' - The user name for the service account.
--
-- 'directoryId', 'workspaceDirectory_directoryId' - The directory identifier.
--
-- 'directoryName', 'workspaceDirectory_directoryName' - The name of the directory.
--
-- 'directoryType', 'workspaceDirectory_directoryType' - The directory type.
--
-- 'dnsIpAddresses', 'workspaceDirectory_dnsIpAddresses' - The IP addresses of the DNS servers for the directory.
--
-- 'iamRoleId', 'workspaceDirectory_iamRoleId' - The identifier of the IAM role. This is the role that allows Amazon
-- WorkSpaces to make calls to other services, such as Amazon EC2, on your
-- behalf.
--
-- 'registrationCode', 'workspaceDirectory_registrationCode' - The registration code for the directory. This is the code that users
-- enter in their Amazon WorkSpaces client application to connect to the
-- directory.
--
-- 'samlProperties', 'workspaceDirectory_samlProperties' - Describes the enablement status, user access URL, and relay state
-- parameter name that are used for configuring federation with an SAML 2.0
-- identity provider.
--
-- 'selfservicePermissions', 'workspaceDirectory_selfservicePermissions' - The default self-service permissions for WorkSpaces in the directory.
--
-- 'state', 'workspaceDirectory_state' - The state of the directory\'s registration with Amazon WorkSpaces. After
-- a directory is deregistered, the @DEREGISTERED@ state is returned very
-- briefly before the directory metadata is cleaned up, so this state is
-- rarely returned. To confirm that a directory is deregistered, check for
-- the directory ID by using
-- <https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeWorkspaceDirectories.html DescribeWorkspaceDirectories>.
-- If the directory ID isn\'t returned, then the directory has been
-- successfully deregistered.
--
-- 'subnetIds', 'workspaceDirectory_subnetIds' - The identifiers of the subnets used with the directory.
--
-- 'tenancy', 'workspaceDirectory_tenancy' - Specifies whether the directory is dedicated or shared. To use Bring
-- Your Own License (BYOL), this value must be set to @DEDICATED@. For more
-- information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Images>.
--
-- 'workspaceAccessProperties', 'workspaceDirectory_workspaceAccessProperties' - The devices and operating systems that users can use to access
-- WorkSpaces.
--
-- 'workspaceCreationProperties', 'workspaceDirectory_workspaceCreationProperties' - The default creation properties for all WorkSpaces in the directory.
--
-- 'workspaceSecurityGroupId', 'workspaceDirectory_workspaceSecurityGroupId' - The identifier of the security group that is assigned to new WorkSpaces.
--
-- 'ipGroupIds', 'workspaceDirectory_ipGroupIds' - The identifiers of the IP access control groups associated with the
-- directory.
newWorkspaceDirectory ::
  WorkspaceDirectory
newWorkspaceDirectory =
  WorkspaceDirectory'
    { alias = Prelude.Nothing,
      certificateBasedAuthProperties = Prelude.Nothing,
      customerUserName = Prelude.Nothing,
      directoryId = Prelude.Nothing,
      directoryName = Prelude.Nothing,
      directoryType = Prelude.Nothing,
      dnsIpAddresses = Prelude.Nothing,
      iamRoleId = Prelude.Nothing,
      registrationCode = Prelude.Nothing,
      samlProperties = Prelude.Nothing,
      selfservicePermissions = Prelude.Nothing,
      state = Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      tenancy = Prelude.Nothing,
      workspaceAccessProperties = Prelude.Nothing,
      workspaceCreationProperties = Prelude.Nothing,
      workspaceSecurityGroupId = Prelude.Nothing,
      ipGroupIds = Prelude.Nothing
    }

-- | The directory alias.
workspaceDirectory_alias :: Lens.Lens' WorkspaceDirectory (Prelude.Maybe Prelude.Text)
workspaceDirectory_alias = Lens.lens (\WorkspaceDirectory' {alias} -> alias) (\s@WorkspaceDirectory' {} a -> s {alias = a} :: WorkspaceDirectory)

-- | The certificate-based authentication properties used to authenticate
-- SAML 2.0 Identity Provider (IdP) user identities to Active Directory for
-- WorkSpaces login.
workspaceDirectory_certificateBasedAuthProperties :: Lens.Lens' WorkspaceDirectory (Prelude.Maybe CertificateBasedAuthProperties)
workspaceDirectory_certificateBasedAuthProperties = Lens.lens (\WorkspaceDirectory' {certificateBasedAuthProperties} -> certificateBasedAuthProperties) (\s@WorkspaceDirectory' {} a -> s {certificateBasedAuthProperties = a} :: WorkspaceDirectory)

-- | The user name for the service account.
workspaceDirectory_customerUserName :: Lens.Lens' WorkspaceDirectory (Prelude.Maybe Prelude.Text)
workspaceDirectory_customerUserName = Lens.lens (\WorkspaceDirectory' {customerUserName} -> customerUserName) (\s@WorkspaceDirectory' {} a -> s {customerUserName = a} :: WorkspaceDirectory)

-- | The directory identifier.
workspaceDirectory_directoryId :: Lens.Lens' WorkspaceDirectory (Prelude.Maybe Prelude.Text)
workspaceDirectory_directoryId = Lens.lens (\WorkspaceDirectory' {directoryId} -> directoryId) (\s@WorkspaceDirectory' {} a -> s {directoryId = a} :: WorkspaceDirectory)

-- | The name of the directory.
workspaceDirectory_directoryName :: Lens.Lens' WorkspaceDirectory (Prelude.Maybe Prelude.Text)
workspaceDirectory_directoryName = Lens.lens (\WorkspaceDirectory' {directoryName} -> directoryName) (\s@WorkspaceDirectory' {} a -> s {directoryName = a} :: WorkspaceDirectory)

-- | The directory type.
workspaceDirectory_directoryType :: Lens.Lens' WorkspaceDirectory (Prelude.Maybe WorkspaceDirectoryType)
workspaceDirectory_directoryType = Lens.lens (\WorkspaceDirectory' {directoryType} -> directoryType) (\s@WorkspaceDirectory' {} a -> s {directoryType = a} :: WorkspaceDirectory)

-- | The IP addresses of the DNS servers for the directory.
workspaceDirectory_dnsIpAddresses :: Lens.Lens' WorkspaceDirectory (Prelude.Maybe [Prelude.Text])
workspaceDirectory_dnsIpAddresses = Lens.lens (\WorkspaceDirectory' {dnsIpAddresses} -> dnsIpAddresses) (\s@WorkspaceDirectory' {} a -> s {dnsIpAddresses = a} :: WorkspaceDirectory) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the IAM role. This is the role that allows Amazon
-- WorkSpaces to make calls to other services, such as Amazon EC2, on your
-- behalf.
workspaceDirectory_iamRoleId :: Lens.Lens' WorkspaceDirectory (Prelude.Maybe Prelude.Text)
workspaceDirectory_iamRoleId = Lens.lens (\WorkspaceDirectory' {iamRoleId} -> iamRoleId) (\s@WorkspaceDirectory' {} a -> s {iamRoleId = a} :: WorkspaceDirectory)

-- | The registration code for the directory. This is the code that users
-- enter in their Amazon WorkSpaces client application to connect to the
-- directory.
workspaceDirectory_registrationCode :: Lens.Lens' WorkspaceDirectory (Prelude.Maybe Prelude.Text)
workspaceDirectory_registrationCode = Lens.lens (\WorkspaceDirectory' {registrationCode} -> registrationCode) (\s@WorkspaceDirectory' {} a -> s {registrationCode = a} :: WorkspaceDirectory)

-- | Describes the enablement status, user access URL, and relay state
-- parameter name that are used for configuring federation with an SAML 2.0
-- identity provider.
workspaceDirectory_samlProperties :: Lens.Lens' WorkspaceDirectory (Prelude.Maybe SamlProperties)
workspaceDirectory_samlProperties = Lens.lens (\WorkspaceDirectory' {samlProperties} -> samlProperties) (\s@WorkspaceDirectory' {} a -> s {samlProperties = a} :: WorkspaceDirectory)

-- | The default self-service permissions for WorkSpaces in the directory.
workspaceDirectory_selfservicePermissions :: Lens.Lens' WorkspaceDirectory (Prelude.Maybe SelfservicePermissions)
workspaceDirectory_selfservicePermissions = Lens.lens (\WorkspaceDirectory' {selfservicePermissions} -> selfservicePermissions) (\s@WorkspaceDirectory' {} a -> s {selfservicePermissions = a} :: WorkspaceDirectory)

-- | The state of the directory\'s registration with Amazon WorkSpaces. After
-- a directory is deregistered, the @DEREGISTERED@ state is returned very
-- briefly before the directory metadata is cleaned up, so this state is
-- rarely returned. To confirm that a directory is deregistered, check for
-- the directory ID by using
-- <https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeWorkspaceDirectories.html DescribeWorkspaceDirectories>.
-- If the directory ID isn\'t returned, then the directory has been
-- successfully deregistered.
workspaceDirectory_state :: Lens.Lens' WorkspaceDirectory (Prelude.Maybe WorkspaceDirectoryState)
workspaceDirectory_state = Lens.lens (\WorkspaceDirectory' {state} -> state) (\s@WorkspaceDirectory' {} a -> s {state = a} :: WorkspaceDirectory)

-- | The identifiers of the subnets used with the directory.
workspaceDirectory_subnetIds :: Lens.Lens' WorkspaceDirectory (Prelude.Maybe [Prelude.Text])
workspaceDirectory_subnetIds = Lens.lens (\WorkspaceDirectory' {subnetIds} -> subnetIds) (\s@WorkspaceDirectory' {} a -> s {subnetIds = a} :: WorkspaceDirectory) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether the directory is dedicated or shared. To use Bring
-- Your Own License (BYOL), this value must be set to @DEDICATED@. For more
-- information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Images>.
workspaceDirectory_tenancy :: Lens.Lens' WorkspaceDirectory (Prelude.Maybe Tenancy)
workspaceDirectory_tenancy = Lens.lens (\WorkspaceDirectory' {tenancy} -> tenancy) (\s@WorkspaceDirectory' {} a -> s {tenancy = a} :: WorkspaceDirectory)

-- | The devices and operating systems that users can use to access
-- WorkSpaces.
workspaceDirectory_workspaceAccessProperties :: Lens.Lens' WorkspaceDirectory (Prelude.Maybe WorkspaceAccessProperties)
workspaceDirectory_workspaceAccessProperties = Lens.lens (\WorkspaceDirectory' {workspaceAccessProperties} -> workspaceAccessProperties) (\s@WorkspaceDirectory' {} a -> s {workspaceAccessProperties = a} :: WorkspaceDirectory)

-- | The default creation properties for all WorkSpaces in the directory.
workspaceDirectory_workspaceCreationProperties :: Lens.Lens' WorkspaceDirectory (Prelude.Maybe DefaultWorkspaceCreationProperties)
workspaceDirectory_workspaceCreationProperties = Lens.lens (\WorkspaceDirectory' {workspaceCreationProperties} -> workspaceCreationProperties) (\s@WorkspaceDirectory' {} a -> s {workspaceCreationProperties = a} :: WorkspaceDirectory)

-- | The identifier of the security group that is assigned to new WorkSpaces.
workspaceDirectory_workspaceSecurityGroupId :: Lens.Lens' WorkspaceDirectory (Prelude.Maybe Prelude.Text)
workspaceDirectory_workspaceSecurityGroupId = Lens.lens (\WorkspaceDirectory' {workspaceSecurityGroupId} -> workspaceSecurityGroupId) (\s@WorkspaceDirectory' {} a -> s {workspaceSecurityGroupId = a} :: WorkspaceDirectory)

-- | The identifiers of the IP access control groups associated with the
-- directory.
workspaceDirectory_ipGroupIds :: Lens.Lens' WorkspaceDirectory (Prelude.Maybe [Prelude.Text])
workspaceDirectory_ipGroupIds = Lens.lens (\WorkspaceDirectory' {ipGroupIds} -> ipGroupIds) (\s@WorkspaceDirectory' {} a -> s {ipGroupIds = a} :: WorkspaceDirectory) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON WorkspaceDirectory where
  parseJSON =
    Data.withObject
      "WorkspaceDirectory"
      ( \x ->
          WorkspaceDirectory'
            Prelude.<$> (x Data..:? "Alias")
            Prelude.<*> (x Data..:? "CertificateBasedAuthProperties")
            Prelude.<*> (x Data..:? "CustomerUserName")
            Prelude.<*> (x Data..:? "DirectoryId")
            Prelude.<*> (x Data..:? "DirectoryName")
            Prelude.<*> (x Data..:? "DirectoryType")
            Prelude.<*> (x Data..:? "DnsIpAddresses" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "IamRoleId")
            Prelude.<*> (x Data..:? "RegistrationCode")
            Prelude.<*> (x Data..:? "SamlProperties")
            Prelude.<*> (x Data..:? "SelfservicePermissions")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "SubnetIds" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Tenancy")
            Prelude.<*> (x Data..:? "WorkspaceAccessProperties")
            Prelude.<*> (x Data..:? "WorkspaceCreationProperties")
            Prelude.<*> (x Data..:? "WorkspaceSecurityGroupId")
            Prelude.<*> (x Data..:? "ipGroupIds" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable WorkspaceDirectory where
  hashWithSalt _salt WorkspaceDirectory' {..} =
    _salt `Prelude.hashWithSalt` alias
      `Prelude.hashWithSalt` certificateBasedAuthProperties
      `Prelude.hashWithSalt` customerUserName
      `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` directoryName
      `Prelude.hashWithSalt` directoryType
      `Prelude.hashWithSalt` dnsIpAddresses
      `Prelude.hashWithSalt` iamRoleId
      `Prelude.hashWithSalt` registrationCode
      `Prelude.hashWithSalt` samlProperties
      `Prelude.hashWithSalt` selfservicePermissions
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` tenancy
      `Prelude.hashWithSalt` workspaceAccessProperties
      `Prelude.hashWithSalt` workspaceCreationProperties
      `Prelude.hashWithSalt` workspaceSecurityGroupId
      `Prelude.hashWithSalt` ipGroupIds

instance Prelude.NFData WorkspaceDirectory where
  rnf WorkspaceDirectory' {..} =
    Prelude.rnf alias
      `Prelude.seq` Prelude.rnf certificateBasedAuthProperties
      `Prelude.seq` Prelude.rnf customerUserName
      `Prelude.seq` Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf directoryName
      `Prelude.seq` Prelude.rnf directoryType
      `Prelude.seq` Prelude.rnf dnsIpAddresses
      `Prelude.seq` Prelude.rnf iamRoleId
      `Prelude.seq` Prelude.rnf registrationCode
      `Prelude.seq` Prelude.rnf samlProperties
      `Prelude.seq` Prelude.rnf selfservicePermissions
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf tenancy
      `Prelude.seq` Prelude.rnf workspaceAccessProperties
      `Prelude.seq` Prelude.rnf
        workspaceCreationProperties
      `Prelude.seq` Prelude.rnf workspaceSecurityGroupId
      `Prelude.seq` Prelude.rnf ipGroupIds

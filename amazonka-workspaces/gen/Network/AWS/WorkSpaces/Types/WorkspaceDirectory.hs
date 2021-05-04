{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceDirectory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.WorkspaceDirectory where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WorkSpaces.Types.DefaultWorkspaceCreationProperties
import Network.AWS.WorkSpaces.Types.SelfservicePermissions
import Network.AWS.WorkSpaces.Types.Tenancy
import Network.AWS.WorkSpaces.Types.WorkspaceAccessProperties
import Network.AWS.WorkSpaces.Types.WorkspaceDirectoryState
import Network.AWS.WorkSpaces.Types.WorkspaceDirectoryType

-- | Describes a directory that is used with Amazon WorkSpaces.
--
-- /See:/ 'newWorkspaceDirectory' smart constructor.
data WorkspaceDirectory = WorkspaceDirectory'
  { -- | The registration code for the directory. This is the code that users
    -- enter in their Amazon WorkSpaces client application to connect to the
    -- directory.
    registrationCode :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the security group that is assigned to new WorkSpaces.
    workspaceSecurityGroupId :: Prelude.Maybe Prelude.Text,
    -- | The directory alias.
    alias :: Prelude.Maybe Prelude.Text,
    -- | The identifiers of the IP access control groups associated with the
    -- directory.
    ipGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The devices and operating systems that users can use to access
    -- WorkSpaces.
    workspaceAccessProperties :: Prelude.Maybe WorkspaceAccessProperties,
    -- | The identifiers of the subnets used with the directory.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | Specifies whether the directory is dedicated or shared. To use Bring
    -- Your Own License (BYOL), this value must be set to @DEDICATED@. For more
    -- information, see
    -- <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Images>.
    tenancy :: Prelude.Maybe Tenancy,
    -- | The user name for the service account.
    customerUserName :: Prelude.Maybe Prelude.Text,
    -- | The state of the directory\'s registration with Amazon WorkSpaces. After
    -- a directory is deregistered, the @DEREGISTERED@ state is returned very
    -- briefly before the directory metadata is cleaned up, so this state is
    -- rarely returned. To confirm that a directory is deregistered, check for
    -- the directory ID by using
    -- <https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeWorkspaceDirectories.html DescribeWorkspaceDirectories>.
    -- If the directory ID isn\'t returned, then the directory has been
    -- successfully deregistered.
    state :: Prelude.Maybe WorkspaceDirectoryState,
    -- | The identifier of the IAM role. This is the role that allows Amazon
    -- WorkSpaces to make calls to other services, such as Amazon EC2, on your
    -- behalf.
    iamRoleId :: Prelude.Maybe Prelude.Text,
    -- | The directory identifier.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | The default self-service permissions for WorkSpaces in the directory.
    selfservicePermissions :: Prelude.Maybe SelfservicePermissions,
    -- | The directory type.
    directoryType :: Prelude.Maybe WorkspaceDirectoryType,
    -- | The name of the directory.
    directoryName :: Prelude.Maybe Prelude.Text,
    -- | The IP addresses of the DNS servers for the directory.
    dnsIpAddresses :: Prelude.Maybe [Prelude.Text],
    -- | The default creation properties for all WorkSpaces in the directory.
    workspaceCreationProperties :: Prelude.Maybe DefaultWorkspaceCreationProperties
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'WorkspaceDirectory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registrationCode', 'workspaceDirectory_registrationCode' - The registration code for the directory. This is the code that users
-- enter in their Amazon WorkSpaces client application to connect to the
-- directory.
--
-- 'workspaceSecurityGroupId', 'workspaceDirectory_workspaceSecurityGroupId' - The identifier of the security group that is assigned to new WorkSpaces.
--
-- 'alias', 'workspaceDirectory_alias' - The directory alias.
--
-- 'ipGroupIds', 'workspaceDirectory_ipGroupIds' - The identifiers of the IP access control groups associated with the
-- directory.
--
-- 'workspaceAccessProperties', 'workspaceDirectory_workspaceAccessProperties' - The devices and operating systems that users can use to access
-- WorkSpaces.
--
-- 'subnetIds', 'workspaceDirectory_subnetIds' - The identifiers of the subnets used with the directory.
--
-- 'tenancy', 'workspaceDirectory_tenancy' - Specifies whether the directory is dedicated or shared. To use Bring
-- Your Own License (BYOL), this value must be set to @DEDICATED@. For more
-- information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Images>.
--
-- 'customerUserName', 'workspaceDirectory_customerUserName' - The user name for the service account.
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
-- 'iamRoleId', 'workspaceDirectory_iamRoleId' - The identifier of the IAM role. This is the role that allows Amazon
-- WorkSpaces to make calls to other services, such as Amazon EC2, on your
-- behalf.
--
-- 'directoryId', 'workspaceDirectory_directoryId' - The directory identifier.
--
-- 'selfservicePermissions', 'workspaceDirectory_selfservicePermissions' - The default self-service permissions for WorkSpaces in the directory.
--
-- 'directoryType', 'workspaceDirectory_directoryType' - The directory type.
--
-- 'directoryName', 'workspaceDirectory_directoryName' - The name of the directory.
--
-- 'dnsIpAddresses', 'workspaceDirectory_dnsIpAddresses' - The IP addresses of the DNS servers for the directory.
--
-- 'workspaceCreationProperties', 'workspaceDirectory_workspaceCreationProperties' - The default creation properties for all WorkSpaces in the directory.
newWorkspaceDirectory ::
  WorkspaceDirectory
newWorkspaceDirectory =
  WorkspaceDirectory'
    { registrationCode =
        Prelude.Nothing,
      workspaceSecurityGroupId = Prelude.Nothing,
      alias = Prelude.Nothing,
      ipGroupIds = Prelude.Nothing,
      workspaceAccessProperties = Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      tenancy = Prelude.Nothing,
      customerUserName = Prelude.Nothing,
      state = Prelude.Nothing,
      iamRoleId = Prelude.Nothing,
      directoryId = Prelude.Nothing,
      selfservicePermissions = Prelude.Nothing,
      directoryType = Prelude.Nothing,
      directoryName = Prelude.Nothing,
      dnsIpAddresses = Prelude.Nothing,
      workspaceCreationProperties = Prelude.Nothing
    }

-- | The registration code for the directory. This is the code that users
-- enter in their Amazon WorkSpaces client application to connect to the
-- directory.
workspaceDirectory_registrationCode :: Lens.Lens' WorkspaceDirectory (Prelude.Maybe Prelude.Text)
workspaceDirectory_registrationCode = Lens.lens (\WorkspaceDirectory' {registrationCode} -> registrationCode) (\s@WorkspaceDirectory' {} a -> s {registrationCode = a} :: WorkspaceDirectory)

-- | The identifier of the security group that is assigned to new WorkSpaces.
workspaceDirectory_workspaceSecurityGroupId :: Lens.Lens' WorkspaceDirectory (Prelude.Maybe Prelude.Text)
workspaceDirectory_workspaceSecurityGroupId = Lens.lens (\WorkspaceDirectory' {workspaceSecurityGroupId} -> workspaceSecurityGroupId) (\s@WorkspaceDirectory' {} a -> s {workspaceSecurityGroupId = a} :: WorkspaceDirectory)

-- | The directory alias.
workspaceDirectory_alias :: Lens.Lens' WorkspaceDirectory (Prelude.Maybe Prelude.Text)
workspaceDirectory_alias = Lens.lens (\WorkspaceDirectory' {alias} -> alias) (\s@WorkspaceDirectory' {} a -> s {alias = a} :: WorkspaceDirectory)

-- | The identifiers of the IP access control groups associated with the
-- directory.
workspaceDirectory_ipGroupIds :: Lens.Lens' WorkspaceDirectory (Prelude.Maybe [Prelude.Text])
workspaceDirectory_ipGroupIds = Lens.lens (\WorkspaceDirectory' {ipGroupIds} -> ipGroupIds) (\s@WorkspaceDirectory' {} a -> s {ipGroupIds = a} :: WorkspaceDirectory) Prelude.. Lens.mapping Prelude._Coerce

-- | The devices and operating systems that users can use to access
-- WorkSpaces.
workspaceDirectory_workspaceAccessProperties :: Lens.Lens' WorkspaceDirectory (Prelude.Maybe WorkspaceAccessProperties)
workspaceDirectory_workspaceAccessProperties = Lens.lens (\WorkspaceDirectory' {workspaceAccessProperties} -> workspaceAccessProperties) (\s@WorkspaceDirectory' {} a -> s {workspaceAccessProperties = a} :: WorkspaceDirectory)

-- | The identifiers of the subnets used with the directory.
workspaceDirectory_subnetIds :: Lens.Lens' WorkspaceDirectory (Prelude.Maybe [Prelude.Text])
workspaceDirectory_subnetIds = Lens.lens (\WorkspaceDirectory' {subnetIds} -> subnetIds) (\s@WorkspaceDirectory' {} a -> s {subnetIds = a} :: WorkspaceDirectory) Prelude.. Lens.mapping Prelude._Coerce

-- | Specifies whether the directory is dedicated or shared. To use Bring
-- Your Own License (BYOL), this value must be set to @DEDICATED@. For more
-- information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Images>.
workspaceDirectory_tenancy :: Lens.Lens' WorkspaceDirectory (Prelude.Maybe Tenancy)
workspaceDirectory_tenancy = Lens.lens (\WorkspaceDirectory' {tenancy} -> tenancy) (\s@WorkspaceDirectory' {} a -> s {tenancy = a} :: WorkspaceDirectory)

-- | The user name for the service account.
workspaceDirectory_customerUserName :: Lens.Lens' WorkspaceDirectory (Prelude.Maybe Prelude.Text)
workspaceDirectory_customerUserName = Lens.lens (\WorkspaceDirectory' {customerUserName} -> customerUserName) (\s@WorkspaceDirectory' {} a -> s {customerUserName = a} :: WorkspaceDirectory)

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

-- | The identifier of the IAM role. This is the role that allows Amazon
-- WorkSpaces to make calls to other services, such as Amazon EC2, on your
-- behalf.
workspaceDirectory_iamRoleId :: Lens.Lens' WorkspaceDirectory (Prelude.Maybe Prelude.Text)
workspaceDirectory_iamRoleId = Lens.lens (\WorkspaceDirectory' {iamRoleId} -> iamRoleId) (\s@WorkspaceDirectory' {} a -> s {iamRoleId = a} :: WorkspaceDirectory)

-- | The directory identifier.
workspaceDirectory_directoryId :: Lens.Lens' WorkspaceDirectory (Prelude.Maybe Prelude.Text)
workspaceDirectory_directoryId = Lens.lens (\WorkspaceDirectory' {directoryId} -> directoryId) (\s@WorkspaceDirectory' {} a -> s {directoryId = a} :: WorkspaceDirectory)

-- | The default self-service permissions for WorkSpaces in the directory.
workspaceDirectory_selfservicePermissions :: Lens.Lens' WorkspaceDirectory (Prelude.Maybe SelfservicePermissions)
workspaceDirectory_selfservicePermissions = Lens.lens (\WorkspaceDirectory' {selfservicePermissions} -> selfservicePermissions) (\s@WorkspaceDirectory' {} a -> s {selfservicePermissions = a} :: WorkspaceDirectory)

-- | The directory type.
workspaceDirectory_directoryType :: Lens.Lens' WorkspaceDirectory (Prelude.Maybe WorkspaceDirectoryType)
workspaceDirectory_directoryType = Lens.lens (\WorkspaceDirectory' {directoryType} -> directoryType) (\s@WorkspaceDirectory' {} a -> s {directoryType = a} :: WorkspaceDirectory)

-- | The name of the directory.
workspaceDirectory_directoryName :: Lens.Lens' WorkspaceDirectory (Prelude.Maybe Prelude.Text)
workspaceDirectory_directoryName = Lens.lens (\WorkspaceDirectory' {directoryName} -> directoryName) (\s@WorkspaceDirectory' {} a -> s {directoryName = a} :: WorkspaceDirectory)

-- | The IP addresses of the DNS servers for the directory.
workspaceDirectory_dnsIpAddresses :: Lens.Lens' WorkspaceDirectory (Prelude.Maybe [Prelude.Text])
workspaceDirectory_dnsIpAddresses = Lens.lens (\WorkspaceDirectory' {dnsIpAddresses} -> dnsIpAddresses) (\s@WorkspaceDirectory' {} a -> s {dnsIpAddresses = a} :: WorkspaceDirectory) Prelude.. Lens.mapping Prelude._Coerce

-- | The default creation properties for all WorkSpaces in the directory.
workspaceDirectory_workspaceCreationProperties :: Lens.Lens' WorkspaceDirectory (Prelude.Maybe DefaultWorkspaceCreationProperties)
workspaceDirectory_workspaceCreationProperties = Lens.lens (\WorkspaceDirectory' {workspaceCreationProperties} -> workspaceCreationProperties) (\s@WorkspaceDirectory' {} a -> s {workspaceCreationProperties = a} :: WorkspaceDirectory)

instance Prelude.FromJSON WorkspaceDirectory where
  parseJSON =
    Prelude.withObject
      "WorkspaceDirectory"
      ( \x ->
          WorkspaceDirectory'
            Prelude.<$> (x Prelude..:? "RegistrationCode")
            Prelude.<*> (x Prelude..:? "WorkspaceSecurityGroupId")
            Prelude.<*> (x Prelude..:? "Alias")
            Prelude.<*> ( x Prelude..:? "ipGroupIds"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "WorkspaceAccessProperties")
            Prelude.<*> ( x Prelude..:? "SubnetIds"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "Tenancy")
            Prelude.<*> (x Prelude..:? "CustomerUserName")
            Prelude.<*> (x Prelude..:? "State")
            Prelude.<*> (x Prelude..:? "IamRoleId")
            Prelude.<*> (x Prelude..:? "DirectoryId")
            Prelude.<*> (x Prelude..:? "SelfservicePermissions")
            Prelude.<*> (x Prelude..:? "DirectoryType")
            Prelude.<*> (x Prelude..:? "DirectoryName")
            Prelude.<*> ( x Prelude..:? "DnsIpAddresses"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "WorkspaceCreationProperties")
      )

instance Prelude.Hashable WorkspaceDirectory

instance Prelude.NFData WorkspaceDirectory

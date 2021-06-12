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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
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
    registrationCode :: Core.Maybe Core.Text,
    -- | The identifier of the security group that is assigned to new WorkSpaces.
    workspaceSecurityGroupId :: Core.Maybe Core.Text,
    -- | The directory alias.
    alias :: Core.Maybe Core.Text,
    -- | The identifiers of the IP access control groups associated with the
    -- directory.
    ipGroupIds :: Core.Maybe [Core.Text],
    -- | The devices and operating systems that users can use to access
    -- WorkSpaces.
    workspaceAccessProperties :: Core.Maybe WorkspaceAccessProperties,
    -- | The identifiers of the subnets used with the directory.
    subnetIds :: Core.Maybe [Core.Text],
    -- | Specifies whether the directory is dedicated or shared. To use Bring
    -- Your Own License (BYOL), this value must be set to @DEDICATED@. For more
    -- information, see
    -- <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Images>.
    tenancy :: Core.Maybe Tenancy,
    -- | The user name for the service account.
    customerUserName :: Core.Maybe Core.Text,
    -- | The state of the directory\'s registration with Amazon WorkSpaces. After
    -- a directory is deregistered, the @DEREGISTERED@ state is returned very
    -- briefly before the directory metadata is cleaned up, so this state is
    -- rarely returned. To confirm that a directory is deregistered, check for
    -- the directory ID by using
    -- <https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeWorkspaceDirectories.html DescribeWorkspaceDirectories>.
    -- If the directory ID isn\'t returned, then the directory has been
    -- successfully deregistered.
    state :: Core.Maybe WorkspaceDirectoryState,
    -- | The identifier of the IAM role. This is the role that allows Amazon
    -- WorkSpaces to make calls to other services, such as Amazon EC2, on your
    -- behalf.
    iamRoleId :: Core.Maybe Core.Text,
    -- | The directory identifier.
    directoryId :: Core.Maybe Core.Text,
    -- | The default self-service permissions for WorkSpaces in the directory.
    selfservicePermissions :: Core.Maybe SelfservicePermissions,
    -- | The directory type.
    directoryType :: Core.Maybe WorkspaceDirectoryType,
    -- | The name of the directory.
    directoryName :: Core.Maybe Core.Text,
    -- | The IP addresses of the DNS servers for the directory.
    dnsIpAddresses :: Core.Maybe [Core.Text],
    -- | The default creation properties for all WorkSpaces in the directory.
    workspaceCreationProperties :: Core.Maybe DefaultWorkspaceCreationProperties
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      workspaceSecurityGroupId = Core.Nothing,
      alias = Core.Nothing,
      ipGroupIds = Core.Nothing,
      workspaceAccessProperties = Core.Nothing,
      subnetIds = Core.Nothing,
      tenancy = Core.Nothing,
      customerUserName = Core.Nothing,
      state = Core.Nothing,
      iamRoleId = Core.Nothing,
      directoryId = Core.Nothing,
      selfservicePermissions = Core.Nothing,
      directoryType = Core.Nothing,
      directoryName = Core.Nothing,
      dnsIpAddresses = Core.Nothing,
      workspaceCreationProperties = Core.Nothing
    }

-- | The registration code for the directory. This is the code that users
-- enter in their Amazon WorkSpaces client application to connect to the
-- directory.
workspaceDirectory_registrationCode :: Lens.Lens' WorkspaceDirectory (Core.Maybe Core.Text)
workspaceDirectory_registrationCode = Lens.lens (\WorkspaceDirectory' {registrationCode} -> registrationCode) (\s@WorkspaceDirectory' {} a -> s {registrationCode = a} :: WorkspaceDirectory)

-- | The identifier of the security group that is assigned to new WorkSpaces.
workspaceDirectory_workspaceSecurityGroupId :: Lens.Lens' WorkspaceDirectory (Core.Maybe Core.Text)
workspaceDirectory_workspaceSecurityGroupId = Lens.lens (\WorkspaceDirectory' {workspaceSecurityGroupId} -> workspaceSecurityGroupId) (\s@WorkspaceDirectory' {} a -> s {workspaceSecurityGroupId = a} :: WorkspaceDirectory)

-- | The directory alias.
workspaceDirectory_alias :: Lens.Lens' WorkspaceDirectory (Core.Maybe Core.Text)
workspaceDirectory_alias = Lens.lens (\WorkspaceDirectory' {alias} -> alias) (\s@WorkspaceDirectory' {} a -> s {alias = a} :: WorkspaceDirectory)

-- | The identifiers of the IP access control groups associated with the
-- directory.
workspaceDirectory_ipGroupIds :: Lens.Lens' WorkspaceDirectory (Core.Maybe [Core.Text])
workspaceDirectory_ipGroupIds = Lens.lens (\WorkspaceDirectory' {ipGroupIds} -> ipGroupIds) (\s@WorkspaceDirectory' {} a -> s {ipGroupIds = a} :: WorkspaceDirectory) Core.. Lens.mapping Lens._Coerce

-- | The devices and operating systems that users can use to access
-- WorkSpaces.
workspaceDirectory_workspaceAccessProperties :: Lens.Lens' WorkspaceDirectory (Core.Maybe WorkspaceAccessProperties)
workspaceDirectory_workspaceAccessProperties = Lens.lens (\WorkspaceDirectory' {workspaceAccessProperties} -> workspaceAccessProperties) (\s@WorkspaceDirectory' {} a -> s {workspaceAccessProperties = a} :: WorkspaceDirectory)

-- | The identifiers of the subnets used with the directory.
workspaceDirectory_subnetIds :: Lens.Lens' WorkspaceDirectory (Core.Maybe [Core.Text])
workspaceDirectory_subnetIds = Lens.lens (\WorkspaceDirectory' {subnetIds} -> subnetIds) (\s@WorkspaceDirectory' {} a -> s {subnetIds = a} :: WorkspaceDirectory) Core.. Lens.mapping Lens._Coerce

-- | Specifies whether the directory is dedicated or shared. To use Bring
-- Your Own License (BYOL), this value must be set to @DEDICATED@. For more
-- information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Images>.
workspaceDirectory_tenancy :: Lens.Lens' WorkspaceDirectory (Core.Maybe Tenancy)
workspaceDirectory_tenancy = Lens.lens (\WorkspaceDirectory' {tenancy} -> tenancy) (\s@WorkspaceDirectory' {} a -> s {tenancy = a} :: WorkspaceDirectory)

-- | The user name for the service account.
workspaceDirectory_customerUserName :: Lens.Lens' WorkspaceDirectory (Core.Maybe Core.Text)
workspaceDirectory_customerUserName = Lens.lens (\WorkspaceDirectory' {customerUserName} -> customerUserName) (\s@WorkspaceDirectory' {} a -> s {customerUserName = a} :: WorkspaceDirectory)

-- | The state of the directory\'s registration with Amazon WorkSpaces. After
-- a directory is deregistered, the @DEREGISTERED@ state is returned very
-- briefly before the directory metadata is cleaned up, so this state is
-- rarely returned. To confirm that a directory is deregistered, check for
-- the directory ID by using
-- <https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeWorkspaceDirectories.html DescribeWorkspaceDirectories>.
-- If the directory ID isn\'t returned, then the directory has been
-- successfully deregistered.
workspaceDirectory_state :: Lens.Lens' WorkspaceDirectory (Core.Maybe WorkspaceDirectoryState)
workspaceDirectory_state = Lens.lens (\WorkspaceDirectory' {state} -> state) (\s@WorkspaceDirectory' {} a -> s {state = a} :: WorkspaceDirectory)

-- | The identifier of the IAM role. This is the role that allows Amazon
-- WorkSpaces to make calls to other services, such as Amazon EC2, on your
-- behalf.
workspaceDirectory_iamRoleId :: Lens.Lens' WorkspaceDirectory (Core.Maybe Core.Text)
workspaceDirectory_iamRoleId = Lens.lens (\WorkspaceDirectory' {iamRoleId} -> iamRoleId) (\s@WorkspaceDirectory' {} a -> s {iamRoleId = a} :: WorkspaceDirectory)

-- | The directory identifier.
workspaceDirectory_directoryId :: Lens.Lens' WorkspaceDirectory (Core.Maybe Core.Text)
workspaceDirectory_directoryId = Lens.lens (\WorkspaceDirectory' {directoryId} -> directoryId) (\s@WorkspaceDirectory' {} a -> s {directoryId = a} :: WorkspaceDirectory)

-- | The default self-service permissions for WorkSpaces in the directory.
workspaceDirectory_selfservicePermissions :: Lens.Lens' WorkspaceDirectory (Core.Maybe SelfservicePermissions)
workspaceDirectory_selfservicePermissions = Lens.lens (\WorkspaceDirectory' {selfservicePermissions} -> selfservicePermissions) (\s@WorkspaceDirectory' {} a -> s {selfservicePermissions = a} :: WorkspaceDirectory)

-- | The directory type.
workspaceDirectory_directoryType :: Lens.Lens' WorkspaceDirectory (Core.Maybe WorkspaceDirectoryType)
workspaceDirectory_directoryType = Lens.lens (\WorkspaceDirectory' {directoryType} -> directoryType) (\s@WorkspaceDirectory' {} a -> s {directoryType = a} :: WorkspaceDirectory)

-- | The name of the directory.
workspaceDirectory_directoryName :: Lens.Lens' WorkspaceDirectory (Core.Maybe Core.Text)
workspaceDirectory_directoryName = Lens.lens (\WorkspaceDirectory' {directoryName} -> directoryName) (\s@WorkspaceDirectory' {} a -> s {directoryName = a} :: WorkspaceDirectory)

-- | The IP addresses of the DNS servers for the directory.
workspaceDirectory_dnsIpAddresses :: Lens.Lens' WorkspaceDirectory (Core.Maybe [Core.Text])
workspaceDirectory_dnsIpAddresses = Lens.lens (\WorkspaceDirectory' {dnsIpAddresses} -> dnsIpAddresses) (\s@WorkspaceDirectory' {} a -> s {dnsIpAddresses = a} :: WorkspaceDirectory) Core.. Lens.mapping Lens._Coerce

-- | The default creation properties for all WorkSpaces in the directory.
workspaceDirectory_workspaceCreationProperties :: Lens.Lens' WorkspaceDirectory (Core.Maybe DefaultWorkspaceCreationProperties)
workspaceDirectory_workspaceCreationProperties = Lens.lens (\WorkspaceDirectory' {workspaceCreationProperties} -> workspaceCreationProperties) (\s@WorkspaceDirectory' {} a -> s {workspaceCreationProperties = a} :: WorkspaceDirectory)

instance Core.FromJSON WorkspaceDirectory where
  parseJSON =
    Core.withObject
      "WorkspaceDirectory"
      ( \x ->
          WorkspaceDirectory'
            Core.<$> (x Core..:? "RegistrationCode")
            Core.<*> (x Core..:? "WorkspaceSecurityGroupId")
            Core.<*> (x Core..:? "Alias")
            Core.<*> (x Core..:? "ipGroupIds" Core..!= Core.mempty)
            Core.<*> (x Core..:? "WorkspaceAccessProperties")
            Core.<*> (x Core..:? "SubnetIds" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Tenancy")
            Core.<*> (x Core..:? "CustomerUserName")
            Core.<*> (x Core..:? "State")
            Core.<*> (x Core..:? "IamRoleId")
            Core.<*> (x Core..:? "DirectoryId")
            Core.<*> (x Core..:? "SelfservicePermissions")
            Core.<*> (x Core..:? "DirectoryType")
            Core.<*> (x Core..:? "DirectoryName")
            Core.<*> (x Core..:? "DnsIpAddresses" Core..!= Core.mempty)
            Core.<*> (x Core..:? "WorkspaceCreationProperties")
      )

instance Core.Hashable WorkspaceDirectory

instance Core.NFData WorkspaceDirectory

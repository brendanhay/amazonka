{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceDirectory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.WorkspaceDirectory
  ( WorkspaceDirectory (..),

    -- * Smart constructor
    mkWorkspaceDirectory,

    -- * Lenses
    wdRegistrationCode,
    wdIAMRoleId,
    wdDirectoryId,
    wdState,
    wdCustomerUserName,
    wdSubnetIds,
    wdIpGroupIds,
    wdAlias,
    wdWorkspaceSecurityGroupId,
    wdDirectoryType,
    wdTenancy,
    wdWorkspaceCreationProperties,
    wdDNSIPAddresses,
    wdWorkspaceAccessProperties,
    wdDirectoryName,
    wdSelfservicePermissions,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkSpaces.Types.DefaultWorkspaceCreationProperties
import Network.AWS.WorkSpaces.Types.SelfservicePermissions
import Network.AWS.WorkSpaces.Types.Tenancy
import Network.AWS.WorkSpaces.Types.WorkspaceAccessProperties
import Network.AWS.WorkSpaces.Types.WorkspaceDirectoryState
import Network.AWS.WorkSpaces.Types.WorkspaceDirectoryType

-- | Describes a directory that is used with Amazon WorkSpaces.
--
-- /See:/ 'mkWorkspaceDirectory' smart constructor.
data WorkspaceDirectory = WorkspaceDirectory'
  { -- | The registration code for the directory. This is the code that users enter in their Amazon WorkSpaces client application to connect to the directory.
    registrationCode :: Lude.Maybe Lude.Text,
    -- | The identifier of the IAM role. This is the role that allows Amazon WorkSpaces to make calls to other services, such as Amazon EC2, on your behalf.
    iamRoleId :: Lude.Maybe Lude.Text,
    -- | The directory identifier.
    directoryId :: Lude.Maybe Lude.Text,
    -- | The state of the directory's registration with Amazon WorkSpaces. After a directory is deregistered, the @DEREGISTERED@ state is returned very briefly before the directory metadata is cleaned up, so this state is rarely returned. To confirm that a directory is deregistered, check for the directory ID by using <https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeWorkspaceDirectories.html DescribeWorkspaceDirectories> . If the directory ID isn't returned, then the directory has been successfully deregistered.
    state :: Lude.Maybe WorkspaceDirectoryState,
    -- | The user name for the service account.
    customerUserName :: Lude.Maybe Lude.Text,
    -- | The identifiers of the subnets used with the directory.
    subnetIds :: Lude.Maybe [Lude.Text],
    -- | The identifiers of the IP access control groups associated with the directory.
    ipGroupIds :: Lude.Maybe [Lude.Text],
    -- | The directory alias.
    alias :: Lude.Maybe Lude.Text,
    -- | The identifier of the security group that is assigned to new WorkSpaces.
    workspaceSecurityGroupId :: Lude.Maybe Lude.Text,
    -- | The directory type.
    directoryType :: Lude.Maybe WorkspaceDirectoryType,
    -- | Specifies whether the directory is dedicated or shared. To use Bring Your Own License (BYOL), this value must be set to @DEDICATED@ . For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Images> .
    tenancy :: Lude.Maybe Tenancy,
    -- | The default creation properties for all WorkSpaces in the directory.
    workspaceCreationProperties :: Lude.Maybe DefaultWorkspaceCreationProperties,
    -- | The IP addresses of the DNS servers for the directory.
    dnsIPAddresses :: Lude.Maybe [Lude.Text],
    -- | The devices and operating systems that users can use to access WorkSpaces.
    workspaceAccessProperties :: Lude.Maybe WorkspaceAccessProperties,
    -- | The name of the directory.
    directoryName :: Lude.Maybe Lude.Text,
    -- | The default self-service permissions for WorkSpaces in the directory.
    selfservicePermissions :: Lude.Maybe SelfservicePermissions
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WorkspaceDirectory' with the minimum fields required to make a request.
--
-- * 'registrationCode' - The registration code for the directory. This is the code that users enter in their Amazon WorkSpaces client application to connect to the directory.
-- * 'iamRoleId' - The identifier of the IAM role. This is the role that allows Amazon WorkSpaces to make calls to other services, such as Amazon EC2, on your behalf.
-- * 'directoryId' - The directory identifier.
-- * 'state' - The state of the directory's registration with Amazon WorkSpaces. After a directory is deregistered, the @DEREGISTERED@ state is returned very briefly before the directory metadata is cleaned up, so this state is rarely returned. To confirm that a directory is deregistered, check for the directory ID by using <https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeWorkspaceDirectories.html DescribeWorkspaceDirectories> . If the directory ID isn't returned, then the directory has been successfully deregistered.
-- * 'customerUserName' - The user name for the service account.
-- * 'subnetIds' - The identifiers of the subnets used with the directory.
-- * 'ipGroupIds' - The identifiers of the IP access control groups associated with the directory.
-- * 'alias' - The directory alias.
-- * 'workspaceSecurityGroupId' - The identifier of the security group that is assigned to new WorkSpaces.
-- * 'directoryType' - The directory type.
-- * 'tenancy' - Specifies whether the directory is dedicated or shared. To use Bring Your Own License (BYOL), this value must be set to @DEDICATED@ . For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Images> .
-- * 'workspaceCreationProperties' - The default creation properties for all WorkSpaces in the directory.
-- * 'dnsIPAddresses' - The IP addresses of the DNS servers for the directory.
-- * 'workspaceAccessProperties' - The devices and operating systems that users can use to access WorkSpaces.
-- * 'directoryName' - The name of the directory.
-- * 'selfservicePermissions' - The default self-service permissions for WorkSpaces in the directory.
mkWorkspaceDirectory ::
  WorkspaceDirectory
mkWorkspaceDirectory =
  WorkspaceDirectory'
    { registrationCode = Lude.Nothing,
      iamRoleId = Lude.Nothing,
      directoryId = Lude.Nothing,
      state = Lude.Nothing,
      customerUserName = Lude.Nothing,
      subnetIds = Lude.Nothing,
      ipGroupIds = Lude.Nothing,
      alias = Lude.Nothing,
      workspaceSecurityGroupId = Lude.Nothing,
      directoryType = Lude.Nothing,
      tenancy = Lude.Nothing,
      workspaceCreationProperties = Lude.Nothing,
      dnsIPAddresses = Lude.Nothing,
      workspaceAccessProperties = Lude.Nothing,
      directoryName = Lude.Nothing,
      selfservicePermissions = Lude.Nothing
    }

-- | The registration code for the directory. This is the code that users enter in their Amazon WorkSpaces client application to connect to the directory.
--
-- /Note:/ Consider using 'registrationCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdRegistrationCode :: Lens.Lens' WorkspaceDirectory (Lude.Maybe Lude.Text)
wdRegistrationCode = Lens.lens (registrationCode :: WorkspaceDirectory -> Lude.Maybe Lude.Text) (\s a -> s {registrationCode = a} :: WorkspaceDirectory)
{-# DEPRECATED wdRegistrationCode "Use generic-lens or generic-optics with 'registrationCode' instead." #-}

-- | The identifier of the IAM role. This is the role that allows Amazon WorkSpaces to make calls to other services, such as Amazon EC2, on your behalf.
--
-- /Note:/ Consider using 'iamRoleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdIAMRoleId :: Lens.Lens' WorkspaceDirectory (Lude.Maybe Lude.Text)
wdIAMRoleId = Lens.lens (iamRoleId :: WorkspaceDirectory -> Lude.Maybe Lude.Text) (\s a -> s {iamRoleId = a} :: WorkspaceDirectory)
{-# DEPRECATED wdIAMRoleId "Use generic-lens or generic-optics with 'iamRoleId' instead." #-}

-- | The directory identifier.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdDirectoryId :: Lens.Lens' WorkspaceDirectory (Lude.Maybe Lude.Text)
wdDirectoryId = Lens.lens (directoryId :: WorkspaceDirectory -> Lude.Maybe Lude.Text) (\s a -> s {directoryId = a} :: WorkspaceDirectory)
{-# DEPRECATED wdDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The state of the directory's registration with Amazon WorkSpaces. After a directory is deregistered, the @DEREGISTERED@ state is returned very briefly before the directory metadata is cleaned up, so this state is rarely returned. To confirm that a directory is deregistered, check for the directory ID by using <https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeWorkspaceDirectories.html DescribeWorkspaceDirectories> . If the directory ID isn't returned, then the directory has been successfully deregistered.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdState :: Lens.Lens' WorkspaceDirectory (Lude.Maybe WorkspaceDirectoryState)
wdState = Lens.lens (state :: WorkspaceDirectory -> Lude.Maybe WorkspaceDirectoryState) (\s a -> s {state = a} :: WorkspaceDirectory)
{-# DEPRECATED wdState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The user name for the service account.
--
-- /Note:/ Consider using 'customerUserName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdCustomerUserName :: Lens.Lens' WorkspaceDirectory (Lude.Maybe Lude.Text)
wdCustomerUserName = Lens.lens (customerUserName :: WorkspaceDirectory -> Lude.Maybe Lude.Text) (\s a -> s {customerUserName = a} :: WorkspaceDirectory)
{-# DEPRECATED wdCustomerUserName "Use generic-lens or generic-optics with 'customerUserName' instead." #-}

-- | The identifiers of the subnets used with the directory.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdSubnetIds :: Lens.Lens' WorkspaceDirectory (Lude.Maybe [Lude.Text])
wdSubnetIds = Lens.lens (subnetIds :: WorkspaceDirectory -> Lude.Maybe [Lude.Text]) (\s a -> s {subnetIds = a} :: WorkspaceDirectory)
{-# DEPRECATED wdSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | The identifiers of the IP access control groups associated with the directory.
--
-- /Note:/ Consider using 'ipGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdIpGroupIds :: Lens.Lens' WorkspaceDirectory (Lude.Maybe [Lude.Text])
wdIpGroupIds = Lens.lens (ipGroupIds :: WorkspaceDirectory -> Lude.Maybe [Lude.Text]) (\s a -> s {ipGroupIds = a} :: WorkspaceDirectory)
{-# DEPRECATED wdIpGroupIds "Use generic-lens or generic-optics with 'ipGroupIds' instead." #-}

-- | The directory alias.
--
-- /Note:/ Consider using 'alias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdAlias :: Lens.Lens' WorkspaceDirectory (Lude.Maybe Lude.Text)
wdAlias = Lens.lens (alias :: WorkspaceDirectory -> Lude.Maybe Lude.Text) (\s a -> s {alias = a} :: WorkspaceDirectory)
{-# DEPRECATED wdAlias "Use generic-lens or generic-optics with 'alias' instead." #-}

-- | The identifier of the security group that is assigned to new WorkSpaces.
--
-- /Note:/ Consider using 'workspaceSecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdWorkspaceSecurityGroupId :: Lens.Lens' WorkspaceDirectory (Lude.Maybe Lude.Text)
wdWorkspaceSecurityGroupId = Lens.lens (workspaceSecurityGroupId :: WorkspaceDirectory -> Lude.Maybe Lude.Text) (\s a -> s {workspaceSecurityGroupId = a} :: WorkspaceDirectory)
{-# DEPRECATED wdWorkspaceSecurityGroupId "Use generic-lens or generic-optics with 'workspaceSecurityGroupId' instead." #-}

-- | The directory type.
--
-- /Note:/ Consider using 'directoryType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdDirectoryType :: Lens.Lens' WorkspaceDirectory (Lude.Maybe WorkspaceDirectoryType)
wdDirectoryType = Lens.lens (directoryType :: WorkspaceDirectory -> Lude.Maybe WorkspaceDirectoryType) (\s a -> s {directoryType = a} :: WorkspaceDirectory)
{-# DEPRECATED wdDirectoryType "Use generic-lens or generic-optics with 'directoryType' instead." #-}

-- | Specifies whether the directory is dedicated or shared. To use Bring Your Own License (BYOL), this value must be set to @DEDICATED@ . For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Images> .
--
-- /Note:/ Consider using 'tenancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdTenancy :: Lens.Lens' WorkspaceDirectory (Lude.Maybe Tenancy)
wdTenancy = Lens.lens (tenancy :: WorkspaceDirectory -> Lude.Maybe Tenancy) (\s a -> s {tenancy = a} :: WorkspaceDirectory)
{-# DEPRECATED wdTenancy "Use generic-lens or generic-optics with 'tenancy' instead." #-}

-- | The default creation properties for all WorkSpaces in the directory.
--
-- /Note:/ Consider using 'workspaceCreationProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdWorkspaceCreationProperties :: Lens.Lens' WorkspaceDirectory (Lude.Maybe DefaultWorkspaceCreationProperties)
wdWorkspaceCreationProperties = Lens.lens (workspaceCreationProperties :: WorkspaceDirectory -> Lude.Maybe DefaultWorkspaceCreationProperties) (\s a -> s {workspaceCreationProperties = a} :: WorkspaceDirectory)
{-# DEPRECATED wdWorkspaceCreationProperties "Use generic-lens or generic-optics with 'workspaceCreationProperties' instead." #-}

-- | The IP addresses of the DNS servers for the directory.
--
-- /Note:/ Consider using 'dnsIPAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdDNSIPAddresses :: Lens.Lens' WorkspaceDirectory (Lude.Maybe [Lude.Text])
wdDNSIPAddresses = Lens.lens (dnsIPAddresses :: WorkspaceDirectory -> Lude.Maybe [Lude.Text]) (\s a -> s {dnsIPAddresses = a} :: WorkspaceDirectory)
{-# DEPRECATED wdDNSIPAddresses "Use generic-lens or generic-optics with 'dnsIPAddresses' instead." #-}

-- | The devices and operating systems that users can use to access WorkSpaces.
--
-- /Note:/ Consider using 'workspaceAccessProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdWorkspaceAccessProperties :: Lens.Lens' WorkspaceDirectory (Lude.Maybe WorkspaceAccessProperties)
wdWorkspaceAccessProperties = Lens.lens (workspaceAccessProperties :: WorkspaceDirectory -> Lude.Maybe WorkspaceAccessProperties) (\s a -> s {workspaceAccessProperties = a} :: WorkspaceDirectory)
{-# DEPRECATED wdWorkspaceAccessProperties "Use generic-lens or generic-optics with 'workspaceAccessProperties' instead." #-}

-- | The name of the directory.
--
-- /Note:/ Consider using 'directoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdDirectoryName :: Lens.Lens' WorkspaceDirectory (Lude.Maybe Lude.Text)
wdDirectoryName = Lens.lens (directoryName :: WorkspaceDirectory -> Lude.Maybe Lude.Text) (\s a -> s {directoryName = a} :: WorkspaceDirectory)
{-# DEPRECATED wdDirectoryName "Use generic-lens or generic-optics with 'directoryName' instead." #-}

-- | The default self-service permissions for WorkSpaces in the directory.
--
-- /Note:/ Consider using 'selfservicePermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdSelfservicePermissions :: Lens.Lens' WorkspaceDirectory (Lude.Maybe SelfservicePermissions)
wdSelfservicePermissions = Lens.lens (selfservicePermissions :: WorkspaceDirectory -> Lude.Maybe SelfservicePermissions) (\s a -> s {selfservicePermissions = a} :: WorkspaceDirectory)
{-# DEPRECATED wdSelfservicePermissions "Use generic-lens or generic-optics with 'selfservicePermissions' instead." #-}

instance Lude.FromJSON WorkspaceDirectory where
  parseJSON =
    Lude.withObject
      "WorkspaceDirectory"
      ( \x ->
          WorkspaceDirectory'
            Lude.<$> (x Lude..:? "RegistrationCode")
            Lude.<*> (x Lude..:? "IamRoleId")
            Lude.<*> (x Lude..:? "DirectoryId")
            Lude.<*> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "CustomerUserName")
            Lude.<*> (x Lude..:? "SubnetIds" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ipGroupIds" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Alias")
            Lude.<*> (x Lude..:? "WorkspaceSecurityGroupId")
            Lude.<*> (x Lude..:? "DirectoryType")
            Lude.<*> (x Lude..:? "Tenancy")
            Lude.<*> (x Lude..:? "WorkspaceCreationProperties")
            Lude.<*> (x Lude..:? "DnsIpAddresses" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "WorkspaceAccessProperties")
            Lude.<*> (x Lude..:? "DirectoryName")
            Lude.<*> (x Lude..:? "SelfservicePermissions")
      )

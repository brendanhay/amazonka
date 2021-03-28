{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceDirectory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.WorkspaceDirectory
  ( WorkspaceDirectory (..)
  -- * Smart constructor
  , mkWorkspaceDirectory
  -- * Lenses
  , wdAlias
  , wdCustomerUserName
  , wdDirectoryId
  , wdDirectoryName
  , wdDirectoryType
  , wdDnsIpAddresses
  , wdIamRoleId
  , wdRegistrationCode
  , wdSelfservicePermissions
  , wdState
  , wdSubnetIds
  , wdTenancy
  , wdWorkspaceAccessProperties
  , wdWorkspaceCreationProperties
  , wdWorkspaceSecurityGroupId
  , wdIpGroupIds
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkSpaces.Types.Alias as Types
import qualified Network.AWS.WorkSpaces.Types.CustomerUserName as Types
import qualified Network.AWS.WorkSpaces.Types.DefaultWorkspaceCreationProperties as Types
import qualified Network.AWS.WorkSpaces.Types.DirectoryId as Types
import qualified Network.AWS.WorkSpaces.Types.DirectoryName as Types
import qualified Network.AWS.WorkSpaces.Types.IamRoleId as Types
import qualified Network.AWS.WorkSpaces.Types.IpAddress as Types
import qualified Network.AWS.WorkSpaces.Types.IpGroupId as Types
import qualified Network.AWS.WorkSpaces.Types.RegistrationCode as Types
import qualified Network.AWS.WorkSpaces.Types.SelfservicePermissions as Types
import qualified Network.AWS.WorkSpaces.Types.SubnetId as Types
import qualified Network.AWS.WorkSpaces.Types.Tenancy as Types
import qualified Network.AWS.WorkSpaces.Types.WorkspaceAccessProperties as Types
import qualified Network.AWS.WorkSpaces.Types.WorkspaceDirectoryState as Types
import qualified Network.AWS.WorkSpaces.Types.WorkspaceDirectoryType as Types
import qualified Network.AWS.WorkSpaces.Types.WorkspaceSecurityGroupId as Types

-- | Describes a directory that is used with Amazon WorkSpaces.
--
-- /See:/ 'mkWorkspaceDirectory' smart constructor.
data WorkspaceDirectory = WorkspaceDirectory'
  { alias :: Core.Maybe Types.Alias
    -- ^ The directory alias.
  , customerUserName :: Core.Maybe Types.CustomerUserName
    -- ^ The user name for the service account.
  , directoryId :: Core.Maybe Types.DirectoryId
    -- ^ The directory identifier.
  , directoryName :: Core.Maybe Types.DirectoryName
    -- ^ The name of the directory.
  , directoryType :: Core.Maybe Types.WorkspaceDirectoryType
    -- ^ The directory type.
  , dnsIpAddresses :: Core.Maybe [Types.IpAddress]
    -- ^ The IP addresses of the DNS servers for the directory.
  , iamRoleId :: Core.Maybe Types.IamRoleId
    -- ^ The identifier of the IAM role. This is the role that allows Amazon WorkSpaces to make calls to other services, such as Amazon EC2, on your behalf.
  , registrationCode :: Core.Maybe Types.RegistrationCode
    -- ^ The registration code for the directory. This is the code that users enter in their Amazon WorkSpaces client application to connect to the directory.
  , selfservicePermissions :: Core.Maybe Types.SelfservicePermissions
    -- ^ The default self-service permissions for WorkSpaces in the directory.
  , state :: Core.Maybe Types.WorkspaceDirectoryState
    -- ^ The state of the directory's registration with Amazon WorkSpaces. After a directory is deregistered, the @DEREGISTERED@ state is returned very briefly before the directory metadata is cleaned up, so this state is rarely returned. To confirm that a directory is deregistered, check for the directory ID by using <https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeWorkspaceDirectories.html DescribeWorkspaceDirectories> . If the directory ID isn't returned, then the directory has been successfully deregistered.
  , subnetIds :: Core.Maybe [Types.SubnetId]
    -- ^ The identifiers of the subnets used with the directory.
  , tenancy :: Core.Maybe Types.Tenancy
    -- ^ Specifies whether the directory is dedicated or shared. To use Bring Your Own License (BYOL), this value must be set to @DEDICATED@ . For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Images> .
  , workspaceAccessProperties :: Core.Maybe Types.WorkspaceAccessProperties
    -- ^ The devices and operating systems that users can use to access WorkSpaces.
  , workspaceCreationProperties :: Core.Maybe Types.DefaultWorkspaceCreationProperties
    -- ^ The default creation properties for all WorkSpaces in the directory.
  , workspaceSecurityGroupId :: Core.Maybe Types.WorkspaceSecurityGroupId
    -- ^ The identifier of the security group that is assigned to new WorkSpaces.
  , ipGroupIds :: Core.Maybe [Types.IpGroupId]
    -- ^ The identifiers of the IP access control groups associated with the directory.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WorkspaceDirectory' value with any optional fields omitted.
mkWorkspaceDirectory
    :: WorkspaceDirectory
mkWorkspaceDirectory
  = WorkspaceDirectory'{alias = Core.Nothing,
                        customerUserName = Core.Nothing, directoryId = Core.Nothing,
                        directoryName = Core.Nothing, directoryType = Core.Nothing,
                        dnsIpAddresses = Core.Nothing, iamRoleId = Core.Nothing,
                        registrationCode = Core.Nothing,
                        selfservicePermissions = Core.Nothing, state = Core.Nothing,
                        subnetIds = Core.Nothing, tenancy = Core.Nothing,
                        workspaceAccessProperties = Core.Nothing,
                        workspaceCreationProperties = Core.Nothing,
                        workspaceSecurityGroupId = Core.Nothing, ipGroupIds = Core.Nothing}

-- | The directory alias.
--
-- /Note:/ Consider using 'alias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdAlias :: Lens.Lens' WorkspaceDirectory (Core.Maybe Types.Alias)
wdAlias = Lens.field @"alias"
{-# INLINEABLE wdAlias #-}
{-# DEPRECATED alias "Use generic-lens or generic-optics with 'alias' instead"  #-}

-- | The user name for the service account.
--
-- /Note:/ Consider using 'customerUserName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdCustomerUserName :: Lens.Lens' WorkspaceDirectory (Core.Maybe Types.CustomerUserName)
wdCustomerUserName = Lens.field @"customerUserName"
{-# INLINEABLE wdCustomerUserName #-}
{-# DEPRECATED customerUserName "Use generic-lens or generic-optics with 'customerUserName' instead"  #-}

-- | The directory identifier.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdDirectoryId :: Lens.Lens' WorkspaceDirectory (Core.Maybe Types.DirectoryId)
wdDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE wdDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | The name of the directory.
--
-- /Note:/ Consider using 'directoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdDirectoryName :: Lens.Lens' WorkspaceDirectory (Core.Maybe Types.DirectoryName)
wdDirectoryName = Lens.field @"directoryName"
{-# INLINEABLE wdDirectoryName #-}
{-# DEPRECATED directoryName "Use generic-lens or generic-optics with 'directoryName' instead"  #-}

-- | The directory type.
--
-- /Note:/ Consider using 'directoryType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdDirectoryType :: Lens.Lens' WorkspaceDirectory (Core.Maybe Types.WorkspaceDirectoryType)
wdDirectoryType = Lens.field @"directoryType"
{-# INLINEABLE wdDirectoryType #-}
{-# DEPRECATED directoryType "Use generic-lens or generic-optics with 'directoryType' instead"  #-}

-- | The IP addresses of the DNS servers for the directory.
--
-- /Note:/ Consider using 'dnsIpAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdDnsIpAddresses :: Lens.Lens' WorkspaceDirectory (Core.Maybe [Types.IpAddress])
wdDnsIpAddresses = Lens.field @"dnsIpAddresses"
{-# INLINEABLE wdDnsIpAddresses #-}
{-# DEPRECATED dnsIpAddresses "Use generic-lens or generic-optics with 'dnsIpAddresses' instead"  #-}

-- | The identifier of the IAM role. This is the role that allows Amazon WorkSpaces to make calls to other services, such as Amazon EC2, on your behalf.
--
-- /Note:/ Consider using 'iamRoleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdIamRoleId :: Lens.Lens' WorkspaceDirectory (Core.Maybe Types.IamRoleId)
wdIamRoleId = Lens.field @"iamRoleId"
{-# INLINEABLE wdIamRoleId #-}
{-# DEPRECATED iamRoleId "Use generic-lens or generic-optics with 'iamRoleId' instead"  #-}

-- | The registration code for the directory. This is the code that users enter in their Amazon WorkSpaces client application to connect to the directory.
--
-- /Note:/ Consider using 'registrationCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdRegistrationCode :: Lens.Lens' WorkspaceDirectory (Core.Maybe Types.RegistrationCode)
wdRegistrationCode = Lens.field @"registrationCode"
{-# INLINEABLE wdRegistrationCode #-}
{-# DEPRECATED registrationCode "Use generic-lens or generic-optics with 'registrationCode' instead"  #-}

-- | The default self-service permissions for WorkSpaces in the directory.
--
-- /Note:/ Consider using 'selfservicePermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdSelfservicePermissions :: Lens.Lens' WorkspaceDirectory (Core.Maybe Types.SelfservicePermissions)
wdSelfservicePermissions = Lens.field @"selfservicePermissions"
{-# INLINEABLE wdSelfservicePermissions #-}
{-# DEPRECATED selfservicePermissions "Use generic-lens or generic-optics with 'selfservicePermissions' instead"  #-}

-- | The state of the directory's registration with Amazon WorkSpaces. After a directory is deregistered, the @DEREGISTERED@ state is returned very briefly before the directory metadata is cleaned up, so this state is rarely returned. To confirm that a directory is deregistered, check for the directory ID by using <https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeWorkspaceDirectories.html DescribeWorkspaceDirectories> . If the directory ID isn't returned, then the directory has been successfully deregistered.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdState :: Lens.Lens' WorkspaceDirectory (Core.Maybe Types.WorkspaceDirectoryState)
wdState = Lens.field @"state"
{-# INLINEABLE wdState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The identifiers of the subnets used with the directory.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdSubnetIds :: Lens.Lens' WorkspaceDirectory (Core.Maybe [Types.SubnetId])
wdSubnetIds = Lens.field @"subnetIds"
{-# INLINEABLE wdSubnetIds #-}
{-# DEPRECATED subnetIds "Use generic-lens or generic-optics with 'subnetIds' instead"  #-}

-- | Specifies whether the directory is dedicated or shared. To use Bring Your Own License (BYOL), this value must be set to @DEDICATED@ . For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Images> .
--
-- /Note:/ Consider using 'tenancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdTenancy :: Lens.Lens' WorkspaceDirectory (Core.Maybe Types.Tenancy)
wdTenancy = Lens.field @"tenancy"
{-# INLINEABLE wdTenancy #-}
{-# DEPRECATED tenancy "Use generic-lens or generic-optics with 'tenancy' instead"  #-}

-- | The devices and operating systems that users can use to access WorkSpaces.
--
-- /Note:/ Consider using 'workspaceAccessProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdWorkspaceAccessProperties :: Lens.Lens' WorkspaceDirectory (Core.Maybe Types.WorkspaceAccessProperties)
wdWorkspaceAccessProperties = Lens.field @"workspaceAccessProperties"
{-# INLINEABLE wdWorkspaceAccessProperties #-}
{-# DEPRECATED workspaceAccessProperties "Use generic-lens or generic-optics with 'workspaceAccessProperties' instead"  #-}

-- | The default creation properties for all WorkSpaces in the directory.
--
-- /Note:/ Consider using 'workspaceCreationProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdWorkspaceCreationProperties :: Lens.Lens' WorkspaceDirectory (Core.Maybe Types.DefaultWorkspaceCreationProperties)
wdWorkspaceCreationProperties = Lens.field @"workspaceCreationProperties"
{-# INLINEABLE wdWorkspaceCreationProperties #-}
{-# DEPRECATED workspaceCreationProperties "Use generic-lens or generic-optics with 'workspaceCreationProperties' instead"  #-}

-- | The identifier of the security group that is assigned to new WorkSpaces.
--
-- /Note:/ Consider using 'workspaceSecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdWorkspaceSecurityGroupId :: Lens.Lens' WorkspaceDirectory (Core.Maybe Types.WorkspaceSecurityGroupId)
wdWorkspaceSecurityGroupId = Lens.field @"workspaceSecurityGroupId"
{-# INLINEABLE wdWorkspaceSecurityGroupId #-}
{-# DEPRECATED workspaceSecurityGroupId "Use generic-lens or generic-optics with 'workspaceSecurityGroupId' instead"  #-}

-- | The identifiers of the IP access control groups associated with the directory.
--
-- /Note:/ Consider using 'ipGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdIpGroupIds :: Lens.Lens' WorkspaceDirectory (Core.Maybe [Types.IpGroupId])
wdIpGroupIds = Lens.field @"ipGroupIds"
{-# INLINEABLE wdIpGroupIds #-}
{-# DEPRECATED ipGroupIds "Use generic-lens or generic-optics with 'ipGroupIds' instead"  #-}

instance Core.FromJSON WorkspaceDirectory where
        parseJSON
          = Core.withObject "WorkspaceDirectory" Core.$
              \ x ->
                WorkspaceDirectory' Core.<$>
                  (x Core..:? "Alias") Core.<*> x Core..:? "CustomerUserName"
                    Core.<*> x Core..:? "DirectoryId"
                    Core.<*> x Core..:? "DirectoryName"
                    Core.<*> x Core..:? "DirectoryType"
                    Core.<*> x Core..:? "DnsIpAddresses"
                    Core.<*> x Core..:? "IamRoleId"
                    Core.<*> x Core..:? "RegistrationCode"
                    Core.<*> x Core..:? "SelfservicePermissions"
                    Core.<*> x Core..:? "State"
                    Core.<*> x Core..:? "SubnetIds"
                    Core.<*> x Core..:? "Tenancy"
                    Core.<*> x Core..:? "WorkspaceAccessProperties"
                    Core.<*> x Core..:? "WorkspaceCreationProperties"
                    Core.<*> x Core..:? "WorkspaceSecurityGroupId"
                    Core.<*> x Core..:? "ipGroupIds"

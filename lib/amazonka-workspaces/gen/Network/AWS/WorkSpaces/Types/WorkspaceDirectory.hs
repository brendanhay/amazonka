{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceDirectory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.WorkspaceDirectory where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WorkSpaces.Types.DefaultWorkspaceCreationProperties
import Network.AWS.WorkSpaces.Types.SelfservicePermissions
import Network.AWS.WorkSpaces.Types.Tenancy
import Network.AWS.WorkSpaces.Types.WorkspaceAccessProperties
import Network.AWS.WorkSpaces.Types.WorkspaceDirectoryState
import Network.AWS.WorkSpaces.Types.WorkspaceDirectoryType

-- | Describes a directory that is used with Amazon WorkSpaces.
--
--
--
-- /See:/ 'workspaceDirectory' smart constructor.
data WorkspaceDirectory = WorkspaceDirectory'
  { _wdRegistrationCode ::
      !(Maybe Text),
    _wdIAMRoleId :: !(Maybe Text),
    _wdDirectoryId :: !(Maybe Text),
    _wdState :: !(Maybe WorkspaceDirectoryState),
    _wdCustomerUserName :: !(Maybe Text),
    _wdSubnetIds :: !(Maybe [Text]),
    _wdIpGroupIds :: !(Maybe [Text]),
    _wdAlias :: !(Maybe Text),
    _wdWorkspaceSecurityGroupId :: !(Maybe Text),
    _wdDirectoryType :: !(Maybe WorkspaceDirectoryType),
    _wdTenancy :: !(Maybe Tenancy),
    _wdWorkspaceCreationProperties ::
      !(Maybe DefaultWorkspaceCreationProperties),
    _wdDNSIPAddresses :: !(Maybe [Text]),
    _wdWorkspaceAccessProperties ::
      !(Maybe WorkspaceAccessProperties),
    _wdDirectoryName :: !(Maybe Text),
    _wdSelfservicePermissions ::
      !(Maybe SelfservicePermissions)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WorkspaceDirectory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wdRegistrationCode' - The registration code for the directory. This is the code that users enter in their Amazon WorkSpaces client application to connect to the directory.
--
-- * 'wdIAMRoleId' - The identifier of the IAM role. This is the role that allows Amazon WorkSpaces to make calls to other services, such as Amazon EC2, on your behalf.
--
-- * 'wdDirectoryId' - The directory identifier.
--
-- * 'wdState' - The state of the directory's registration with Amazon WorkSpaces. After a directory is deregistered, the @DEREGISTERED@ state is returned very briefly before the directory metadata is cleaned up, so this state is rarely returned. To confirm that a directory is deregistered, check for the directory ID by using <https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeWorkspaceDirectories.html DescribeWorkspaceDirectories> . If the directory ID isn't returned, then the directory has been successfully deregistered.
--
-- * 'wdCustomerUserName' - The user name for the service account.
--
-- * 'wdSubnetIds' - The identifiers of the subnets used with the directory.
--
-- * 'wdIpGroupIds' - The identifiers of the IP access control groups associated with the directory.
--
-- * 'wdAlias' - The directory alias.
--
-- * 'wdWorkspaceSecurityGroupId' - The identifier of the security group that is assigned to new WorkSpaces.
--
-- * 'wdDirectoryType' - The directory type.
--
-- * 'wdTenancy' - Specifies whether the directory is dedicated or shared. To use Bring Your Own License (BYOL), this value must be set to @DEDICATED@ . For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Images> .
--
-- * 'wdWorkspaceCreationProperties' - The default creation properties for all WorkSpaces in the directory.
--
-- * 'wdDNSIPAddresses' - The IP addresses of the DNS servers for the directory.
--
-- * 'wdWorkspaceAccessProperties' - The devices and operating systems that users can use to access WorkSpaces.
--
-- * 'wdDirectoryName' - The name of the directory.
--
-- * 'wdSelfservicePermissions' - The default self-service permissions for WorkSpaces in the directory.
workspaceDirectory ::
  WorkspaceDirectory
workspaceDirectory =
  WorkspaceDirectory'
    { _wdRegistrationCode = Nothing,
      _wdIAMRoleId = Nothing,
      _wdDirectoryId = Nothing,
      _wdState = Nothing,
      _wdCustomerUserName = Nothing,
      _wdSubnetIds = Nothing,
      _wdIpGroupIds = Nothing,
      _wdAlias = Nothing,
      _wdWorkspaceSecurityGroupId = Nothing,
      _wdDirectoryType = Nothing,
      _wdTenancy = Nothing,
      _wdWorkspaceCreationProperties = Nothing,
      _wdDNSIPAddresses = Nothing,
      _wdWorkspaceAccessProperties = Nothing,
      _wdDirectoryName = Nothing,
      _wdSelfservicePermissions = Nothing
    }

-- | The registration code for the directory. This is the code that users enter in their Amazon WorkSpaces client application to connect to the directory.
wdRegistrationCode :: Lens' WorkspaceDirectory (Maybe Text)
wdRegistrationCode = lens _wdRegistrationCode (\s a -> s {_wdRegistrationCode = a})

-- | The identifier of the IAM role. This is the role that allows Amazon WorkSpaces to make calls to other services, such as Amazon EC2, on your behalf.
wdIAMRoleId :: Lens' WorkspaceDirectory (Maybe Text)
wdIAMRoleId = lens _wdIAMRoleId (\s a -> s {_wdIAMRoleId = a})

-- | The directory identifier.
wdDirectoryId :: Lens' WorkspaceDirectory (Maybe Text)
wdDirectoryId = lens _wdDirectoryId (\s a -> s {_wdDirectoryId = a})

-- | The state of the directory's registration with Amazon WorkSpaces. After a directory is deregistered, the @DEREGISTERED@ state is returned very briefly before the directory metadata is cleaned up, so this state is rarely returned. To confirm that a directory is deregistered, check for the directory ID by using <https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeWorkspaceDirectories.html DescribeWorkspaceDirectories> . If the directory ID isn't returned, then the directory has been successfully deregistered.
wdState :: Lens' WorkspaceDirectory (Maybe WorkspaceDirectoryState)
wdState = lens _wdState (\s a -> s {_wdState = a})

-- | The user name for the service account.
wdCustomerUserName :: Lens' WorkspaceDirectory (Maybe Text)
wdCustomerUserName = lens _wdCustomerUserName (\s a -> s {_wdCustomerUserName = a})

-- | The identifiers of the subnets used with the directory.
wdSubnetIds :: Lens' WorkspaceDirectory [Text]
wdSubnetIds = lens _wdSubnetIds (\s a -> s {_wdSubnetIds = a}) . _Default . _Coerce

-- | The identifiers of the IP access control groups associated with the directory.
wdIpGroupIds :: Lens' WorkspaceDirectory [Text]
wdIpGroupIds = lens _wdIpGroupIds (\s a -> s {_wdIpGroupIds = a}) . _Default . _Coerce

-- | The directory alias.
wdAlias :: Lens' WorkspaceDirectory (Maybe Text)
wdAlias = lens _wdAlias (\s a -> s {_wdAlias = a})

-- | The identifier of the security group that is assigned to new WorkSpaces.
wdWorkspaceSecurityGroupId :: Lens' WorkspaceDirectory (Maybe Text)
wdWorkspaceSecurityGroupId = lens _wdWorkspaceSecurityGroupId (\s a -> s {_wdWorkspaceSecurityGroupId = a})

-- | The directory type.
wdDirectoryType :: Lens' WorkspaceDirectory (Maybe WorkspaceDirectoryType)
wdDirectoryType = lens _wdDirectoryType (\s a -> s {_wdDirectoryType = a})

-- | Specifies whether the directory is dedicated or shared. To use Bring Your Own License (BYOL), this value must be set to @DEDICATED@ . For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/byol-windows-images.html Bring Your Own Windows Desktop Images> .
wdTenancy :: Lens' WorkspaceDirectory (Maybe Tenancy)
wdTenancy = lens _wdTenancy (\s a -> s {_wdTenancy = a})

-- | The default creation properties for all WorkSpaces in the directory.
wdWorkspaceCreationProperties :: Lens' WorkspaceDirectory (Maybe DefaultWorkspaceCreationProperties)
wdWorkspaceCreationProperties = lens _wdWorkspaceCreationProperties (\s a -> s {_wdWorkspaceCreationProperties = a})

-- | The IP addresses of the DNS servers for the directory.
wdDNSIPAddresses :: Lens' WorkspaceDirectory [Text]
wdDNSIPAddresses = lens _wdDNSIPAddresses (\s a -> s {_wdDNSIPAddresses = a}) . _Default . _Coerce

-- | The devices and operating systems that users can use to access WorkSpaces.
wdWorkspaceAccessProperties :: Lens' WorkspaceDirectory (Maybe WorkspaceAccessProperties)
wdWorkspaceAccessProperties = lens _wdWorkspaceAccessProperties (\s a -> s {_wdWorkspaceAccessProperties = a})

-- | The name of the directory.
wdDirectoryName :: Lens' WorkspaceDirectory (Maybe Text)
wdDirectoryName = lens _wdDirectoryName (\s a -> s {_wdDirectoryName = a})

-- | The default self-service permissions for WorkSpaces in the directory.
wdSelfservicePermissions :: Lens' WorkspaceDirectory (Maybe SelfservicePermissions)
wdSelfservicePermissions = lens _wdSelfservicePermissions (\s a -> s {_wdSelfservicePermissions = a})

instance FromJSON WorkspaceDirectory where
  parseJSON =
    withObject
      "WorkspaceDirectory"
      ( \x ->
          WorkspaceDirectory'
            <$> (x .:? "RegistrationCode")
            <*> (x .:? "IamRoleId")
            <*> (x .:? "DirectoryId")
            <*> (x .:? "State")
            <*> (x .:? "CustomerUserName")
            <*> (x .:? "SubnetIds" .!= mempty)
            <*> (x .:? "ipGroupIds" .!= mempty)
            <*> (x .:? "Alias")
            <*> (x .:? "WorkspaceSecurityGroupId")
            <*> (x .:? "DirectoryType")
            <*> (x .:? "Tenancy")
            <*> (x .:? "WorkspaceCreationProperties")
            <*> (x .:? "DnsIpAddresses" .!= mempty)
            <*> (x .:? "WorkspaceAccessProperties")
            <*> (x .:? "DirectoryName")
            <*> (x .:? "SelfservicePermissions")
      )

instance Hashable WorkspaceDirectory

instance NFData WorkspaceDirectory

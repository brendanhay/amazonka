{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.Product
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.Product where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.WorkSpaces.Types.Sum

-- | Contains information about the compute type of a WorkSpace bundle.
--
-- /See:/ 'computeType' smart constructor.
newtype ComputeType = ComputeType'
    { _ctName :: Maybe Compute
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ComputeType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctName'
computeType
    :: ComputeType
computeType =
    ComputeType'
    { _ctName = Nothing
    }

-- | The name of the compute type for the bundle.
ctName :: Lens' ComputeType (Maybe Compute)
ctName = lens _ctName (\ s a -> s{_ctName = a});

instance FromJSON ComputeType where
        parseJSON
          = withObject "ComputeType"
              (\ x -> ComputeType' <$> (x .:? "Name"))

instance Hashable ComputeType

instance NFData ComputeType

-- | Contains default WorkSpace creation information.
--
-- /See:/ 'defaultWorkspaceCreationProperties' smart constructor.
data DefaultWorkspaceCreationProperties = DefaultWorkspaceCreationProperties'
    { _dwcpCustomSecurityGroupId           :: !(Maybe Text)
    , _dwcpUserEnabledAsLocalAdministrator :: !(Maybe Bool)
    , _dwcpEnableWorkDocs                  :: !(Maybe Bool)
    , _dwcpEnableInternetAccess            :: !(Maybe Bool)
    , _dwcpDefaultOu                       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DefaultWorkspaceCreationProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dwcpCustomSecurityGroupId'
--
-- * 'dwcpUserEnabledAsLocalAdministrator'
--
-- * 'dwcpEnableWorkDocs'
--
-- * 'dwcpEnableInternetAccess'
--
-- * 'dwcpDefaultOu'
defaultWorkspaceCreationProperties
    :: DefaultWorkspaceCreationProperties
defaultWorkspaceCreationProperties =
    DefaultWorkspaceCreationProperties'
    { _dwcpCustomSecurityGroupId = Nothing
    , _dwcpUserEnabledAsLocalAdministrator = Nothing
    , _dwcpEnableWorkDocs = Nothing
    , _dwcpEnableInternetAccess = Nothing
    , _dwcpDefaultOu = Nothing
    }

-- | The identifier of any custom security groups that are applied to the WorkSpaces when they are created.
dwcpCustomSecurityGroupId :: Lens' DefaultWorkspaceCreationProperties (Maybe Text)
dwcpCustomSecurityGroupId = lens _dwcpCustomSecurityGroupId (\ s a -> s{_dwcpCustomSecurityGroupId = a});

-- | The WorkSpace user is an administrator on the WorkSpace.
dwcpUserEnabledAsLocalAdministrator :: Lens' DefaultWorkspaceCreationProperties (Maybe Bool)
dwcpUserEnabledAsLocalAdministrator = lens _dwcpUserEnabledAsLocalAdministrator (\ s a -> s{_dwcpUserEnabledAsLocalAdministrator = a});

-- | Specifies if the directory is enabled for Amazon WorkDocs.
dwcpEnableWorkDocs :: Lens' DefaultWorkspaceCreationProperties (Maybe Bool)
dwcpEnableWorkDocs = lens _dwcpEnableWorkDocs (\ s a -> s{_dwcpEnableWorkDocs = a});

-- | A public IP address will be attached to all WorkSpaces that are created or rebuilt.
dwcpEnableInternetAccess :: Lens' DefaultWorkspaceCreationProperties (Maybe Bool)
dwcpEnableInternetAccess = lens _dwcpEnableInternetAccess (\ s a -> s{_dwcpEnableInternetAccess = a});

-- | The organizational unit (OU) in the directory that the WorkSpace machine accounts are placed in.
dwcpDefaultOu :: Lens' DefaultWorkspaceCreationProperties (Maybe Text)
dwcpDefaultOu = lens _dwcpDefaultOu (\ s a -> s{_dwcpDefaultOu = a});

instance FromJSON DefaultWorkspaceCreationProperties
         where
        parseJSON
          = withObject "DefaultWorkspaceCreationProperties"
              (\ x ->
                 DefaultWorkspaceCreationProperties' <$>
                   (x .:? "CustomSecurityGroupId") <*>
                     (x .:? "UserEnabledAsLocalAdministrator")
                     <*> (x .:? "EnableWorkDocs")
                     <*> (x .:? "EnableInternetAccess")
                     <*> (x .:? "DefaultOu"))

instance Hashable DefaultWorkspaceCreationProperties

instance NFData DefaultWorkspaceCreationProperties

-- | Contains information about a WorkSpace that could not be created.
--
-- /See:/ 'failedCreateWorkspaceRequest' smart constructor.
data FailedCreateWorkspaceRequest = FailedCreateWorkspaceRequest'
    { _fcwrWorkspaceRequest :: !(Maybe WorkspaceRequest)
    , _fcwrErrorCode        :: !(Maybe Text)
    , _fcwrErrorMessage     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'FailedCreateWorkspaceRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fcwrWorkspaceRequest'
--
-- * 'fcwrErrorCode'
--
-- * 'fcwrErrorMessage'
failedCreateWorkspaceRequest
    :: FailedCreateWorkspaceRequest
failedCreateWorkspaceRequest =
    FailedCreateWorkspaceRequest'
    { _fcwrWorkspaceRequest = Nothing
    , _fcwrErrorCode = Nothing
    , _fcwrErrorMessage = Nothing
    }

-- | A < WorkspaceRequest> object that contains the information about the WorkSpace that could not be created.
fcwrWorkspaceRequest :: Lens' FailedCreateWorkspaceRequest (Maybe WorkspaceRequest)
fcwrWorkspaceRequest = lens _fcwrWorkspaceRequest (\ s a -> s{_fcwrWorkspaceRequest = a});

-- | The error code.
fcwrErrorCode :: Lens' FailedCreateWorkspaceRequest (Maybe Text)
fcwrErrorCode = lens _fcwrErrorCode (\ s a -> s{_fcwrErrorCode = a});

-- | The textual error message.
fcwrErrorMessage :: Lens' FailedCreateWorkspaceRequest (Maybe Text)
fcwrErrorMessage = lens _fcwrErrorMessage (\ s a -> s{_fcwrErrorMessage = a});

instance FromJSON FailedCreateWorkspaceRequest where
        parseJSON
          = withObject "FailedCreateWorkspaceRequest"
              (\ x ->
                 FailedCreateWorkspaceRequest' <$>
                   (x .:? "WorkspaceRequest") <*> (x .:? "ErrorCode")
                     <*> (x .:? "ErrorMessage"))

instance Hashable FailedCreateWorkspaceRequest

instance NFData FailedCreateWorkspaceRequest

-- | Contains information about a WorkSpace that could not be rebooted (< RebootWorkspaces>), rebuilt (< RebuildWorkspaces>), or terminated (< TerminateWorkspaces>).
--
-- /See:/ 'failedWorkspaceChangeRequest' smart constructor.
data FailedWorkspaceChangeRequest = FailedWorkspaceChangeRequest'
    { _fwcrErrorCode    :: !(Maybe Text)
    , _fwcrWorkspaceId  :: !(Maybe Text)
    , _fwcrErrorMessage :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'FailedWorkspaceChangeRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fwcrErrorCode'
--
-- * 'fwcrWorkspaceId'
--
-- * 'fwcrErrorMessage'
failedWorkspaceChangeRequest
    :: FailedWorkspaceChangeRequest
failedWorkspaceChangeRequest =
    FailedWorkspaceChangeRequest'
    { _fwcrErrorCode = Nothing
    , _fwcrWorkspaceId = Nothing
    , _fwcrErrorMessage = Nothing
    }

-- | The error code.
fwcrErrorCode :: Lens' FailedWorkspaceChangeRequest (Maybe Text)
fwcrErrorCode = lens _fwcrErrorCode (\ s a -> s{_fwcrErrorCode = a});

-- | The identifier of the WorkSpace.
fwcrWorkspaceId :: Lens' FailedWorkspaceChangeRequest (Maybe Text)
fwcrWorkspaceId = lens _fwcrWorkspaceId (\ s a -> s{_fwcrWorkspaceId = a});

-- | The textual error message.
fwcrErrorMessage :: Lens' FailedWorkspaceChangeRequest (Maybe Text)
fwcrErrorMessage = lens _fwcrErrorMessage (\ s a -> s{_fwcrErrorMessage = a});

instance FromJSON FailedWorkspaceChangeRequest where
        parseJSON
          = withObject "FailedWorkspaceChangeRequest"
              (\ x ->
                 FailedWorkspaceChangeRequest' <$>
                   (x .:? "ErrorCode") <*> (x .:? "WorkspaceId") <*>
                     (x .:? "ErrorMessage"))

instance Hashable FailedWorkspaceChangeRequest

instance NFData FailedWorkspaceChangeRequest

-- | Contains information used with the < RebootWorkspaces> operation to reboot a WorkSpace.
--
-- /See:/ 'rebootRequest' smart constructor.
newtype RebootRequest = RebootRequest'
    { _rWorkspaceId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RebootRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rWorkspaceId'
rebootRequest
    :: Text -- ^ 'rWorkspaceId'
    -> RebootRequest
rebootRequest pWorkspaceId_ =
    RebootRequest'
    { _rWorkspaceId = pWorkspaceId_
    }

-- | The identifier of the WorkSpace to reboot.
rWorkspaceId :: Lens' RebootRequest Text
rWorkspaceId = lens _rWorkspaceId (\ s a -> s{_rWorkspaceId = a});

instance Hashable RebootRequest

instance NFData RebootRequest

instance ToJSON RebootRequest where
        toJSON RebootRequest'{..}
          = object
              (catMaybes [Just ("WorkspaceId" .= _rWorkspaceId)])

-- | Contains information used with the < RebuildWorkspaces> operation to rebuild a WorkSpace.
--
-- /See:/ 'rebuildRequest' smart constructor.
newtype RebuildRequest = RebuildRequest'
    { _rrWorkspaceId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RebuildRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrWorkspaceId'
rebuildRequest
    :: Text -- ^ 'rrWorkspaceId'
    -> RebuildRequest
rebuildRequest pWorkspaceId_ =
    RebuildRequest'
    { _rrWorkspaceId = pWorkspaceId_
    }

-- | The identifier of the WorkSpace to rebuild.
rrWorkspaceId :: Lens' RebuildRequest Text
rrWorkspaceId = lens _rrWorkspaceId (\ s a -> s{_rrWorkspaceId = a});

instance Hashable RebuildRequest

instance NFData RebuildRequest

instance ToJSON RebuildRequest where
        toJSON RebuildRequest'{..}
          = object
              (catMaybes [Just ("WorkspaceId" .= _rrWorkspaceId)])

-- | Contains information used with the < TerminateWorkspaces> operation to terminate a WorkSpace.
--
-- /See:/ 'terminateRequest' smart constructor.
newtype TerminateRequest = TerminateRequest'
    { _trWorkspaceId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TerminateRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trWorkspaceId'
terminateRequest
    :: Text -- ^ 'trWorkspaceId'
    -> TerminateRequest
terminateRequest pWorkspaceId_ =
    TerminateRequest'
    { _trWorkspaceId = pWorkspaceId_
    }

-- | The identifier of the WorkSpace to terminate.
trWorkspaceId :: Lens' TerminateRequest Text
trWorkspaceId = lens _trWorkspaceId (\ s a -> s{_trWorkspaceId = a});

instance Hashable TerminateRequest

instance NFData TerminateRequest

instance ToJSON TerminateRequest where
        toJSON TerminateRequest'{..}
          = object
              (catMaybes [Just ("WorkspaceId" .= _trWorkspaceId)])

-- | Contains information about the user storage for a WorkSpace bundle.
--
-- /See:/ 'userStorage' smart constructor.
newtype UserStorage = UserStorage'
    { _usCapacity :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UserStorage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usCapacity'
userStorage
    :: UserStorage
userStorage =
    UserStorage'
    { _usCapacity = Nothing
    }

-- | The amount of user storage for the bundle.
usCapacity :: Lens' UserStorage (Maybe Text)
usCapacity = lens _usCapacity (\ s a -> s{_usCapacity = a});

instance FromJSON UserStorage where
        parseJSON
          = withObject "UserStorage"
              (\ x -> UserStorage' <$> (x .:? "Capacity"))

instance Hashable UserStorage

instance NFData UserStorage

-- | Contains information about a WorkSpace.
--
-- /See:/ 'workspace' smart constructor.
data Workspace = Workspace'
    { _wDirectoryId                 :: !(Maybe Text)
    , _wState                       :: !(Maybe WorkspaceState)
    , _wIPAddress                   :: !(Maybe Text)
    , _wUserName                    :: !(Maybe Text)
    , _wSubnetId                    :: !(Maybe Text)
    , _wBundleId                    :: !(Maybe Text)
    , _wRootVolumeEncryptionEnabled :: !(Maybe Bool)
    , _wErrorCode                   :: !(Maybe Text)
    , _wVolumeEncryptionKey         :: !(Maybe Text)
    , _wComputerName                :: !(Maybe Text)
    , _wWorkspaceId                 :: !(Maybe Text)
    , _wUserVolumeEncryptionEnabled :: !(Maybe Bool)
    , _wErrorMessage                :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Workspace' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wDirectoryId'
--
-- * 'wState'
--
-- * 'wIPAddress'
--
-- * 'wUserName'
--
-- * 'wSubnetId'
--
-- * 'wBundleId'
--
-- * 'wRootVolumeEncryptionEnabled'
--
-- * 'wErrorCode'
--
-- * 'wVolumeEncryptionKey'
--
-- * 'wComputerName'
--
-- * 'wWorkspaceId'
--
-- * 'wUserVolumeEncryptionEnabled'
--
-- * 'wErrorMessage'
workspace
    :: Workspace
workspace =
    Workspace'
    { _wDirectoryId = Nothing
    , _wState = Nothing
    , _wIPAddress = Nothing
    , _wUserName = Nothing
    , _wSubnetId = Nothing
    , _wBundleId = Nothing
    , _wRootVolumeEncryptionEnabled = Nothing
    , _wErrorCode = Nothing
    , _wVolumeEncryptionKey = Nothing
    , _wComputerName = Nothing
    , _wWorkspaceId = Nothing
    , _wUserVolumeEncryptionEnabled = Nothing
    , _wErrorMessage = Nothing
    }

-- | The identifier of the AWS Directory Service directory that the WorkSpace belongs to.
wDirectoryId :: Lens' Workspace (Maybe Text)
wDirectoryId = lens _wDirectoryId (\ s a -> s{_wDirectoryId = a});

-- | The operational state of the WorkSpace.
wState :: Lens' Workspace (Maybe WorkspaceState)
wState = lens _wState (\ s a -> s{_wState = a});

-- | The IP address of the WorkSpace.
wIPAddress :: Lens' Workspace (Maybe Text)
wIPAddress = lens _wIPAddress (\ s a -> s{_wIPAddress = a});

-- | The user that the WorkSpace is assigned to.
wUserName :: Lens' Workspace (Maybe Text)
wUserName = lens _wUserName (\ s a -> s{_wUserName = a});

-- | The identifier of the subnet that the WorkSpace is in.
wSubnetId :: Lens' Workspace (Maybe Text)
wSubnetId = lens _wSubnetId (\ s a -> s{_wSubnetId = a});

-- | The identifier of the bundle that the WorkSpace was created from.
wBundleId :: Lens' Workspace (Maybe Text)
wBundleId = lens _wBundleId (\ s a -> s{_wBundleId = a});

-- | Specifies whether the data stored on the root volume, or C: drive, is encrypted.
wRootVolumeEncryptionEnabled :: Lens' Workspace (Maybe Bool)
wRootVolumeEncryptionEnabled = lens _wRootVolumeEncryptionEnabled (\ s a -> s{_wRootVolumeEncryptionEnabled = a});

-- | If the WorkSpace could not be created, this contains the error code.
wErrorCode :: Lens' Workspace (Maybe Text)
wErrorCode = lens _wErrorCode (\ s a -> s{_wErrorCode = a});

-- | The KMS key used to encrypt data stored on your WorkSpace.
wVolumeEncryptionKey :: Lens' Workspace (Maybe Text)
wVolumeEncryptionKey = lens _wVolumeEncryptionKey (\ s a -> s{_wVolumeEncryptionKey = a});

-- | The name of the WorkSpace as seen by the operating system.
wComputerName :: Lens' Workspace (Maybe Text)
wComputerName = lens _wComputerName (\ s a -> s{_wComputerName = a});

-- | The identifier of the WorkSpace.
wWorkspaceId :: Lens' Workspace (Maybe Text)
wWorkspaceId = lens _wWorkspaceId (\ s a -> s{_wWorkspaceId = a});

-- | Specifies whether the data stored on the user volume, or D: drive, is encrypted.
wUserVolumeEncryptionEnabled :: Lens' Workspace (Maybe Bool)
wUserVolumeEncryptionEnabled = lens _wUserVolumeEncryptionEnabled (\ s a -> s{_wUserVolumeEncryptionEnabled = a});

-- | If the WorkSpace could not be created, this contains a textual error message that describes the failure.
wErrorMessage :: Lens' Workspace (Maybe Text)
wErrorMessage = lens _wErrorMessage (\ s a -> s{_wErrorMessage = a});

instance FromJSON Workspace where
        parseJSON
          = withObject "Workspace"
              (\ x ->
                 Workspace' <$>
                   (x .:? "DirectoryId") <*> (x .:? "State") <*>
                     (x .:? "IpAddress")
                     <*> (x .:? "UserName")
                     <*> (x .:? "SubnetId")
                     <*> (x .:? "BundleId")
                     <*> (x .:? "RootVolumeEncryptionEnabled")
                     <*> (x .:? "ErrorCode")
                     <*> (x .:? "VolumeEncryptionKey")
                     <*> (x .:? "ComputerName")
                     <*> (x .:? "WorkspaceId")
                     <*> (x .:? "UserVolumeEncryptionEnabled")
                     <*> (x .:? "ErrorMessage"))

instance Hashable Workspace

instance NFData Workspace

-- | Contains information about a WorkSpace bundle.
--
-- /See:/ 'workspaceBundle' smart constructor.
data WorkspaceBundle = WorkspaceBundle'
    { _wbBundleId    :: !(Maybe Text)
    , _wbOwner       :: !(Maybe Text)
    , _wbName        :: !(Maybe Text)
    , _wbComputeType :: !(Maybe ComputeType)
    , _wbUserStorage :: !(Maybe UserStorage)
    , _wbDescription :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'WorkspaceBundle' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wbBundleId'
--
-- * 'wbOwner'
--
-- * 'wbName'
--
-- * 'wbComputeType'
--
-- * 'wbUserStorage'
--
-- * 'wbDescription'
workspaceBundle
    :: WorkspaceBundle
workspaceBundle =
    WorkspaceBundle'
    { _wbBundleId = Nothing
    , _wbOwner = Nothing
    , _wbName = Nothing
    , _wbComputeType = Nothing
    , _wbUserStorage = Nothing
    , _wbDescription = Nothing
    }

-- | The bundle identifier.
wbBundleId :: Lens' WorkspaceBundle (Maybe Text)
wbBundleId = lens _wbBundleId (\ s a -> s{_wbBundleId = a});

-- | The owner of the bundle. This contains the owner\'s account identifier, or 'AMAZON' if the bundle is provided by AWS.
wbOwner :: Lens' WorkspaceBundle (Maybe Text)
wbOwner = lens _wbOwner (\ s a -> s{_wbOwner = a});

-- | The name of the bundle.
wbName :: Lens' WorkspaceBundle (Maybe Text)
wbName = lens _wbName (\ s a -> s{_wbName = a});

-- | A < ComputeType> object that specifies the compute type for the bundle.
wbComputeType :: Lens' WorkspaceBundle (Maybe ComputeType)
wbComputeType = lens _wbComputeType (\ s a -> s{_wbComputeType = a});

-- | A < UserStorage> object that specifies the amount of user storage that the bundle contains.
wbUserStorage :: Lens' WorkspaceBundle (Maybe UserStorage)
wbUserStorage = lens _wbUserStorage (\ s a -> s{_wbUserStorage = a});

-- | The bundle description.
wbDescription :: Lens' WorkspaceBundle (Maybe Text)
wbDescription = lens _wbDescription (\ s a -> s{_wbDescription = a});

instance FromJSON WorkspaceBundle where
        parseJSON
          = withObject "WorkspaceBundle"
              (\ x ->
                 WorkspaceBundle' <$>
                   (x .:? "BundleId") <*> (x .:? "Owner") <*>
                     (x .:? "Name")
                     <*> (x .:? "ComputeType")
                     <*> (x .:? "UserStorage")
                     <*> (x .:? "Description"))

instance Hashable WorkspaceBundle

instance NFData WorkspaceBundle

-- | Contains information about an AWS Directory Service directory for use with Amazon WorkSpaces.
--
-- /See:/ 'workspaceDirectory' smart constructor.
data WorkspaceDirectory = WorkspaceDirectory'
    { _wdRegistrationCode            :: !(Maybe Text)
    , _wdIAMRoleId                   :: !(Maybe Text)
    , _wdDirectoryId                 :: !(Maybe Text)
    , _wdState                       :: !(Maybe WorkspaceDirectoryState)
    , _wdCustomerUserName            :: !(Maybe Text)
    , _wdSubnetIds                   :: !(Maybe [Text])
    , _wdAlias                       :: !(Maybe Text)
    , _wdWorkspaceSecurityGroupId    :: !(Maybe Text)
    , _wdDirectoryType               :: !(Maybe WorkspaceDirectoryType)
    , _wdWorkspaceCreationProperties :: !(Maybe DefaultWorkspaceCreationProperties)
    , _wdDNSIPAddresses              :: !(Maybe [Text])
    , _wdDirectoryName               :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'WorkspaceDirectory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wdRegistrationCode'
--
-- * 'wdIAMRoleId'
--
-- * 'wdDirectoryId'
--
-- * 'wdState'
--
-- * 'wdCustomerUserName'
--
-- * 'wdSubnetIds'
--
-- * 'wdAlias'
--
-- * 'wdWorkspaceSecurityGroupId'
--
-- * 'wdDirectoryType'
--
-- * 'wdWorkspaceCreationProperties'
--
-- * 'wdDNSIPAddresses'
--
-- * 'wdDirectoryName'
workspaceDirectory
    :: WorkspaceDirectory
workspaceDirectory =
    WorkspaceDirectory'
    { _wdRegistrationCode = Nothing
    , _wdIAMRoleId = Nothing
    , _wdDirectoryId = Nothing
    , _wdState = Nothing
    , _wdCustomerUserName = Nothing
    , _wdSubnetIds = Nothing
    , _wdAlias = Nothing
    , _wdWorkspaceSecurityGroupId = Nothing
    , _wdDirectoryType = Nothing
    , _wdWorkspaceCreationProperties = Nothing
    , _wdDNSIPAddresses = Nothing
    , _wdDirectoryName = Nothing
    }

-- | The registration code for the directory. This is the code that users enter in their Amazon WorkSpaces client application to connect to the directory.
wdRegistrationCode :: Lens' WorkspaceDirectory (Maybe Text)
wdRegistrationCode = lens _wdRegistrationCode (\ s a -> s{_wdRegistrationCode = a});

-- | The identifier of the IAM role. This is the role that allows Amazon WorkSpaces to make calls to other services, such as Amazon EC2, on your behalf.
wdIAMRoleId :: Lens' WorkspaceDirectory (Maybe Text)
wdIAMRoleId = lens _wdIAMRoleId (\ s a -> s{_wdIAMRoleId = a});

-- | The directory identifier.
wdDirectoryId :: Lens' WorkspaceDirectory (Maybe Text)
wdDirectoryId = lens _wdDirectoryId (\ s a -> s{_wdDirectoryId = a});

-- | The state of the directory\'s registration with Amazon WorkSpaces
wdState :: Lens' WorkspaceDirectory (Maybe WorkspaceDirectoryState)
wdState = lens _wdState (\ s a -> s{_wdState = a});

-- | The user name for the service account.
wdCustomerUserName :: Lens' WorkspaceDirectory (Maybe Text)
wdCustomerUserName = lens _wdCustomerUserName (\ s a -> s{_wdCustomerUserName = a});

-- | An array of strings that contains the identifiers of the subnets used with the directory.
wdSubnetIds :: Lens' WorkspaceDirectory [Text]
wdSubnetIds = lens _wdSubnetIds (\ s a -> s{_wdSubnetIds = a}) . _Default . _Coerce;

-- | The directory alias.
wdAlias :: Lens' WorkspaceDirectory (Maybe Text)
wdAlias = lens _wdAlias (\ s a -> s{_wdAlias = a});

-- | The identifier of the security group that is assigned to new WorkSpaces.
wdWorkspaceSecurityGroupId :: Lens' WorkspaceDirectory (Maybe Text)
wdWorkspaceSecurityGroupId = lens _wdWorkspaceSecurityGroupId (\ s a -> s{_wdWorkspaceSecurityGroupId = a});

-- | The directory type.
wdDirectoryType :: Lens' WorkspaceDirectory (Maybe WorkspaceDirectoryType)
wdDirectoryType = lens _wdDirectoryType (\ s a -> s{_wdDirectoryType = a});

-- | A structure that specifies the default creation properties for all WorkSpaces in the directory.
wdWorkspaceCreationProperties :: Lens' WorkspaceDirectory (Maybe DefaultWorkspaceCreationProperties)
wdWorkspaceCreationProperties = lens _wdWorkspaceCreationProperties (\ s a -> s{_wdWorkspaceCreationProperties = a});

-- | An array of strings that contains the IP addresses of the DNS servers for the directory.
wdDNSIPAddresses :: Lens' WorkspaceDirectory [Text]
wdDNSIPAddresses = lens _wdDNSIPAddresses (\ s a -> s{_wdDNSIPAddresses = a}) . _Default . _Coerce;

-- | The name of the directory.
wdDirectoryName :: Lens' WorkspaceDirectory (Maybe Text)
wdDirectoryName = lens _wdDirectoryName (\ s a -> s{_wdDirectoryName = a});

instance FromJSON WorkspaceDirectory where
        parseJSON
          = withObject "WorkspaceDirectory"
              (\ x ->
                 WorkspaceDirectory' <$>
                   (x .:? "RegistrationCode") <*> (x .:? "IamRoleId")
                     <*> (x .:? "DirectoryId")
                     <*> (x .:? "State")
                     <*> (x .:? "CustomerUserName")
                     <*> (x .:? "SubnetIds" .!= mempty)
                     <*> (x .:? "Alias")
                     <*> (x .:? "WorkspaceSecurityGroupId")
                     <*> (x .:? "DirectoryType")
                     <*> (x .:? "WorkspaceCreationProperties")
                     <*> (x .:? "DnsIpAddresses" .!= mempty)
                     <*> (x .:? "DirectoryName"))

instance Hashable WorkspaceDirectory

instance NFData WorkspaceDirectory

-- | Contains information about a WorkSpace creation request.
--
-- /See:/ 'workspaceRequest' smart constructor.
data WorkspaceRequest = WorkspaceRequest'
    { _wrRootVolumeEncryptionEnabled :: !(Maybe Bool)
    , _wrVolumeEncryptionKey         :: !(Maybe Text)
    , _wrUserVolumeEncryptionEnabled :: !(Maybe Bool)
    , _wrDirectoryId                 :: !Text
    , _wrUserName                    :: !Text
    , _wrBundleId                    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'WorkspaceRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wrRootVolumeEncryptionEnabled'
--
-- * 'wrVolumeEncryptionKey'
--
-- * 'wrUserVolumeEncryptionEnabled'
--
-- * 'wrDirectoryId'
--
-- * 'wrUserName'
--
-- * 'wrBundleId'
workspaceRequest
    :: Text -- ^ 'wrDirectoryId'
    -> Text -- ^ 'wrUserName'
    -> Text -- ^ 'wrBundleId'
    -> WorkspaceRequest
workspaceRequest pDirectoryId_ pUserName_ pBundleId_ =
    WorkspaceRequest'
    { _wrRootVolumeEncryptionEnabled = Nothing
    , _wrVolumeEncryptionKey = Nothing
    , _wrUserVolumeEncryptionEnabled = Nothing
    , _wrDirectoryId = pDirectoryId_
    , _wrUserName = pUserName_
    , _wrBundleId = pBundleId_
    }

-- | Specifies whether the data stored on the root volume, or C: drive, is encrypted.
wrRootVolumeEncryptionEnabled :: Lens' WorkspaceRequest (Maybe Bool)
wrRootVolumeEncryptionEnabled = lens _wrRootVolumeEncryptionEnabled (\ s a -> s{_wrRootVolumeEncryptionEnabled = a});

-- | The KMS key used to encrypt data stored on your WorkSpace.
wrVolumeEncryptionKey :: Lens' WorkspaceRequest (Maybe Text)
wrVolumeEncryptionKey = lens _wrVolumeEncryptionKey (\ s a -> s{_wrVolumeEncryptionKey = a});

-- | Specifies whether the data stored on the user volume, or D: drive, is encrypted.
wrUserVolumeEncryptionEnabled :: Lens' WorkspaceRequest (Maybe Bool)
wrUserVolumeEncryptionEnabled = lens _wrUserVolumeEncryptionEnabled (\ s a -> s{_wrUserVolumeEncryptionEnabled = a});

-- | The identifier of the AWS Directory Service directory to create the WorkSpace in. You can use the < DescribeWorkspaceDirectories> operation to obtain a list of the directories that are available.
wrDirectoryId :: Lens' WorkspaceRequest Text
wrDirectoryId = lens _wrDirectoryId (\ s a -> s{_wrDirectoryId = a});

-- | The username that the WorkSpace is assigned to. This username must exist in the AWS Directory Service directory specified by the 'DirectoryId' member.
wrUserName :: Lens' WorkspaceRequest Text
wrUserName = lens _wrUserName (\ s a -> s{_wrUserName = a});

-- | The identifier of the bundle to create the WorkSpace from. You can use the < DescribeWorkspaceBundles> operation to obtain a list of the bundles that are available.
wrBundleId :: Lens' WorkspaceRequest Text
wrBundleId = lens _wrBundleId (\ s a -> s{_wrBundleId = a});

instance FromJSON WorkspaceRequest where
        parseJSON
          = withObject "WorkspaceRequest"
              (\ x ->
                 WorkspaceRequest' <$>
                   (x .:? "RootVolumeEncryptionEnabled") <*>
                     (x .:? "VolumeEncryptionKey")
                     <*> (x .:? "UserVolumeEncryptionEnabled")
                     <*> (x .: "DirectoryId")
                     <*> (x .: "UserName")
                     <*> (x .: "BundleId"))

instance Hashable WorkspaceRequest

instance NFData WorkspaceRequest

instance ToJSON WorkspaceRequest where
        toJSON WorkspaceRequest'{..}
          = object
              (catMaybes
                 [("RootVolumeEncryptionEnabled" .=) <$>
                    _wrRootVolumeEncryptionEnabled,
                  ("VolumeEncryptionKey" .=) <$>
                    _wrVolumeEncryptionKey,
                  ("UserVolumeEncryptionEnabled" .=) <$>
                    _wrUserVolumeEncryptionEnabled,
                  Just ("DirectoryId" .= _wrDirectoryId),
                  Just ("UserName" .= _wrUserName),
                  Just ("BundleId" .= _wrBundleId)])

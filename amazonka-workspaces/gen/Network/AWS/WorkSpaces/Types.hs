{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Network.AWS.WorkSpaces.Types
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.WorkSpaces.Types
    (
    -- * Service
      WorkSpaces
    -- ** Errors
    , JSONError

    -- * Compute
    , Compute (..)

    -- * ComputeType
    , ComputeType
    , computeType
    , ctName

    -- * DefaultWorkspaceCreationProperties
    , DefaultWorkspaceCreationProperties
    , defaultWorkspaceCreationProperties
    , dwcpCustomSecurityGroupId
    , dwcpUserEnabledAsLocalAdministrator
    , dwcpEnableWorkDocs
    , dwcpEnableInternetAccess
    , dwcpDefaultOu

    -- * FailedCreateWorkspaceRequest
    , FailedCreateWorkspaceRequest
    , failedCreateWorkspaceRequest
    , fcwrWorkspaceRequest
    , fcwrErrorCode
    , fcwrErrorMessage

    -- * FailedWorkspaceChangeRequest
    , FailedWorkspaceChangeRequest
    , failedWorkspaceChangeRequest
    , fwcrErrorCode
    , fwcrWorkspaceId
    , fwcrErrorMessage

    -- * RebootRequest
    , RebootRequest
    , rebootRequest
    , rebWorkspaceId

    -- * RebuildRequest
    , RebuildRequest
    , rebuildRequest
    , rrWorkspaceId

    -- * TerminateRequest
    , TerminateRequest
    , terminateRequest
    , trWorkspaceId

    -- * UserStorage
    , UserStorage
    , userStorage
    , usCapacity

    -- * Workspace
    , Workspace
    , workspace
    , worDirectoryId
    , worIPAddress
    , worState
    , worUserName
    , worSubnetId
    , worBundleId
    , worErrorCode
    , worWorkspaceId
    , worErrorMessage

    -- * WorkspaceBundle
    , WorkspaceBundle
    , workspaceBundle
    , wbOwner
    , wbBundleId
    , wbName
    , wbComputeType
    , wbUserStorage
    , wbDescription

    -- * WorkspaceDirectory
    , WorkspaceDirectory
    , workspaceDirectory
    , wdRegistrationCode
    , wdIAMRoleId
    , wdDirectoryId
    , wdState
    , wdCustomerUserName
    , wdSubnetIds
    , wdAlias
    , wdDirectoryType
    , wdWorkspaceSecurityGroupId
    , wdWorkspaceCreationProperties
    , wdDNSIPAddresses
    , wdDirectoryName

    -- * WorkspaceDirectoryState
    , WorkspaceDirectoryState (..)

    -- * WorkspaceDirectoryType
    , WorkspaceDirectoryType (..)

    -- * WorkspaceRequest
    , WorkspaceRequest
    , workspaceRequest
    , wrDirectoryId
    , wrUserName
    , wrBundleId

    -- * WorkspaceState
    , WorkspaceState (..)
    ) where

import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | Version @2015-04-08@ of the Amazon WorkSpaces SDK.
data WorkSpaces

instance AWSService WorkSpaces where
    type Sg WorkSpaces = V4
    type Er WorkSpaces = JSONError

    service = service'
      where
        service' :: Service WorkSpaces
        service' = Service
            { _svcAbbrev  = "WorkSpaces"
            , _svcPrefix  = "workspaces"
            , _svcVersion = "2015-04-08"
            , _svcHandle  = handle
            , _svcRetry   = retry
            }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError JSONError)
        handle = jsonError statusSuccess service'

        retry :: Retry WorkSpaces
        retry = undefined

        check :: Status
              -> JSONError
              -> Bool
        check (statusCode -> s) (awsErrorCode -> e) = undefined

data Compute = Performance | Value | Standard deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText Compute where
    parser = takeLowerText >>= \case
        "PERFORMANCE" -> pure Performance
        "STANDARD" -> pure Standard
        "VALUE" -> pure Value
        e -> fail ("Failure parsing Compute from " ++ show e)

instance ToText Compute where
    toText = \case
        Performance -> "PERFORMANCE"
        Standard -> "STANDARD"
        Value -> "VALUE"

instance Hashable Compute
instance ToQuery Compute
instance ToHeader Compute

instance FromJSON Compute where
    parseJSON = parseJSONText "Compute"

-- | /See:/ 'computeType' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ctName'
newtype ComputeType = ComputeType'{_ctName :: Maybe Compute} deriving (Eq, Read, Show)

-- | 'ComputeType' smart constructor.
computeType :: ComputeType
computeType = ComputeType'{_ctName = Nothing};

-- | The name of the compute type for the bundle.
ctName :: Lens' ComputeType (Maybe Compute)
ctName = lens _ctName (\ s a -> s{_ctName = a});

instance FromJSON ComputeType where
        parseJSON
          = withObject "ComputeType"
              (\ x -> ComputeType' <$> x .:? "Name")

-- | /See:/ 'defaultWorkspaceCreationProperties' smart constructor.
--
-- The fields accessible through corresponding lenses are:
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
data DefaultWorkspaceCreationProperties = DefaultWorkspaceCreationProperties'{_dwcpCustomSecurityGroupId :: Maybe Text, _dwcpUserEnabledAsLocalAdministrator :: Maybe Bool, _dwcpEnableWorkDocs :: Maybe Bool, _dwcpEnableInternetAccess :: Maybe Bool, _dwcpDefaultOu :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DefaultWorkspaceCreationProperties' smart constructor.
defaultWorkspaceCreationProperties :: DefaultWorkspaceCreationProperties
defaultWorkspaceCreationProperties = DefaultWorkspaceCreationProperties'{_dwcpCustomSecurityGroupId = Nothing, _dwcpUserEnabledAsLocalAdministrator = Nothing, _dwcpEnableWorkDocs = Nothing, _dwcpEnableInternetAccess = Nothing, _dwcpDefaultOu = Nothing};

-- | The identifier of any custom security groups that are applied to the
-- WorkSpaces when they are created.
dwcpCustomSecurityGroupId :: Lens' DefaultWorkspaceCreationProperties (Maybe Text)
dwcpCustomSecurityGroupId = lens _dwcpCustomSecurityGroupId (\ s a -> s{_dwcpCustomSecurityGroupId = a});

-- | The WorkSpace user is an administrator on the WorkSpace.
dwcpUserEnabledAsLocalAdministrator :: Lens' DefaultWorkspaceCreationProperties (Maybe Bool)
dwcpUserEnabledAsLocalAdministrator = lens _dwcpUserEnabledAsLocalAdministrator (\ s a -> s{_dwcpUserEnabledAsLocalAdministrator = a});

-- | Specifies if the directory is enabled for Amazon WorkDocs.
dwcpEnableWorkDocs :: Lens' DefaultWorkspaceCreationProperties (Maybe Bool)
dwcpEnableWorkDocs = lens _dwcpEnableWorkDocs (\ s a -> s{_dwcpEnableWorkDocs = a});

-- | A public IP address will be attached to all WorkSpaces that are created
-- or rebuilt.
dwcpEnableInternetAccess :: Lens' DefaultWorkspaceCreationProperties (Maybe Bool)
dwcpEnableInternetAccess = lens _dwcpEnableInternetAccess (\ s a -> s{_dwcpEnableInternetAccess = a});

-- | The organizational unit (OU) in the directory that the WorkSpace machine
-- accounts are placed in.
dwcpDefaultOu :: Lens' DefaultWorkspaceCreationProperties (Maybe Text)
dwcpDefaultOu = lens _dwcpDefaultOu (\ s a -> s{_dwcpDefaultOu = a});

instance FromJSON DefaultWorkspaceCreationProperties
         where
        parseJSON
          = withObject "DefaultWorkspaceCreationProperties"
              (\ x ->
                 DefaultWorkspaceCreationProperties' <$>
                   x .:? "CustomSecurityGroupId" <*>
                     x .:? "UserEnabledAsLocalAdministrator"
                     <*> x .:? "EnableWorkDocs"
                     <*> x .:? "EnableInternetAccess"
                     <*> x .:? "DefaultOu")

-- | /See:/ 'failedCreateWorkspaceRequest' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'fcwrWorkspaceRequest'
--
-- * 'fcwrErrorCode'
--
-- * 'fcwrErrorMessage'
data FailedCreateWorkspaceRequest = FailedCreateWorkspaceRequest'{_fcwrWorkspaceRequest :: Maybe WorkspaceRequest, _fcwrErrorCode :: Maybe Text, _fcwrErrorMessage :: Maybe Text} deriving (Eq, Read, Show)

-- | 'FailedCreateWorkspaceRequest' smart constructor.
failedCreateWorkspaceRequest :: FailedCreateWorkspaceRequest
failedCreateWorkspaceRequest = FailedCreateWorkspaceRequest'{_fcwrWorkspaceRequest = Nothing, _fcwrErrorCode = Nothing, _fcwrErrorMessage = Nothing};

-- | A WorkspaceRequest object that contains the information about the
-- WorkSpace that could not be created.
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
                   x .:? "WorkspaceRequest" <*> x .:? "ErrorCode" <*>
                     x .:? "ErrorMessage")

-- | /See:/ 'failedWorkspaceChangeRequest' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'fwcrErrorCode'
--
-- * 'fwcrWorkspaceId'
--
-- * 'fwcrErrorMessage'
data FailedWorkspaceChangeRequest = FailedWorkspaceChangeRequest'{_fwcrErrorCode :: Maybe Text, _fwcrWorkspaceId :: Maybe Text, _fwcrErrorMessage :: Maybe Text} deriving (Eq, Read, Show)

-- | 'FailedWorkspaceChangeRequest' smart constructor.
failedWorkspaceChangeRequest :: FailedWorkspaceChangeRequest
failedWorkspaceChangeRequest = FailedWorkspaceChangeRequest'{_fwcrErrorCode = Nothing, _fwcrWorkspaceId = Nothing, _fwcrErrorMessage = Nothing};

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
                   x .:? "ErrorCode" <*> x .:? "WorkspaceId" <*>
                     x .:? "ErrorMessage")

-- | /See:/ 'rebootRequest' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rebWorkspaceId'
newtype RebootRequest = RebootRequest'{_rebWorkspaceId :: Text} deriving (Eq, Read, Show)

-- | 'RebootRequest' smart constructor.
rebootRequest :: Text -> RebootRequest
rebootRequest pWorkspaceId = RebootRequest'{_rebWorkspaceId = pWorkspaceId};

-- | The identifier of the WorkSpace to reboot.
rebWorkspaceId :: Lens' RebootRequest Text
rebWorkspaceId = lens _rebWorkspaceId (\ s a -> s{_rebWorkspaceId = a});

instance ToJSON RebootRequest where
        toJSON RebootRequest'{..}
          = object ["WorkspaceId" .= _rebWorkspaceId]

-- | /See:/ 'rebuildRequest' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rrWorkspaceId'
newtype RebuildRequest = RebuildRequest'{_rrWorkspaceId :: Text} deriving (Eq, Read, Show)

-- | 'RebuildRequest' smart constructor.
rebuildRequest :: Text -> RebuildRequest
rebuildRequest pWorkspaceId = RebuildRequest'{_rrWorkspaceId = pWorkspaceId};

-- | The identifier of the WorkSpace to rebuild.
rrWorkspaceId :: Lens' RebuildRequest Text
rrWorkspaceId = lens _rrWorkspaceId (\ s a -> s{_rrWorkspaceId = a});

instance ToJSON RebuildRequest where
        toJSON RebuildRequest'{..}
          = object ["WorkspaceId" .= _rrWorkspaceId]

-- | /See:/ 'terminateRequest' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'trWorkspaceId'
newtype TerminateRequest = TerminateRequest'{_trWorkspaceId :: Text} deriving (Eq, Read, Show)

-- | 'TerminateRequest' smart constructor.
terminateRequest :: Text -> TerminateRequest
terminateRequest pWorkspaceId = TerminateRequest'{_trWorkspaceId = pWorkspaceId};

-- | The identifier of the WorkSpace to terminate.
trWorkspaceId :: Lens' TerminateRequest Text
trWorkspaceId = lens _trWorkspaceId (\ s a -> s{_trWorkspaceId = a});

instance ToJSON TerminateRequest where
        toJSON TerminateRequest'{..}
          = object ["WorkspaceId" .= _trWorkspaceId]

-- | /See:/ 'userStorage' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'usCapacity'
newtype UserStorage = UserStorage'{_usCapacity :: Maybe Text} deriving (Eq, Read, Show)

-- | 'UserStorage' smart constructor.
userStorage :: UserStorage
userStorage = UserStorage'{_usCapacity = Nothing};

-- | The amount of user storage for the bundle.
usCapacity :: Lens' UserStorage (Maybe Text)
usCapacity = lens _usCapacity (\ s a -> s{_usCapacity = a});

instance FromJSON UserStorage where
        parseJSON
          = withObject "UserStorage"
              (\ x -> UserStorage' <$> x .:? "Capacity")

-- | /See:/ 'workspace' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'worDirectoryId'
--
-- * 'worIPAddress'
--
-- * 'worState'
--
-- * 'worUserName'
--
-- * 'worSubnetId'
--
-- * 'worBundleId'
--
-- * 'worErrorCode'
--
-- * 'worWorkspaceId'
--
-- * 'worErrorMessage'
data Workspace = Workspace'{_worDirectoryId :: Maybe Text, _worIPAddress :: Maybe Text, _worState :: Maybe WorkspaceState, _worUserName :: Maybe Text, _worSubnetId :: Maybe Text, _worBundleId :: Maybe Text, _worErrorCode :: Maybe Text, _worWorkspaceId :: Maybe Text, _worErrorMessage :: Maybe Text} deriving (Eq, Read, Show)

-- | 'Workspace' smart constructor.
workspace :: Workspace
workspace = Workspace'{_worDirectoryId = Nothing, _worIPAddress = Nothing, _worState = Nothing, _worUserName = Nothing, _worSubnetId = Nothing, _worBundleId = Nothing, _worErrorCode = Nothing, _worWorkspaceId = Nothing, _worErrorMessage = Nothing};

-- | The identifier of the AWS Directory Service directory that the WorkSpace
-- belongs to.
worDirectoryId :: Lens' Workspace (Maybe Text)
worDirectoryId = lens _worDirectoryId (\ s a -> s{_worDirectoryId = a});

-- | The IP address of the WorkSpace.
worIPAddress :: Lens' Workspace (Maybe Text)
worIPAddress = lens _worIPAddress (\ s a -> s{_worIPAddress = a});

-- | The operational state of the WorkSpace.
worState :: Lens' Workspace (Maybe WorkspaceState)
worState = lens _worState (\ s a -> s{_worState = a});

-- | The user that the WorkSpace is assigned to.
worUserName :: Lens' Workspace (Maybe Text)
worUserName = lens _worUserName (\ s a -> s{_worUserName = a});

-- | The identifier of the subnet that the WorkSpace is in.
worSubnetId :: Lens' Workspace (Maybe Text)
worSubnetId = lens _worSubnetId (\ s a -> s{_worSubnetId = a});

-- | The identifier of the bundle that the WorkSpace was created from.
worBundleId :: Lens' Workspace (Maybe Text)
worBundleId = lens _worBundleId (\ s a -> s{_worBundleId = a});

-- | If the WorkSpace could not be created, this contains the error code.
worErrorCode :: Lens' Workspace (Maybe Text)
worErrorCode = lens _worErrorCode (\ s a -> s{_worErrorCode = a});

-- | The identifier of the WorkSpace.
worWorkspaceId :: Lens' Workspace (Maybe Text)
worWorkspaceId = lens _worWorkspaceId (\ s a -> s{_worWorkspaceId = a});

-- | If the WorkSpace could not be created, this contains a textual error
-- message that describes the failure.
worErrorMessage :: Lens' Workspace (Maybe Text)
worErrorMessage = lens _worErrorMessage (\ s a -> s{_worErrorMessage = a});

instance FromJSON Workspace where
        parseJSON
          = withObject "Workspace"
              (\ x ->
                 Workspace' <$>
                   x .:? "DirectoryId" <*> x .:? "IpAddress" <*>
                     x .:? "State"
                     <*> x .:? "UserName"
                     <*> x .:? "SubnetId"
                     <*> x .:? "BundleId"
                     <*> x .:? "ErrorCode"
                     <*> x .:? "WorkspaceId"
                     <*> x .:? "ErrorMessage")

-- | /See:/ 'workspaceBundle' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'wbOwner'
--
-- * 'wbBundleId'
--
-- * 'wbName'
--
-- * 'wbComputeType'
--
-- * 'wbUserStorage'
--
-- * 'wbDescription'
data WorkspaceBundle = WorkspaceBundle'{_wbOwner :: Maybe Text, _wbBundleId :: Maybe Text, _wbName :: Maybe Text, _wbComputeType :: Maybe ComputeType, _wbUserStorage :: Maybe UserStorage, _wbDescription :: Maybe Text} deriving (Eq, Read, Show)

-- | 'WorkspaceBundle' smart constructor.
workspaceBundle :: WorkspaceBundle
workspaceBundle = WorkspaceBundle'{_wbOwner = Nothing, _wbBundleId = Nothing, _wbName = Nothing, _wbComputeType = Nothing, _wbUserStorage = Nothing, _wbDescription = Nothing};

-- | The owner of the bundle. This contains the owner\'s account identifier,
-- or @AMAZON@ if the bundle is provided by AWS.
wbOwner :: Lens' WorkspaceBundle (Maybe Text)
wbOwner = lens _wbOwner (\ s a -> s{_wbOwner = a});

-- | The bundle identifier.
wbBundleId :: Lens' WorkspaceBundle (Maybe Text)
wbBundleId = lens _wbBundleId (\ s a -> s{_wbBundleId = a});

-- | The name of the bundle.
wbName :: Lens' WorkspaceBundle (Maybe Text)
wbName = lens _wbName (\ s a -> s{_wbName = a});

-- | A ComputeType object that specifies the compute type for the bundle.
wbComputeType :: Lens' WorkspaceBundle (Maybe ComputeType)
wbComputeType = lens _wbComputeType (\ s a -> s{_wbComputeType = a});

-- | A UserStorage object that specifies the amount of user storage that the
-- bundle contains.
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
                   x .:? "Owner" <*> x .:? "BundleId" <*> x .:? "Name"
                     <*> x .:? "ComputeType"
                     <*> x .:? "UserStorage"
                     <*> x .:? "Description")

-- | /See:/ 'workspaceDirectory' smart constructor.
--
-- The fields accessible through corresponding lenses are:
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
-- * 'wdDirectoryType'
--
-- * 'wdWorkspaceSecurityGroupId'
--
-- * 'wdWorkspaceCreationProperties'
--
-- * 'wdDNSIPAddresses'
--
-- * 'wdDirectoryName'
data WorkspaceDirectory = WorkspaceDirectory'{_wdRegistrationCode :: Maybe Text, _wdIAMRoleId :: Maybe Text, _wdDirectoryId :: Maybe Text, _wdState :: Maybe WorkspaceDirectoryState, _wdCustomerUserName :: Maybe Text, _wdSubnetIds :: Maybe [Text], _wdAlias :: Maybe Text, _wdDirectoryType :: Maybe WorkspaceDirectoryType, _wdWorkspaceSecurityGroupId :: Maybe Text, _wdWorkspaceCreationProperties :: Maybe DefaultWorkspaceCreationProperties, _wdDNSIPAddresses :: Maybe [Text], _wdDirectoryName :: Maybe Text} deriving (Eq, Read, Show)

-- | 'WorkspaceDirectory' smart constructor.
workspaceDirectory :: WorkspaceDirectory
workspaceDirectory = WorkspaceDirectory'{_wdRegistrationCode = Nothing, _wdIAMRoleId = Nothing, _wdDirectoryId = Nothing, _wdState = Nothing, _wdCustomerUserName = Nothing, _wdSubnetIds = Nothing, _wdAlias = Nothing, _wdDirectoryType = Nothing, _wdWorkspaceSecurityGroupId = Nothing, _wdWorkspaceCreationProperties = Nothing, _wdDNSIPAddresses = Nothing, _wdDirectoryName = Nothing};

-- | The registration code for the directory. This is the code that users
-- enter in their Amazon WorkSpaces client application to connect to the
-- directory.
wdRegistrationCode :: Lens' WorkspaceDirectory (Maybe Text)
wdRegistrationCode = lens _wdRegistrationCode (\ s a -> s{_wdRegistrationCode = a});

-- | The identifier of the IAM role. This is the role that allows Amazon
-- WorkSpaces to make calls to other services, such as Amazon EC2, on your
-- behalf.
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

-- | An array of strings that contains the identifiers of the subnets used
-- with the directory.
wdSubnetIds :: Lens' WorkspaceDirectory (Maybe [Text])
wdSubnetIds = lens _wdSubnetIds (\ s a -> s{_wdSubnetIds = a});

-- | The directory alias.
wdAlias :: Lens' WorkspaceDirectory (Maybe Text)
wdAlias = lens _wdAlias (\ s a -> s{_wdAlias = a});

-- | The directory type.
wdDirectoryType :: Lens' WorkspaceDirectory (Maybe WorkspaceDirectoryType)
wdDirectoryType = lens _wdDirectoryType (\ s a -> s{_wdDirectoryType = a});

-- | The identifier of the security group that is assigned to new WorkSpaces.
wdWorkspaceSecurityGroupId :: Lens' WorkspaceDirectory (Maybe Text)
wdWorkspaceSecurityGroupId = lens _wdWorkspaceSecurityGroupId (\ s a -> s{_wdWorkspaceSecurityGroupId = a});

-- | A structure that specifies the default creation properties for all
-- WorkSpaces in the directory.
wdWorkspaceCreationProperties :: Lens' WorkspaceDirectory (Maybe DefaultWorkspaceCreationProperties)
wdWorkspaceCreationProperties = lens _wdWorkspaceCreationProperties (\ s a -> s{_wdWorkspaceCreationProperties = a});

-- | An array of strings that contains the IP addresses of the DNS servers
-- for the directory.
wdDNSIPAddresses :: Lens' WorkspaceDirectory (Maybe [Text])
wdDNSIPAddresses = lens _wdDNSIPAddresses (\ s a -> s{_wdDNSIPAddresses = a});

-- | The name of the directory.
wdDirectoryName :: Lens' WorkspaceDirectory (Maybe Text)
wdDirectoryName = lens _wdDirectoryName (\ s a -> s{_wdDirectoryName = a});

instance FromJSON WorkspaceDirectory where
        parseJSON
          = withObject "WorkspaceDirectory"
              (\ x ->
                 WorkspaceDirectory' <$>
                   x .:? "RegistrationCode" <*> x .:? "IamRoleId" <*>
                     x .:? "DirectoryId"
                     <*> x .:? "State"
                     <*> x .:? "CustomerUserName"
                     <*> x .:? "SubnetIds" .!= mempty
                     <*> x .:? "Alias"
                     <*> x .:? "DirectoryType"
                     <*> x .:? "WorkspaceSecurityGroupId"
                     <*> x .:? "WorkspaceCreationProperties"
                     <*> x .:? "DnsIpAddresses" .!= mempty
                     <*> x .:? "DirectoryName")

data WorkspaceDirectoryState = Deregistering | Error | Registered | Registering | Deregistered deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText WorkspaceDirectoryState where
    parser = takeLowerText >>= \case
        "DEREGISTERED" -> pure Deregistered
        "DEREGISTERING" -> pure Deregistering
        "ERROR" -> pure Error
        "REGISTERED" -> pure Registered
        "REGISTERING" -> pure Registering
        e -> fail ("Failure parsing WorkspaceDirectoryState from " ++ show e)

instance ToText WorkspaceDirectoryState where
    toText = \case
        Deregistered -> "DEREGISTERED"
        Deregistering -> "DEREGISTERING"
        Error -> "ERROR"
        Registered -> "REGISTERED"
        Registering -> "REGISTERING"

instance Hashable WorkspaceDirectoryState
instance ToQuery WorkspaceDirectoryState
instance ToHeader WorkspaceDirectoryState

instance FromJSON WorkspaceDirectoryState where
    parseJSON = parseJSONText "WorkspaceDirectoryState"

data WorkspaceDirectoryType = ADConnector | SimpleAD deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText WorkspaceDirectoryType where
    parser = takeLowerText >>= \case
        "AD_CONNECTOR" -> pure ADConnector
        "SIMPLE_AD" -> pure SimpleAD
        e -> fail ("Failure parsing WorkspaceDirectoryType from " ++ show e)

instance ToText WorkspaceDirectoryType where
    toText = \case
        ADConnector -> "AD_CONNECTOR"
        SimpleAD -> "SIMPLE_AD"

instance Hashable WorkspaceDirectoryType
instance ToQuery WorkspaceDirectoryType
instance ToHeader WorkspaceDirectoryType

instance FromJSON WorkspaceDirectoryType where
    parseJSON = parseJSONText "WorkspaceDirectoryType"

-- | /See:/ 'workspaceRequest' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'wrDirectoryId'
--
-- * 'wrUserName'
--
-- * 'wrBundleId'
data WorkspaceRequest = WorkspaceRequest'{_wrDirectoryId :: Text, _wrUserName :: Text, _wrBundleId :: Text} deriving (Eq, Read, Show)

-- | 'WorkspaceRequest' smart constructor.
workspaceRequest :: Text -> Text -> Text -> WorkspaceRequest
workspaceRequest pDirectoryId pUserName pBundleId = WorkspaceRequest'{_wrDirectoryId = pDirectoryId, _wrUserName = pUserName, _wrBundleId = pBundleId};

-- | The identifier of the AWS Directory Service directory to create the
-- WorkSpace in. You can use the DescribeWorkspaceDirectories operation to
-- obtain a list of the directories that are available.
wrDirectoryId :: Lens' WorkspaceRequest Text
wrDirectoryId = lens _wrDirectoryId (\ s a -> s{_wrDirectoryId = a});

-- | The username that the WorkSpace is assigned to. This username must exist
-- in the AWS Directory Service directory specified by the @DirectoryId@
-- member.
wrUserName :: Lens' WorkspaceRequest Text
wrUserName = lens _wrUserName (\ s a -> s{_wrUserName = a});

-- | The identifier of the bundle to create the WorkSpace from. You can use
-- the DescribeWorkspaceBundles operation to obtain a list of the bundles
-- that are available.
wrBundleId :: Lens' WorkspaceRequest Text
wrBundleId = lens _wrBundleId (\ s a -> s{_wrBundleId = a});

instance FromJSON WorkspaceRequest where
        parseJSON
          = withObject "WorkspaceRequest"
              (\ x ->
                 WorkspaceRequest' <$>
                   x .: "DirectoryId" <*> x .: "UserName" <*>
                     x .: "BundleId")

instance ToJSON WorkspaceRequest where
        toJSON WorkspaceRequest'{..}
          = object
              ["DirectoryId" .= _wrDirectoryId,
               "UserName" .= _wrUserName, "BundleId" .= _wrBundleId]

data WorkspaceState = WSError | WSSuspended | WSUnhealthy | WSRebooting | WSTerminating | WSImpaired | WSPending | WSRebuilding | WSAvailable | WSTerminated deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText WorkspaceState where
    parser = takeLowerText >>= \case
        "AVAILABLE" -> pure WSAvailable
        "ERROR" -> pure WSError
        "IMPAIRED" -> pure WSImpaired
        "PENDING" -> pure WSPending
        "REBOOTING" -> pure WSRebooting
        "REBUILDING" -> pure WSRebuilding
        "SUSPENDED" -> pure WSSuspended
        "TERMINATED" -> pure WSTerminated
        "TERMINATING" -> pure WSTerminating
        "UNHEALTHY" -> pure WSUnhealthy
        e -> fail ("Failure parsing WorkspaceState from " ++ show e)

instance ToText WorkspaceState where
    toText = \case
        WSAvailable -> "AVAILABLE"
        WSError -> "ERROR"
        WSImpaired -> "IMPAIRED"
        WSPending -> "PENDING"
        WSRebooting -> "REBOOTING"
        WSRebuilding -> "REBUILDING"
        WSSuspended -> "SUSPENDED"
        WSTerminated -> "TERMINATED"
        WSTerminating -> "TERMINATING"
        WSUnhealthy -> "UNHEALTHY"

instance Hashable WorkspaceState
instance ToQuery WorkspaceState
instance ToHeader WorkspaceState

instance FromJSON WorkspaceState where
    parseJSON = parseJSONText "WorkspaceState"

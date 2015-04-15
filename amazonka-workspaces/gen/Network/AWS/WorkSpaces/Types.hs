{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE ViewPatterns                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.WorkSpaces.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
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
    -- ** Error
    , JSONError

    -- * WorkspaceRequest
    , WorkspaceRequest
    , workspaceRequest
    , wBundleId
    , wDirectoryId
    , wUserName

    -- * WorkspaceDirectory
    , WorkspaceDirectory
    , workspaceDirectory
    , wdAlias
    , wdCustomerUserName
    , wdDirectoryId
    , wdDirectoryName
    , wdDirectoryType
    , wdDnsIpAddresses
    , wdIamRoleId
    , wdRegistrationCode
    , wdState
    , wdSubnetIds
    , wdWorkspaceCreationProperties
    , wdWorkspaceSecurityGroupId

    -- * Compute
    , Compute (..)

    -- * Workspace
    , Workspace
    , workspace
    , w1BundleId
    , w1DirectoryId
    , w1ErrorCode
    , w1ErrorMessage
    , w1IpAddress
    , w1State
    , w1SubnetId
    , w1UserName
    , w1WorkspaceId

    -- * RebuildRequest
    , RebuildRequest
    , rebuildRequest
    , rWorkspaceId

    -- * FailedCreateWorkspaceRequest
    , FailedCreateWorkspaceRequest
    , failedCreateWorkspaceRequest
    , fcwErrorCode
    , fcwErrorMessage
    , fcwWorkspaceRequest

    -- * WorkspaceDirectoryState
    , WorkspaceDirectoryState (..)

    -- * FailedWorkspaceChangeRequest
    , FailedWorkspaceChangeRequest
    , failedWorkspaceChangeRequest
    , fwcErrorCode
    , fwcErrorMessage
    , fwcWorkspaceId

    -- * WorkspaceDirectoryType
    , WorkspaceDirectoryType (..)

    -- * WorkspaceState
    , WorkspaceState (..)

    -- * ComputeType
    , ComputeType
    , computeType
    , ctName

    -- * RebootRequest
    , RebootRequest
    , rebootRequest
    , r1WorkspaceId

    -- * UserStorage
    , UserStorage
    , userStorage
    , usCapacity

    -- * TerminateRequest
    , TerminateRequest
    , terminateRequest
    , tWorkspaceId

    -- * DefaultWorkspaceCreationProperties
    , DefaultWorkspaceCreationProperties
    , defaultWorkspaceCreationProperties
    , dwcpCustomSecurityGroupId
    , dwcpDefaultOu
    , dwcpEnableInternetAccess
    , dwcpEnableWorkDocs
    , dwcpUserEnabledAsLocalAdministrator

    -- * WorkspaceBundle
    , WorkspaceBundle
    , workspaceBundle
    , wbBundleId
    , wbComputeType
    , wbDescription
    , wbName
    , wbOwner
    , wbUserStorage
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing
import qualified GHC.Exts

-- | Version @2015-04-08@ of the Amazon WorkSpaces service.
data WorkSpaces

instance AWSService WorkSpaces where
    type Sg WorkSpaces = V4
    type Er WorkSpaces = JSONError

    service = service'
      where
        service' :: Service WorkSpaces
        service' = Service
            { _svcAbbrev       = "WorkSpaces"
            , _svcPrefix       = "workspaces"
            , _svcVersion      = "2015-04-08"
            , _svcTargetPrefix = Just "WorkspacesService"
            , _svcJSONVersion  = Just "1.1"
            , _svcHandle       = handle
            , _svcRetry        = retry
            }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError JSONError)
        handle = jsonError statusSuccess service'

        retry :: Retry WorkSpaces
        retry = Exponential
            { _retryBase     = 0.05
            , _retryGrowth   = 2
            , _retryAttempts = 5
            , _retryCheck    = check
            }

        check :: Status
              -> JSONError
              -> Bool
        check (statusCode -> s) (awsErrorCode -> e)
            | s == 500  = True -- General Server Error
            | s == 509  = True -- Limit Exceeded
            | s == 503  = True -- Service Unavailable
            | otherwise = False

data WorkspaceRequest = WorkspaceRequest
    { _wBundleId    :: Text
    , _wDirectoryId :: Text
    , _wUserName    :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'WorkspaceRequest' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'wBundleId' @::@ 'Text'
--
-- * 'wDirectoryId' @::@ 'Text'
--
-- * 'wUserName' @::@ 'Text'
--
workspaceRequest :: Text -- ^ 'wDirectoryId'
                 -> Text -- ^ 'wUserName'
                 -> Text -- ^ 'wBundleId'
                 -> WorkspaceRequest
workspaceRequest p1 p2 p3 = WorkspaceRequest
    { _wDirectoryId = p1
    , _wUserName    = p2
    , _wBundleId    = p3
    }

-- | The identifier of the bundle to create the WorkSpace from. You can use the 'DescribeWorkspaceBundles' operation to obtain a list of the bundles that are available.
wBundleId :: Lens' WorkspaceRequest Text
wBundleId = lens _wBundleId (\s a -> s { _wBundleId = a })

-- | The identifier of the AWS Directory Service directory to create the WorkSpace
-- in. You can use the 'DescribeWorkspaceDirectories' operation to obtain a list
-- of the directories that are available.
wDirectoryId :: Lens' WorkspaceRequest Text
wDirectoryId = lens _wDirectoryId (\s a -> s { _wDirectoryId = a })

-- | The username that the WorkSpace is assigned to. This username must exist in
-- the AWS Directory Service directory specified by the 'DirectoryId' member.
wUserName :: Lens' WorkspaceRequest Text
wUserName = lens _wUserName (\s a -> s { _wUserName = a })

instance FromJSON WorkspaceRequest where
    parseJSON = withObject "WorkspaceRequest" $ \o -> WorkspaceRequest
        <$> o .:  "BundleId"
        <*> o .:  "DirectoryId"
        <*> o .:  "UserName"

instance ToJSON WorkspaceRequest where
    toJSON WorkspaceRequest{..} = object
        [ "DirectoryId" .= _wDirectoryId
        , "UserName"    .= _wUserName
        , "BundleId"    .= _wBundleId
        ]

data WorkspaceDirectory = WorkspaceDirectory
    { _wdAlias                       :: Maybe Text
    , _wdCustomerUserName            :: Maybe Text
    , _wdDirectoryId                 :: Maybe Text
    , _wdDirectoryName               :: Maybe Text
    , _wdDirectoryType               :: Maybe WorkspaceDirectoryType
    , _wdDnsIpAddresses              :: List "DnsIpAddresses" Text
    , _wdIamRoleId                   :: Maybe Text
    , _wdRegistrationCode            :: Maybe Text
    , _wdState                       :: Maybe WorkspaceDirectoryState
    , _wdSubnetIds                   :: List "SubnetIds" Text
    , _wdWorkspaceCreationProperties :: Maybe DefaultWorkspaceCreationProperties
    , _wdWorkspaceSecurityGroupId    :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'WorkspaceDirectory' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'wdAlias' @::@ 'Maybe' 'Text'
--
-- * 'wdCustomerUserName' @::@ 'Maybe' 'Text'
--
-- * 'wdDirectoryId' @::@ 'Maybe' 'Text'
--
-- * 'wdDirectoryName' @::@ 'Maybe' 'Text'
--
-- * 'wdDirectoryType' @::@ 'Maybe' 'WorkspaceDirectoryType'
--
-- * 'wdDnsIpAddresses' @::@ ['Text']
--
-- * 'wdIamRoleId' @::@ 'Maybe' 'Text'
--
-- * 'wdRegistrationCode' @::@ 'Maybe' 'Text'
--
-- * 'wdState' @::@ 'Maybe' 'WorkspaceDirectoryState'
--
-- * 'wdSubnetIds' @::@ ['Text']
--
-- * 'wdWorkspaceCreationProperties' @::@ 'Maybe' 'DefaultWorkspaceCreationProperties'
--
-- * 'wdWorkspaceSecurityGroupId' @::@ 'Maybe' 'Text'
--
workspaceDirectory :: WorkspaceDirectory
workspaceDirectory = WorkspaceDirectory
    { _wdDirectoryId                 = Nothing
    , _wdAlias                       = Nothing
    , _wdDirectoryName               = Nothing
    , _wdRegistrationCode            = Nothing
    , _wdSubnetIds                   = mempty
    , _wdDnsIpAddresses              = mempty
    , _wdCustomerUserName            = Nothing
    , _wdIamRoleId                   = Nothing
    , _wdDirectoryType               = Nothing
    , _wdWorkspaceSecurityGroupId    = Nothing
    , _wdState                       = Nothing
    , _wdWorkspaceCreationProperties = Nothing
    }

-- | The directory alias.
wdAlias :: Lens' WorkspaceDirectory (Maybe Text)
wdAlias = lens _wdAlias (\s a -> s { _wdAlias = a })

-- | The user name for the service account.
wdCustomerUserName :: Lens' WorkspaceDirectory (Maybe Text)
wdCustomerUserName =
    lens _wdCustomerUserName (\s a -> s { _wdCustomerUserName = a })

-- | The directory identifier.
wdDirectoryId :: Lens' WorkspaceDirectory (Maybe Text)
wdDirectoryId = lens _wdDirectoryId (\s a -> s { _wdDirectoryId = a })

-- | The name of the directory.
wdDirectoryName :: Lens' WorkspaceDirectory (Maybe Text)
wdDirectoryName = lens _wdDirectoryName (\s a -> s { _wdDirectoryName = a })

-- | The directory type.
wdDirectoryType :: Lens' WorkspaceDirectory (Maybe WorkspaceDirectoryType)
wdDirectoryType = lens _wdDirectoryType (\s a -> s { _wdDirectoryType = a })

-- | An array of strings that contains the IP addresses of the DNS servers for the
-- directory.
wdDnsIpAddresses :: Lens' WorkspaceDirectory [Text]
wdDnsIpAddresses = lens _wdDnsIpAddresses (\s a -> s { _wdDnsIpAddresses = a }) . _List

-- | The identifier of the IAM role. This is the role that allows Amazon
-- WorkSpaces to make calls to other services, such as Amazon EC2, on your
-- behalf.
wdIamRoleId :: Lens' WorkspaceDirectory (Maybe Text)
wdIamRoleId = lens _wdIamRoleId (\s a -> s { _wdIamRoleId = a })

-- | The registration code for the directory. This is the code that users enter in
-- their Amazon WorkSpaces client application to connect to the directory.
wdRegistrationCode :: Lens' WorkspaceDirectory (Maybe Text)
wdRegistrationCode =
    lens _wdRegistrationCode (\s a -> s { _wdRegistrationCode = a })

-- | The state of the directory's registration with Amazon WorkSpaces
wdState :: Lens' WorkspaceDirectory (Maybe WorkspaceDirectoryState)
wdState = lens _wdState (\s a -> s { _wdState = a })

-- | An array of strings that contains the identifiers of the subnets used with
-- the directory.
wdSubnetIds :: Lens' WorkspaceDirectory [Text]
wdSubnetIds = lens _wdSubnetIds (\s a -> s { _wdSubnetIds = a }) . _List

-- | A structure that specifies the default creation properties for all WorkSpaces
-- in the directory.
wdWorkspaceCreationProperties :: Lens' WorkspaceDirectory (Maybe DefaultWorkspaceCreationProperties)
wdWorkspaceCreationProperties =
    lens _wdWorkspaceCreationProperties
        (\s a -> s { _wdWorkspaceCreationProperties = a })

-- | The identifier of the security group that is assigned to new WorkSpaces.
wdWorkspaceSecurityGroupId :: Lens' WorkspaceDirectory (Maybe Text)
wdWorkspaceSecurityGroupId =
    lens _wdWorkspaceSecurityGroupId
        (\s a -> s { _wdWorkspaceSecurityGroupId = a })

instance FromJSON WorkspaceDirectory where
    parseJSON = withObject "WorkspaceDirectory" $ \o -> WorkspaceDirectory
        <$> o .:? "Alias"
        <*> o .:? "CustomerUserName"
        <*> o .:? "DirectoryId"
        <*> o .:? "DirectoryName"
        <*> o .:? "DirectoryType"
        <*> o .:? "DnsIpAddresses" .!= mempty
        <*> o .:? "IamRoleId"
        <*> o .:? "RegistrationCode"
        <*> o .:? "State"
        <*> o .:? "SubnetIds" .!= mempty
        <*> o .:? "WorkspaceCreationProperties"
        <*> o .:? "WorkspaceSecurityGroupId"

instance ToJSON WorkspaceDirectory where
    toJSON WorkspaceDirectory{..} = object
        [ "DirectoryId"                 .= _wdDirectoryId
        , "Alias"                       .= _wdAlias
        , "DirectoryName"               .= _wdDirectoryName
        , "RegistrationCode"            .= _wdRegistrationCode
        , "SubnetIds"                   .= _wdSubnetIds
        , "DnsIpAddresses"              .= _wdDnsIpAddresses
        , "CustomerUserName"            .= _wdCustomerUserName
        , "IamRoleId"                   .= _wdIamRoleId
        , "DirectoryType"               .= _wdDirectoryType
        , "WorkspaceSecurityGroupId"    .= _wdWorkspaceSecurityGroupId
        , "State"                       .= _wdState
        , "WorkspaceCreationProperties" .= _wdWorkspaceCreationProperties
        ]

data Compute
    = Performance -- ^ PERFORMANCE
    | Standard    -- ^ STANDARD
    | Value       -- ^ VALUE
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable Compute

instance FromText Compute where
    parser = takeLowerText >>= \case
        "performance" -> pure Performance
        "standard"    -> pure Standard
        "value"       -> pure Value
        e             -> fail $
            "Failure parsing Compute from " ++ show e

instance ToText Compute where
    toText = \case
        Performance -> "PERFORMANCE"
        Standard    -> "STANDARD"
        Value       -> "VALUE"

instance ToByteString Compute
instance ToHeader     Compute
instance ToQuery      Compute

instance FromJSON Compute where
    parseJSON = parseJSONText "Compute"

instance ToJSON Compute where
    toJSON = toJSONText

data Workspace = Workspace
    { _w1BundleId     :: Maybe Text
    , _w1DirectoryId  :: Maybe Text
    , _w1ErrorCode    :: Maybe Text
    , _w1ErrorMessage :: Maybe Text
    , _w1IpAddress    :: Maybe Text
    , _w1State        :: Maybe WorkspaceState
    , _w1SubnetId     :: Maybe Text
    , _w1UserName     :: Maybe Text
    , _w1WorkspaceId  :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'Workspace' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'w1BundleId' @::@ 'Maybe' 'Text'
--
-- * 'w1DirectoryId' @::@ 'Maybe' 'Text'
--
-- * 'w1ErrorCode' @::@ 'Maybe' 'Text'
--
-- * 'w1ErrorMessage' @::@ 'Maybe' 'Text'
--
-- * 'w1IpAddress' @::@ 'Maybe' 'Text'
--
-- * 'w1State' @::@ 'Maybe' 'WorkspaceState'
--
-- * 'w1SubnetId' @::@ 'Maybe' 'Text'
--
-- * 'w1UserName' @::@ 'Maybe' 'Text'
--
-- * 'w1WorkspaceId' @::@ 'Maybe' 'Text'
--
workspace :: Workspace
workspace = Workspace
    { _w1WorkspaceId  = Nothing
    , _w1DirectoryId  = Nothing
    , _w1UserName     = Nothing
    , _w1IpAddress    = Nothing
    , _w1State        = Nothing
    , _w1BundleId     = Nothing
    , _w1SubnetId     = Nothing
    , _w1ErrorMessage = Nothing
    , _w1ErrorCode    = Nothing
    }

-- | The identifier of the bundle that the WorkSpace was created from.
w1BundleId :: Lens' Workspace (Maybe Text)
w1BundleId = lens _w1BundleId (\s a -> s { _w1BundleId = a })

-- | The identifier of the AWS Directory Service directory that the WorkSpace
-- belongs to.
w1DirectoryId :: Lens' Workspace (Maybe Text)
w1DirectoryId = lens _w1DirectoryId (\s a -> s { _w1DirectoryId = a })

-- | If the WorkSpace could not be created, this contains the error code.
w1ErrorCode :: Lens' Workspace (Maybe Text)
w1ErrorCode = lens _w1ErrorCode (\s a -> s { _w1ErrorCode = a })

-- | If the WorkSpace could not be created, this contains a textual error message
-- that describes the failure.
w1ErrorMessage :: Lens' Workspace (Maybe Text)
w1ErrorMessage = lens _w1ErrorMessage (\s a -> s { _w1ErrorMessage = a })

-- | The IP address of the WorkSpace.
w1IpAddress :: Lens' Workspace (Maybe Text)
w1IpAddress = lens _w1IpAddress (\s a -> s { _w1IpAddress = a })

-- | The operational state of the WorkSpace.
w1State :: Lens' Workspace (Maybe WorkspaceState)
w1State = lens _w1State (\s a -> s { _w1State = a })

-- | The identifier of the subnet that the WorkSpace is in.
w1SubnetId :: Lens' Workspace (Maybe Text)
w1SubnetId = lens _w1SubnetId (\s a -> s { _w1SubnetId = a })

-- | The user that the WorkSpace is assigned to.
w1UserName :: Lens' Workspace (Maybe Text)
w1UserName = lens _w1UserName (\s a -> s { _w1UserName = a })

-- | The identifier of the WorkSpace.
w1WorkspaceId :: Lens' Workspace (Maybe Text)
w1WorkspaceId = lens _w1WorkspaceId (\s a -> s { _w1WorkspaceId = a })

instance FromJSON Workspace where
    parseJSON = withObject "Workspace" $ \o -> Workspace
        <$> o .:? "BundleId"
        <*> o .:? "DirectoryId"
        <*> o .:? "ErrorCode"
        <*> o .:? "ErrorMessage"
        <*> o .:? "IpAddress"
        <*> o .:? "State"
        <*> o .:? "SubnetId"
        <*> o .:? "UserName"
        <*> o .:? "WorkspaceId"

instance ToJSON Workspace where
    toJSON Workspace{..} = object
        [ "WorkspaceId"  .= _w1WorkspaceId
        , "DirectoryId"  .= _w1DirectoryId
        , "UserName"     .= _w1UserName
        , "IpAddress"    .= _w1IpAddress
        , "State"        .= _w1State
        , "BundleId"     .= _w1BundleId
        , "SubnetId"     .= _w1SubnetId
        , "ErrorMessage" .= _w1ErrorMessage
        , "ErrorCode"    .= _w1ErrorCode
        ]

newtype RebuildRequest = RebuildRequest
    { _rWorkspaceId :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'RebuildRequest' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rWorkspaceId' @::@ 'Text'
--
rebuildRequest :: Text -- ^ 'rWorkspaceId'
               -> RebuildRequest
rebuildRequest p1 = RebuildRequest
    { _rWorkspaceId = p1
    }

-- | The identifier of the WorkSpace to rebuild.
rWorkspaceId :: Lens' RebuildRequest Text
rWorkspaceId = lens _rWorkspaceId (\s a -> s { _rWorkspaceId = a })

instance FromJSON RebuildRequest where
    parseJSON = withObject "RebuildRequest" $ \o -> RebuildRequest
        <$> o .:  "WorkspaceId"

instance ToJSON RebuildRequest where
    toJSON RebuildRequest{..} = object
        [ "WorkspaceId" .= _rWorkspaceId
        ]

data FailedCreateWorkspaceRequest = FailedCreateWorkspaceRequest
    { _fcwErrorCode        :: Maybe Text
    , _fcwErrorMessage     :: Maybe Text
    , _fcwWorkspaceRequest :: Maybe WorkspaceRequest
    } deriving (Eq, Read, Show)

-- | 'FailedCreateWorkspaceRequest' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'fcwErrorCode' @::@ 'Maybe' 'Text'
--
-- * 'fcwErrorMessage' @::@ 'Maybe' 'Text'
--
-- * 'fcwWorkspaceRequest' @::@ 'Maybe' 'WorkspaceRequest'
--
failedCreateWorkspaceRequest :: FailedCreateWorkspaceRequest
failedCreateWorkspaceRequest = FailedCreateWorkspaceRequest
    { _fcwWorkspaceRequest = Nothing
    , _fcwErrorCode        = Nothing
    , _fcwErrorMessage     = Nothing
    }

-- | The error code.
fcwErrorCode :: Lens' FailedCreateWorkspaceRequest (Maybe Text)
fcwErrorCode = lens _fcwErrorCode (\s a -> s { _fcwErrorCode = a })

-- | The textual error message.
fcwErrorMessage :: Lens' FailedCreateWorkspaceRequest (Maybe Text)
fcwErrorMessage = lens _fcwErrorMessage (\s a -> s { _fcwErrorMessage = a })

-- | A 'WorkspaceRequest' object that contains the information about the WorkSpace
-- that could not be created.
fcwWorkspaceRequest :: Lens' FailedCreateWorkspaceRequest (Maybe WorkspaceRequest)
fcwWorkspaceRequest =
    lens _fcwWorkspaceRequest (\s a -> s { _fcwWorkspaceRequest = a })

instance FromJSON FailedCreateWorkspaceRequest where
    parseJSON = withObject "FailedCreateWorkspaceRequest" $ \o -> FailedCreateWorkspaceRequest
        <$> o .:? "ErrorCode"
        <*> o .:? "ErrorMessage"
        <*> o .:? "WorkspaceRequest"

instance ToJSON FailedCreateWorkspaceRequest where
    toJSON FailedCreateWorkspaceRequest{..} = object
        [ "WorkspaceRequest" .= _fcwWorkspaceRequest
        , "ErrorCode"        .= _fcwErrorCode
        , "ErrorMessage"     .= _fcwErrorMessage
        ]

data WorkspaceDirectoryState
    = Deregistered  -- ^ DEREGISTERED
    | Deregistering -- ^ DEREGISTERING
    | Error         -- ^ ERROR
    | Registered    -- ^ REGISTERED
    | Registering   -- ^ REGISTERING
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable WorkspaceDirectoryState

instance FromText WorkspaceDirectoryState where
    parser = takeLowerText >>= \case
        "deregistered"  -> pure Deregistered
        "deregistering" -> pure Deregistering
        "error"         -> pure Error
        "registered"    -> pure Registered
        "registering"   -> pure Registering
        e               -> fail $
            "Failure parsing WorkspaceDirectoryState from " ++ show e

instance ToText WorkspaceDirectoryState where
    toText = \case
        Deregistered  -> "DEREGISTERED"
        Deregistering -> "DEREGISTERING"
        Error         -> "ERROR"
        Registered    -> "REGISTERED"
        Registering   -> "REGISTERING"

instance ToByteString WorkspaceDirectoryState
instance ToHeader     WorkspaceDirectoryState
instance ToQuery      WorkspaceDirectoryState

instance FromJSON WorkspaceDirectoryState where
    parseJSON = parseJSONText "WorkspaceDirectoryState"

instance ToJSON WorkspaceDirectoryState where
    toJSON = toJSONText

data FailedWorkspaceChangeRequest = FailedWorkspaceChangeRequest
    { _fwcErrorCode    :: Maybe Text
    , _fwcErrorMessage :: Maybe Text
    , _fwcWorkspaceId  :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'FailedWorkspaceChangeRequest' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'fwcErrorCode' @::@ 'Maybe' 'Text'
--
-- * 'fwcErrorMessage' @::@ 'Maybe' 'Text'
--
-- * 'fwcWorkspaceId' @::@ 'Maybe' 'Text'
--
failedWorkspaceChangeRequest :: FailedWorkspaceChangeRequest
failedWorkspaceChangeRequest = FailedWorkspaceChangeRequest
    { _fwcWorkspaceId  = Nothing
    , _fwcErrorCode    = Nothing
    , _fwcErrorMessage = Nothing
    }

-- | The error code.
fwcErrorCode :: Lens' FailedWorkspaceChangeRequest (Maybe Text)
fwcErrorCode = lens _fwcErrorCode (\s a -> s { _fwcErrorCode = a })

-- | The textual error message.
fwcErrorMessage :: Lens' FailedWorkspaceChangeRequest (Maybe Text)
fwcErrorMessage = lens _fwcErrorMessage (\s a -> s { _fwcErrorMessage = a })

-- | The identifier of the WorkSpace.
fwcWorkspaceId :: Lens' FailedWorkspaceChangeRequest (Maybe Text)
fwcWorkspaceId = lens _fwcWorkspaceId (\s a -> s { _fwcWorkspaceId = a })

instance FromJSON FailedWorkspaceChangeRequest where
    parseJSON = withObject "FailedWorkspaceChangeRequest" $ \o -> FailedWorkspaceChangeRequest
        <$> o .:? "ErrorCode"
        <*> o .:? "ErrorMessage"
        <*> o .:? "WorkspaceId"

instance ToJSON FailedWorkspaceChangeRequest where
    toJSON FailedWorkspaceChangeRequest{..} = object
        [ "WorkspaceId"  .= _fwcWorkspaceId
        , "ErrorCode"    .= _fwcErrorCode
        , "ErrorMessage" .= _fwcErrorMessage
        ]

data WorkspaceDirectoryType
    = AdConnector -- ^ AD_CONNECTOR
    | SimpleAd    -- ^ SIMPLE_AD
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable WorkspaceDirectoryType

instance FromText WorkspaceDirectoryType where
    parser = takeLowerText >>= \case
        "ad_connector" -> pure AdConnector
        "simple_ad"    -> pure SimpleAd
        e              -> fail $
            "Failure parsing WorkspaceDirectoryType from " ++ show e

instance ToText WorkspaceDirectoryType where
    toText = \case
        AdConnector -> "AD_CONNECTOR"
        SimpleAd    -> "SIMPLE_AD"

instance ToByteString WorkspaceDirectoryType
instance ToHeader     WorkspaceDirectoryType
instance ToQuery      WorkspaceDirectoryType

instance FromJSON WorkspaceDirectoryType where
    parseJSON = parseJSONText "WorkspaceDirectoryType"

instance ToJSON WorkspaceDirectoryType where
    toJSON = toJSONText

data WorkspaceState
    = WSAvailable   -- ^ AVAILABLE
    | WSError       -- ^ ERROR
    | WSImpaired    -- ^ IMPAIRED
    | WSPending     -- ^ PENDING
    | WSRebooting   -- ^ REBOOTING
    | WSRebuilding  -- ^ REBUILDING
    | WSSuspended   -- ^ SUSPENDED
    | WSTerminated  -- ^ TERMINATED
    | WSTerminating -- ^ TERMINATING
    | WSUnhealthy   -- ^ UNHEALTHY
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable WorkspaceState

instance FromText WorkspaceState where
    parser = takeLowerText >>= \case
        "available"   -> pure WSAvailable
        "error"       -> pure WSError
        "impaired"    -> pure WSImpaired
        "pending"     -> pure WSPending
        "rebooting"   -> pure WSRebooting
        "rebuilding"  -> pure WSRebuilding
        "suspended"   -> pure WSSuspended
        "terminated"  -> pure WSTerminated
        "terminating" -> pure WSTerminating
        "unhealthy"   -> pure WSUnhealthy
        e             -> fail $
            "Failure parsing WorkspaceState from " ++ show e

instance ToText WorkspaceState where
    toText = \case
        WSAvailable   -> "AVAILABLE"
        WSError       -> "ERROR"
        WSImpaired    -> "IMPAIRED"
        WSPending     -> "PENDING"
        WSRebooting   -> "REBOOTING"
        WSRebuilding  -> "REBUILDING"
        WSSuspended   -> "SUSPENDED"
        WSTerminated  -> "TERMINATED"
        WSTerminating -> "TERMINATING"
        WSUnhealthy   -> "UNHEALTHY"

instance ToByteString WorkspaceState
instance ToHeader     WorkspaceState
instance ToQuery      WorkspaceState

instance FromJSON WorkspaceState where
    parseJSON = parseJSONText "WorkspaceState"

instance ToJSON WorkspaceState where
    toJSON = toJSONText

newtype ComputeType = ComputeType
    { _ctName :: Maybe Compute
    } deriving (Eq, Read, Show)

-- | 'ComputeType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ctName' @::@ 'Maybe' 'Compute'
--
computeType :: ComputeType
computeType = ComputeType
    { _ctName = Nothing
    }

-- | The name of the compute type for the bundle.
ctName :: Lens' ComputeType (Maybe Compute)
ctName = lens _ctName (\s a -> s { _ctName = a })

instance FromJSON ComputeType where
    parseJSON = withObject "ComputeType" $ \o -> ComputeType
        <$> o .:? "Name"

instance ToJSON ComputeType where
    toJSON ComputeType{..} = object
        [ "Name" .= _ctName
        ]

newtype RebootRequest = RebootRequest
    { _r1WorkspaceId :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'RebootRequest' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'r1WorkspaceId' @::@ 'Text'
--
rebootRequest :: Text -- ^ 'r1WorkspaceId'
              -> RebootRequest
rebootRequest p1 = RebootRequest
    { _r1WorkspaceId = p1
    }

-- | The identifier of the WorkSpace to reboot.
r1WorkspaceId :: Lens' RebootRequest Text
r1WorkspaceId = lens _r1WorkspaceId (\s a -> s { _r1WorkspaceId = a })

instance FromJSON RebootRequest where
    parseJSON = withObject "RebootRequest" $ \o -> RebootRequest
        <$> o .:  "WorkspaceId"

instance ToJSON RebootRequest where
    toJSON RebootRequest{..} = object
        [ "WorkspaceId" .= _r1WorkspaceId
        ]

newtype UserStorage = UserStorage
    { _usCapacity :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'UserStorage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'usCapacity' @::@ 'Maybe' 'Text'
--
userStorage :: UserStorage
userStorage = UserStorage
    { _usCapacity = Nothing
    }

-- | The amount of user storage for the bundle.
usCapacity :: Lens' UserStorage (Maybe Text)
usCapacity = lens _usCapacity (\s a -> s { _usCapacity = a })

instance FromJSON UserStorage where
    parseJSON = withObject "UserStorage" $ \o -> UserStorage
        <$> o .:? "Capacity"

instance ToJSON UserStorage where
    toJSON UserStorage{..} = object
        [ "Capacity" .= _usCapacity
        ]

newtype TerminateRequest = TerminateRequest
    { _tWorkspaceId :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'TerminateRequest' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tWorkspaceId' @::@ 'Text'
--
terminateRequest :: Text -- ^ 'tWorkspaceId'
                 -> TerminateRequest
terminateRequest p1 = TerminateRequest
    { _tWorkspaceId = p1
    }

-- | The identifier of the WorkSpace to terminate.
tWorkspaceId :: Lens' TerminateRequest Text
tWorkspaceId = lens _tWorkspaceId (\s a -> s { _tWorkspaceId = a })

instance FromJSON TerminateRequest where
    parseJSON = withObject "TerminateRequest" $ \o -> TerminateRequest
        <$> o .:  "WorkspaceId"

instance ToJSON TerminateRequest where
    toJSON TerminateRequest{..} = object
        [ "WorkspaceId" .= _tWorkspaceId
        ]

data DefaultWorkspaceCreationProperties = DefaultWorkspaceCreationProperties
    { _dwcpCustomSecurityGroupId           :: Maybe Text
    , _dwcpDefaultOu                       :: Maybe Text
    , _dwcpEnableInternetAccess            :: Maybe Bool
    , _dwcpEnableWorkDocs                  :: Maybe Bool
    , _dwcpUserEnabledAsLocalAdministrator :: Maybe Bool
    } deriving (Eq, Ord, Read, Show)

-- | 'DefaultWorkspaceCreationProperties' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dwcpCustomSecurityGroupId' @::@ 'Maybe' 'Text'
--
-- * 'dwcpDefaultOu' @::@ 'Maybe' 'Text'
--
-- * 'dwcpEnableInternetAccess' @::@ 'Maybe' 'Bool'
--
-- * 'dwcpEnableWorkDocs' @::@ 'Maybe' 'Bool'
--
-- * 'dwcpUserEnabledAsLocalAdministrator' @::@ 'Maybe' 'Bool'
--
defaultWorkspaceCreationProperties :: DefaultWorkspaceCreationProperties
defaultWorkspaceCreationProperties = DefaultWorkspaceCreationProperties
    { _dwcpEnableWorkDocs                  = Nothing
    , _dwcpEnableInternetAccess            = Nothing
    , _dwcpDefaultOu                       = Nothing
    , _dwcpCustomSecurityGroupId           = Nothing
    , _dwcpUserEnabledAsLocalAdministrator = Nothing
    }

-- | The identifier of any custom security groups that are applied to the
-- WorkSpaces when they are created.
dwcpCustomSecurityGroupId :: Lens' DefaultWorkspaceCreationProperties (Maybe Text)
dwcpCustomSecurityGroupId =
    lens _dwcpCustomSecurityGroupId
        (\s a -> s { _dwcpCustomSecurityGroupId = a })

-- | The organizational unit (OU) in the directory that the WorkSpace machine
-- accounts are placed in.
dwcpDefaultOu :: Lens' DefaultWorkspaceCreationProperties (Maybe Text)
dwcpDefaultOu = lens _dwcpDefaultOu (\s a -> s { _dwcpDefaultOu = a })

-- | A public IP address will be attached to all WorkSpaces that are created or
-- rebuilt.
dwcpEnableInternetAccess :: Lens' DefaultWorkspaceCreationProperties (Maybe Bool)
dwcpEnableInternetAccess =
    lens _dwcpEnableInternetAccess
        (\s a -> s { _dwcpEnableInternetAccess = a })

-- | Specifies if the directory is enabled for Amazon WorkDocs.
dwcpEnableWorkDocs :: Lens' DefaultWorkspaceCreationProperties (Maybe Bool)
dwcpEnableWorkDocs =
    lens _dwcpEnableWorkDocs (\s a -> s { _dwcpEnableWorkDocs = a })

-- | The WorkSpace user is an administrator on the WorkSpace.
dwcpUserEnabledAsLocalAdministrator :: Lens' DefaultWorkspaceCreationProperties (Maybe Bool)
dwcpUserEnabledAsLocalAdministrator =
    lens _dwcpUserEnabledAsLocalAdministrator
        (\s a -> s { _dwcpUserEnabledAsLocalAdministrator = a })

instance FromJSON DefaultWorkspaceCreationProperties where
    parseJSON = withObject "DefaultWorkspaceCreationProperties" $ \o -> DefaultWorkspaceCreationProperties
        <$> o .:? "CustomSecurityGroupId"
        <*> o .:? "DefaultOu"
        <*> o .:? "EnableInternetAccess"
        <*> o .:? "EnableWorkDocs"
        <*> o .:? "UserEnabledAsLocalAdministrator"

instance ToJSON DefaultWorkspaceCreationProperties where
    toJSON DefaultWorkspaceCreationProperties{..} = object
        [ "EnableWorkDocs"                  .= _dwcpEnableWorkDocs
        , "EnableInternetAccess"            .= _dwcpEnableInternetAccess
        , "DefaultOu"                       .= _dwcpDefaultOu
        , "CustomSecurityGroupId"           .= _dwcpCustomSecurityGroupId
        , "UserEnabledAsLocalAdministrator" .= _dwcpUserEnabledAsLocalAdministrator
        ]

data WorkspaceBundle = WorkspaceBundle
    { _wbBundleId    :: Maybe Text
    , _wbComputeType :: Maybe ComputeType
    , _wbDescription :: Maybe Text
    , _wbName        :: Maybe Text
    , _wbOwner       :: Maybe Text
    , _wbUserStorage :: Maybe UserStorage
    } deriving (Eq, Read, Show)

-- | 'WorkspaceBundle' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'wbBundleId' @::@ 'Maybe' 'Text'
--
-- * 'wbComputeType' @::@ 'Maybe' 'ComputeType'
--
-- * 'wbDescription' @::@ 'Maybe' 'Text'
--
-- * 'wbName' @::@ 'Maybe' 'Text'
--
-- * 'wbOwner' @::@ 'Maybe' 'Text'
--
-- * 'wbUserStorage' @::@ 'Maybe' 'UserStorage'
--
workspaceBundle :: WorkspaceBundle
workspaceBundle = WorkspaceBundle
    { _wbBundleId    = Nothing
    , _wbName        = Nothing
    , _wbOwner       = Nothing
    , _wbDescription = Nothing
    , _wbUserStorage = Nothing
    , _wbComputeType = Nothing
    }

-- | The bundle identifier.
wbBundleId :: Lens' WorkspaceBundle (Maybe Text)
wbBundleId = lens _wbBundleId (\s a -> s { _wbBundleId = a })

-- | A 'ComputeType' object that specifies the compute type for the bundle.
wbComputeType :: Lens' WorkspaceBundle (Maybe ComputeType)
wbComputeType = lens _wbComputeType (\s a -> s { _wbComputeType = a })

-- | The bundle description.
wbDescription :: Lens' WorkspaceBundle (Maybe Text)
wbDescription = lens _wbDescription (\s a -> s { _wbDescription = a })

-- | The name of the bundle.
wbName :: Lens' WorkspaceBundle (Maybe Text)
wbName = lens _wbName (\s a -> s { _wbName = a })

-- | The owner of the bundle. This contains the owner's account identifier, or 'AMAZON' if the bundle is provided by AWS.
wbOwner :: Lens' WorkspaceBundle (Maybe Text)
wbOwner = lens _wbOwner (\s a -> s { _wbOwner = a })

-- | A 'UserStorage' object that specifies the amount of user storage that the
-- bundle contains.
wbUserStorage :: Lens' WorkspaceBundle (Maybe UserStorage)
wbUserStorage = lens _wbUserStorage (\s a -> s { _wbUserStorage = a })

instance FromJSON WorkspaceBundle where
    parseJSON = withObject "WorkspaceBundle" $ \o -> WorkspaceBundle
        <$> o .:? "BundleId"
        <*> o .:? "ComputeType"
        <*> o .:? "Description"
        <*> o .:? "Name"
        <*> o .:? "Owner"
        <*> o .:? "UserStorage"

instance ToJSON WorkspaceBundle where
    toJSON WorkspaceBundle{..} = object
        [ "BundleId"    .= _wbBundleId
        , "Name"        .= _wbName
        , "Owner"       .= _wbOwner
        , "Description" .= _wbDescription
        , "UserStorage" .= _wbUserStorage
        , "ComputeType" .= _wbComputeType
        ]

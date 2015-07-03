{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.DirectoryService.Types
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

module Network.AWS.DirectoryService.Types
    (
    -- * Service
      DirectoryService

    -- * Errors
    , _AuthenticationFailedException
    , _DirectoryUnavailableException
    , _InvalidParameterException
    , _UnsupportedOperationException
    , _EntityAlreadyExistsException
    , _DirectoryLimitExceededException
    , _EntityDoesNotExistException
    , _InsufficientPermissionsException
    , _InvalidNextTokenException
    , _ServiceException
    , _SnapshotLimitExceededException
    , _ClientException

    -- * DirectorySize
    , DirectorySize (..)

    -- * DirectoryStage
    , DirectoryStage (..)

    -- * DirectoryType
    , DirectoryType (..)

    -- * RadiusAuthenticationProtocol
    , RadiusAuthenticationProtocol (..)

    -- * RadiusStatus
    , RadiusStatus (..)

    -- * SnapshotStatus
    , SnapshotStatus (..)

    -- * SnapshotType
    , SnapshotType (..)

    -- * Attribute
    , Attribute
    , attribute
    , attValue
    , attName

    -- * Computer
    , Computer
    , computer
    , comComputerId
    , comComputerAttributes
    , comComputerName

    -- * DirectoryConnectSettings
    , DirectoryConnectSettings
    , directoryConnectSettings
    , dcsVPCId
    , dcsSubnetIds
    , dcsCustomerDNSIPs
    , dcsCustomerUserName

    -- * DirectoryConnectSettingsDescription
    , DirectoryConnectSettingsDescription
    , directoryConnectSettingsDescription
    , dcsdCustomerUserName
    , dcsdSubnetIds
    , dcsdVPCId
    , dcsdConnectIPs
    , dcsdSecurityGroupId
    , dcsdAvailabilityZones

    -- * DirectoryDescription
    , DirectoryDescription
    , directoryDescription
    , ddRadiusStatus
    , ddDirectoryId
    , ddStage
    , ddAccessURL
    , ddShortName
    , ddSize
    , ddRadiusSettings
    , ddLaunchTime
    , ddAlias
    , ddName
    , ddSsoEnabled
    , ddStageLastUpdatedDateTime
    , ddStageReason
    , ddDNSIPAddrs
    , ddVPCSettings
    , ddType
    , ddConnectSettings
    , ddDescription

    -- * DirectoryLimits
    , DirectoryLimits
    , directoryLimits
    , dlConnectedDirectoriesCurrentCount
    , dlConnectedDirectoriesLimit
    , dlConnectedDirectoriesLimitReached
    , dlCloudOnlyDirectoriesLimit
    , dlCloudOnlyDirectoriesCurrentCount
    , dlCloudOnlyDirectoriesLimitReached

    -- * DirectoryVPCSettings
    , DirectoryVPCSettings
    , directoryVPCSettings
    , dvsVPCId
    , dvsSubnetIds

    -- * DirectoryVPCSettingsDescription
    , DirectoryVPCSettingsDescription
    , directoryVPCSettingsDescription
    , dvsdSubnetIds
    , dvsdVPCId
    , dvsdSecurityGroupId
    , dvsdAvailabilityZones

    -- * RadiusSettings
    , RadiusSettings
    , radiusSettings
    , rsDisplayLabel
    , rsRadiusServers
    , rsRadiusRetries
    , rsAuthenticationProtocol
    , rsUseSameUsername
    , rsSharedSecret
    , rsRadiusTimeout
    , rsRadiusPort

    -- * Snapshot
    , Snapshot
    , snapshot
    , snaDirectoryId
    , snaStatus
    , snaStartTime
    , snaName
    , snaType
    , snaSnapshotId

    -- * SnapshotLimits
    , SnapshotLimits
    , snapshotLimits
    , slManualSnapshotsLimitReached
    , slManualSnapshotsCurrentCount
    , slManualSnapshotsLimit
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2015-04-16@ of the Amazon Directory Service SDK.
data DirectoryService

instance AWSService DirectoryService where
    type Sg DirectoryService = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "DirectoryService"
            , _svcPrefix = "ds"
            , _svcVersion = "2015-04-16"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = Just 70000000
            , _svcStatus = statusSuccess
            , _svcError = parseJSONError
            , _svcRetry = retry
            }
        retry =
            Exponential
            { _retryBase = 5.0e-2
            , _retryGrowth = 2
            , _retryAttempts = 5
            , _retryCheck = check
            }
        check e
          | has (hasCode "ThrottlingException" . hasStatus 400) e =
              Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | An authentication error occurred.
_AuthenticationFailedException :: AWSError a => Getting (First ServiceError) a ServiceError
_AuthenticationFailedException =
    _ServiceError . hasCode "AuthenticationFailedException"

-- | The specified directory is unavailable or could not be found.
_DirectoryUnavailableException :: AWSError a => Getting (First ServiceError) a ServiceError
_DirectoryUnavailableException =
    _ServiceError . hasCode "DirectoryUnavailableException"

-- | One or more parameters are not valid.
_InvalidParameterException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidParameterException =
    _ServiceError . hasCode "InvalidParameterException"

-- | The operation is not supported.
_UnsupportedOperationException :: AWSError a => Getting (First ServiceError) a ServiceError
_UnsupportedOperationException =
    _ServiceError . hasCode "UnsupportedOperationException"

-- | The specified entity already exists.
_EntityAlreadyExistsException :: AWSError a => Getting (First ServiceError) a ServiceError
_EntityAlreadyExistsException =
    _ServiceError . hasCode "EntityAlreadyExistsException"

-- | The maximum number of directories in the region has been reached. You
-- can use the GetDirectoryLimits operation to determine your directory
-- limits in the region.
_DirectoryLimitExceededException :: AWSError a => Getting (First ServiceError) a ServiceError
_DirectoryLimitExceededException =
    _ServiceError . hasCode "DirectoryLimitExceededException"

-- | The specified entity could not be found.
_EntityDoesNotExistException :: AWSError a => Getting (First ServiceError) a ServiceError
_EntityDoesNotExistException =
    _ServiceError . hasCode "EntityDoesNotExistException"

-- | The account does not have sufficient permission to perform the
-- operation.
_InsufficientPermissionsException :: AWSError a => Getting (First ServiceError) a ServiceError
_InsufficientPermissionsException =
    _ServiceError . hasCode "InsufficientPermissionsException"

-- | The /NextToken/ value is not valid.
_InvalidNextTokenException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidNextTokenException =
    _ServiceError . hasCode "InvalidNextTokenException"

-- | An exception has occurred in AWS Directory Service.
_ServiceException :: AWSError a => Getting (First ServiceError) a ServiceError
_ServiceException = _ServiceError . hasCode "ServiceException"

-- | The maximum number of manual snapshots for the directory has been
-- reached. You can use the GetSnapshotLimits operation to determine the
-- snapshot limits for a directory.
_SnapshotLimitExceededException :: AWSError a => Getting (First ServiceError) a ServiceError
_SnapshotLimitExceededException =
    _ServiceError . hasCode "SnapshotLimitExceededException"

-- | A client exception has occurred.
_ClientException :: AWSError a => Getting (First ServiceError) a ServiceError
_ClientException = _ServiceError . hasCode "ClientException"

data DirectorySize
    = Small
    | Large
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText DirectorySize where
    parser = takeLowerText >>= \case
        "large" -> pure Large
        "small" -> pure Small
        e -> fromTextError $ "Failure parsing DirectorySize from value: '" <> e
           <> "'. Accepted values: large, small"

instance ToText DirectorySize where
    toText = \case
        Large -> "large"
        Small -> "small"

instance Hashable DirectorySize where
    hashWithSalt = hashUsing fromEnum

instance ToQuery DirectorySize
instance ToHeader DirectorySize

instance ToJSON DirectorySize where
    toJSON = toJSONText

instance FromJSON DirectorySize where
    parseJSON = parseJSONText "DirectorySize"

data DirectoryStage
    = DSRestoreFailed
    | DSDeleted
    | DSRestoring
    | DSImpaired
    | DSDeleting
    | DSFailed
    | DSRequested
    | DSCreated
    | DSInoperable
    | DSActive
    | DSCreating
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText DirectoryStage where
    parser = takeLowerText >>= \case
        "active" -> pure DSActive
        "created" -> pure DSCreated
        "creating" -> pure DSCreating
        "deleted" -> pure DSDeleted
        "deleting" -> pure DSDeleting
        "failed" -> pure DSFailed
        "impaired" -> pure DSImpaired
        "inoperable" -> pure DSInoperable
        "requested" -> pure DSRequested
        "restorefailed" -> pure DSRestoreFailed
        "restoring" -> pure DSRestoring
        e -> fromTextError $ "Failure parsing DirectoryStage from value: '" <> e
           <> "'. Accepted values: active, created, creating, deleted, deleting, failed, impaired, inoperable, requested, restorefailed, restoring"

instance ToText DirectoryStage where
    toText = \case
        DSActive -> "active"
        DSCreated -> "created"
        DSCreating -> "creating"
        DSDeleted -> "deleted"
        DSDeleting -> "deleting"
        DSFailed -> "failed"
        DSImpaired -> "impaired"
        DSInoperable -> "inoperable"
        DSRequested -> "requested"
        DSRestoreFailed -> "restorefailed"
        DSRestoring -> "restoring"

instance Hashable DirectoryStage where
    hashWithSalt = hashUsing fromEnum

instance ToQuery DirectoryStage
instance ToHeader DirectoryStage

instance FromJSON DirectoryStage where
    parseJSON = parseJSONText "DirectoryStage"

data DirectoryType
    = ADConnector
    | SimpleAD
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText DirectoryType where
    parser = takeLowerText >>= \case
        "adconnector" -> pure ADConnector
        "simplead" -> pure SimpleAD
        e -> fromTextError $ "Failure parsing DirectoryType from value: '" <> e
           <> "'. Accepted values: adconnector, simplead"

instance ToText DirectoryType where
    toText = \case
        ADConnector -> "adconnector"
        SimpleAD -> "simplead"

instance Hashable DirectoryType where
    hashWithSalt = hashUsing fromEnum

instance ToQuery DirectoryType
instance ToHeader DirectoryType

instance FromJSON DirectoryType where
    parseJSON = parseJSONText "DirectoryType"

data RadiusAuthenticationProtocol
    = Chap
    | MSCHAPV1
    | MSCHAPV2
    | Pap
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText RadiusAuthenticationProtocol where
    parser = takeLowerText >>= \case
        "chap" -> pure Chap
        "ms-chapv1" -> pure MSCHAPV1
        "ms-chapv2" -> pure MSCHAPV2
        "pap" -> pure Pap
        e -> fromTextError $ "Failure parsing RadiusAuthenticationProtocol from value: '" <> e
           <> "'. Accepted values: chap, ms-chapv1, ms-chapv2, pap"

instance ToText RadiusAuthenticationProtocol where
    toText = \case
        Chap -> "chap"
        MSCHAPV1 -> "ms-chapv1"
        MSCHAPV2 -> "ms-chapv2"
        Pap -> "pap"

instance Hashable RadiusAuthenticationProtocol where
    hashWithSalt = hashUsing fromEnum

instance ToQuery RadiusAuthenticationProtocol
instance ToHeader RadiusAuthenticationProtocol

instance ToJSON RadiusAuthenticationProtocol where
    toJSON = toJSONText

instance FromJSON RadiusAuthenticationProtocol where
    parseJSON = parseJSONText "RadiusAuthenticationProtocol"

data RadiusStatus
    = Creating
    | Completed
    | Failed
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText RadiusStatus where
    parser = takeLowerText >>= \case
        "completed" -> pure Completed
        "creating" -> pure Creating
        "failed" -> pure Failed
        e -> fromTextError $ "Failure parsing RadiusStatus from value: '" <> e
           <> "'. Accepted values: completed, creating, failed"

instance ToText RadiusStatus where
    toText = \case
        Completed -> "completed"
        Creating -> "creating"
        Failed -> "failed"

instance Hashable RadiusStatus where
    hashWithSalt = hashUsing fromEnum

instance ToQuery RadiusStatus
instance ToHeader RadiusStatus

instance FromJSON RadiusStatus where
    parseJSON = parseJSONText "RadiusStatus"

data SnapshotStatus
    = SSCompleted
    | SSFailed
    | SSCreating
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText SnapshotStatus where
    parser = takeLowerText >>= \case
        "completed" -> pure SSCompleted
        "creating" -> pure SSCreating
        "failed" -> pure SSFailed
        e -> fromTextError $ "Failure parsing SnapshotStatus from value: '" <> e
           <> "'. Accepted values: completed, creating, failed"

instance ToText SnapshotStatus where
    toText = \case
        SSCompleted -> "completed"
        SSCreating -> "creating"
        SSFailed -> "failed"

instance Hashable SnapshotStatus where
    hashWithSalt = hashUsing fromEnum

instance ToQuery SnapshotStatus
instance ToHeader SnapshotStatus

instance FromJSON SnapshotStatus where
    parseJSON = parseJSONText "SnapshotStatus"

data SnapshotType
    = Auto
    | Manual
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText SnapshotType where
    parser = takeLowerText >>= \case
        "auto" -> pure Auto
        "manual" -> pure Manual
        e -> fromTextError $ "Failure parsing SnapshotType from value: '" <> e
           <> "'. Accepted values: auto, manual"

instance ToText SnapshotType where
    toText = \case
        Auto -> "auto"
        Manual -> "manual"

instance Hashable SnapshotType where
    hashWithSalt = hashUsing fromEnum

instance ToQuery SnapshotType
instance ToHeader SnapshotType

instance FromJSON SnapshotType where
    parseJSON = parseJSONText "SnapshotType"

-- | Represents a named directory attribute.
--
-- /See:/ 'attribute' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'attValue'
--
-- * 'attName'
data Attribute = Attribute'
    { _attValue :: !(Maybe Text)
    , _attName  :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'Attribute' smart constructor.
attribute :: Attribute
attribute =
    Attribute'
    { _attValue = Nothing
    , _attName = Nothing
    }

-- | The value of the attribute.
attValue :: Lens' Attribute (Maybe Text)
attValue = lens _attValue (\ s a -> s{_attValue = a});

-- | The name of the attribute.
attName :: Lens' Attribute (Maybe Text)
attName = lens _attName (\ s a -> s{_attName = a});

instance FromJSON Attribute where
        parseJSON
          = withObject "Attribute"
              (\ x ->
                 Attribute' <$> (x .:? "Value") <*> (x .:? "Name"))

instance ToJSON Attribute where
        toJSON Attribute'{..}
          = object ["Value" .= _attValue, "Name" .= _attName]

-- | Contains information about a computer account in a directory.
--
-- /See:/ 'computer' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'comComputerId'
--
-- * 'comComputerAttributes'
--
-- * 'comComputerName'
data Computer = Computer'
    { _comComputerId         :: !(Maybe Text)
    , _comComputerAttributes :: !(Maybe [Attribute])
    , _comComputerName       :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'Computer' smart constructor.
computer :: Computer
computer =
    Computer'
    { _comComputerId = Nothing
    , _comComputerAttributes = Nothing
    , _comComputerName = Nothing
    }

-- | The identifier of the computer.
comComputerId :: Lens' Computer (Maybe Text)
comComputerId = lens _comComputerId (\ s a -> s{_comComputerId = a});

-- | An array of Attribute objects that contain the LDAP attributes that
-- belong to the computer account.
comComputerAttributes :: Lens' Computer [Attribute]
comComputerAttributes = lens _comComputerAttributes (\ s a -> s{_comComputerAttributes = a}) . _Default;

-- | The computer name.
comComputerName :: Lens' Computer (Maybe Text)
comComputerName = lens _comComputerName (\ s a -> s{_comComputerName = a});

instance FromJSON Computer where
        parseJSON
          = withObject "Computer"
              (\ x ->
                 Computer' <$>
                   (x .:? "ComputerId") <*>
                     (x .:? "ComputerAttributes" .!= mempty)
                     <*> (x .:? "ComputerName"))

-- | Contains information for the ConnectDirectory operation when an AD
-- Connector directory is being created.
--
-- /See:/ 'directoryConnectSettings' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsVPCId'
--
-- * 'dcsSubnetIds'
--
-- * 'dcsCustomerDNSIPs'
--
-- * 'dcsCustomerUserName'
data DirectoryConnectSettings = DirectoryConnectSettings'
    { _dcsVPCId            :: !Text
    , _dcsSubnetIds        :: ![Text]
    , _dcsCustomerDNSIPs   :: ![Text]
    , _dcsCustomerUserName :: !Text
    } deriving (Eq,Read,Show)

-- | 'DirectoryConnectSettings' smart constructor.
directoryConnectSettings :: Text -> Text -> DirectoryConnectSettings
directoryConnectSettings pVPCId pCustomerUserName =
    DirectoryConnectSettings'
    { _dcsVPCId = pVPCId
    , _dcsSubnetIds = mempty
    , _dcsCustomerDNSIPs = mempty
    , _dcsCustomerUserName = pCustomerUserName
    }

-- | The identifier of the VPC that the AD Connector is created in.
dcsVPCId :: Lens' DirectoryConnectSettings Text
dcsVPCId = lens _dcsVPCId (\ s a -> s{_dcsVPCId = a});

-- | A list of subnet identifiers in the VPC that the AD Connector is created
-- in.
dcsSubnetIds :: Lens' DirectoryConnectSettings [Text]
dcsSubnetIds = lens _dcsSubnetIds (\ s a -> s{_dcsSubnetIds = a});

-- | A list of one or more IP addresses of DNS servers or domain controllers
-- in the on-premises directory.
dcsCustomerDNSIPs :: Lens' DirectoryConnectSettings [Text]
dcsCustomerDNSIPs = lens _dcsCustomerDNSIPs (\ s a -> s{_dcsCustomerDNSIPs = a});

-- | The username of an account in the on-premises directory that is used to
-- connect to the directory. This account must have the following
-- privileges:
--
-- -   Read users and groups
-- -   Create computer objects
-- -   Join computers to the domain
dcsCustomerUserName :: Lens' DirectoryConnectSettings Text
dcsCustomerUserName = lens _dcsCustomerUserName (\ s a -> s{_dcsCustomerUserName = a});

instance ToJSON DirectoryConnectSettings where
        toJSON DirectoryConnectSettings'{..}
          = object
              ["VpcId" .= _dcsVPCId, "SubnetIds" .= _dcsSubnetIds,
               "CustomerDnsIps" .= _dcsCustomerDNSIPs,
               "CustomerUserName" .= _dcsCustomerUserName]

-- | Contains information about an AD Connector directory.
--
-- /See:/ 'directoryConnectSettingsDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsdCustomerUserName'
--
-- * 'dcsdSubnetIds'
--
-- * 'dcsdVPCId'
--
-- * 'dcsdConnectIPs'
--
-- * 'dcsdSecurityGroupId'
--
-- * 'dcsdAvailabilityZones'
data DirectoryConnectSettingsDescription = DirectoryConnectSettingsDescription'
    { _dcsdCustomerUserName  :: !(Maybe Text)
    , _dcsdSubnetIds         :: !(Maybe [Text])
    , _dcsdVPCId             :: !(Maybe Text)
    , _dcsdConnectIPs        :: !(Maybe [Text])
    , _dcsdSecurityGroupId   :: !(Maybe Text)
    , _dcsdAvailabilityZones :: !(Maybe [Text])
    } deriving (Eq,Read,Show)

-- | 'DirectoryConnectSettingsDescription' smart constructor.
directoryConnectSettingsDescription :: DirectoryConnectSettingsDescription
directoryConnectSettingsDescription =
    DirectoryConnectSettingsDescription'
    { _dcsdCustomerUserName = Nothing
    , _dcsdSubnetIds = Nothing
    , _dcsdVPCId = Nothing
    , _dcsdConnectIPs = Nothing
    , _dcsdSecurityGroupId = Nothing
    , _dcsdAvailabilityZones = Nothing
    }

-- | The username of the service account in the on-premises directory.
dcsdCustomerUserName :: Lens' DirectoryConnectSettingsDescription (Maybe Text)
dcsdCustomerUserName = lens _dcsdCustomerUserName (\ s a -> s{_dcsdCustomerUserName = a});

-- | A list of subnet identifiers in the VPC that the AD connector is in.
dcsdSubnetIds :: Lens' DirectoryConnectSettingsDescription [Text]
dcsdSubnetIds = lens _dcsdSubnetIds (\ s a -> s{_dcsdSubnetIds = a}) . _Default;

-- | The identifier of the VPC that the AD Connector is in.
dcsdVPCId :: Lens' DirectoryConnectSettingsDescription (Maybe Text)
dcsdVPCId = lens _dcsdVPCId (\ s a -> s{_dcsdVPCId = a});

-- | The IP addresses of the AD Connector servers.
dcsdConnectIPs :: Lens' DirectoryConnectSettingsDescription [Text]
dcsdConnectIPs = lens _dcsdConnectIPs (\ s a -> s{_dcsdConnectIPs = a}) . _Default;

-- | The security group identifier for the AD Connector directory.
dcsdSecurityGroupId :: Lens' DirectoryConnectSettingsDescription (Maybe Text)
dcsdSecurityGroupId = lens _dcsdSecurityGroupId (\ s a -> s{_dcsdSecurityGroupId = a});

-- | A list of the Availability Zones that the directory is in.
dcsdAvailabilityZones :: Lens' DirectoryConnectSettingsDescription [Text]
dcsdAvailabilityZones = lens _dcsdAvailabilityZones (\ s a -> s{_dcsdAvailabilityZones = a}) . _Default;

instance FromJSON DirectoryConnectSettingsDescription
         where
        parseJSON
          = withObject "DirectoryConnectSettingsDescription"
              (\ x ->
                 DirectoryConnectSettingsDescription' <$>
                   (x .:? "CustomerUserName") <*>
                     (x .:? "SubnetIds" .!= mempty)
                     <*> (x .:? "VpcId")
                     <*> (x .:? "ConnectIps" .!= mempty)
                     <*> (x .:? "SecurityGroupId")
                     <*> (x .:? "AvailabilityZones" .!= mempty))

-- | Contains information about an AWS Directory Service directory.
--
-- /See:/ 'directoryDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddRadiusStatus'
--
-- * 'ddDirectoryId'
--
-- * 'ddStage'
--
-- * 'ddAccessURL'
--
-- * 'ddShortName'
--
-- * 'ddSize'
--
-- * 'ddRadiusSettings'
--
-- * 'ddLaunchTime'
--
-- * 'ddAlias'
--
-- * 'ddName'
--
-- * 'ddSsoEnabled'
--
-- * 'ddStageLastUpdatedDateTime'
--
-- * 'ddStageReason'
--
-- * 'ddDNSIPAddrs'
--
-- * 'ddVPCSettings'
--
-- * 'ddType'
--
-- * 'ddConnectSettings'
--
-- * 'ddDescription'
data DirectoryDescription = DirectoryDescription'
    { _ddRadiusStatus             :: !(Maybe RadiusStatus)
    , _ddDirectoryId              :: !(Maybe Text)
    , _ddStage                    :: !(Maybe DirectoryStage)
    , _ddAccessURL                :: !(Maybe Text)
    , _ddShortName                :: !(Maybe Text)
    , _ddSize                     :: !(Maybe DirectorySize)
    , _ddRadiusSettings           :: !(Maybe RadiusSettings)
    , _ddLaunchTime               :: !(Maybe POSIX)
    , _ddAlias                    :: !(Maybe Text)
    , _ddName                     :: !(Maybe Text)
    , _ddSsoEnabled               :: !(Maybe Bool)
    , _ddStageLastUpdatedDateTime :: !(Maybe POSIX)
    , _ddStageReason              :: !(Maybe Text)
    , _ddDNSIPAddrs               :: !(Maybe [Text])
    , _ddVPCSettings              :: !(Maybe DirectoryVPCSettingsDescription)
    , _ddType                     :: !(Maybe DirectoryType)
    , _ddConnectSettings          :: !(Maybe DirectoryConnectSettingsDescription)
    , _ddDescription              :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'DirectoryDescription' smart constructor.
directoryDescription :: DirectoryDescription
directoryDescription =
    DirectoryDescription'
    { _ddRadiusStatus = Nothing
    , _ddDirectoryId = Nothing
    , _ddStage = Nothing
    , _ddAccessURL = Nothing
    , _ddShortName = Nothing
    , _ddSize = Nothing
    , _ddRadiusSettings = Nothing
    , _ddLaunchTime = Nothing
    , _ddAlias = Nothing
    , _ddName = Nothing
    , _ddSsoEnabled = Nothing
    , _ddStageLastUpdatedDateTime = Nothing
    , _ddStageReason = Nothing
    , _ddDNSIPAddrs = Nothing
    , _ddVPCSettings = Nothing
    , _ddType = Nothing
    , _ddConnectSettings = Nothing
    , _ddDescription = Nothing
    }

-- | The status of the RADIUS MFA server connection.
ddRadiusStatus :: Lens' DirectoryDescription (Maybe RadiusStatus)
ddRadiusStatus = lens _ddRadiusStatus (\ s a -> s{_ddRadiusStatus = a});

-- | The directory identifier.
ddDirectoryId :: Lens' DirectoryDescription (Maybe Text)
ddDirectoryId = lens _ddDirectoryId (\ s a -> s{_ddDirectoryId = a});

-- | The current stage of the directory.
ddStage :: Lens' DirectoryDescription (Maybe DirectoryStage)
ddStage = lens _ddStage (\ s a -> s{_ddStage = a});

-- | The access URL for the directory, such as
-- @http:\/\/\<alias>.awsapps.com@.
ddAccessURL :: Lens' DirectoryDescription (Maybe Text)
ddAccessURL = lens _ddAccessURL (\ s a -> s{_ddAccessURL = a});

-- | The short name of the directory.
ddShortName :: Lens' DirectoryDescription (Maybe Text)
ddShortName = lens _ddShortName (\ s a -> s{_ddShortName = a});

-- | The directory size.
ddSize :: Lens' DirectoryDescription (Maybe DirectorySize)
ddSize = lens _ddSize (\ s a -> s{_ddSize = a});

-- | A RadiusSettings object that contains information about the RADIUS
-- server configured for this directory.
ddRadiusSettings :: Lens' DirectoryDescription (Maybe RadiusSettings)
ddRadiusSettings = lens _ddRadiusSettings (\ s a -> s{_ddRadiusSettings = a});

-- | Specifies when the directory was created.
ddLaunchTime :: Lens' DirectoryDescription (Maybe UTCTime)
ddLaunchTime = lens _ddLaunchTime (\ s a -> s{_ddLaunchTime = a}) . mapping _Time;

-- | The alias for the directory.
ddAlias :: Lens' DirectoryDescription (Maybe Text)
ddAlias = lens _ddAlias (\ s a -> s{_ddAlias = a});

-- | The fully-qualified name of the directory.
ddName :: Lens' DirectoryDescription (Maybe Text)
ddName = lens _ddName (\ s a -> s{_ddName = a});

-- | Indicates if single-sign on is enabled for the directory. For more
-- information, see EnableSso and DisableSso.
ddSsoEnabled :: Lens' DirectoryDescription (Maybe Bool)
ddSsoEnabled = lens _ddSsoEnabled (\ s a -> s{_ddSsoEnabled = a});

-- | The date and time that the stage was last updated.
ddStageLastUpdatedDateTime :: Lens' DirectoryDescription (Maybe UTCTime)
ddStageLastUpdatedDateTime = lens _ddStageLastUpdatedDateTime (\ s a -> s{_ddStageLastUpdatedDateTime = a}) . mapping _Time;

-- | Additional information about the directory stage.
ddStageReason :: Lens' DirectoryDescription (Maybe Text)
ddStageReason = lens _ddStageReason (\ s a -> s{_ddStageReason = a});

-- | The IP addresses of the DNS servers for the directory. For a Simple AD
-- directory, these are the IP addresses of the Simple AD directory
-- servers. For an AD Connector directory, these are the IP addresses of
-- the DNS servers or domain controllers in the on-premises directory that
-- the AD Connector is connected to.
ddDNSIPAddrs :: Lens' DirectoryDescription [Text]
ddDNSIPAddrs = lens _ddDNSIPAddrs (\ s a -> s{_ddDNSIPAddrs = a}) . _Default;

-- | A DirectoryVpcSettingsDescription object that contains additional
-- information about a Simple AD directory. This member is only present if
-- the directory is a Simple AD directory.
ddVPCSettings :: Lens' DirectoryDescription (Maybe DirectoryVPCSettingsDescription)
ddVPCSettings = lens _ddVPCSettings (\ s a -> s{_ddVPCSettings = a});

-- | The directory size.
ddType :: Lens' DirectoryDescription (Maybe DirectoryType)
ddType = lens _ddType (\ s a -> s{_ddType = a});

-- | A DirectoryConnectSettingsDescription object that contains additional
-- information about an AD Connector directory. This member is only present
-- if the directory is an AD Connector directory.
ddConnectSettings :: Lens' DirectoryDescription (Maybe DirectoryConnectSettingsDescription)
ddConnectSettings = lens _ddConnectSettings (\ s a -> s{_ddConnectSettings = a});

-- | The textual description for the directory.
ddDescription :: Lens' DirectoryDescription (Maybe Text)
ddDescription = lens _ddDescription (\ s a -> s{_ddDescription = a});

instance FromJSON DirectoryDescription where
        parseJSON
          = withObject "DirectoryDescription"
              (\ x ->
                 DirectoryDescription' <$>
                   (x .:? "RadiusStatus") <*> (x .:? "DirectoryId") <*>
                     (x .:? "Stage")
                     <*> (x .:? "AccessUrl")
                     <*> (x .:? "ShortName")
                     <*> (x .:? "Size")
                     <*> (x .:? "RadiusSettings")
                     <*> (x .:? "LaunchTime")
                     <*> (x .:? "Alias")
                     <*> (x .:? "Name")
                     <*> (x .:? "SsoEnabled")
                     <*> (x .:? "StageLastUpdatedDateTime")
                     <*> (x .:? "StageReason")
                     <*> (x .:? "DnsIpAddrs" .!= mempty)
                     <*> (x .:? "VpcSettings")
                     <*> (x .:? "Type")
                     <*> (x .:? "ConnectSettings")
                     <*> (x .:? "Description"))

-- | Contains directory limit information for a region.
--
-- /See:/ 'directoryLimits' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlConnectedDirectoriesCurrentCount'
--
-- * 'dlConnectedDirectoriesLimit'
--
-- * 'dlConnectedDirectoriesLimitReached'
--
-- * 'dlCloudOnlyDirectoriesLimit'
--
-- * 'dlCloudOnlyDirectoriesCurrentCount'
--
-- * 'dlCloudOnlyDirectoriesLimitReached'
data DirectoryLimits = DirectoryLimits'
    { _dlConnectedDirectoriesCurrentCount :: !(Maybe Nat)
    , _dlConnectedDirectoriesLimit        :: !(Maybe Nat)
    , _dlConnectedDirectoriesLimitReached :: !(Maybe Bool)
    , _dlCloudOnlyDirectoriesLimit        :: !(Maybe Nat)
    , _dlCloudOnlyDirectoriesCurrentCount :: !(Maybe Nat)
    , _dlCloudOnlyDirectoriesLimitReached :: !(Maybe Bool)
    } deriving (Eq,Read,Show)

-- | 'DirectoryLimits' smart constructor.
directoryLimits :: DirectoryLimits
directoryLimits =
    DirectoryLimits'
    { _dlConnectedDirectoriesCurrentCount = Nothing
    , _dlConnectedDirectoriesLimit = Nothing
    , _dlConnectedDirectoriesLimitReached = Nothing
    , _dlCloudOnlyDirectoriesLimit = Nothing
    , _dlCloudOnlyDirectoriesCurrentCount = Nothing
    , _dlCloudOnlyDirectoriesLimitReached = Nothing
    }

-- | The current number of connected directories in the region.
dlConnectedDirectoriesCurrentCount :: Lens' DirectoryLimits (Maybe Natural)
dlConnectedDirectoriesCurrentCount = lens _dlConnectedDirectoriesCurrentCount (\ s a -> s{_dlConnectedDirectoriesCurrentCount = a}) . mapping _Nat;

-- | The maximum number of connected directories allowed in the region.
dlConnectedDirectoriesLimit :: Lens' DirectoryLimits (Maybe Natural)
dlConnectedDirectoriesLimit = lens _dlConnectedDirectoriesLimit (\ s a -> s{_dlConnectedDirectoriesLimit = a}) . mapping _Nat;

-- | Indicates if the connected directory limit has been reached.
dlConnectedDirectoriesLimitReached :: Lens' DirectoryLimits (Maybe Bool)
dlConnectedDirectoriesLimitReached = lens _dlConnectedDirectoriesLimitReached (\ s a -> s{_dlConnectedDirectoriesLimitReached = a});

-- | The maximum number of cloud directories allowed in the region.
dlCloudOnlyDirectoriesLimit :: Lens' DirectoryLimits (Maybe Natural)
dlCloudOnlyDirectoriesLimit = lens _dlCloudOnlyDirectoriesLimit (\ s a -> s{_dlCloudOnlyDirectoriesLimit = a}) . mapping _Nat;

-- | The current number of cloud directories in the region.
dlCloudOnlyDirectoriesCurrentCount :: Lens' DirectoryLimits (Maybe Natural)
dlCloudOnlyDirectoriesCurrentCount = lens _dlCloudOnlyDirectoriesCurrentCount (\ s a -> s{_dlCloudOnlyDirectoriesCurrentCount = a}) . mapping _Nat;

-- | Indicates if the cloud directory limit has been reached.
dlCloudOnlyDirectoriesLimitReached :: Lens' DirectoryLimits (Maybe Bool)
dlCloudOnlyDirectoriesLimitReached = lens _dlCloudOnlyDirectoriesLimitReached (\ s a -> s{_dlCloudOnlyDirectoriesLimitReached = a});

instance FromJSON DirectoryLimits where
        parseJSON
          = withObject "DirectoryLimits"
              (\ x ->
                 DirectoryLimits' <$>
                   (x .:? "ConnectedDirectoriesCurrentCount") <*>
                     (x .:? "ConnectedDirectoriesLimit")
                     <*> (x .:? "ConnectedDirectoriesLimitReached")
                     <*> (x .:? "CloudOnlyDirectoriesLimit")
                     <*> (x .:? "CloudOnlyDirectoriesCurrentCount")
                     <*> (x .:? "CloudOnlyDirectoriesLimitReached"))

-- | Contains information for the CreateDirectory operation when a Simple AD
-- directory is being created.
--
-- /See:/ 'directoryVPCSettings' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvsVPCId'
--
-- * 'dvsSubnetIds'
data DirectoryVPCSettings = DirectoryVPCSettings'
    { _dvsVPCId     :: !Text
    , _dvsSubnetIds :: ![Text]
    } deriving (Eq,Read,Show)

-- | 'DirectoryVPCSettings' smart constructor.
directoryVPCSettings :: Text -> DirectoryVPCSettings
directoryVPCSettings pVPCId =
    DirectoryVPCSettings'
    { _dvsVPCId = pVPCId
    , _dvsSubnetIds = mempty
    }

-- | The identifier of the VPC to create the Simple AD directory in.
dvsVPCId :: Lens' DirectoryVPCSettings Text
dvsVPCId = lens _dvsVPCId (\ s a -> s{_dvsVPCId = a});

-- | The identifiers of the subnets for the directory servers. The two
-- subnets must be in different Availability Zones. AWS Directory Service
-- creates a directory server and a DNS server in each of these subnets.
dvsSubnetIds :: Lens' DirectoryVPCSettings [Text]
dvsSubnetIds = lens _dvsSubnetIds (\ s a -> s{_dvsSubnetIds = a});

instance ToJSON DirectoryVPCSettings where
        toJSON DirectoryVPCSettings'{..}
          = object
              ["VpcId" .= _dvsVPCId, "SubnetIds" .= _dvsSubnetIds]

-- | Contains information about a Simple AD directory.
--
-- /See:/ 'directoryVPCSettingsDescription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvsdSubnetIds'
--
-- * 'dvsdVPCId'
--
-- * 'dvsdSecurityGroupId'
--
-- * 'dvsdAvailabilityZones'
data DirectoryVPCSettingsDescription = DirectoryVPCSettingsDescription'
    { _dvsdSubnetIds         :: !(Maybe [Text])
    , _dvsdVPCId             :: !(Maybe Text)
    , _dvsdSecurityGroupId   :: !(Maybe Text)
    , _dvsdAvailabilityZones :: !(Maybe [Text])
    } deriving (Eq,Read,Show)

-- | 'DirectoryVPCSettingsDescription' smart constructor.
directoryVPCSettingsDescription :: DirectoryVPCSettingsDescription
directoryVPCSettingsDescription =
    DirectoryVPCSettingsDescription'
    { _dvsdSubnetIds = Nothing
    , _dvsdVPCId = Nothing
    , _dvsdSecurityGroupId = Nothing
    , _dvsdAvailabilityZones = Nothing
    }

-- | The identifiers of the subnets for the directory servers.
dvsdSubnetIds :: Lens' DirectoryVPCSettingsDescription [Text]
dvsdSubnetIds = lens _dvsdSubnetIds (\ s a -> s{_dvsdSubnetIds = a}) . _Default;

-- | The identifier of the VPC that the directory is in.
dvsdVPCId :: Lens' DirectoryVPCSettingsDescription (Maybe Text)
dvsdVPCId = lens _dvsdVPCId (\ s a -> s{_dvsdVPCId = a});

-- | The security group identifier for the directory.
dvsdSecurityGroupId :: Lens' DirectoryVPCSettingsDescription (Maybe Text)
dvsdSecurityGroupId = lens _dvsdSecurityGroupId (\ s a -> s{_dvsdSecurityGroupId = a});

-- | The list of Availability Zones that the directory is in.
dvsdAvailabilityZones :: Lens' DirectoryVPCSettingsDescription [Text]
dvsdAvailabilityZones = lens _dvsdAvailabilityZones (\ s a -> s{_dvsdAvailabilityZones = a}) . _Default;

instance FromJSON DirectoryVPCSettingsDescription
         where
        parseJSON
          = withObject "DirectoryVPCSettingsDescription"
              (\ x ->
                 DirectoryVPCSettingsDescription' <$>
                   (x .:? "SubnetIds" .!= mempty) <*> (x .:? "VpcId")
                     <*> (x .:? "SecurityGroupId")
                     <*> (x .:? "AvailabilityZones" .!= mempty))

-- | Contains information about a Remote Authentication Dial In User Service
-- (RADIUS) server.
--
-- /See:/ 'radiusSettings' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rsDisplayLabel'
--
-- * 'rsRadiusServers'
--
-- * 'rsRadiusRetries'
--
-- * 'rsAuthenticationProtocol'
--
-- * 'rsUseSameUsername'
--
-- * 'rsSharedSecret'
--
-- * 'rsRadiusTimeout'
--
-- * 'rsRadiusPort'
data RadiusSettings = RadiusSettings'
    { _rsDisplayLabel           :: !(Maybe Text)
    , _rsRadiusServers          :: !(Maybe [Text])
    , _rsRadiusRetries          :: !(Maybe Nat)
    , _rsAuthenticationProtocol :: !(Maybe RadiusAuthenticationProtocol)
    , _rsUseSameUsername        :: !(Maybe Bool)
    , _rsSharedSecret           :: !(Maybe (Sensitive Text))
    , _rsRadiusTimeout          :: !(Maybe Nat)
    , _rsRadiusPort             :: !(Maybe Nat)
    } deriving (Eq,Read,Show)

-- | 'RadiusSettings' smart constructor.
radiusSettings :: RadiusSettings
radiusSettings =
    RadiusSettings'
    { _rsDisplayLabel = Nothing
    , _rsRadiusServers = Nothing
    , _rsRadiusRetries = Nothing
    , _rsAuthenticationProtocol = Nothing
    , _rsUseSameUsername = Nothing
    , _rsSharedSecret = Nothing
    , _rsRadiusTimeout = Nothing
    , _rsRadiusPort = Nothing
    }

-- | Not currently used.
rsDisplayLabel :: Lens' RadiusSettings (Maybe Text)
rsDisplayLabel = lens _rsDisplayLabel (\ s a -> s{_rsDisplayLabel = a});

-- | An array of strings that contains the IP addresses of the RADIUS server
-- endpoints, or the IP addresses of your RADIUS server load balancer.
rsRadiusServers :: Lens' RadiusSettings [Text]
rsRadiusServers = lens _rsRadiusServers (\ s a -> s{_rsRadiusServers = a}) . _Default;

-- | The maximum number of times that communication with the RADIUS server is
-- attempted.
rsRadiusRetries :: Lens' RadiusSettings (Maybe Natural)
rsRadiusRetries = lens _rsRadiusRetries (\ s a -> s{_rsRadiusRetries = a}) . mapping _Nat;

-- | The protocol specified for your RADIUS endpoints.
rsAuthenticationProtocol :: Lens' RadiusSettings (Maybe RadiusAuthenticationProtocol)
rsAuthenticationProtocol = lens _rsAuthenticationProtocol (\ s a -> s{_rsAuthenticationProtocol = a});

-- | Not currently used.
rsUseSameUsername :: Lens' RadiusSettings (Maybe Bool)
rsUseSameUsername = lens _rsUseSameUsername (\ s a -> s{_rsUseSameUsername = a});

-- | The shared secret code that was specified when your RADIUS endpoints
-- were created.
rsSharedSecret :: Lens' RadiusSettings (Maybe Text)
rsSharedSecret = lens _rsSharedSecret (\ s a -> s{_rsSharedSecret = a}) . mapping _Sensitive;

-- | The amount of time, in seconds, to wait for the RADIUS server to
-- respond.
rsRadiusTimeout :: Lens' RadiusSettings (Maybe Natural)
rsRadiusTimeout = lens _rsRadiusTimeout (\ s a -> s{_rsRadiusTimeout = a}) . mapping _Nat;

-- | The port that your RADIUS server is using for communications. Your
-- on-premises network must allow inbound traffic over this port from the
-- AWS Directory Service servers.
rsRadiusPort :: Lens' RadiusSettings (Maybe Natural)
rsRadiusPort = lens _rsRadiusPort (\ s a -> s{_rsRadiusPort = a}) . mapping _Nat;

instance FromJSON RadiusSettings where
        parseJSON
          = withObject "RadiusSettings"
              (\ x ->
                 RadiusSettings' <$>
                   (x .:? "DisplayLabel") <*>
                     (x .:? "RadiusServers" .!= mempty)
                     <*> (x .:? "RadiusRetries")
                     <*> (x .:? "AuthenticationProtocol")
                     <*> (x .:? "UseSameUsername")
                     <*> (x .:? "SharedSecret")
                     <*> (x .:? "RadiusTimeout")
                     <*> (x .:? "RadiusPort"))

instance ToJSON RadiusSettings where
        toJSON RadiusSettings'{..}
          = object
              ["DisplayLabel" .= _rsDisplayLabel,
               "RadiusServers" .= _rsRadiusServers,
               "RadiusRetries" .= _rsRadiusRetries,
               "AuthenticationProtocol" .=
                 _rsAuthenticationProtocol,
               "UseSameUsername" .= _rsUseSameUsername,
               "SharedSecret" .= _rsSharedSecret,
               "RadiusTimeout" .= _rsRadiusTimeout,
               "RadiusPort" .= _rsRadiusPort]

-- | Describes a directory snapshot.
--
-- /See:/ 'snapshot' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'snaDirectoryId'
--
-- * 'snaStatus'
--
-- * 'snaStartTime'
--
-- * 'snaName'
--
-- * 'snaType'
--
-- * 'snaSnapshotId'
data Snapshot = Snapshot'
    { _snaDirectoryId :: !(Maybe Text)
    , _snaStatus      :: !(Maybe SnapshotStatus)
    , _snaStartTime   :: !(Maybe POSIX)
    , _snaName        :: !(Maybe Text)
    , _snaType        :: !(Maybe SnapshotType)
    , _snaSnapshotId  :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'Snapshot' smart constructor.
snapshot :: Snapshot
snapshot =
    Snapshot'
    { _snaDirectoryId = Nothing
    , _snaStatus = Nothing
    , _snaStartTime = Nothing
    , _snaName = Nothing
    , _snaType = Nothing
    , _snaSnapshotId = Nothing
    }

-- | The directory identifier.
snaDirectoryId :: Lens' Snapshot (Maybe Text)
snaDirectoryId = lens _snaDirectoryId (\ s a -> s{_snaDirectoryId = a});

-- | The snapshot status.
snaStatus :: Lens' Snapshot (Maybe SnapshotStatus)
snaStatus = lens _snaStatus (\ s a -> s{_snaStatus = a});

-- | The date and time that the snapshot was taken.
snaStartTime :: Lens' Snapshot (Maybe UTCTime)
snaStartTime = lens _snaStartTime (\ s a -> s{_snaStartTime = a}) . mapping _Time;

-- | The descriptive name of the snapshot.
snaName :: Lens' Snapshot (Maybe Text)
snaName = lens _snaName (\ s a -> s{_snaName = a});

-- | The snapshot type.
snaType :: Lens' Snapshot (Maybe SnapshotType)
snaType = lens _snaType (\ s a -> s{_snaType = a});

-- | The snapshot identifier.
snaSnapshotId :: Lens' Snapshot (Maybe Text)
snaSnapshotId = lens _snaSnapshotId (\ s a -> s{_snaSnapshotId = a});

instance FromJSON Snapshot where
        parseJSON
          = withObject "Snapshot"
              (\ x ->
                 Snapshot' <$>
                   (x .:? "DirectoryId") <*> (x .:? "Status") <*>
                     (x .:? "StartTime")
                     <*> (x .:? "Name")
                     <*> (x .:? "Type")
                     <*> (x .:? "SnapshotId"))

-- | Contains manual snapshot limit information for a directory.
--
-- /See:/ 'snapshotLimits' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'slManualSnapshotsLimitReached'
--
-- * 'slManualSnapshotsCurrentCount'
--
-- * 'slManualSnapshotsLimit'
data SnapshotLimits = SnapshotLimits'
    { _slManualSnapshotsLimitReached :: !(Maybe Bool)
    , _slManualSnapshotsCurrentCount :: !(Maybe Nat)
    , _slManualSnapshotsLimit        :: !(Maybe Nat)
    } deriving (Eq,Read,Show)

-- | 'SnapshotLimits' smart constructor.
snapshotLimits :: SnapshotLimits
snapshotLimits =
    SnapshotLimits'
    { _slManualSnapshotsLimitReached = Nothing
    , _slManualSnapshotsCurrentCount = Nothing
    , _slManualSnapshotsLimit = Nothing
    }

-- | Indicates if the manual snapshot limit has been reached.
slManualSnapshotsLimitReached :: Lens' SnapshotLimits (Maybe Bool)
slManualSnapshotsLimitReached = lens _slManualSnapshotsLimitReached (\ s a -> s{_slManualSnapshotsLimitReached = a});

-- | The current number of manual snapshots of the directory.
slManualSnapshotsCurrentCount :: Lens' SnapshotLimits (Maybe Natural)
slManualSnapshotsCurrentCount = lens _slManualSnapshotsCurrentCount (\ s a -> s{_slManualSnapshotsCurrentCount = a}) . mapping _Nat;

-- | The maximum number of manual snapshots allowed.
slManualSnapshotsLimit :: Lens' SnapshotLimits (Maybe Natural)
slManualSnapshotsLimit = lens _slManualSnapshotsLimit (\ s a -> s{_slManualSnapshotsLimit = a}) . mapping _Nat;

instance FromJSON SnapshotLimits where
        parseJSON
          = withObject "SnapshotLimits"
              (\ x ->
                 SnapshotLimits' <$>
                   (x .:? "ManualSnapshotsLimitReached") <*>
                     (x .:? "ManualSnapshotsCurrentCount")
                     <*> (x .:? "ManualSnapshotsLimit"))

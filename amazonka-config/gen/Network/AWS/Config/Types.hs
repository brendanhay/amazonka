{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Config.Types
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

module Network.AWS.Config.Types
    (
    -- * Service
      Config

    -- * Errors
    , _ValidationException
    , _InvalidTimeRangeException
    , _InvalidRecordingGroupException
    , _InvalidSNSTopicARNException
    , _InvalidRoleException
    , _LastDeliveryChannelDeleteFailedException
    , _InvalidLimitException
    , _InvalidDeliveryChannelNameException
    , _NoSuchDeliveryChannelException
    , _ResourceNotDiscoveredException
    , _InvalidNextTokenException
    , _NoSuchBucketException
    , _NoAvailableConfigurationRecorderException
    , _NoAvailableDeliveryChannelException
    , _NoRunningConfigurationRecorderException
    , _MaxNumberOfConfigurationRecordersExceededException
    , _InvalidConfigurationRecorderNameException
    , _InsufficientDeliveryPolicyException
    , _MaxNumberOfDeliveryChannelsExceededException
    , _NoSuchConfigurationRecorderException
    , _InvalidS3KeyPrefixException

    -- * ChronologicalOrder
    , ChronologicalOrder (..)

    -- * ConfigurationItemStatus
    , ConfigurationItemStatus (..)

    -- * DeliveryStatus
    , DeliveryStatus (..)

    -- * RecorderStatus
    , RecorderStatus (..)

    -- * ResourceType
    , ResourceType (..)

    -- * ConfigExportDeliveryInfo
    , ConfigExportDeliveryInfo
    , configExportDeliveryInfo
    , cediLastErrorCode
    , cediLastAttemptTime
    , cediLastSuccessfulTime
    , cediLastStatus
    , cediLastErrorMessage

    -- * ConfigStreamDeliveryInfo
    , ConfigStreamDeliveryInfo
    , configStreamDeliveryInfo
    , csdiLastErrorCode
    , csdiLastStatusChangeTime
    , csdiLastStatus
    , csdiLastErrorMessage

    -- * ConfigurationItem
    , ConfigurationItem
    , configurationItem
    , ciResourceId
    , ciConfigurationStateId
    , ciResourceType
    , ciArn
    , ciResourceCreationTime
    , ciConfigurationItemStatus
    , ciAccountId
    , ciConfigurationItemCaptureTime
    , ciAvailabilityZone
    , ciRelationships
    , ciVersion
    , ciRelatedEvents
    , ciConfiguration
    , ciConfigurationItemMD5Hash
    , ciTags

    -- * ConfigurationRecorder
    , ConfigurationRecorder
    , configurationRecorder
    , crName
    , crRecordingGroup
    , crRoleARN

    -- * ConfigurationRecorderStatus
    , ConfigurationRecorderStatus
    , configurationRecorderStatus
    , crsLastErrorCode
    , crsLastStopTime
    , crsLastStatusChangeTime
    , crsRecording
    , crsLastStatus
    , crsLastErrorMessage
    , crsName
    , crsLastStartTime

    -- * DeliveryChannel
    , DeliveryChannel
    , deliveryChannel
    , dcS3KeyPrefix
    , dcSnsTopicARN
    , dcName
    , dcS3BucketName

    -- * DeliveryChannelStatus
    , DeliveryChannelStatus
    , deliveryChannelStatus
    , dcsConfigStreamDeliveryInfo
    , dcsConfigSnapshotDeliveryInfo
    , dcsConfigHistoryDeliveryInfo
    , dcsName

    -- * RecordingGroup
    , RecordingGroup
    , recordingGroup
    , rgAllSupported
    , rgResourceTypes

    -- * Relationship
    , Relationship
    , relationship
    , relResourceId
    , relResourceType
    , relRelationshipName
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2014-11-12@ of the Amazon Config SDK.
data Config

instance AWSService Config where
    type Sg Config = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "Config"
            , _svcPrefix = "config"
            , _svcVersion = "2014-11-12"
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

-- | The requested action is not valid.
_ValidationException :: AWSError a => Getting (First ServiceError) a ServiceError
_ValidationException = _ServiceError . hasCode "ValidationException"

-- | The specified time range is not valid. The earlier time is not
-- chronologically before the later time.
_InvalidTimeRangeException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidTimeRangeException =
    _ServiceError . hasCode "InvalidTimeRangeException"

-- | AWS Config throws an exception if the recording group does not contain a
-- valid list of resource types. Invalid values could also be incorrectly
-- formatted.
_InvalidRecordingGroupException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidRecordingGroupException =
    _ServiceError . hasCode "InvalidRecordingGroupException"

-- | The specified Amazon SNS topic does not exist.
_InvalidSNSTopicARNException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidSNSTopicARNException =
    _ServiceError . hasCode "InvalidSNSTopicARNException"

-- | You have provided a null or empty role ARN.
_InvalidRoleException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidRoleException = _ServiceError . hasCode "InvalidRoleException"

-- | You cannot delete the delivery channel you specified because the
-- configuration recorder is running.
_LastDeliveryChannelDeleteFailedException :: AWSError a => Getting (First ServiceError) a ServiceError
_LastDeliveryChannelDeleteFailedException =
    _ServiceError . hasCode "LastDeliveryChannelDeleteFailedException"

-- | You have reached the limit on the pagination.
_InvalidLimitException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidLimitException = _ServiceError . hasCode "InvalidLimitException"

-- | The specified delivery channel name is not valid.
_InvalidDeliveryChannelNameException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidDeliveryChannelNameException =
    _ServiceError . hasCode "InvalidDeliveryChannelNameException"

-- | You have specified a delivery channel that does not exist.
_NoSuchDeliveryChannelException :: AWSError a => Getting (First ServiceError) a ServiceError
_NoSuchDeliveryChannelException =
    _ServiceError . hasCode "NoSuchDeliveryChannelException"

-- | You have specified a resource that is either unknown or has not been
-- discovered.
_ResourceNotDiscoveredException :: AWSError a => Getting (First ServiceError) a ServiceError
_ResourceNotDiscoveredException =
    _ServiceError . hasCode "ResourceNotDiscoveredException"

-- | The specified nextToken for pagination is not valid.
_InvalidNextTokenException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidNextTokenException =
    _ServiceError . hasCode "InvalidNextTokenException"

-- | The specified Amazon S3 bucket does not exist.
_NoSuchBucketException :: AWSError a => Getting (First ServiceError) a ServiceError
_NoSuchBucketException = _ServiceError . hasCode "NoSuchBucketException"

-- | There are no configuration recorders available to provide the role
-- needed to describe your resources.
_NoAvailableConfigurationRecorderException :: AWSError a => Getting (First ServiceError) a ServiceError
_NoAvailableConfigurationRecorderException =
    _ServiceError . hasCode "NoAvailableConfigurationRecorderException"

-- | There is no delivery channel available to record configurations.
_NoAvailableDeliveryChannelException :: AWSError a => Getting (First ServiceError) a ServiceError
_NoAvailableDeliveryChannelException =
    _ServiceError . hasCode "NoAvailableDeliveryChannelException"

-- | There is no configuration recorder running.
_NoRunningConfigurationRecorderException :: AWSError a => Getting (First ServiceError) a ServiceError
_NoRunningConfigurationRecorderException =
    _ServiceError . hasCode "NoRunningConfigurationRecorderException"

-- | You have reached the limit on the number of recorders you can create.
_MaxNumberOfConfigurationRecordersExceededException :: AWSError a => Getting (First ServiceError) a ServiceError
_MaxNumberOfConfigurationRecordersExceededException =
    _ServiceError .
    hasCode "MaxNumberOfConfigurationRecordersExceededException"

-- | You have provided a configuration recorder name that is not valid.
_InvalidConfigurationRecorderNameException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidConfigurationRecorderNameException =
    _ServiceError . hasCode "InvalidConfigurationRecorderNameException"

-- | Your Amazon S3 bucket policy does not permit AWS Config to write to it.
_InsufficientDeliveryPolicyException :: AWSError a => Getting (First ServiceError) a ServiceError
_InsufficientDeliveryPolicyException =
    _ServiceError . hasCode "InsufficientDeliveryPolicyException"

-- | You have reached the limit on the number of delivery channels you can
-- create.
_MaxNumberOfDeliveryChannelsExceededException :: AWSError a => Getting (First ServiceError) a ServiceError
_MaxNumberOfDeliveryChannelsExceededException =
    _ServiceError . hasCode "MaxNumberOfDeliveryChannelsExceededException"

-- | You have specified a configuration recorder that does not exist.
_NoSuchConfigurationRecorderException :: AWSError a => Getting (First ServiceError) a ServiceError
_NoSuchConfigurationRecorderException =
    _ServiceError . hasCode "NoSuchConfigurationRecorderException"

-- | The specified Amazon S3 key prefix is not valid.
_InvalidS3KeyPrefixException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidS3KeyPrefixException =
    _ServiceError . hasCode "InvalidS3KeyPrefixException"

data ChronologicalOrder
    = Forward
    | Reverse
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText ChronologicalOrder where
    parser = takeLowerText >>= \case
        "forward" -> pure Forward
        "reverse" -> pure Reverse
        e -> fromTextError $ "Failure parsing ChronologicalOrder from value: '" <> e
           <> "'. Accepted values: forward, reverse"

instance ToText ChronologicalOrder where
    toText = \case
        Forward -> "forward"
        Reverse -> "reverse"

instance Hashable ChronologicalOrder where
    hashWithSalt = hashUsing fromEnum

instance ToQuery ChronologicalOrder
instance ToHeader ChronologicalOrder

instance ToJSON ChronologicalOrder where
    toJSON = toJSONText

data ConfigurationItemStatus
    = OK
    | Discovered
    | Deleted
    | Failed
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText ConfigurationItemStatus where
    parser = takeLowerText >>= \case
        "deleted" -> pure Deleted
        "discovered" -> pure Discovered
        "failed" -> pure Failed
        "ok" -> pure OK
        e -> fromTextError $ "Failure parsing ConfigurationItemStatus from value: '" <> e
           <> "'. Accepted values: deleted, discovered, failed, ok"

instance ToText ConfigurationItemStatus where
    toText = \case
        Deleted -> "deleted"
        Discovered -> "discovered"
        Failed -> "failed"
        OK -> "ok"

instance Hashable ConfigurationItemStatus where
    hashWithSalt = hashUsing fromEnum

instance ToQuery ConfigurationItemStatus
instance ToHeader ConfigurationItemStatus

instance FromJSON ConfigurationItemStatus where
    parseJSON = parseJSONText "ConfigurationItemStatus"

data DeliveryStatus
    = Success
    | NotApplicable
    | Failure
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText DeliveryStatus where
    parser = takeLowerText >>= \case
        "failure" -> pure Failure
        "not_applicable" -> pure NotApplicable
        "success" -> pure Success
        e -> fromTextError $ "Failure parsing DeliveryStatus from value: '" <> e
           <> "'. Accepted values: failure, not_applicable, success"

instance ToText DeliveryStatus where
    toText = \case
        Failure -> "failure"
        NotApplicable -> "not_applicable"
        Success -> "success"

instance Hashable DeliveryStatus where
    hashWithSalt = hashUsing fromEnum

instance ToQuery DeliveryStatus
instance ToHeader DeliveryStatus

instance FromJSON DeliveryStatus where
    parseJSON = parseJSONText "DeliveryStatus"

data RecorderStatus
    = RSPending
    | RSFailure
    | RSSuccess
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText RecorderStatus where
    parser = takeLowerText >>= \case
        "failure" -> pure RSFailure
        "pending" -> pure RSPending
        "success" -> pure RSSuccess
        e -> fromTextError $ "Failure parsing RecorderStatus from value: '" <> e
           <> "'. Accepted values: failure, pending, success"

instance ToText RecorderStatus where
    toText = \case
        RSFailure -> "failure"
        RSPending -> "pending"
        RSSuccess -> "success"

instance Hashable RecorderStatus where
    hashWithSalt = hashUsing fromEnum

instance ToQuery RecorderStatus
instance ToHeader RecorderStatus

instance FromJSON RecorderStatus where
    parseJSON = parseJSONText "RecorderStatus"

data ResourceType
    = AWSCloudTrailTrail
    | AWSEC2VPNConnection
    | AWSEC2SecurityGroup
    | AWSEC2Instance
    | AWSEC2NetworkACL
    | AWSEC2VPNGateway
    | AWSEC2VPC
    | AWSEC2NetworkInterface
    | AWSEC2InternetGateway
    | AWSEC2Subnet
    | AWSEC2EIP
    | AWSEC2CustomerGateway
    | AWSEC2RouteTable
    | AWSEC2Volume
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText ResourceType where
    parser = takeLowerText >>= \case
        "aws::cloudtrail::trail" -> pure AWSCloudTrailTrail
        "aws::ec2::customergateway" -> pure AWSEC2CustomerGateway
        "aws::ec2::eip" -> pure AWSEC2EIP
        "aws::ec2::instance" -> pure AWSEC2Instance
        "aws::ec2::internetgateway" -> pure AWSEC2InternetGateway
        "aws::ec2::networkacl" -> pure AWSEC2NetworkACL
        "aws::ec2::networkinterface" -> pure AWSEC2NetworkInterface
        "aws::ec2::routetable" -> pure AWSEC2RouteTable
        "aws::ec2::securitygroup" -> pure AWSEC2SecurityGroup
        "aws::ec2::subnet" -> pure AWSEC2Subnet
        "aws::ec2::vpc" -> pure AWSEC2VPC
        "aws::ec2::vpnconnection" -> pure AWSEC2VPNConnection
        "aws::ec2::vpngateway" -> pure AWSEC2VPNGateway
        "aws::ec2::volume" -> pure AWSEC2Volume
        e -> fromTextError $ "Failure parsing ResourceType from value: '" <> e
           <> "'. Accepted values: aws::cloudtrail::trail, aws::ec2::customergateway, aws::ec2::eip, aws::ec2::instance, aws::ec2::internetgateway, aws::ec2::networkacl, aws::ec2::networkinterface, aws::ec2::routetable, aws::ec2::securitygroup, aws::ec2::subnet, aws::ec2::vpc, aws::ec2::vpnconnection, aws::ec2::vpngateway, aws::ec2::volume"

instance ToText ResourceType where
    toText = \case
        AWSCloudTrailTrail -> "aws::cloudtrail::trail"
        AWSEC2CustomerGateway -> "aws::ec2::customergateway"
        AWSEC2EIP -> "aws::ec2::eip"
        AWSEC2Instance -> "aws::ec2::instance"
        AWSEC2InternetGateway -> "aws::ec2::internetgateway"
        AWSEC2NetworkACL -> "aws::ec2::networkacl"
        AWSEC2NetworkInterface -> "aws::ec2::networkinterface"
        AWSEC2RouteTable -> "aws::ec2::routetable"
        AWSEC2SecurityGroup -> "aws::ec2::securitygroup"
        AWSEC2Subnet -> "aws::ec2::subnet"
        AWSEC2VPC -> "aws::ec2::vpc"
        AWSEC2VPNConnection -> "aws::ec2::vpnconnection"
        AWSEC2VPNGateway -> "aws::ec2::vpngateway"
        AWSEC2Volume -> "aws::ec2::volume"

instance Hashable ResourceType where
    hashWithSalt = hashUsing fromEnum

instance ToQuery ResourceType
instance ToHeader ResourceType

instance ToJSON ResourceType where
    toJSON = toJSONText

instance FromJSON ResourceType where
    parseJSON = parseJSONText "ResourceType"

-- | A list that contains the status of the delivery of either the snapshot
-- or the configuration history to the specified Amazon S3 bucket.
--
-- /See:/ 'configExportDeliveryInfo' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cediLastErrorCode'
--
-- * 'cediLastAttemptTime'
--
-- * 'cediLastSuccessfulTime'
--
-- * 'cediLastStatus'
--
-- * 'cediLastErrorMessage'
data ConfigExportDeliveryInfo = ConfigExportDeliveryInfo'
    { _cediLastErrorCode      :: !(Maybe Text)
    , _cediLastAttemptTime    :: !(Maybe POSIX)
    , _cediLastSuccessfulTime :: !(Maybe POSIX)
    , _cediLastStatus         :: !(Maybe DeliveryStatus)
    , _cediLastErrorMessage   :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'ConfigExportDeliveryInfo' smart constructor.
configExportDeliveryInfo :: ConfigExportDeliveryInfo
configExportDeliveryInfo =
    ConfigExportDeliveryInfo'
    { _cediLastErrorCode = Nothing
    , _cediLastAttemptTime = Nothing
    , _cediLastSuccessfulTime = Nothing
    , _cediLastStatus = Nothing
    , _cediLastErrorMessage = Nothing
    }

-- | The error code from the last attempted delivery.
cediLastErrorCode :: Lens' ConfigExportDeliveryInfo (Maybe Text)
cediLastErrorCode = lens _cediLastErrorCode (\ s a -> s{_cediLastErrorCode = a});

-- | The time of the last attempted delivery.
cediLastAttemptTime :: Lens' ConfigExportDeliveryInfo (Maybe UTCTime)
cediLastAttemptTime = lens _cediLastAttemptTime (\ s a -> s{_cediLastAttemptTime = a}) . mapping _Time;

-- | The time of the last successful delivery.
cediLastSuccessfulTime :: Lens' ConfigExportDeliveryInfo (Maybe UTCTime)
cediLastSuccessfulTime = lens _cediLastSuccessfulTime (\ s a -> s{_cediLastSuccessfulTime = a}) . mapping _Time;

-- | Status of the last attempted delivery.
cediLastStatus :: Lens' ConfigExportDeliveryInfo (Maybe DeliveryStatus)
cediLastStatus = lens _cediLastStatus (\ s a -> s{_cediLastStatus = a});

-- | The error message from the last attempted delivery.
cediLastErrorMessage :: Lens' ConfigExportDeliveryInfo (Maybe Text)
cediLastErrorMessage = lens _cediLastErrorMessage (\ s a -> s{_cediLastErrorMessage = a});

instance FromJSON ConfigExportDeliveryInfo where
        parseJSON
          = withObject "ConfigExportDeliveryInfo"
              (\ x ->
                 ConfigExportDeliveryInfo' <$>
                   (x .:? "lastErrorCode") <*> (x .:? "lastAttemptTime")
                     <*> (x .:? "lastSuccessfulTime")
                     <*> (x .:? "lastStatus")
                     <*> (x .:? "lastErrorMessage"))

-- | A list that contains the status of the delivery of the configuration
-- stream notification to the Amazon SNS topic.
--
-- /See:/ 'configStreamDeliveryInfo' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csdiLastErrorCode'
--
-- * 'csdiLastStatusChangeTime'
--
-- * 'csdiLastStatus'
--
-- * 'csdiLastErrorMessage'
data ConfigStreamDeliveryInfo = ConfigStreamDeliveryInfo'
    { _csdiLastErrorCode        :: !(Maybe Text)
    , _csdiLastStatusChangeTime :: !(Maybe POSIX)
    , _csdiLastStatus           :: !(Maybe DeliveryStatus)
    , _csdiLastErrorMessage     :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'ConfigStreamDeliveryInfo' smart constructor.
configStreamDeliveryInfo :: ConfigStreamDeliveryInfo
configStreamDeliveryInfo =
    ConfigStreamDeliveryInfo'
    { _csdiLastErrorCode = Nothing
    , _csdiLastStatusChangeTime = Nothing
    , _csdiLastStatus = Nothing
    , _csdiLastErrorMessage = Nothing
    }

-- | The error code from the last attempted delivery.
csdiLastErrorCode :: Lens' ConfigStreamDeliveryInfo (Maybe Text)
csdiLastErrorCode = lens _csdiLastErrorCode (\ s a -> s{_csdiLastErrorCode = a});

-- | The time from the last status change.
csdiLastStatusChangeTime :: Lens' ConfigStreamDeliveryInfo (Maybe UTCTime)
csdiLastStatusChangeTime = lens _csdiLastStatusChangeTime (\ s a -> s{_csdiLastStatusChangeTime = a}) . mapping _Time;

-- | Status of the last attempted delivery.
--
-- __Note__ Providing an SNS topic on a
-- <http://docs.aws.amazon.com/config/latest/APIReference/API_DeliveryChannel.html DeliveryChannel>
-- for AWS Config is optional. If the SNS delivery is turned off, the last
-- status will be __Not_Applicable__.
csdiLastStatus :: Lens' ConfigStreamDeliveryInfo (Maybe DeliveryStatus)
csdiLastStatus = lens _csdiLastStatus (\ s a -> s{_csdiLastStatus = a});

-- | The error message from the last attempted delivery.
csdiLastErrorMessage :: Lens' ConfigStreamDeliveryInfo (Maybe Text)
csdiLastErrorMessage = lens _csdiLastErrorMessage (\ s a -> s{_csdiLastErrorMessage = a});

instance FromJSON ConfigStreamDeliveryInfo where
        parseJSON
          = withObject "ConfigStreamDeliveryInfo"
              (\ x ->
                 ConfigStreamDeliveryInfo' <$>
                   (x .:? "lastErrorCode") <*>
                     (x .:? "lastStatusChangeTime")
                     <*> (x .:? "lastStatus")
                     <*> (x .:? "lastErrorMessage"))

-- | A list that contains detailed configurations of a specified resource.
--
-- Currently, the list does not contain information about non-AWS
-- components (for example, applications on your Amazon EC2 instances).
--
-- /See:/ 'configurationItem' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ciResourceId'
--
-- * 'ciConfigurationStateId'
--
-- * 'ciResourceType'
--
-- * 'ciArn'
--
-- * 'ciResourceCreationTime'
--
-- * 'ciConfigurationItemStatus'
--
-- * 'ciAccountId'
--
-- * 'ciConfigurationItemCaptureTime'
--
-- * 'ciAvailabilityZone'
--
-- * 'ciRelationships'
--
-- * 'ciVersion'
--
-- * 'ciRelatedEvents'
--
-- * 'ciConfiguration'
--
-- * 'ciConfigurationItemMD5Hash'
--
-- * 'ciTags'
data ConfigurationItem = ConfigurationItem'
    { _ciResourceId                   :: !(Maybe Text)
    , _ciConfigurationStateId         :: !(Maybe Text)
    , _ciResourceType                 :: !(Maybe ResourceType)
    , _ciArn                          :: !(Maybe Text)
    , _ciResourceCreationTime         :: !(Maybe POSIX)
    , _ciConfigurationItemStatus      :: !(Maybe ConfigurationItemStatus)
    , _ciAccountId                    :: !(Maybe Text)
    , _ciConfigurationItemCaptureTime :: !(Maybe POSIX)
    , _ciAvailabilityZone             :: !(Maybe Text)
    , _ciRelationships                :: !(Maybe [Relationship])
    , _ciVersion                      :: !(Maybe Text)
    , _ciRelatedEvents                :: !(Maybe [Text])
    , _ciConfiguration                :: !(Maybe Text)
    , _ciConfigurationItemMD5Hash     :: !(Maybe Text)
    , _ciTags                         :: !(Maybe (Map Text Text))
    } deriving (Eq,Read,Show)

-- | 'ConfigurationItem' smart constructor.
configurationItem :: ConfigurationItem
configurationItem =
    ConfigurationItem'
    { _ciResourceId = Nothing
    , _ciConfigurationStateId = Nothing
    , _ciResourceType = Nothing
    , _ciArn = Nothing
    , _ciResourceCreationTime = Nothing
    , _ciConfigurationItemStatus = Nothing
    , _ciAccountId = Nothing
    , _ciConfigurationItemCaptureTime = Nothing
    , _ciAvailabilityZone = Nothing
    , _ciRelationships = Nothing
    , _ciVersion = Nothing
    , _ciRelatedEvents = Nothing
    , _ciConfiguration = Nothing
    , _ciConfigurationItemMD5Hash = Nothing
    , _ciTags = Nothing
    }

-- | The ID of the resource (for example., @sg-xxxxxx@).
ciResourceId :: Lens' ConfigurationItem (Maybe Text)
ciResourceId = lens _ciResourceId (\ s a -> s{_ciResourceId = a});

-- | An identifier that indicates the ordering of the configuration items of
-- a resource.
ciConfigurationStateId :: Lens' ConfigurationItem (Maybe Text)
ciConfigurationStateId = lens _ciConfigurationStateId (\ s a -> s{_ciConfigurationStateId = a});

-- | The type of AWS resource.
ciResourceType :: Lens' ConfigurationItem (Maybe ResourceType)
ciResourceType = lens _ciResourceType (\ s a -> s{_ciResourceType = a});

-- | The Amazon Resource Name (ARN) of the resource.
ciArn :: Lens' ConfigurationItem (Maybe Text)
ciArn = lens _ciArn (\ s a -> s{_ciArn = a});

-- | The time stamp when the resource was created.
ciResourceCreationTime :: Lens' ConfigurationItem (Maybe UTCTime)
ciResourceCreationTime = lens _ciResourceCreationTime (\ s a -> s{_ciResourceCreationTime = a}) . mapping _Time;

-- | The configuration item status.
ciConfigurationItemStatus :: Lens' ConfigurationItem (Maybe ConfigurationItemStatus)
ciConfigurationItemStatus = lens _ciConfigurationItemStatus (\ s a -> s{_ciConfigurationItemStatus = a});

-- | The 12 digit AWS account ID associated with the resource.
ciAccountId :: Lens' ConfigurationItem (Maybe Text)
ciAccountId = lens _ciAccountId (\ s a -> s{_ciAccountId = a});

-- | The time when the configuration recording was initiated.
ciConfigurationItemCaptureTime :: Lens' ConfigurationItem (Maybe UTCTime)
ciConfigurationItemCaptureTime = lens _ciConfigurationItemCaptureTime (\ s a -> s{_ciConfigurationItemCaptureTime = a}) . mapping _Time;

-- | The Availability Zone associated with the resource.
ciAvailabilityZone :: Lens' ConfigurationItem (Maybe Text)
ciAvailabilityZone = lens _ciAvailabilityZone (\ s a -> s{_ciAvailabilityZone = a});

-- | A list of related AWS resources.
ciRelationships :: Lens' ConfigurationItem [Relationship]
ciRelationships = lens _ciRelationships (\ s a -> s{_ciRelationships = a}) . _Default;

-- | The version number of the resource configuration.
ciVersion :: Lens' ConfigurationItem (Maybe Text)
ciVersion = lens _ciVersion (\ s a -> s{_ciVersion = a});

-- | A list of CloudTrail event IDs.
--
-- A populated field indicates that the current configuration was initiated
-- by the events recorded in the CloudTrail log. For more information about
-- CloudTrail, see
-- <http://docs.aws.amazon.com/awscloudtrail/latest/userguide/what_is_cloud_trail_top_level.html What is AWS CloudTrail?>.
--
-- An empty field indicates that the current configuration was not
-- initiated by any event.
ciRelatedEvents :: Lens' ConfigurationItem [Text]
ciRelatedEvents = lens _ciRelatedEvents (\ s a -> s{_ciRelatedEvents = a}) . _Default;

-- | The description of the resource configuration.
ciConfiguration :: Lens' ConfigurationItem (Maybe Text)
ciConfiguration = lens _ciConfiguration (\ s a -> s{_ciConfiguration = a});

-- | Unique MD5 hash that represents the configuration item\'s state.
--
-- You can use MD5 hash to compare the states of two or more configuration
-- items that are associated with the same resource.
ciConfigurationItemMD5Hash :: Lens' ConfigurationItem (Maybe Text)
ciConfigurationItemMD5Hash = lens _ciConfigurationItemMD5Hash (\ s a -> s{_ciConfigurationItemMD5Hash = a});

-- | A mapping of key value tags associated with the resource.
ciTags :: Lens' ConfigurationItem (HashMap Text Text)
ciTags = lens _ciTags (\ s a -> s{_ciTags = a}) . _Default . _Map;

instance FromJSON ConfigurationItem where
        parseJSON
          = withObject "ConfigurationItem"
              (\ x ->
                 ConfigurationItem' <$>
                   (x .:? "resourceId") <*>
                     (x .:? "configurationStateId")
                     <*> (x .:? "resourceType")
                     <*> (x .:? "arn")
                     <*> (x .:? "resourceCreationTime")
                     <*> (x .:? "configurationItemStatus")
                     <*> (x .:? "accountId")
                     <*> (x .:? "configurationItemCaptureTime")
                     <*> (x .:? "availabilityZone")
                     <*> (x .:? "relationships" .!= mempty)
                     <*> (x .:? "version")
                     <*> (x .:? "relatedEvents" .!= mempty)
                     <*> (x .:? "configuration")
                     <*> (x .:? "configurationItemMD5Hash")
                     <*> (x .:? "tags" .!= mempty))

-- | An object that represents the recording of configuration changes of an
-- AWS resource.
--
-- /See:/ 'configurationRecorder' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crName'
--
-- * 'crRecordingGroup'
--
-- * 'crRoleARN'
data ConfigurationRecorder = ConfigurationRecorder'
    { _crName           :: !(Maybe Text)
    , _crRecordingGroup :: !(Maybe RecordingGroup)
    , _crRoleARN        :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'ConfigurationRecorder' smart constructor.
configurationRecorder :: ConfigurationRecorder
configurationRecorder =
    ConfigurationRecorder'
    { _crName = Nothing
    , _crRecordingGroup = Nothing
    , _crRoleARN = Nothing
    }

-- | The name of the recorder. By default, AWS Config automatically assigns
-- the name \"default\" when creating the configuration recorder. You
-- cannot change the assigned name.
crName :: Lens' ConfigurationRecorder (Maybe Text)
crName = lens _crName (\ s a -> s{_crName = a});

-- | The recording group specifies either to record configurations for all
-- supported resources or to provide a list of resource types to record.
-- The list of resource types must be a subset of supported resource types.
crRecordingGroup :: Lens' ConfigurationRecorder (Maybe RecordingGroup)
crRecordingGroup = lens _crRecordingGroup (\ s a -> s{_crRecordingGroup = a});

-- | Amazon Resource Name (ARN) of the IAM role used to describe the AWS
-- resources associated with the account.
crRoleARN :: Lens' ConfigurationRecorder (Maybe Text)
crRoleARN = lens _crRoleARN (\ s a -> s{_crRoleARN = a});

instance FromJSON ConfigurationRecorder where
        parseJSON
          = withObject "ConfigurationRecorder"
              (\ x ->
                 ConfigurationRecorder' <$>
                   (x .:? "name") <*> (x .:? "recordingGroup") <*>
                     (x .:? "roleARN"))

instance ToJSON ConfigurationRecorder where
        toJSON ConfigurationRecorder'{..}
          = object
              ["name" .= _crName,
               "recordingGroup" .= _crRecordingGroup,
               "roleARN" .= _crRoleARN]

-- | The current status of the configuration recorder.
--
-- /See:/ 'configurationRecorderStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crsLastErrorCode'
--
-- * 'crsLastStopTime'
--
-- * 'crsLastStatusChangeTime'
--
-- * 'crsRecording'
--
-- * 'crsLastStatus'
--
-- * 'crsLastErrorMessage'
--
-- * 'crsName'
--
-- * 'crsLastStartTime'
data ConfigurationRecorderStatus = ConfigurationRecorderStatus'
    { _crsLastErrorCode        :: !(Maybe Text)
    , _crsLastStopTime         :: !(Maybe POSIX)
    , _crsLastStatusChangeTime :: !(Maybe POSIX)
    , _crsRecording            :: !(Maybe Bool)
    , _crsLastStatus           :: !(Maybe RecorderStatus)
    , _crsLastErrorMessage     :: !(Maybe Text)
    , _crsName                 :: !(Maybe Text)
    , _crsLastStartTime        :: !(Maybe POSIX)
    } deriving (Eq,Read,Show)

-- | 'ConfigurationRecorderStatus' smart constructor.
configurationRecorderStatus :: ConfigurationRecorderStatus
configurationRecorderStatus =
    ConfigurationRecorderStatus'
    { _crsLastErrorCode = Nothing
    , _crsLastStopTime = Nothing
    , _crsLastStatusChangeTime = Nothing
    , _crsRecording = Nothing
    , _crsLastStatus = Nothing
    , _crsLastErrorMessage = Nothing
    , _crsName = Nothing
    , _crsLastStartTime = Nothing
    }

-- | The error code indicating that the recording failed.
crsLastErrorCode :: Lens' ConfigurationRecorderStatus (Maybe Text)
crsLastErrorCode = lens _crsLastErrorCode (\ s a -> s{_crsLastErrorCode = a});

-- | The time the recorder was last stopped.
crsLastStopTime :: Lens' ConfigurationRecorderStatus (Maybe UTCTime)
crsLastStopTime = lens _crsLastStopTime (\ s a -> s{_crsLastStopTime = a}) . mapping _Time;

-- | The time when the status was last changed.
crsLastStatusChangeTime :: Lens' ConfigurationRecorderStatus (Maybe UTCTime)
crsLastStatusChangeTime = lens _crsLastStatusChangeTime (\ s a -> s{_crsLastStatusChangeTime = a}) . mapping _Time;

-- | Specifies whether the recorder is currently recording or not.
crsRecording :: Lens' ConfigurationRecorderStatus (Maybe Bool)
crsRecording = lens _crsRecording (\ s a -> s{_crsRecording = a});

-- | The last (previous) status of the recorder.
crsLastStatus :: Lens' ConfigurationRecorderStatus (Maybe RecorderStatus)
crsLastStatus = lens _crsLastStatus (\ s a -> s{_crsLastStatus = a});

-- | The message indicating that the recording failed due to an error.
crsLastErrorMessage :: Lens' ConfigurationRecorderStatus (Maybe Text)
crsLastErrorMessage = lens _crsLastErrorMessage (\ s a -> s{_crsLastErrorMessage = a});

-- | The name of the configuration recorder.
crsName :: Lens' ConfigurationRecorderStatus (Maybe Text)
crsName = lens _crsName (\ s a -> s{_crsName = a});

-- | The time the recorder was last started.
crsLastStartTime :: Lens' ConfigurationRecorderStatus (Maybe UTCTime)
crsLastStartTime = lens _crsLastStartTime (\ s a -> s{_crsLastStartTime = a}) . mapping _Time;

instance FromJSON ConfigurationRecorderStatus where
        parseJSON
          = withObject "ConfigurationRecorderStatus"
              (\ x ->
                 ConfigurationRecorderStatus' <$>
                   (x .:? "lastErrorCode") <*> (x .:? "lastStopTime")
                     <*> (x .:? "lastStatusChangeTime")
                     <*> (x .:? "recording")
                     <*> (x .:? "lastStatus")
                     <*> (x .:? "lastErrorMessage")
                     <*> (x .:? "name")
                     <*> (x .:? "lastStartTime"))

-- | A logical container used for storing the configuration changes of an AWS
-- resource.
--
-- /See:/ 'deliveryChannel' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcS3KeyPrefix'
--
-- * 'dcSnsTopicARN'
--
-- * 'dcName'
--
-- * 'dcS3BucketName'
data DeliveryChannel = DeliveryChannel'
    { _dcS3KeyPrefix  :: !(Maybe Text)
    , _dcSnsTopicARN  :: !(Maybe Text)
    , _dcName         :: !(Maybe Text)
    , _dcS3BucketName :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'DeliveryChannel' smart constructor.
deliveryChannel :: DeliveryChannel
deliveryChannel =
    DeliveryChannel'
    { _dcS3KeyPrefix = Nothing
    , _dcSnsTopicARN = Nothing
    , _dcName = Nothing
    , _dcS3BucketName = Nothing
    }

-- | The prefix for the specified Amazon S3 bucket.
dcS3KeyPrefix :: Lens' DeliveryChannel (Maybe Text)
dcS3KeyPrefix = lens _dcS3KeyPrefix (\ s a -> s{_dcS3KeyPrefix = a});

-- | The Amazon Resource Name (ARN) of the IAM role used for accessing the
-- Amazon S3 bucket and the Amazon SNS topic.
dcSnsTopicARN :: Lens' DeliveryChannel (Maybe Text)
dcSnsTopicARN = lens _dcSnsTopicARN (\ s a -> s{_dcSnsTopicARN = a});

-- | The name of the delivery channel. By default, AWS Config automatically
-- assigns the name \"default\" when creating the delivery channel. You
-- cannot change the assigned name.
dcName :: Lens' DeliveryChannel (Maybe Text)
dcName = lens _dcName (\ s a -> s{_dcName = a});

-- | The name of the Amazon S3 bucket used to store configuration history for
-- the delivery channel.
dcS3BucketName :: Lens' DeliveryChannel (Maybe Text)
dcS3BucketName = lens _dcS3BucketName (\ s a -> s{_dcS3BucketName = a});

instance FromJSON DeliveryChannel where
        parseJSON
          = withObject "DeliveryChannel"
              (\ x ->
                 DeliveryChannel' <$>
                   (x .:? "s3KeyPrefix") <*> (x .:? "snsTopicARN") <*>
                     (x .:? "name")
                     <*> (x .:? "s3BucketName"))

instance ToJSON DeliveryChannel where
        toJSON DeliveryChannel'{..}
          = object
              ["s3KeyPrefix" .= _dcS3KeyPrefix,
               "snsTopicARN" .= _dcSnsTopicARN, "name" .= _dcName,
               "s3BucketName" .= _dcS3BucketName]

-- | The status of a specified delivery channel.
--
-- Valid values: @Success@ | @Failure@
--
-- /See:/ 'deliveryChannelStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsConfigStreamDeliveryInfo'
--
-- * 'dcsConfigSnapshotDeliveryInfo'
--
-- * 'dcsConfigHistoryDeliveryInfo'
--
-- * 'dcsName'
data DeliveryChannelStatus = DeliveryChannelStatus'
    { _dcsConfigStreamDeliveryInfo   :: !(Maybe ConfigStreamDeliveryInfo)
    , _dcsConfigSnapshotDeliveryInfo :: !(Maybe ConfigExportDeliveryInfo)
    , _dcsConfigHistoryDeliveryInfo  :: !(Maybe ConfigExportDeliveryInfo)
    , _dcsName                       :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'DeliveryChannelStatus' smart constructor.
deliveryChannelStatus :: DeliveryChannelStatus
deliveryChannelStatus =
    DeliveryChannelStatus'
    { _dcsConfigStreamDeliveryInfo = Nothing
    , _dcsConfigSnapshotDeliveryInfo = Nothing
    , _dcsConfigHistoryDeliveryInfo = Nothing
    , _dcsName = Nothing
    }

-- | A list containing the status of the delivery of the configuration stream
-- notification to the specified Amazon SNS topic.
dcsConfigStreamDeliveryInfo :: Lens' DeliveryChannelStatus (Maybe ConfigStreamDeliveryInfo)
dcsConfigStreamDeliveryInfo = lens _dcsConfigStreamDeliveryInfo (\ s a -> s{_dcsConfigStreamDeliveryInfo = a});

-- | A list containing the status of the delivery of the snapshot to the
-- specified Amazon S3 bucket.
dcsConfigSnapshotDeliveryInfo :: Lens' DeliveryChannelStatus (Maybe ConfigExportDeliveryInfo)
dcsConfigSnapshotDeliveryInfo = lens _dcsConfigSnapshotDeliveryInfo (\ s a -> s{_dcsConfigSnapshotDeliveryInfo = a});

-- | A list that contains the status of the delivery of the configuration
-- history to the specified Amazon S3 bucket.
dcsConfigHistoryDeliveryInfo :: Lens' DeliveryChannelStatus (Maybe ConfigExportDeliveryInfo)
dcsConfigHistoryDeliveryInfo = lens _dcsConfigHistoryDeliveryInfo (\ s a -> s{_dcsConfigHistoryDeliveryInfo = a});

-- | The name of the delivery channel.
dcsName :: Lens' DeliveryChannelStatus (Maybe Text)
dcsName = lens _dcsName (\ s a -> s{_dcsName = a});

instance FromJSON DeliveryChannelStatus where
        parseJSON
          = withObject "DeliveryChannelStatus"
              (\ x ->
                 DeliveryChannelStatus' <$>
                   (x .:? "configStreamDeliveryInfo") <*>
                     (x .:? "configSnapshotDeliveryInfo")
                     <*> (x .:? "configHistoryDeliveryInfo")
                     <*> (x .:? "name"))

-- | The group of AWS resource types that AWS Config records when starting
-- the configuration recorder.
--
-- __recordingGroup__ can have one and only one parameter. Choose either
-- __allSupported__ or __resourceTypes__.
--
-- /See:/ 'recordingGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rgAllSupported'
--
-- * 'rgResourceTypes'
data RecordingGroup = RecordingGroup'
    { _rgAllSupported  :: !(Maybe Bool)
    , _rgResourceTypes :: !(Maybe [ResourceType])
    } deriving (Eq,Read,Show)

-- | 'RecordingGroup' smart constructor.
recordingGroup :: RecordingGroup
recordingGroup =
    RecordingGroup'
    { _rgAllSupported = Nothing
    , _rgResourceTypes = Nothing
    }

-- | Records all supported resource types in the recording group. For a list
-- of supported resource types, see
-- <http://docs.aws.amazon.com/config/latest/developerguide/resource-config-reference.html#supported-resources Supported resource types>.
-- If you specify __allSupported__, you cannot enumerate a list of
-- __resourceTypes__.
rgAllSupported :: Lens' RecordingGroup (Maybe Bool)
rgAllSupported = lens _rgAllSupported (\ s a -> s{_rgAllSupported = a});

-- | A comma-separated list of strings representing valid AWS resource types
-- (e.g., @AWS::EC2::Instance@ or @AWS::CloudTrail::Trail@).
-- __resourceTypes__ is only valid if you have chosen not to select
-- __allSupported__. For a list of valid __resourceTypes__ values, see the
-- __resourceType Value__ column in the following topic:
-- <http://docs.aws.amazon.com/config/latest/developerguide/resource-config-reference.html#supported-resources Supported AWS Resource Types>.
rgResourceTypes :: Lens' RecordingGroup [ResourceType]
rgResourceTypes = lens _rgResourceTypes (\ s a -> s{_rgResourceTypes = a}) . _Default;

instance FromJSON RecordingGroup where
        parseJSON
          = withObject "RecordingGroup"
              (\ x ->
                 RecordingGroup' <$>
                   (x .:? "allSupported") <*>
                     (x .:? "resourceTypes" .!= mempty))

instance ToJSON RecordingGroup where
        toJSON RecordingGroup'{..}
          = object
              ["allSupported" .= _rgAllSupported,
               "resourceTypes" .= _rgResourceTypes]

-- | The relationship of the related resource to the main resource.
--
-- /See:/ 'relationship' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'relResourceId'
--
-- * 'relResourceType'
--
-- * 'relRelationshipName'
data Relationship = Relationship'
    { _relResourceId       :: !(Maybe Text)
    , _relResourceType     :: !(Maybe ResourceType)
    , _relRelationshipName :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'Relationship' smart constructor.
relationship :: Relationship
relationship =
    Relationship'
    { _relResourceId = Nothing
    , _relResourceType = Nothing
    , _relRelationshipName = Nothing
    }

-- | The resource ID of the related resource (for example, @sg-xxxxxx@).
relResourceId :: Lens' Relationship (Maybe Text)
relResourceId = lens _relResourceId (\ s a -> s{_relResourceId = a});

-- | The resource type of the related resource.
relResourceType :: Lens' Relationship (Maybe ResourceType)
relResourceType = lens _relResourceType (\ s a -> s{_relResourceType = a});

-- | The name of the related resource.
relRelationshipName :: Lens' Relationship (Maybe Text)
relRelationshipName = lens _relRelationshipName (\ s a -> s{_relRelationshipName = a});

instance FromJSON Relationship where
        parseJSON
          = withObject "Relationship"
              (\ x ->
                 Relationship' <$>
                   (x .:? "resourceId") <*> (x .:? "resourceType") <*>
                     (x .:? "relationshipName"))

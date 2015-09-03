{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.Product
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.Product where

import           Network.AWS.Config.Types.Sum
import           Network.AWS.Prelude

-- | A list that contains the status of the delivery of either the snapshot
-- or the configuration history to the specified Amazon S3 bucket.
--
-- /See:/ 'configExportDeliveryInfo' smart constructor.
data ConfigExportDeliveryInfo = ConfigExportDeliveryInfo'
    { _cediLastErrorCode      :: !(Maybe Text)
    , _cediLastAttemptTime    :: !(Maybe POSIX)
    , _cediLastSuccessfulTime :: !(Maybe POSIX)
    , _cediLastStatus         :: !(Maybe DeliveryStatus)
    , _cediLastErrorMessage   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ConfigExportDeliveryInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
configExportDeliveryInfo
    :: ConfigExportDeliveryInfo
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
data ConfigStreamDeliveryInfo = ConfigStreamDeliveryInfo'
    { _csdiLastErrorCode        :: !(Maybe Text)
    , _csdiLastStatusChangeTime :: !(Maybe POSIX)
    , _csdiLastStatus           :: !(Maybe DeliveryStatus)
    , _csdiLastErrorMessage     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ConfigStreamDeliveryInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csdiLastErrorCode'
--
-- * 'csdiLastStatusChangeTime'
--
-- * 'csdiLastStatus'
--
-- * 'csdiLastErrorMessage'
configStreamDeliveryInfo
    :: ConfigStreamDeliveryInfo
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
data ConfigurationItem = ConfigurationItem'
    { _ciResourceId                   :: !(Maybe Text)
    , _ciResourceType                 :: !(Maybe ResourceType)
    , _ciConfigurationStateId         :: !(Maybe Text)
    , _ciArn                          :: !(Maybe Text)
    , _ciResourceName                 :: !(Maybe Text)
    , _ciResourceCreationTime         :: !(Maybe POSIX)
    , _ciConfigurationItemStatus      :: !(Maybe ConfigurationItemStatus)
    , _ciConfigurationItemCaptureTime :: !(Maybe POSIX)
    , _ciAccountId                    :: !(Maybe Text)
    , _ciAvailabilityZone             :: !(Maybe Text)
    , _ciRelationships                :: !(Maybe [Relationship])
    , _ciVersion                      :: !(Maybe Text)
    , _ciAwsRegion                    :: !(Maybe Text)
    , _ciRelatedEvents                :: !(Maybe [Text])
    , _ciConfiguration                :: !(Maybe Text)
    , _ciConfigurationItemMD5Hash     :: !(Maybe Text)
    , _ciTags                         :: !(Maybe (Map Text Text))
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ConfigurationItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciResourceId'
--
-- * 'ciResourceType'
--
-- * 'ciConfigurationStateId'
--
-- * 'ciArn'
--
-- * 'ciResourceName'
--
-- * 'ciResourceCreationTime'
--
-- * 'ciConfigurationItemStatus'
--
-- * 'ciConfigurationItemCaptureTime'
--
-- * 'ciAccountId'
--
-- * 'ciAvailabilityZone'
--
-- * 'ciRelationships'
--
-- * 'ciVersion'
--
-- * 'ciAwsRegion'
--
-- * 'ciRelatedEvents'
--
-- * 'ciConfiguration'
--
-- * 'ciConfigurationItemMD5Hash'
--
-- * 'ciTags'
configurationItem
    :: ConfigurationItem
configurationItem =
    ConfigurationItem'
    { _ciResourceId = Nothing
    , _ciResourceType = Nothing
    , _ciConfigurationStateId = Nothing
    , _ciArn = Nothing
    , _ciResourceName = Nothing
    , _ciResourceCreationTime = Nothing
    , _ciConfigurationItemStatus = Nothing
    , _ciConfigurationItemCaptureTime = Nothing
    , _ciAccountId = Nothing
    , _ciAvailabilityZone = Nothing
    , _ciRelationships = Nothing
    , _ciVersion = Nothing
    , _ciAwsRegion = Nothing
    , _ciRelatedEvents = Nothing
    , _ciConfiguration = Nothing
    , _ciConfigurationItemMD5Hash = Nothing
    , _ciTags = Nothing
    }

-- | The ID of the resource (for example., 'sg-xxxxxx').
ciResourceId :: Lens' ConfigurationItem (Maybe Text)
ciResourceId = lens _ciResourceId (\ s a -> s{_ciResourceId = a});

-- | The type of AWS resource.
ciResourceType :: Lens' ConfigurationItem (Maybe ResourceType)
ciResourceType = lens _ciResourceType (\ s a -> s{_ciResourceType = a});

-- | An identifier that indicates the ordering of the configuration items of
-- a resource.
ciConfigurationStateId :: Lens' ConfigurationItem (Maybe Text)
ciConfigurationStateId = lens _ciConfigurationStateId (\ s a -> s{_ciConfigurationStateId = a});

-- | The Amazon Resource Name (ARN) of the resource.
ciArn :: Lens' ConfigurationItem (Maybe Text)
ciArn = lens _ciArn (\ s a -> s{_ciArn = a});

-- | The custom name of the resource, if available.
ciResourceName :: Lens' ConfigurationItem (Maybe Text)
ciResourceName = lens _ciResourceName (\ s a -> s{_ciResourceName = a});

-- | The time stamp when the resource was created.
ciResourceCreationTime :: Lens' ConfigurationItem (Maybe UTCTime)
ciResourceCreationTime = lens _ciResourceCreationTime (\ s a -> s{_ciResourceCreationTime = a}) . mapping _Time;

-- | The configuration item status.
ciConfigurationItemStatus :: Lens' ConfigurationItem (Maybe ConfigurationItemStatus)
ciConfigurationItemStatus = lens _ciConfigurationItemStatus (\ s a -> s{_ciConfigurationItemStatus = a});

-- | The time when the configuration recording was initiated.
ciConfigurationItemCaptureTime :: Lens' ConfigurationItem (Maybe UTCTime)
ciConfigurationItemCaptureTime = lens _ciConfigurationItemCaptureTime (\ s a -> s{_ciConfigurationItemCaptureTime = a}) . mapping _Time;

-- | The 12 digit AWS account ID associated with the resource.
ciAccountId :: Lens' ConfigurationItem (Maybe Text)
ciAccountId = lens _ciAccountId (\ s a -> s{_ciAccountId = a});

-- | The Availability Zone associated with the resource.
ciAvailabilityZone :: Lens' ConfigurationItem (Maybe Text)
ciAvailabilityZone = lens _ciAvailabilityZone (\ s a -> s{_ciAvailabilityZone = a});

-- | A list of related AWS resources.
ciRelationships :: Lens' ConfigurationItem [Relationship]
ciRelationships = lens _ciRelationships (\ s a -> s{_ciRelationships = a}) . _Default . _Coerce;

-- | The version number of the resource configuration.
ciVersion :: Lens' ConfigurationItem (Maybe Text)
ciVersion = lens _ciVersion (\ s a -> s{_ciVersion = a});

-- | The region where the resource resides.
ciAwsRegion :: Lens' ConfigurationItem (Maybe Text)
ciAwsRegion = lens _ciAwsRegion (\ s a -> s{_ciAwsRegion = a});

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
ciRelatedEvents = lens _ciRelatedEvents (\ s a -> s{_ciRelatedEvents = a}) . _Default . _Coerce;

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
                   (x .:? "resourceId") <*> (x .:? "resourceType") <*>
                     (x .:? "configurationStateId")
                     <*> (x .:? "arn")
                     <*> (x .:? "resourceName")
                     <*> (x .:? "resourceCreationTime")
                     <*> (x .:? "configurationItemStatus")
                     <*> (x .:? "configurationItemCaptureTime")
                     <*> (x .:? "accountId")
                     <*> (x .:? "availabilityZone")
                     <*> (x .:? "relationships" .!= mempty)
                     <*> (x .:? "version")
                     <*> (x .:? "awsRegion")
                     <*> (x .:? "relatedEvents" .!= mempty)
                     <*> (x .:? "configuration")
                     <*> (x .:? "configurationItemMD5Hash")
                     <*> (x .:? "tags" .!= mempty))

-- | An object that represents the recording of configuration changes of an
-- AWS resource.
--
-- /See:/ 'configurationRecorder' smart constructor.
data ConfigurationRecorder = ConfigurationRecorder'
    { _crName           :: !(Maybe Text)
    , _crRecordingGroup :: !(Maybe RecordingGroup)
    , _crRoleARN        :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ConfigurationRecorder' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crName'
--
-- * 'crRecordingGroup'
--
-- * 'crRoleARN'
configurationRecorder
    :: ConfigurationRecorder
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
              (catMaybes
                 [("name" .=) <$> _crName,
                  ("recordingGroup" .=) <$> _crRecordingGroup,
                  ("roleARN" .=) <$> _crRoleARN])

-- | The current status of the configuration recorder.
--
-- /See:/ 'configurationRecorderStatus' smart constructor.
data ConfigurationRecorderStatus = ConfigurationRecorderStatus'
    { _crsLastErrorCode        :: !(Maybe Text)
    , _crsLastStopTime         :: !(Maybe POSIX)
    , _crsLastStatusChangeTime :: !(Maybe POSIX)
    , _crsRecording            :: !(Maybe Bool)
    , _crsLastStatus           :: !(Maybe RecorderStatus)
    , _crsLastErrorMessage     :: !(Maybe Text)
    , _crsName                 :: !(Maybe Text)
    , _crsLastStartTime        :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ConfigurationRecorderStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
configurationRecorderStatus
    :: ConfigurationRecorderStatus
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
data DeliveryChannel = DeliveryChannel'
    { _dcS3KeyPrefix  :: !(Maybe Text)
    , _dcSnsTopicARN  :: !(Maybe Text)
    , _dcName         :: !(Maybe Text)
    , _dcS3BucketName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeliveryChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcS3KeyPrefix'
--
-- * 'dcSnsTopicARN'
--
-- * 'dcName'
--
-- * 'dcS3BucketName'
deliveryChannel
    :: DeliveryChannel
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

-- | The Amazon Resource Name (ARN) of the SNS topic that AWS Config delivers
-- notifications to.
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
              (catMaybes
                 [("s3KeyPrefix" .=) <$> _dcS3KeyPrefix,
                  ("snsTopicARN" .=) <$> _dcSnsTopicARN,
                  ("name" .=) <$> _dcName,
                  ("s3BucketName" .=) <$> _dcS3BucketName])

-- | The status of a specified delivery channel.
--
-- Valid values: 'Success' | 'Failure'
--
-- /See:/ 'deliveryChannelStatus' smart constructor.
data DeliveryChannelStatus = DeliveryChannelStatus'
    { _dcsConfigSnapshotDeliveryInfo :: !(Maybe ConfigExportDeliveryInfo)
    , _dcsConfigStreamDeliveryInfo   :: !(Maybe ConfigStreamDeliveryInfo)
    , _dcsConfigHistoryDeliveryInfo  :: !(Maybe ConfigExportDeliveryInfo)
    , _dcsName                       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeliveryChannelStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsConfigSnapshotDeliveryInfo'
--
-- * 'dcsConfigStreamDeliveryInfo'
--
-- * 'dcsConfigHistoryDeliveryInfo'
--
-- * 'dcsName'
deliveryChannelStatus
    :: DeliveryChannelStatus
deliveryChannelStatus =
    DeliveryChannelStatus'
    { _dcsConfigSnapshotDeliveryInfo = Nothing
    , _dcsConfigStreamDeliveryInfo = Nothing
    , _dcsConfigHistoryDeliveryInfo = Nothing
    , _dcsName = Nothing
    }

-- | A list containing the status of the delivery of the snapshot to the
-- specified Amazon S3 bucket.
dcsConfigSnapshotDeliveryInfo :: Lens' DeliveryChannelStatus (Maybe ConfigExportDeliveryInfo)
dcsConfigSnapshotDeliveryInfo = lens _dcsConfigSnapshotDeliveryInfo (\ s a -> s{_dcsConfigSnapshotDeliveryInfo = a});

-- | A list containing the status of the delivery of the configuration stream
-- notification to the specified Amazon SNS topic.
dcsConfigStreamDeliveryInfo :: Lens' DeliveryChannelStatus (Maybe ConfigStreamDeliveryInfo)
dcsConfigStreamDeliveryInfo = lens _dcsConfigStreamDeliveryInfo (\ s a -> s{_dcsConfigStreamDeliveryInfo = a});

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
                   (x .:? "configSnapshotDeliveryInfo") <*>
                     (x .:? "configStreamDeliveryInfo")
                     <*> (x .:? "configHistoryDeliveryInfo")
                     <*> (x .:? "name"))

-- | The group of AWS resource types that AWS Config records when starting
-- the configuration recorder.
--
-- __recordingGroup__ can have one and only one parameter. Choose either
-- __allSupported__ or __resourceTypes__.
--
-- /See:/ 'recordingGroup' smart constructor.
data RecordingGroup = RecordingGroup'
    { _rgAllSupported  :: !(Maybe Bool)
    , _rgResourceTypes :: !(Maybe [ResourceType])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RecordingGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rgAllSupported'
--
-- * 'rgResourceTypes'
recordingGroup
    :: RecordingGroup
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
-- (for example, 'AWS::EC2::Instance' or 'AWS::CloudTrail::Trail').
-- __resourceTypes__ is only valid if you have chosen not to select
-- __allSupported__. For a list of valid __resourceTypes__ values, see the
-- __resourceType Value__ column in the following topic:
-- <http://docs.aws.amazon.com/config/latest/developerguide/resource-config-reference.html#supported-resources Supported AWS Resource Types>.
rgResourceTypes :: Lens' RecordingGroup [ResourceType]
rgResourceTypes = lens _rgResourceTypes (\ s a -> s{_rgResourceTypes = a}) . _Default . _Coerce;

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
              (catMaybes
                 [("allSupported" .=) <$> _rgAllSupported,
                  ("resourceTypes" .=) <$> _rgResourceTypes])

-- | The relationship of the related resource to the main resource.
--
-- /See:/ 'relationship' smart constructor.
data Relationship = Relationship'
    { _rResourceId       :: !(Maybe Text)
    , _rResourceType     :: !(Maybe ResourceType)
    , _rResourceName     :: !(Maybe Text)
    , _rRelationshipName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Relationship' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rResourceId'
--
-- * 'rResourceType'
--
-- * 'rResourceName'
--
-- * 'rRelationshipName'
relationship
    :: Relationship
relationship =
    Relationship'
    { _rResourceId = Nothing
    , _rResourceType = Nothing
    , _rResourceName = Nothing
    , _rRelationshipName = Nothing
    }

-- | The ID of the related resource (for example, 'sg-xxxxxx').
rResourceId :: Lens' Relationship (Maybe Text)
rResourceId = lens _rResourceId (\ s a -> s{_rResourceId = a});

-- | The resource type of the related resource.
rResourceType :: Lens' Relationship (Maybe ResourceType)
rResourceType = lens _rResourceType (\ s a -> s{_rResourceType = a});

-- | The custom name of the related resource, if available.
rResourceName :: Lens' Relationship (Maybe Text)
rResourceName = lens _rResourceName (\ s a -> s{_rResourceName = a});

-- | The type of relationship with the related resource.
rRelationshipName :: Lens' Relationship (Maybe Text)
rRelationshipName = lens _rRelationshipName (\ s a -> s{_rRelationshipName = a});

instance FromJSON Relationship where
        parseJSON
          = withObject "Relationship"
              (\ x ->
                 Relationship' <$>
                   (x .:? "resourceId") <*> (x .:? "resourceType") <*>
                     (x .:? "resourceName")
                     <*> (x .:? "relationshipName"))

-- | The details that identify a resource that is discovered by AWS Config,
-- including the resource type, ID, and (if available) the custom resource
-- name.
--
-- /See:/ 'resourceIdentifier' smart constructor.
data ResourceIdentifier = ResourceIdentifier'
    { _riResourceId           :: !(Maybe Text)
    , _riResourceType         :: !(Maybe ResourceType)
    , _riResourceName         :: !(Maybe Text)
    , _riResourceDeletionTime :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ResourceIdentifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'riResourceId'
--
-- * 'riResourceType'
--
-- * 'riResourceName'
--
-- * 'riResourceDeletionTime'
resourceIdentifier
    :: ResourceIdentifier
resourceIdentifier =
    ResourceIdentifier'
    { _riResourceId = Nothing
    , _riResourceType = Nothing
    , _riResourceName = Nothing
    , _riResourceDeletionTime = Nothing
    }

-- | The ID of the resource (for example., 'sg-xxxxxx').
riResourceId :: Lens' ResourceIdentifier (Maybe Text)
riResourceId = lens _riResourceId (\ s a -> s{_riResourceId = a});

-- | The type of resource.
riResourceType :: Lens' ResourceIdentifier (Maybe ResourceType)
riResourceType = lens _riResourceType (\ s a -> s{_riResourceType = a});

-- | The custom name of the resource (if available).
riResourceName :: Lens' ResourceIdentifier (Maybe Text)
riResourceName = lens _riResourceName (\ s a -> s{_riResourceName = a});

-- | The time that the resource was deleted.
riResourceDeletionTime :: Lens' ResourceIdentifier (Maybe UTCTime)
riResourceDeletionTime = lens _riResourceDeletionTime (\ s a -> s{_riResourceDeletionTime = a}) . mapping _Time;

instance FromJSON ResourceIdentifier where
        parseJSON
          = withObject "ResourceIdentifier"
              (\ x ->
                 ResourceIdentifier' <$>
                   (x .:? "resourceId") <*> (x .:? "resourceType") <*>
                     (x .:? "resourceName")
                     <*> (x .:? "resourceDeletionTime"))

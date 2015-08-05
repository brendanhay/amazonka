{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
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
    , rResourceId
    , rResourceType
    , rRelationshipName
    ) where

import           Network.AWS.Config.Types.Product
import           Network.AWS.Config.Types.Sum
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
            , _svcTimeout = Just 70
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
_ValidationException :: AsError a => Getting (First ServiceError) a ServiceError
_ValidationException = _ServiceError . hasCode "ValidationException"

-- | The specified time range is not valid. The earlier time is not
-- chronologically before the later time.
_InvalidTimeRangeException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTimeRangeException =
    _ServiceError . hasCode "InvalidTimeRangeException"

-- | AWS Config throws an exception if the recording group does not contain a
-- valid list of resource types. Invalid values could also be incorrectly
-- formatted.
_InvalidRecordingGroupException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRecordingGroupException =
    _ServiceError . hasCode "InvalidRecordingGroupException"

-- | The specified Amazon SNS topic does not exist.
_InvalidSNSTopicARNException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSNSTopicARNException =
    _ServiceError . hasCode "InvalidSNSTopicARNException"

-- | You have provided a null or empty role ARN.
_InvalidRoleException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRoleException = _ServiceError . hasCode "InvalidRoleException"

-- | You cannot delete the delivery channel you specified because the
-- configuration recorder is running.
_LastDeliveryChannelDeleteFailedException :: AsError a => Getting (First ServiceError) a ServiceError
_LastDeliveryChannelDeleteFailedException =
    _ServiceError . hasCode "LastDeliveryChannelDeleteFailedException"

-- | You have reached the limit on the pagination.
_InvalidLimitException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidLimitException = _ServiceError . hasCode "InvalidLimitException"

-- | The specified delivery channel name is not valid.
_InvalidDeliveryChannelNameException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDeliveryChannelNameException =
    _ServiceError . hasCode "InvalidDeliveryChannelNameException"

-- | You have specified a delivery channel that does not exist.
_NoSuchDeliveryChannelException :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchDeliveryChannelException =
    _ServiceError . hasCode "NoSuchDeliveryChannelException"

-- | You have specified a resource that is either unknown or has not been
-- discovered.
_ResourceNotDiscoveredException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotDiscoveredException =
    _ServiceError . hasCode "ResourceNotDiscoveredException"

-- | The specified nextToken for pagination is not valid.
_InvalidNextTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidNextTokenException =
    _ServiceError . hasCode "InvalidNextTokenException"

-- | The specified Amazon S3 bucket does not exist.
_NoSuchBucketException :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchBucketException = _ServiceError . hasCode "NoSuchBucketException"

-- | There are no configuration recorders available to provide the role
-- needed to describe your resources.
_NoAvailableConfigurationRecorderException :: AsError a => Getting (First ServiceError) a ServiceError
_NoAvailableConfigurationRecorderException =
    _ServiceError . hasCode "NoAvailableConfigurationRecorderException"

-- | There is no delivery channel available to record configurations.
_NoAvailableDeliveryChannelException :: AsError a => Getting (First ServiceError) a ServiceError
_NoAvailableDeliveryChannelException =
    _ServiceError . hasCode "NoAvailableDeliveryChannelException"

-- | There is no configuration recorder running.
_NoRunningConfigurationRecorderException :: AsError a => Getting (First ServiceError) a ServiceError
_NoRunningConfigurationRecorderException =
    _ServiceError . hasCode "NoRunningConfigurationRecorderException"

-- | You have reached the limit on the number of recorders you can create.
_MaxNumberOfConfigurationRecordersExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_MaxNumberOfConfigurationRecordersExceededException =
    _ServiceError .
    hasCode "MaxNumberOfConfigurationRecordersExceededException"

-- | You have provided a configuration recorder name that is not valid.
_InvalidConfigurationRecorderNameException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidConfigurationRecorderNameException =
    _ServiceError . hasCode "InvalidConfigurationRecorderNameException"

-- | Your Amazon S3 bucket policy does not permit AWS Config to write to it.
_InsufficientDeliveryPolicyException :: AsError a => Getting (First ServiceError) a ServiceError
_InsufficientDeliveryPolicyException =
    _ServiceError . hasCode "InsufficientDeliveryPolicyException"

-- | You have reached the limit on the number of delivery channels you can
-- create.
_MaxNumberOfDeliveryChannelsExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_MaxNumberOfDeliveryChannelsExceededException =
    _ServiceError . hasCode "MaxNumberOfDeliveryChannelsExceededException"

-- | You have specified a configuration recorder that does not exist.
_NoSuchConfigurationRecorderException :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchConfigurationRecorderException =
    _ServiceError . hasCode "NoSuchConfigurationRecorderException"

-- | The specified Amazon S3 key prefix is not valid.
_InvalidS3KeyPrefixException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidS3KeyPrefixException =
    _ServiceError . hasCode "InvalidS3KeyPrefixException"

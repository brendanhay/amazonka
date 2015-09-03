{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types
    (
    -- * Service Configuration
      config

    -- * Errors
    , _InvalidTimeRangeException
    , _InvalidSNSTopicARNException
    , _InvalidRecordingGroupException
    , _ValidationException
    , _InvalidRoleException
    , _LastDeliveryChannelDeleteFailedException
    , _InvalidLimitException
    , _InvalidDeliveryChannelNameException
    , _NoSuchDeliveryChannelException
    , _ResourceNotDiscoveredException
    , _InvalidNextTokenException
    , _NoAvailableConfigurationRecorderException
    , _NoSuchBucketException
    , _NoAvailableDeliveryChannelException
    , _InvalidConfigurationRecorderNameException
    , _NoRunningConfigurationRecorderException
    , _MaxNumberOfConfigurationRecordersExceededException
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
    , ciResourceType
    , ciConfigurationStateId
    , ciArn
    , ciResourceName
    , ciResourceCreationTime
    , ciConfigurationItemStatus
    , ciConfigurationItemCaptureTime
    , ciAccountId
    , ciAvailabilityZone
    , ciRelationships
    , ciVersion
    , ciAwsRegion
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
    , dcsConfigSnapshotDeliveryInfo
    , dcsConfigStreamDeliveryInfo
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
    , rResourceName
    , rRelationshipName

    -- * ResourceIdentifier
    , ResourceIdentifier
    , resourceIdentifier
    , riResourceId
    , riResourceType
    , riResourceName
    , riResourceDeletionTime
    ) where

import           Network.AWS.Config.Types.Product
import           Network.AWS.Config.Types.Sum
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | API version '2014-11-12' of the Amazon Config SDK configuration.
config :: Service
config =
    Service
    { _svcAbbrev = "Config"
    , _svcSigner = v4
    , _svcPrefix = "config"
    , _svcVersion = "2014-11-12"
    , _svcEndpoint = defaultEndpoint config
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError
    , _svcRetry = retry
    }
  where
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

-- | The specified time range is not valid. The earlier time is not
-- chronologically before the later time.
_InvalidTimeRangeException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTimeRangeException =
    _ServiceError . hasCode "InvalidTimeRangeException"

-- | The specified Amazon SNS topic does not exist.
_InvalidSNSTopicARNException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSNSTopicARNException =
    _ServiceError . hasCode "InvalidSNSTopicARNException"

-- | AWS Config throws an exception if the recording group does not contain a
-- valid list of resource types. Invalid values could also be incorrectly
-- formatted.
_InvalidRecordingGroupException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRecordingGroupException =
    _ServiceError . hasCode "InvalidRecordingGroupException"

-- | The requested action is not valid.
_ValidationException :: AsError a => Getting (First ServiceError) a ServiceError
_ValidationException = _ServiceError . hasCode "ValidationException"

-- | You have provided a null or empty role ARN.
_InvalidRoleException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRoleException = _ServiceError . hasCode "InvalidRoleException"

-- | You cannot delete the delivery channel you specified because the
-- configuration recorder is running.
_LastDeliveryChannelDeleteFailedException :: AsError a => Getting (First ServiceError) a ServiceError
_LastDeliveryChannelDeleteFailedException =
    _ServiceError . hasCode "LastDeliveryChannelDeleteFailedException"

-- | The specified limit is outside the allowable range.
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

-- | The specified next token is invalid. Specify the 'nextToken' string that
-- was returned in the previous response to get the next page of results.
_InvalidNextTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidNextTokenException =
    _ServiceError . hasCode "InvalidNextTokenException"

-- | There are no configuration recorders available to provide the role
-- needed to describe your resources. Create a configuration recorder.
_NoAvailableConfigurationRecorderException :: AsError a => Getting (First ServiceError) a ServiceError
_NoAvailableConfigurationRecorderException =
    _ServiceError . hasCode "NoAvailableConfigurationRecorderException"

-- | The specified Amazon S3 bucket does not exist.
_NoSuchBucketException :: AsError a => Getting (First ServiceError) a ServiceError
_NoSuchBucketException = _ServiceError . hasCode "NoSuchBucketException"

-- | There is no delivery channel available to record configurations.
_NoAvailableDeliveryChannelException :: AsError a => Getting (First ServiceError) a ServiceError
_NoAvailableDeliveryChannelException =
    _ServiceError . hasCode "NoAvailableDeliveryChannelException"

-- | You have provided a configuration recorder name that is not valid.
_InvalidConfigurationRecorderNameException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidConfigurationRecorderNameException =
    _ServiceError . hasCode "InvalidConfigurationRecorderNameException"

-- | There is no configuration recorder running.
_NoRunningConfigurationRecorderException :: AsError a => Getting (First ServiceError) a ServiceError
_NoRunningConfigurationRecorderException =
    _ServiceError . hasCode "NoRunningConfigurationRecorderException"

-- | You have reached the limit on the number of recorders you can create.
_MaxNumberOfConfigurationRecordersExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_MaxNumberOfConfigurationRecordersExceededException =
    _ServiceError .
    hasCode "MaxNumberOfConfigurationRecordersExceededException"

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

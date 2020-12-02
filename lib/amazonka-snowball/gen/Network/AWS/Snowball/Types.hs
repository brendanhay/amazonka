{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Snowball.Types
    (
    -- * Service Configuration
      snowball

    -- * Errors
    , _InvalidResourceException
    , _UnsupportedAddressException
    , _KMSRequestFailedException
    , _InvalidJobStateException
    , _InvalidInputCombinationException
    , _InvalidNextTokenException
    , _InvalidAddressException
    , _ClusterLimitExceededException

    -- * ClusterState
    , ClusterState (..)

    -- * JobState
    , JobState (..)

    -- * JobType
    , JobType (..)

    -- * ShippingOption
    , ShippingOption (..)

    -- * SnowballCapacity
    , SnowballCapacity (..)

    -- * SnowballType
    , SnowballType (..)

    -- * Address
    , Address
    , address
    , aIsRestricted
    , aStreet3
    , aLandmark
    , aPostalCode
    , aCountry
    , aStateOrProvince
    , aStreet2
    , aAddressId
    , aCity
    , aPhoneNumber
    , aCompany
    , aName
    , aPrefectureOrDistrict
    , aStreet1

    -- * ClusterListEntry
    , ClusterListEntry
    , clusterListEntry
    , cleClusterState
    , cleClusterId
    , cleCreationDate
    , cleDescription

    -- * ClusterMetadata
    , ClusterMetadata
    , clusterMetadata
    , cmJobType
    , cmKMSKeyARN
    , cmClusterState
    , cmNotification
    , cmForwardingAddressId
    , cmAddressId
    , cmSnowballType
    , cmShippingOption
    , cmResources
    , cmClusterId
    , cmCreationDate
    , cmDescription
    , cmRoleARN

    -- * DataTransfer
    , DataTransfer
    , dataTransfer
    , dtTotalObjects
    , dtTotalBytes
    , dtObjectsTransferred
    , dtBytesTransferred

    -- * EventTriggerDefinition
    , EventTriggerDefinition
    , eventTriggerDefinition
    , etdEventResourceARN

    -- * JobListEntry
    , JobListEntry
    , jobListEntry
    , jleJobType
    , jleJobId
    , jleJobState
    , jleSnowballType
    , jleCreationDate
    , jleDescription
    , jleIsMaster

    -- * JobLogs
    , JobLogs
    , jobLogs
    , jlJobFailureLogURI
    , jlJobCompletionReportURI
    , jlJobSuccessLogURI

    -- * JobMetadata
    , JobMetadata
    , jobMetadata
    , jmJobType
    , jmKMSKeyARN
    , jmJobId
    , jmJobLogInfo
    , jmNotification
    , jmJobState
    , jmForwardingAddressId
    , jmShippingDetails
    , jmAddressId
    , jmSnowballType
    , jmDataTransferProgress
    , jmResources
    , jmClusterId
    , jmCreationDate
    , jmDescription
    , jmRoleARN
    , jmSnowballCapacityPreference

    -- * JobResource
    , JobResource
    , jobResource
    , jrLambdaResources
    , jrS3Resources

    -- * KeyRange
    , KeyRange
    , keyRange
    , krEndMarker
    , krBeginMarker

    -- * LambdaResource
    , LambdaResource
    , lambdaResource
    , lrEventTriggers
    , lrLambdaARN

    -- * Notification
    , Notification
    , notification
    , nNotifyAll
    , nSNSTopicARN
    , nJobStatesToNotify

    -- * S3Resource
    , S3Resource
    , s3Resource
    , srKeyRange
    , srBucketARN

    -- * Shipment
    , Shipment
    , shipment
    , sStatus
    , sTrackingNumber

    -- * ShippingDetails
    , ShippingDetails
    , shippingDetails
    , sdShippingOption
    , sdOutboundShipment
    , sdInboundShipment
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4
import Network.AWS.Snowball.Types.Product
import Network.AWS.Snowball.Types.Sum

-- | API version @2016-06-30@ of the Amazon Import/Export Snowball SDK configuration.
snowball :: Service
snowball =
  Service
    { _svcAbbrev = "Snowball"
    , _svcSigner = v4
    , _svcPrefix = "snowball"
    , _svcVersion = "2016-06-30"
    , _svcEndpoint = defaultEndpoint snowball
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "Snowball"
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
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | The specified resource can't be found. Check the information you provided in your last request, and try again.
--
--
_InvalidResourceException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidResourceException =
  _MatchServiceError snowball "InvalidResourceException"


-- | The address is either outside the serviceable area for your region, or an error occurred. Check the address with your region's carrier and try again. If the issue persists, contact AWS Support.
--
--
_UnsupportedAddressException :: AsError a => Getting (First ServiceError) a ServiceError
_UnsupportedAddressException =
  _MatchServiceError snowball "UnsupportedAddressException"


-- | The provided AWS Key Management Service key lacks the permissions to perform the specified 'CreateJob' or 'UpdateJob' action.
--
--
_KMSRequestFailedException :: AsError a => Getting (First ServiceError) a ServiceError
_KMSRequestFailedException =
  _MatchServiceError snowball "KMSRequestFailedException"


-- | The action can't be performed because the job's current state doesn't allow that action to be performed.
--
--
_InvalidJobStateException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidJobStateException =
  _MatchServiceError snowball "InvalidJobStateException"


-- | Job or cluster creation failed. One ore more inputs were invalid. Confirm that the 'CreateClusterRequest$SnowballType' value supports your 'CreateJobRequest$JobType' , and try again.
--
--
_InvalidInputCombinationException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidInputCombinationException =
  _MatchServiceError snowball "InvalidInputCombinationException"


-- | The @NextToken@ string was altered unexpectedly, and the operation has stopped. Run the operation without changing the @NextToken@ string, and try again.
--
--
_InvalidNextTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidNextTokenException =
  _MatchServiceError snowball "InvalidNextTokenException"


-- | The address provided was invalid. Check the address with your region's carrier, and try again.
--
--
_InvalidAddressException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidAddressException = _MatchServiceError snowball "InvalidAddressException"


-- | Job creation failed. Currently, clusters support five nodes. If you have less than five nodes for your cluster and you have more nodes to create for this cluster, try again and create jobs until your cluster has exactly five notes.
--
--
_ClusterLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterLimitExceededException =
  _MatchServiceError snowball "ClusterLimitExceededException"


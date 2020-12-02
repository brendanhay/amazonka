{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types
    (
    -- * Service Configuration
      ioTAnalytics

    -- * Errors
    , _InvalidRequestException
    , _ResourceAlreadyExistsException
    , _ThrottlingException
    , _InternalFailureException
    , _ServiceUnavailableException
    , _ResourceNotFoundException
    , _LimitExceededException

    -- * ChannelStatus
    , ChannelStatus (..)

    -- * DatasetContentState
    , DatasetContentState (..)

    -- * DatasetStatus
    , DatasetStatus (..)

    -- * DatastoreStatus
    , DatastoreStatus (..)

    -- * LoggingLevel
    , LoggingLevel (..)

    -- * ReprocessingStatus
    , ReprocessingStatus (..)

    -- * AddAttributesActivity
    , AddAttributesActivity
    , addAttributesActivity
    , aaaNext
    , aaaName
    , aaaAttributes

    -- * BatchPutMessageErrorEntry
    , BatchPutMessageErrorEntry
    , batchPutMessageErrorEntry
    , bpmeeErrorCode
    , bpmeeErrorMessage
    , bpmeeMessageId

    -- * Channel
    , Channel
    , channel
    , cCreationTime
    , cStatus
    , cArn
    , cRetentionPeriod
    , cName
    , cLastUpdateTime

    -- * ChannelActivity
    , ChannelActivity
    , channelActivity
    , caNext
    , caName
    , caChannelName

    -- * ChannelSummary
    , ChannelSummary
    , channelSummary
    , csCreationTime
    , csStatus
    , csChannelName
    , csLastUpdateTime

    -- * Dataset
    , Dataset
    , dataset
    , dCreationTime
    , dStatus
    , dArn
    , dActions
    , dTriggers
    , dName
    , dLastUpdateTime

    -- * DatasetAction
    , DatasetAction
    , datasetAction
    , daQueryAction
    , daActionName

    -- * DatasetContentStatus
    , DatasetContentStatus
    , datasetContentStatus
    , dcsState
    , dcsReason

    -- * DatasetEntry
    , DatasetEntry
    , datasetEntry
    , deEntryName
    , deDataURI

    -- * DatasetSummary
    , DatasetSummary
    , datasetSummary
    , dssCreationTime
    , dssStatus
    , dssDatasetName
    , dssLastUpdateTime

    -- * DatasetTrigger
    , DatasetTrigger
    , datasetTrigger
    , dtSchedule

    -- * Datastore
    , Datastore
    , datastore
    , datCreationTime
    , datStatus
    , datArn
    , datRetentionPeriod
    , datName
    , datLastUpdateTime

    -- * DatastoreActivity
    , DatastoreActivity
    , datastoreActivity
    , daName
    , daDatastoreName

    -- * DatastoreSummary
    , DatastoreSummary
    , datastoreSummary
    , dsCreationTime
    , dsStatus
    , dsDatastoreName
    , dsLastUpdateTime

    -- * DeviceRegistryEnrichActivity
    , DeviceRegistryEnrichActivity
    , deviceRegistryEnrichActivity
    , dreaNext
    , dreaName
    , dreaAttribute
    , dreaThingName
    , dreaRoleARN

    -- * DeviceShadowEnrichActivity
    , DeviceShadowEnrichActivity
    , deviceShadowEnrichActivity
    , dseaNext
    , dseaName
    , dseaAttribute
    , dseaThingName
    , dseaRoleARN

    -- * FilterActivity
    , FilterActivity
    , filterActivity
    , faNext
    , faName
    , faFilter

    -- * LambdaActivity
    , LambdaActivity
    , lambdaActivity
    , laNext
    , laName
    , laLambdaName
    , laBatchSize

    -- * LoggingOptions
    , LoggingOptions
    , loggingOptions
    , loRoleARN
    , loLevel
    , loEnabled

    -- * MathActivity
    , MathActivity
    , mathActivity
    , maNext
    , maName
    , maAttribute
    , maMath

    -- * Message
    , Message
    , message
    , mMessageId
    , mPayload

    -- * Pipeline
    , Pipeline
    , pipeline
    , pCreationTime
    , pArn
    , pActivities
    , pName
    , pReprocessingSummaries
    , pLastUpdateTime

    -- * PipelineActivity
    , PipelineActivity
    , pipelineActivity
    , paSelectAttributes
    , paChannel
    , paAddAttributes
    , paDeviceRegistryEnrich
    , paRemoveAttributes
    , paLambda
    , paDatastore
    , paDeviceShadowEnrich
    , paFilter
    , paMath

    -- * PipelineSummary
    , PipelineSummary
    , pipelineSummary
    , psCreationTime
    , psPipelineName
    , psReprocessingSummaries
    , psLastUpdateTime

    -- * RemoveAttributesActivity
    , RemoveAttributesActivity
    , removeAttributesActivity
    , raaNext
    , raaName
    , raaAttributes

    -- * ReprocessingSummary
    , ReprocessingSummary
    , reprocessingSummary
    , rsCreationTime
    , rsStatus
    , rsId

    -- * RetentionPeriod
    , RetentionPeriod
    , retentionPeriod
    , rpUnlimited
    , rpNumberOfDays

    -- * Schedule
    , Schedule
    , schedule
    , sExpression

    -- * SelectAttributesActivity
    , SelectAttributesActivity
    , selectAttributesActivity
    , saaNext
    , saaName
    , saaAttributes

    -- * SqlQueryDatasetAction
    , SqlQueryDatasetAction
    , sqlQueryDatasetAction
    , sqdaSqlQuery
    ) where

import Network.AWS.IoTAnalytics.Types.Product
import Network.AWS.IoTAnalytics.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-11-27@ of the Amazon IoT Analytics SDK configuration.
ioTAnalytics :: Service
ioTAnalytics =
  Service
    { _svcAbbrev = "IoTAnalytics"
    , _svcSigner = v4
    , _svcPrefix = "iotanalytics"
    , _svcVersion = "2017-11-27"
    , _svcEndpoint = defaultEndpoint ioTAnalytics
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "IoTAnalytics"
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


-- | The request was not valid.
--
--
_InvalidRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRequestException =
  _MatchServiceError ioTAnalytics "InvalidRequestException" . hasStatus 400


-- | A resource with the same name already exists.
--
--
_ResourceAlreadyExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceAlreadyExistsException =
  _MatchServiceError ioTAnalytics "ResourceAlreadyExistsException" .
  hasStatus 409


-- | The request was denied due to request throttling.
--
--
_ThrottlingException :: AsError a => Getting (First ServiceError) a ServiceError
_ThrottlingException =
  _MatchServiceError ioTAnalytics "ThrottlingException" . hasStatus 429


-- | There was an internal failure.
--
--
_InternalFailureException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalFailureException =
  _MatchServiceError ioTAnalytics "InternalFailureException" . hasStatus 500


-- | The service is temporarily unavailable.
--
--
_ServiceUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_ServiceUnavailableException =
  _MatchServiceError ioTAnalytics "ServiceUnavailableException" . hasStatus 503


-- | A resource with the specified name could not be found.
--
--
_ResourceNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_ResourceNotFoundException =
  _MatchServiceError ioTAnalytics "ResourceNotFoundException" . hasStatus 404


-- | The command caused an internal limit to be exceeded.
--
--
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
  _MatchServiceError ioTAnalytics "LimitExceededException" . hasStatus 410


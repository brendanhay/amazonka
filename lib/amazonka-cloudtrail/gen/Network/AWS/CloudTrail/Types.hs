{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types
  ( -- * Service Configuration
    cloudTrail,

    -- * Errors

    -- * EventCategory
    EventCategory (..),

    -- * InsightType
    InsightType (..),

    -- * LookupAttributeKey
    LookupAttributeKey (..),

    -- * ReadWriteType
    ReadWriteType (..),

    -- * AdvancedEventSelector
    AdvancedEventSelector,
    advancedEventSelector,
    aesName,
    aesFieldSelectors,

    -- * AdvancedFieldSelector
    AdvancedFieldSelector,
    advancedFieldSelector,
    afsEndsWith,
    afsNotStartsWith,
    afsEquals,
    afsNotEquals,
    afsNotEndsWith,
    afsStartsWith,
    afsField,

    -- * DataResource
    DataResource,
    dataResource,
    drValues,
    drType,

    -- * Event
    Event,
    event,
    eUsername,
    eResources,
    eEventTime,
    eCloudTrailEvent,
    eEventName,
    eReadOnly,
    eAccessKeyId,
    eEventSource,
    eEventId,

    -- * EventSelector
    EventSelector,
    eventSelector,
    esDataResources,
    esReadWriteType,
    esExcludeManagementEventSources,
    esIncludeManagementEvents,

    -- * InsightSelector
    InsightSelector,
    insightSelector,
    isInsightType,

    -- * LookupAttribute
    LookupAttribute,
    lookupAttribute,
    laAttributeKey,
    laAttributeValue,

    -- * PublicKey
    PublicKey,
    publicKey,
    pkFingerprint,
    pkValidityEndTime,
    pkValue,
    pkValidityStartTime,

    -- * Resource
    Resource,
    resource,
    rResourceType,
    rResourceName,

    -- * ResourceTag
    ResourceTag,
    resourceTag,
    rResourceId,
    rTagsList,

    -- * Tag
    Tag,
    tag,
    tagValue,
    tagKey,

    -- * Trail
    Trail,
    trail,
    tLogFileValidationEnabled,
    tTrailARN,
    tS3KeyPrefix,
    tHasInsightSelectors,
    tSNSTopicARN,
    tSNSTopicName,
    tCloudWatchLogsLogGroupARN,
    tKMSKeyId,
    tHomeRegion,
    tName,
    tIncludeGlobalServiceEvents,
    tHasCustomEventSelectors,
    tIsOrganizationTrail,
    tCloudWatchLogsRoleARN,
    tS3BucketName,
    tIsMultiRegionTrail,

    -- * TrailInfo
    TrailInfo,
    trailInfo,
    tiTrailARN,
    tiHomeRegion,
    tiName,
  )
where

import Network.AWS.CloudTrail.Types.AdvancedEventSelector
import Network.AWS.CloudTrail.Types.AdvancedFieldSelector
import Network.AWS.CloudTrail.Types.DataResource
import Network.AWS.CloudTrail.Types.Event
import Network.AWS.CloudTrail.Types.EventCategory
import Network.AWS.CloudTrail.Types.EventSelector
import Network.AWS.CloudTrail.Types.InsightSelector
import Network.AWS.CloudTrail.Types.InsightType
import Network.AWS.CloudTrail.Types.LookupAttribute
import Network.AWS.CloudTrail.Types.LookupAttributeKey
import Network.AWS.CloudTrail.Types.PublicKey
import Network.AWS.CloudTrail.Types.ReadWriteType
import Network.AWS.CloudTrail.Types.Resource
import Network.AWS.CloudTrail.Types.ResourceTag
import Network.AWS.CloudTrail.Types.Tag
import Network.AWS.CloudTrail.Types.Trail
import Network.AWS.CloudTrail.Types.TrailInfo
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2013-11-01@ of the Amazon CloudTrail SDK configuration.
cloudTrail :: Service
cloudTrail =
  Service
    { _svcAbbrev = "CloudTrail",
      _svcSigner = v4,
      _svcPrefix = "cloudtrail",
      _svcVersion = "2013-11-01",
      _svcEndpoint = defaultEndpoint cloudTrail,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "CloudTrail",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

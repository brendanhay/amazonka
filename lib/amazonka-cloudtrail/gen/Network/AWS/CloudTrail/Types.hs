-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types
  ( -- * Service configuration
    cloudTrailService,

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
    AdvancedEventSelector (..),
    mkAdvancedEventSelector,
    aesName,
    aesFieldSelectors,

    -- * AdvancedFieldSelector
    AdvancedFieldSelector (..),
    mkAdvancedFieldSelector,
    afsEndsWith,
    afsNotStartsWith,
    afsEquals,
    afsNotEquals,
    afsNotEndsWith,
    afsStartsWith,
    afsField,

    -- * DataResource
    DataResource (..),
    mkDataResource,
    drValues,
    drType,

    -- * Event
    Event (..),
    mkEvent,
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
    EventSelector (..),
    mkEventSelector,
    esDataResources,
    esReadWriteType,
    esExcludeManagementEventSources,
    esIncludeManagementEvents,

    -- * InsightSelector
    InsightSelector (..),
    mkInsightSelector,
    isInsightType,

    -- * LookupAttribute
    LookupAttribute (..),
    mkLookupAttribute,
    laAttributeKey,
    laAttributeValue,

    -- * PublicKey
    PublicKey (..),
    mkPublicKey,
    pkFingerprint,
    pkValidityEndTime,
    pkValue,
    pkValidityStartTime,

    -- * Resource
    Resource (..),
    mkResource,
    rResourceType,
    rResourceName,

    -- * ResourceTag
    ResourceTag (..),
    mkResourceTag,
    rResourceId,
    rTagsList,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- * Trail
    Trail (..),
    mkTrail,
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
    TrailInfo (..),
    mkTrailInfo,
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2013-11-01@ of the Amazon CloudTrail SDK configuration.
cloudTrailService :: Lude.Service
cloudTrailService =
  Lude.Service
    { Lude._svcAbbrev = "CloudTrail",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "cloudtrail",
      Lude._svcVersion = "2013-11-01",
      Lude._svcEndpoint = Lude.defaultEndpoint cloudTrailService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "CloudTrail",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing

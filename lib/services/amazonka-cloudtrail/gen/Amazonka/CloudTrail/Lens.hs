{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudTrail.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudTrail.Lens
  ( -- * Operations

    -- ** AddTags
    addTags_tagsList,
    addTags_resourceId,
    addTagsResponse_httpStatus,

    -- ** CreateTrail
    createTrail_s3KeyPrefix,
    createTrail_snsTopicName,
    createTrail_isOrganizationTrail,
    createTrail_includeGlobalServiceEvents,
    createTrail_tagsList,
    createTrail_isMultiRegionTrail,
    createTrail_kmsKeyId,
    createTrail_enableLogFileValidation,
    createTrail_cloudWatchLogsRoleArn,
    createTrail_cloudWatchLogsLogGroupArn,
    createTrail_name,
    createTrail_s3BucketName,
    createTrailResponse_s3KeyPrefix,
    createTrailResponse_name,
    createTrailResponse_logFileValidationEnabled,
    createTrailResponse_snsTopicName,
    createTrailResponse_isOrganizationTrail,
    createTrailResponse_includeGlobalServiceEvents,
    createTrailResponse_s3BucketName,
    createTrailResponse_snsTopicARN,
    createTrailResponse_isMultiRegionTrail,
    createTrailResponse_kmsKeyId,
    createTrailResponse_cloudWatchLogsRoleArn,
    createTrailResponse_cloudWatchLogsLogGroupArn,
    createTrailResponse_trailARN,
    createTrailResponse_httpStatus,

    -- ** DeleteTrail
    deleteTrail_name,
    deleteTrailResponse_httpStatus,

    -- ** DescribeTrails
    describeTrails_trailNameList,
    describeTrails_includeShadowTrails,
    describeTrailsResponse_trailList,
    describeTrailsResponse_httpStatus,

    -- ** GetEventSelectors
    getEventSelectors_trailName,
    getEventSelectorsResponse_advancedEventSelectors,
    getEventSelectorsResponse_eventSelectors,
    getEventSelectorsResponse_trailARN,
    getEventSelectorsResponse_httpStatus,

    -- ** GetInsightSelectors
    getInsightSelectors_trailName,
    getInsightSelectorsResponse_insightSelectors,
    getInsightSelectorsResponse_trailARN,
    getInsightSelectorsResponse_httpStatus,

    -- ** GetTrail
    getTrail_name,
    getTrailResponse_trail,
    getTrailResponse_httpStatus,

    -- ** GetTrailStatus
    getTrailStatus_name,
    getTrailStatusResponse_latestCloudWatchLogsDeliveryTime,
    getTrailStatusResponse_latestNotificationTime,
    getTrailStatusResponse_timeLoggingStarted,
    getTrailStatusResponse_latestNotificationAttemptTime,
    getTrailStatusResponse_startLoggingTime,
    getTrailStatusResponse_latestCloudWatchLogsDeliveryError,
    getTrailStatusResponse_latestDeliveryTime,
    getTrailStatusResponse_latestDeliveryError,
    getTrailStatusResponse_isLogging,
    getTrailStatusResponse_latestNotificationAttemptSucceeded,
    getTrailStatusResponse_latestNotificationError,
    getTrailStatusResponse_stopLoggingTime,
    getTrailStatusResponse_latestDigestDeliveryTime,
    getTrailStatusResponse_latestDigestDeliveryError,
    getTrailStatusResponse_latestDeliveryAttemptTime,
    getTrailStatusResponse_timeLoggingStopped,
    getTrailStatusResponse_latestDeliveryAttemptSucceeded,
    getTrailStatusResponse_httpStatus,

    -- ** ListPublicKeys
    listPublicKeys_nextToken,
    listPublicKeys_endTime,
    listPublicKeys_startTime,
    listPublicKeysResponse_nextToken,
    listPublicKeysResponse_publicKeyList,
    listPublicKeysResponse_httpStatus,

    -- ** ListTags
    listTags_nextToken,
    listTags_resourceIdList,
    listTagsResponse_nextToken,
    listTagsResponse_resourceTagList,
    listTagsResponse_httpStatus,

    -- ** ListTrails
    listTrails_nextToken,
    listTrailsResponse_nextToken,
    listTrailsResponse_trails,
    listTrailsResponse_httpStatus,

    -- ** LookupEvents
    lookupEvents_nextToken,
    lookupEvents_lookupAttributes,
    lookupEvents_endTime,
    lookupEvents_maxResults,
    lookupEvents_startTime,
    lookupEvents_eventCategory,
    lookupEventsResponse_nextToken,
    lookupEventsResponse_events,
    lookupEventsResponse_httpStatus,

    -- ** PutEventSelectors
    putEventSelectors_advancedEventSelectors,
    putEventSelectors_eventSelectors,
    putEventSelectors_trailName,
    putEventSelectorsResponse_advancedEventSelectors,
    putEventSelectorsResponse_eventSelectors,
    putEventSelectorsResponse_trailARN,
    putEventSelectorsResponse_httpStatus,

    -- ** PutInsightSelectors
    putInsightSelectors_trailName,
    putInsightSelectors_insightSelectors,
    putInsightSelectorsResponse_insightSelectors,
    putInsightSelectorsResponse_trailARN,
    putInsightSelectorsResponse_httpStatus,

    -- ** RemoveTags
    removeTags_tagsList,
    removeTags_resourceId,
    removeTagsResponse_httpStatus,

    -- ** StartLogging
    startLogging_name,
    startLoggingResponse_httpStatus,

    -- ** StopLogging
    stopLogging_name,
    stopLoggingResponse_httpStatus,

    -- ** UpdateTrail
    updateTrail_s3KeyPrefix,
    updateTrail_snsTopicName,
    updateTrail_isOrganizationTrail,
    updateTrail_includeGlobalServiceEvents,
    updateTrail_s3BucketName,
    updateTrail_isMultiRegionTrail,
    updateTrail_kmsKeyId,
    updateTrail_enableLogFileValidation,
    updateTrail_cloudWatchLogsRoleArn,
    updateTrail_cloudWatchLogsLogGroupArn,
    updateTrail_name,
    updateTrailResponse_s3KeyPrefix,
    updateTrailResponse_name,
    updateTrailResponse_logFileValidationEnabled,
    updateTrailResponse_snsTopicName,
    updateTrailResponse_isOrganizationTrail,
    updateTrailResponse_includeGlobalServiceEvents,
    updateTrailResponse_s3BucketName,
    updateTrailResponse_snsTopicARN,
    updateTrailResponse_isMultiRegionTrail,
    updateTrailResponse_kmsKeyId,
    updateTrailResponse_cloudWatchLogsRoleArn,
    updateTrailResponse_cloudWatchLogsLogGroupArn,
    updateTrailResponse_trailARN,
    updateTrailResponse_httpStatus,

    -- * Types

    -- ** AdvancedEventSelector
    advancedEventSelector_name,
    advancedEventSelector_fieldSelectors,

    -- ** AdvancedFieldSelector
    advancedFieldSelector_notEquals,
    advancedFieldSelector_equals,
    advancedFieldSelector_endsWith,
    advancedFieldSelector_startsWith,
    advancedFieldSelector_notEndsWith,
    advancedFieldSelector_notStartsWith,
    advancedFieldSelector_field,

    -- ** DataResource
    dataResource_type,
    dataResource_values,

    -- ** Event
    event_username,
    event_readOnly,
    event_eventName,
    event_eventId,
    event_resources,
    event_eventTime,
    event_cloudTrailEvent,
    event_accessKeyId,
    event_eventSource,

    -- ** EventSelector
    eventSelector_excludeManagementEventSources,
    eventSelector_includeManagementEvents,
    eventSelector_dataResources,
    eventSelector_readWriteType,

    -- ** InsightSelector
    insightSelector_insightType,

    -- ** LookupAttribute
    lookupAttribute_attributeKey,
    lookupAttribute_attributeValue,

    -- ** PublicKey
    publicKey_validityStartTime,
    publicKey_validityEndTime,
    publicKey_fingerprint,
    publicKey_value,

    -- ** Resource
    resource_resourceType,
    resource_resourceName,

    -- ** ResourceTag
    resourceTag_resourceId,
    resourceTag_tagsList,

    -- ** Tag
    tag_value,
    tag_key,

    -- ** Trail
    trail_s3KeyPrefix,
    trail_name,
    trail_logFileValidationEnabled,
    trail_snsTopicName,
    trail_isOrganizationTrail,
    trail_includeGlobalServiceEvents,
    trail_s3BucketName,
    trail_hasCustomEventSelectors,
    trail_snsTopicARN,
    trail_isMultiRegionTrail,
    trail_kmsKeyId,
    trail_cloudWatchLogsRoleArn,
    trail_homeRegion,
    trail_cloudWatchLogsLogGroupArn,
    trail_hasInsightSelectors,
    trail_trailARN,

    -- ** TrailInfo
    trailInfo_name,
    trailInfo_homeRegion,
    trailInfo_trailARN,
  )
where

import Amazonka.CloudTrail.AddTags
import Amazonka.CloudTrail.CreateTrail
import Amazonka.CloudTrail.DeleteTrail
import Amazonka.CloudTrail.DescribeTrails
import Amazonka.CloudTrail.GetEventSelectors
import Amazonka.CloudTrail.GetInsightSelectors
import Amazonka.CloudTrail.GetTrail
import Amazonka.CloudTrail.GetTrailStatus
import Amazonka.CloudTrail.ListPublicKeys
import Amazonka.CloudTrail.ListTags
import Amazonka.CloudTrail.ListTrails
import Amazonka.CloudTrail.LookupEvents
import Amazonka.CloudTrail.PutEventSelectors
import Amazonka.CloudTrail.PutInsightSelectors
import Amazonka.CloudTrail.RemoveTags
import Amazonka.CloudTrail.StartLogging
import Amazonka.CloudTrail.StopLogging
import Amazonka.CloudTrail.Types.AdvancedEventSelector
import Amazonka.CloudTrail.Types.AdvancedFieldSelector
import Amazonka.CloudTrail.Types.DataResource
import Amazonka.CloudTrail.Types.Event
import Amazonka.CloudTrail.Types.EventSelector
import Amazonka.CloudTrail.Types.InsightSelector
import Amazonka.CloudTrail.Types.LookupAttribute
import Amazonka.CloudTrail.Types.PublicKey
import Amazonka.CloudTrail.Types.Resource
import Amazonka.CloudTrail.Types.ResourceTag
import Amazonka.CloudTrail.Types.Tag
import Amazonka.CloudTrail.Types.Trail
import Amazonka.CloudTrail.Types.TrailInfo
import Amazonka.CloudTrail.UpdateTrail

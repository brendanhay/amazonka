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

    -- ** DescribeTrails
    describeTrails_includeShadowTrails,
    describeTrails_trailNameList,
    describeTrailsResponse_trailList,
    describeTrailsResponse_httpStatus,

    -- ** ListPublicKeys
    listPublicKeys_startTime,
    listPublicKeys_nextToken,
    listPublicKeys_endTime,
    listPublicKeysResponse_publicKeyList,
    listPublicKeysResponse_nextToken,
    listPublicKeysResponse_httpStatus,

    -- ** RemoveTags
    removeTags_tagsList,
    removeTags_resourceId,
    removeTagsResponse_httpStatus,

    -- ** LookupEvents
    lookupEvents_eventCategory,
    lookupEvents_startTime,
    lookupEvents_lookupAttributes,
    lookupEvents_nextToken,
    lookupEvents_endTime,
    lookupEvents_maxResults,
    lookupEventsResponse_nextToken,
    lookupEventsResponse_events,
    lookupEventsResponse_httpStatus,

    -- ** StopLogging
    stopLogging_name,
    stopLoggingResponse_httpStatus,

    -- ** DeleteTrail
    deleteTrail_name,
    deleteTrailResponse_httpStatus,

    -- ** UpdateTrail
    updateTrail_s3KeyPrefix,
    updateTrail_snsTopicName,
    updateTrail_enableLogFileValidation,
    updateTrail_cloudWatchLogsLogGroupArn,
    updateTrail_kmsKeyId,
    updateTrail_includeGlobalServiceEvents,
    updateTrail_isOrganizationTrail,
    updateTrail_cloudWatchLogsRoleArn,
    updateTrail_s3BucketName,
    updateTrail_isMultiRegionTrail,
    updateTrail_name,
    updateTrailResponse_logFileValidationEnabled,
    updateTrailResponse_trailARN,
    updateTrailResponse_s3KeyPrefix,
    updateTrailResponse_snsTopicARN,
    updateTrailResponse_snsTopicName,
    updateTrailResponse_cloudWatchLogsLogGroupArn,
    updateTrailResponse_kmsKeyId,
    updateTrailResponse_name,
    updateTrailResponse_includeGlobalServiceEvents,
    updateTrailResponse_isOrganizationTrail,
    updateTrailResponse_cloudWatchLogsRoleArn,
    updateTrailResponse_s3BucketName,
    updateTrailResponse_isMultiRegionTrail,
    updateTrailResponse_httpStatus,

    -- ** CreateTrail
    createTrail_s3KeyPrefix,
    createTrail_snsTopicName,
    createTrail_enableLogFileValidation,
    createTrail_cloudWatchLogsLogGroupArn,
    createTrail_kmsKeyId,
    createTrail_includeGlobalServiceEvents,
    createTrail_tagsList,
    createTrail_isOrganizationTrail,
    createTrail_cloudWatchLogsRoleArn,
    createTrail_isMultiRegionTrail,
    createTrail_name,
    createTrail_s3BucketName,
    createTrailResponse_logFileValidationEnabled,
    createTrailResponse_trailARN,
    createTrailResponse_s3KeyPrefix,
    createTrailResponse_snsTopicARN,
    createTrailResponse_snsTopicName,
    createTrailResponse_cloudWatchLogsLogGroupArn,
    createTrailResponse_kmsKeyId,
    createTrailResponse_name,
    createTrailResponse_includeGlobalServiceEvents,
    createTrailResponse_isOrganizationTrail,
    createTrailResponse_cloudWatchLogsRoleArn,
    createTrailResponse_s3BucketName,
    createTrailResponse_isMultiRegionTrail,
    createTrailResponse_httpStatus,

    -- ** PutInsightSelectors
    putInsightSelectors_trailName,
    putInsightSelectors_insightSelectors,
    putInsightSelectorsResponse_trailARN,
    putInsightSelectorsResponse_insightSelectors,
    putInsightSelectorsResponse_httpStatus,

    -- ** GetEventSelectors
    getEventSelectors_trailName,
    getEventSelectorsResponse_trailARN,
    getEventSelectorsResponse_eventSelectors,
    getEventSelectorsResponse_advancedEventSelectors,
    getEventSelectorsResponse_httpStatus,

    -- ** GetTrail
    getTrail_name,
    getTrailResponse_trail,
    getTrailResponse_httpStatus,

    -- ** GetTrailStatus
    getTrailStatus_name,
    getTrailStatusResponse_timeLoggingStopped,
    getTrailStatusResponse_latestDeliveryError,
    getTrailStatusResponse_latestDigestDeliveryTime,
    getTrailStatusResponse_latestNotificationAttemptSucceeded,
    getTrailStatusResponse_startLoggingTime,
    getTrailStatusResponse_latestNotificationError,
    getTrailStatusResponse_latestDeliveryAttemptSucceeded,
    getTrailStatusResponse_isLogging,
    getTrailStatusResponse_timeLoggingStarted,
    getTrailStatusResponse_latestDigestDeliveryError,
    getTrailStatusResponse_latestDeliveryAttemptTime,
    getTrailStatusResponse_latestDeliveryTime,
    getTrailStatusResponse_latestCloudWatchLogsDeliveryTime,
    getTrailStatusResponse_latestCloudWatchLogsDeliveryError,
    getTrailStatusResponse_latestNotificationTime,
    getTrailStatusResponse_latestNotificationAttemptTime,
    getTrailStatusResponse_stopLoggingTime,
    getTrailStatusResponse_httpStatus,

    -- ** AddTags
    addTags_tagsList,
    addTags_resourceId,
    addTagsResponse_httpStatus,

    -- ** ListTags
    listTags_nextToken,
    listTags_resourceIdList,
    listTagsResponse_nextToken,
    listTagsResponse_resourceTagList,
    listTagsResponse_httpStatus,

    -- ** PutEventSelectors
    putEventSelectors_eventSelectors,
    putEventSelectors_advancedEventSelectors,
    putEventSelectors_trailName,
    putEventSelectorsResponse_trailARN,
    putEventSelectorsResponse_eventSelectors,
    putEventSelectorsResponse_advancedEventSelectors,
    putEventSelectorsResponse_httpStatus,

    -- ** StartLogging
    startLogging_name,
    startLoggingResponse_httpStatus,

    -- ** ListTrails
    listTrails_nextToken,
    listTrailsResponse_nextToken,
    listTrailsResponse_trails,
    listTrailsResponse_httpStatus,

    -- ** GetInsightSelectors
    getInsightSelectors_trailName,
    getInsightSelectorsResponse_trailARN,
    getInsightSelectorsResponse_insightSelectors,
    getInsightSelectorsResponse_httpStatus,

    -- * Types

    -- ** AdvancedEventSelector
    advancedEventSelector_name,
    advancedEventSelector_fieldSelectors,

    -- ** AdvancedFieldSelector
    advancedFieldSelector_endsWith,
    advancedFieldSelector_notStartsWith,
    advancedFieldSelector_equals,
    advancedFieldSelector_notEquals,
    advancedFieldSelector_notEndsWith,
    advancedFieldSelector_startsWith,
    advancedFieldSelector_field,

    -- ** DataResource
    dataResource_values,
    dataResource_type,

    -- ** Event
    event_username,
    event_resources,
    event_eventTime,
    event_cloudTrailEvent,
    event_eventName,
    event_readOnly,
    event_accessKeyId,
    event_eventSource,
    event_eventId,

    -- ** EventSelector
    eventSelector_dataResources,
    eventSelector_readWriteType,
    eventSelector_excludeManagementEventSources,
    eventSelector_includeManagementEvents,

    -- ** InsightSelector
    insightSelector_insightType,

    -- ** LookupAttribute
    lookupAttribute_attributeKey,
    lookupAttribute_attributeValue,

    -- ** PublicKey
    publicKey_fingerprint,
    publicKey_validityEndTime,
    publicKey_value,
    publicKey_validityStartTime,

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
    trail_logFileValidationEnabled,
    trail_trailARN,
    trail_s3KeyPrefix,
    trail_hasInsightSelectors,
    trail_snsTopicARN,
    trail_snsTopicName,
    trail_cloudWatchLogsLogGroupArn,
    trail_kmsKeyId,
    trail_homeRegion,
    trail_name,
    trail_includeGlobalServiceEvents,
    trail_hasCustomEventSelectors,
    trail_isOrganizationTrail,
    trail_cloudWatchLogsRoleArn,
    trail_s3BucketName,
    trail_isMultiRegionTrail,

    -- ** TrailInfo
    trailInfo_trailARN,
    trailInfo_homeRegion,
    trailInfo_name,
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

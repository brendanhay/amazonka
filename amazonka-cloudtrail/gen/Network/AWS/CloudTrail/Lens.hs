{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Lens
  ( -- * Operations

    -- ** RemoveTags
    removeTags_tagsList,
    removeTags_resourceId,
    removeTagsResponse_httpStatus,

    -- ** ListPublicKeys
    listPublicKeys_nextToken,
    listPublicKeys_startTime,
    listPublicKeys_endTime,
    listPublicKeysResponse_nextToken,
    listPublicKeysResponse_publicKeyList,
    listPublicKeysResponse_httpStatus,

    -- ** GetEventSelectors
    getEventSelectors_trailName,
    getEventSelectorsResponse_trailARN,
    getEventSelectorsResponse_eventSelectors,
    getEventSelectorsResponse_advancedEventSelectors,
    getEventSelectorsResponse_httpStatus,

    -- ** DescribeTrails
    describeTrails_trailNameList,
    describeTrails_includeShadowTrails,
    describeTrailsResponse_trailList,
    describeTrailsResponse_httpStatus,

    -- ** CreateTrail
    createTrail_isOrganizationTrail,
    createTrail_snsTopicName,
    createTrail_includeGlobalServiceEvents,
    createTrail_kmsKeyId,
    createTrail_s3KeyPrefix,
    createTrail_cloudWatchLogsLogGroupArn,
    createTrail_isMultiRegionTrail,
    createTrail_cloudWatchLogsRoleArn,
    createTrail_tagsList,
    createTrail_enableLogFileValidation,
    createTrail_name,
    createTrail_s3BucketName,
    createTrailResponse_trailARN,
    createTrailResponse_logFileValidationEnabled,
    createTrailResponse_isOrganizationTrail,
    createTrailResponse_snsTopicName,
    createTrailResponse_includeGlobalServiceEvents,
    createTrailResponse_kmsKeyId,
    createTrailResponse_s3KeyPrefix,
    createTrailResponse_name,
    createTrailResponse_cloudWatchLogsLogGroupArn,
    createTrailResponse_isMultiRegionTrail,
    createTrailResponse_s3BucketName,
    createTrailResponse_cloudWatchLogsRoleArn,
    createTrailResponse_snsTopicARN,
    createTrailResponse_httpStatus,

    -- ** PutEventSelectors
    putEventSelectors_eventSelectors,
    putEventSelectors_advancedEventSelectors,
    putEventSelectors_trailName,
    putEventSelectorsResponse_trailARN,
    putEventSelectorsResponse_eventSelectors,
    putEventSelectorsResponse_advancedEventSelectors,
    putEventSelectorsResponse_httpStatus,

    -- ** AddTags
    addTags_tagsList,
    addTags_resourceId,
    addTagsResponse_httpStatus,

    -- ** GetTrail
    getTrail_name,
    getTrailResponse_trail,
    getTrailResponse_httpStatus,

    -- ** PutInsightSelectors
    putInsightSelectors_trailName,
    putInsightSelectors_insightSelectors,
    putInsightSelectorsResponse_trailARN,
    putInsightSelectorsResponse_insightSelectors,
    putInsightSelectorsResponse_httpStatus,

    -- ** GetInsightSelectors
    getInsightSelectors_trailName,
    getInsightSelectorsResponse_trailARN,
    getInsightSelectorsResponse_insightSelectors,
    getInsightSelectorsResponse_httpStatus,

    -- ** StopLogging
    stopLogging_name,
    stopLoggingResponse_httpStatus,

    -- ** DeleteTrail
    deleteTrail_name,
    deleteTrailResponse_httpStatus,

    -- ** StartLogging
    startLogging_name,
    startLoggingResponse_httpStatus,

    -- ** UpdateTrail
    updateTrail_isOrganizationTrail,
    updateTrail_snsTopicName,
    updateTrail_includeGlobalServiceEvents,
    updateTrail_kmsKeyId,
    updateTrail_s3KeyPrefix,
    updateTrail_cloudWatchLogsLogGroupArn,
    updateTrail_isMultiRegionTrail,
    updateTrail_s3BucketName,
    updateTrail_cloudWatchLogsRoleArn,
    updateTrail_enableLogFileValidation,
    updateTrail_name,
    updateTrailResponse_trailARN,
    updateTrailResponse_logFileValidationEnabled,
    updateTrailResponse_isOrganizationTrail,
    updateTrailResponse_snsTopicName,
    updateTrailResponse_includeGlobalServiceEvents,
    updateTrailResponse_kmsKeyId,
    updateTrailResponse_s3KeyPrefix,
    updateTrailResponse_name,
    updateTrailResponse_cloudWatchLogsLogGroupArn,
    updateTrailResponse_isMultiRegionTrail,
    updateTrailResponse_s3BucketName,
    updateTrailResponse_cloudWatchLogsRoleArn,
    updateTrailResponse_snsTopicARN,
    updateTrailResponse_httpStatus,

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

    -- ** GetTrailStatus
    getTrailStatus_name,
    getTrailStatusResponse_latestDigestDeliveryError,
    getTrailStatusResponse_timeLoggingStopped,
    getTrailStatusResponse_latestDeliveryAttemptTime,
    getTrailStatusResponse_latestDeliveryAttemptSucceeded,
    getTrailStatusResponse_latestNotificationError,
    getTrailStatusResponse_latestCloudWatchLogsDeliveryError,
    getTrailStatusResponse_latestNotificationAttemptSucceeded,
    getTrailStatusResponse_latestCloudWatchLogsDeliveryTime,
    getTrailStatusResponse_latestDigestDeliveryTime,
    getTrailStatusResponse_latestDeliveryError,
    getTrailStatusResponse_timeLoggingStarted,
    getTrailStatusResponse_isLogging,
    getTrailStatusResponse_stopLoggingTime,
    getTrailStatusResponse_latestNotificationAttemptTime,
    getTrailStatusResponse_startLoggingTime,
    getTrailStatusResponse_latestNotificationTime,
    getTrailStatusResponse_latestDeliveryTime,
    getTrailStatusResponse_httpStatus,

    -- ** LookupEvents
    lookupEvents_nextToken,
    lookupEvents_maxResults,
    lookupEvents_startTime,
    lookupEvents_endTime,
    lookupEvents_eventCategory,
    lookupEvents_lookupAttributes,
    lookupEventsResponse_nextToken,
    lookupEventsResponse_events,
    lookupEventsResponse_httpStatus,

    -- * Types

    -- ** AdvancedEventSelector
    advancedEventSelector_name,
    advancedEventSelector_fieldSelectors,

    -- ** AdvancedFieldSelector
    advancedFieldSelector_notStartsWith,
    advancedFieldSelector_notEndsWith,
    advancedFieldSelector_notEquals,
    advancedFieldSelector_equals,
    advancedFieldSelector_startsWith,
    advancedFieldSelector_endsWith,
    advancedFieldSelector_field,

    -- ** DataResource
    dataResource_values,
    dataResource_type,

    -- ** Event
    event_cloudTrailEvent,
    event_eventId,
    event_eventSource,
    event_readOnly,
    event_eventName,
    event_resources,
    event_eventTime,
    event_accessKeyId,
    event_username,

    -- ** EventSelector
    eventSelector_readWriteType,
    eventSelector_excludeManagementEventSources,
    eventSelector_includeManagementEvents,
    eventSelector_dataResources,

    -- ** InsightSelector
    insightSelector_insightType,

    -- ** LookupAttribute
    lookupAttribute_attributeKey,
    lookupAttribute_attributeValue,

    -- ** PublicKey
    publicKey_validityStartTime,
    publicKey_value,
    publicKey_validityEndTime,
    publicKey_fingerprint,

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
    trail_trailARN,
    trail_logFileValidationEnabled,
    trail_isOrganizationTrail,
    trail_hasCustomEventSelectors,
    trail_snsTopicName,
    trail_includeGlobalServiceEvents,
    trail_homeRegion,
    trail_kmsKeyId,
    trail_s3KeyPrefix,
    trail_name,
    trail_cloudWatchLogsLogGroupArn,
    trail_isMultiRegionTrail,
    trail_s3BucketName,
    trail_cloudWatchLogsRoleArn,
    trail_snsTopicARN,
    trail_hasInsightSelectors,

    -- ** TrailInfo
    trailInfo_trailARN,
    trailInfo_homeRegion,
    trailInfo_name,
  )
where

import Network.AWS.CloudTrail.AddTags
import Network.AWS.CloudTrail.CreateTrail
import Network.AWS.CloudTrail.DeleteTrail
import Network.AWS.CloudTrail.DescribeTrails
import Network.AWS.CloudTrail.GetEventSelectors
import Network.AWS.CloudTrail.GetInsightSelectors
import Network.AWS.CloudTrail.GetTrail
import Network.AWS.CloudTrail.GetTrailStatus
import Network.AWS.CloudTrail.ListPublicKeys
import Network.AWS.CloudTrail.ListTags
import Network.AWS.CloudTrail.ListTrails
import Network.AWS.CloudTrail.LookupEvents
import Network.AWS.CloudTrail.PutEventSelectors
import Network.AWS.CloudTrail.PutInsightSelectors
import Network.AWS.CloudTrail.RemoveTags
import Network.AWS.CloudTrail.StartLogging
import Network.AWS.CloudTrail.StopLogging
import Network.AWS.CloudTrail.Types.AdvancedEventSelector
import Network.AWS.CloudTrail.Types.AdvancedFieldSelector
import Network.AWS.CloudTrail.Types.DataResource
import Network.AWS.CloudTrail.Types.Event
import Network.AWS.CloudTrail.Types.EventSelector
import Network.AWS.CloudTrail.Types.InsightSelector
import Network.AWS.CloudTrail.Types.LookupAttribute
import Network.AWS.CloudTrail.Types.PublicKey
import Network.AWS.CloudTrail.Types.Resource
import Network.AWS.CloudTrail.Types.ResourceTag
import Network.AWS.CloudTrail.Types.Tag
import Network.AWS.CloudTrail.Types.Trail
import Network.AWS.CloudTrail.Types.TrailInfo
import Network.AWS.CloudTrail.UpdateTrail

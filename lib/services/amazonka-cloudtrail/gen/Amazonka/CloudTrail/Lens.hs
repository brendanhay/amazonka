{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudTrail.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudTrail.Lens
  ( -- * Operations

    -- ** AddTags
    addTags_resourceId,
    addTags_tagsList,
    addTagsResponse_httpStatus,

    -- ** CancelQuery
    cancelQuery_eventDataStore,
    cancelQuery_queryId,
    cancelQueryResponse_httpStatus,
    cancelQueryResponse_queryId,
    cancelQueryResponse_queryStatus,

    -- ** CreateEventDataStore
    createEventDataStore_advancedEventSelectors,
    createEventDataStore_kmsKeyId,
    createEventDataStore_multiRegionEnabled,
    createEventDataStore_organizationEnabled,
    createEventDataStore_retentionPeriod,
    createEventDataStore_tagsList,
    createEventDataStore_terminationProtectionEnabled,
    createEventDataStore_name,
    createEventDataStoreResponse_advancedEventSelectors,
    createEventDataStoreResponse_createdTimestamp,
    createEventDataStoreResponse_eventDataStoreArn,
    createEventDataStoreResponse_kmsKeyId,
    createEventDataStoreResponse_multiRegionEnabled,
    createEventDataStoreResponse_name,
    createEventDataStoreResponse_organizationEnabled,
    createEventDataStoreResponse_retentionPeriod,
    createEventDataStoreResponse_status,
    createEventDataStoreResponse_tagsList,
    createEventDataStoreResponse_terminationProtectionEnabled,
    createEventDataStoreResponse_updatedTimestamp,
    createEventDataStoreResponse_httpStatus,

    -- ** CreateTrail
    createTrail_cloudWatchLogsLogGroupArn,
    createTrail_cloudWatchLogsRoleArn,
    createTrail_enableLogFileValidation,
    createTrail_includeGlobalServiceEvents,
    createTrail_isMultiRegionTrail,
    createTrail_isOrganizationTrail,
    createTrail_kmsKeyId,
    createTrail_s3KeyPrefix,
    createTrail_snsTopicName,
    createTrail_tagsList,
    createTrail_name,
    createTrail_s3BucketName,
    createTrailResponse_cloudWatchLogsLogGroupArn,
    createTrailResponse_cloudWatchLogsRoleArn,
    createTrailResponse_includeGlobalServiceEvents,
    createTrailResponse_isMultiRegionTrail,
    createTrailResponse_isOrganizationTrail,
    createTrailResponse_kmsKeyId,
    createTrailResponse_logFileValidationEnabled,
    createTrailResponse_name,
    createTrailResponse_s3BucketName,
    createTrailResponse_s3KeyPrefix,
    createTrailResponse_snsTopicARN,
    createTrailResponse_snsTopicName,
    createTrailResponse_trailARN,
    createTrailResponse_httpStatus,

    -- ** DeleteEventDataStore
    deleteEventDataStore_eventDataStore,
    deleteEventDataStoreResponse_httpStatus,

    -- ** DeleteTrail
    deleteTrail_name,
    deleteTrailResponse_httpStatus,

    -- ** DeregisterOrganizationDelegatedAdmin
    deregisterOrganizationDelegatedAdmin_delegatedAdminAccountId,
    deregisterOrganizationDelegatedAdminResponse_httpStatus,

    -- ** DescribeQuery
    describeQuery_eventDataStore,
    describeQuery_queryId,
    describeQueryResponse_deliveryS3Uri,
    describeQueryResponse_deliveryStatus,
    describeQueryResponse_errorMessage,
    describeQueryResponse_queryId,
    describeQueryResponse_queryStatistics,
    describeQueryResponse_queryStatus,
    describeQueryResponse_queryString,
    describeQueryResponse_httpStatus,

    -- ** DescribeTrails
    describeTrails_includeShadowTrails,
    describeTrails_trailNameList,
    describeTrailsResponse_trailList,
    describeTrailsResponse_httpStatus,

    -- ** GetChannel
    getChannel_channel,
    getChannelResponse_channelArn,
    getChannelResponse_destinations,
    getChannelResponse_name,
    getChannelResponse_source,
    getChannelResponse_sourceConfig,
    getChannelResponse_httpStatus,

    -- ** GetEventDataStore
    getEventDataStore_eventDataStore,
    getEventDataStoreResponse_advancedEventSelectors,
    getEventDataStoreResponse_createdTimestamp,
    getEventDataStoreResponse_eventDataStoreArn,
    getEventDataStoreResponse_kmsKeyId,
    getEventDataStoreResponse_multiRegionEnabled,
    getEventDataStoreResponse_name,
    getEventDataStoreResponse_organizationEnabled,
    getEventDataStoreResponse_retentionPeriod,
    getEventDataStoreResponse_status,
    getEventDataStoreResponse_terminationProtectionEnabled,
    getEventDataStoreResponse_updatedTimestamp,
    getEventDataStoreResponse_httpStatus,

    -- ** GetEventSelectors
    getEventSelectors_trailName,
    getEventSelectorsResponse_advancedEventSelectors,
    getEventSelectorsResponse_eventSelectors,
    getEventSelectorsResponse_trailARN,
    getEventSelectorsResponse_httpStatus,

    -- ** GetImport
    getImport_importId,
    getImportResponse_createdTimestamp,
    getImportResponse_destinations,
    getImportResponse_endEventTime,
    getImportResponse_importId,
    getImportResponse_importSource,
    getImportResponse_importStatistics,
    getImportResponse_importStatus,
    getImportResponse_startEventTime,
    getImportResponse_updatedTimestamp,
    getImportResponse_httpStatus,

    -- ** GetInsightSelectors
    getInsightSelectors_trailName,
    getInsightSelectorsResponse_insightSelectors,
    getInsightSelectorsResponse_trailARN,
    getInsightSelectorsResponse_httpStatus,

    -- ** GetQueryResults
    getQueryResults_eventDataStore,
    getQueryResults_maxQueryResults,
    getQueryResults_nextToken,
    getQueryResults_queryId,
    getQueryResultsResponse_errorMessage,
    getQueryResultsResponse_nextToken,
    getQueryResultsResponse_queryResultRows,
    getQueryResultsResponse_queryStatistics,
    getQueryResultsResponse_queryStatus,
    getQueryResultsResponse_httpStatus,

    -- ** GetTrail
    getTrail_name,
    getTrailResponse_trail,
    getTrailResponse_httpStatus,

    -- ** GetTrailStatus
    getTrailStatus_name,
    getTrailStatusResponse_isLogging,
    getTrailStatusResponse_latestCloudWatchLogsDeliveryError,
    getTrailStatusResponse_latestCloudWatchLogsDeliveryTime,
    getTrailStatusResponse_latestDeliveryAttemptSucceeded,
    getTrailStatusResponse_latestDeliveryAttemptTime,
    getTrailStatusResponse_latestDeliveryError,
    getTrailStatusResponse_latestDeliveryTime,
    getTrailStatusResponse_latestDigestDeliveryError,
    getTrailStatusResponse_latestDigestDeliveryTime,
    getTrailStatusResponse_latestNotificationAttemptSucceeded,
    getTrailStatusResponse_latestNotificationAttemptTime,
    getTrailStatusResponse_latestNotificationError,
    getTrailStatusResponse_latestNotificationTime,
    getTrailStatusResponse_startLoggingTime,
    getTrailStatusResponse_stopLoggingTime,
    getTrailStatusResponse_timeLoggingStarted,
    getTrailStatusResponse_timeLoggingStopped,
    getTrailStatusResponse_httpStatus,

    -- ** ListChannels
    listChannels_maxResults,
    listChannels_nextToken,
    listChannelsResponse_channels,
    listChannelsResponse_nextToken,
    listChannelsResponse_httpStatus,

    -- ** ListEventDataStores
    listEventDataStores_maxResults,
    listEventDataStores_nextToken,
    listEventDataStoresResponse_eventDataStores,
    listEventDataStoresResponse_nextToken,
    listEventDataStoresResponse_httpStatus,

    -- ** ListImportFailures
    listImportFailures_maxResults,
    listImportFailures_nextToken,
    listImportFailures_importId,
    listImportFailuresResponse_failures,
    listImportFailuresResponse_nextToken,
    listImportFailuresResponse_httpStatus,

    -- ** ListImports
    listImports_destination,
    listImports_importStatus,
    listImports_maxResults,
    listImports_nextToken,
    listImportsResponse_imports,
    listImportsResponse_nextToken,
    listImportsResponse_httpStatus,

    -- ** ListPublicKeys
    listPublicKeys_endTime,
    listPublicKeys_nextToken,
    listPublicKeys_startTime,
    listPublicKeysResponse_nextToken,
    listPublicKeysResponse_publicKeyList,
    listPublicKeysResponse_httpStatus,

    -- ** ListQueries
    listQueries_endTime,
    listQueries_maxResults,
    listQueries_nextToken,
    listQueries_queryStatus,
    listQueries_startTime,
    listQueries_eventDataStore,
    listQueriesResponse_nextToken,
    listQueriesResponse_queries,
    listQueriesResponse_httpStatus,

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
    lookupEvents_endTime,
    lookupEvents_eventCategory,
    lookupEvents_lookupAttributes,
    lookupEvents_maxResults,
    lookupEvents_nextToken,
    lookupEvents_startTime,
    lookupEventsResponse_events,
    lookupEventsResponse_nextToken,
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

    -- ** RegisterOrganizationDelegatedAdmin
    registerOrganizationDelegatedAdmin_memberAccountId,
    registerOrganizationDelegatedAdminResponse_httpStatus,

    -- ** RemoveTags
    removeTags_resourceId,
    removeTags_tagsList,
    removeTagsResponse_httpStatus,

    -- ** RestoreEventDataStore
    restoreEventDataStore_eventDataStore,
    restoreEventDataStoreResponse_advancedEventSelectors,
    restoreEventDataStoreResponse_createdTimestamp,
    restoreEventDataStoreResponse_eventDataStoreArn,
    restoreEventDataStoreResponse_kmsKeyId,
    restoreEventDataStoreResponse_multiRegionEnabled,
    restoreEventDataStoreResponse_name,
    restoreEventDataStoreResponse_organizationEnabled,
    restoreEventDataStoreResponse_retentionPeriod,
    restoreEventDataStoreResponse_status,
    restoreEventDataStoreResponse_terminationProtectionEnabled,
    restoreEventDataStoreResponse_updatedTimestamp,
    restoreEventDataStoreResponse_httpStatus,

    -- ** StartImport
    startImport_destinations,
    startImport_endEventTime,
    startImport_importId,
    startImport_importSource,
    startImport_startEventTime,
    startImportResponse_createdTimestamp,
    startImportResponse_destinations,
    startImportResponse_endEventTime,
    startImportResponse_importId,
    startImportResponse_importSource,
    startImportResponse_importStatus,
    startImportResponse_startEventTime,
    startImportResponse_updatedTimestamp,
    startImportResponse_httpStatus,

    -- ** StartLogging
    startLogging_name,
    startLoggingResponse_httpStatus,

    -- ** StartQuery
    startQuery_deliveryS3Uri,
    startQuery_queryStatement,
    startQueryResponse_queryId,
    startQueryResponse_httpStatus,

    -- ** StopImport
    stopImport_importId,
    stopImportResponse_createdTimestamp,
    stopImportResponse_destinations,
    stopImportResponse_endEventTime,
    stopImportResponse_importId,
    stopImportResponse_importSource,
    stopImportResponse_importStatistics,
    stopImportResponse_importStatus,
    stopImportResponse_startEventTime,
    stopImportResponse_updatedTimestamp,
    stopImportResponse_httpStatus,

    -- ** StopLogging
    stopLogging_name,
    stopLoggingResponse_httpStatus,

    -- ** UpdateEventDataStore
    updateEventDataStore_advancedEventSelectors,
    updateEventDataStore_kmsKeyId,
    updateEventDataStore_multiRegionEnabled,
    updateEventDataStore_name,
    updateEventDataStore_organizationEnabled,
    updateEventDataStore_retentionPeriod,
    updateEventDataStore_terminationProtectionEnabled,
    updateEventDataStore_eventDataStore,
    updateEventDataStoreResponse_advancedEventSelectors,
    updateEventDataStoreResponse_createdTimestamp,
    updateEventDataStoreResponse_eventDataStoreArn,
    updateEventDataStoreResponse_kmsKeyId,
    updateEventDataStoreResponse_multiRegionEnabled,
    updateEventDataStoreResponse_name,
    updateEventDataStoreResponse_organizationEnabled,
    updateEventDataStoreResponse_retentionPeriod,
    updateEventDataStoreResponse_status,
    updateEventDataStoreResponse_terminationProtectionEnabled,
    updateEventDataStoreResponse_updatedTimestamp,
    updateEventDataStoreResponse_httpStatus,

    -- ** UpdateTrail
    updateTrail_cloudWatchLogsLogGroupArn,
    updateTrail_cloudWatchLogsRoleArn,
    updateTrail_enableLogFileValidation,
    updateTrail_includeGlobalServiceEvents,
    updateTrail_isMultiRegionTrail,
    updateTrail_isOrganizationTrail,
    updateTrail_kmsKeyId,
    updateTrail_s3BucketName,
    updateTrail_s3KeyPrefix,
    updateTrail_snsTopicName,
    updateTrail_name,
    updateTrailResponse_cloudWatchLogsLogGroupArn,
    updateTrailResponse_cloudWatchLogsRoleArn,
    updateTrailResponse_includeGlobalServiceEvents,
    updateTrailResponse_isMultiRegionTrail,
    updateTrailResponse_isOrganizationTrail,
    updateTrailResponse_kmsKeyId,
    updateTrailResponse_logFileValidationEnabled,
    updateTrailResponse_name,
    updateTrailResponse_s3BucketName,
    updateTrailResponse_s3KeyPrefix,
    updateTrailResponse_snsTopicARN,
    updateTrailResponse_snsTopicName,
    updateTrailResponse_trailARN,
    updateTrailResponse_httpStatus,

    -- * Types

    -- ** AdvancedEventSelector
    advancedEventSelector_name,
    advancedEventSelector_fieldSelectors,

    -- ** AdvancedFieldSelector
    advancedFieldSelector_endsWith,
    advancedFieldSelector_equals,
    advancedFieldSelector_notEndsWith,
    advancedFieldSelector_notEquals,
    advancedFieldSelector_notStartsWith,
    advancedFieldSelector_startsWith,
    advancedFieldSelector_field,

    -- ** Channel
    channel_channelArn,
    channel_name,

    -- ** DataResource
    dataResource_type,
    dataResource_values,

    -- ** Destination
    destination_type,
    destination_location,

    -- ** Event
    event_accessKeyId,
    event_cloudTrailEvent,
    event_eventId,
    event_eventName,
    event_eventSource,
    event_eventTime,
    event_readOnly,
    event_resources,
    event_username,

    -- ** EventDataStore
    eventDataStore_advancedEventSelectors,
    eventDataStore_createdTimestamp,
    eventDataStore_eventDataStoreArn,
    eventDataStore_multiRegionEnabled,
    eventDataStore_name,
    eventDataStore_organizationEnabled,
    eventDataStore_retentionPeriod,
    eventDataStore_status,
    eventDataStore_terminationProtectionEnabled,
    eventDataStore_updatedTimestamp,

    -- ** EventSelector
    eventSelector_dataResources,
    eventSelector_excludeManagementEventSources,
    eventSelector_includeManagementEvents,
    eventSelector_readWriteType,

    -- ** ImportFailureListItem
    importFailureListItem_errorMessage,
    importFailureListItem_errorType,
    importFailureListItem_lastUpdatedTime,
    importFailureListItem_location,
    importFailureListItem_status,

    -- ** ImportSource
    importSource_s3,

    -- ** ImportStatistics
    importStatistics_eventsCompleted,
    importStatistics_failedEntries,
    importStatistics_filesCompleted,
    importStatistics_prefixesCompleted,
    importStatistics_prefixesFound,

    -- ** ImportsListItem
    importsListItem_createdTimestamp,
    importsListItem_destinations,
    importsListItem_importId,
    importsListItem_importStatus,
    importsListItem_updatedTimestamp,

    -- ** InsightSelector
    insightSelector_insightType,

    -- ** LookupAttribute
    lookupAttribute_attributeKey,
    lookupAttribute_attributeValue,

    -- ** PublicKey
    publicKey_fingerprint,
    publicKey_validityEndTime,
    publicKey_validityStartTime,
    publicKey_value,

    -- ** Query
    query_creationTime,
    query_queryId,
    query_queryStatus,

    -- ** QueryStatistics
    queryStatistics_bytesScanned,
    queryStatistics_resultsCount,
    queryStatistics_totalResultsCount,

    -- ** QueryStatisticsForDescribeQuery
    queryStatisticsForDescribeQuery_bytesScanned,
    queryStatisticsForDescribeQuery_creationTime,
    queryStatisticsForDescribeQuery_eventsMatched,
    queryStatisticsForDescribeQuery_eventsScanned,
    queryStatisticsForDescribeQuery_executionTimeInMillis,

    -- ** Resource
    resource_resourceName,
    resource_resourceType,

    -- ** ResourceTag
    resourceTag_resourceId,
    resourceTag_tagsList,

    -- ** S3ImportSource
    s3ImportSource_s3LocationUri,
    s3ImportSource_s3BucketRegion,
    s3ImportSource_s3BucketAccessRoleArn,

    -- ** SourceConfig
    sourceConfig_advancedEventSelectors,
    sourceConfig_applyToAllRegions,

    -- ** Tag
    tag_value,
    tag_key,

    -- ** Trail
    trail_cloudWatchLogsLogGroupArn,
    trail_cloudWatchLogsRoleArn,
    trail_hasCustomEventSelectors,
    trail_hasInsightSelectors,
    trail_homeRegion,
    trail_includeGlobalServiceEvents,
    trail_isMultiRegionTrail,
    trail_isOrganizationTrail,
    trail_kmsKeyId,
    trail_logFileValidationEnabled,
    trail_name,
    trail_s3BucketName,
    trail_s3KeyPrefix,
    trail_snsTopicARN,
    trail_snsTopicName,
    trail_trailARN,

    -- ** TrailInfo
    trailInfo_homeRegion,
    trailInfo_name,
    trailInfo_trailARN,
  )
where

import Amazonka.CloudTrail.AddTags
import Amazonka.CloudTrail.CancelQuery
import Amazonka.CloudTrail.CreateEventDataStore
import Amazonka.CloudTrail.CreateTrail
import Amazonka.CloudTrail.DeleteEventDataStore
import Amazonka.CloudTrail.DeleteTrail
import Amazonka.CloudTrail.DeregisterOrganizationDelegatedAdmin
import Amazonka.CloudTrail.DescribeQuery
import Amazonka.CloudTrail.DescribeTrails
import Amazonka.CloudTrail.GetChannel
import Amazonka.CloudTrail.GetEventDataStore
import Amazonka.CloudTrail.GetEventSelectors
import Amazonka.CloudTrail.GetImport
import Amazonka.CloudTrail.GetInsightSelectors
import Amazonka.CloudTrail.GetQueryResults
import Amazonka.CloudTrail.GetTrail
import Amazonka.CloudTrail.GetTrailStatus
import Amazonka.CloudTrail.ListChannels
import Amazonka.CloudTrail.ListEventDataStores
import Amazonka.CloudTrail.ListImportFailures
import Amazonka.CloudTrail.ListImports
import Amazonka.CloudTrail.ListPublicKeys
import Amazonka.CloudTrail.ListQueries
import Amazonka.CloudTrail.ListTags
import Amazonka.CloudTrail.ListTrails
import Amazonka.CloudTrail.LookupEvents
import Amazonka.CloudTrail.PutEventSelectors
import Amazonka.CloudTrail.PutInsightSelectors
import Amazonka.CloudTrail.RegisterOrganizationDelegatedAdmin
import Amazonka.CloudTrail.RemoveTags
import Amazonka.CloudTrail.RestoreEventDataStore
import Amazonka.CloudTrail.StartImport
import Amazonka.CloudTrail.StartLogging
import Amazonka.CloudTrail.StartQuery
import Amazonka.CloudTrail.StopImport
import Amazonka.CloudTrail.StopLogging
import Amazonka.CloudTrail.Types.AdvancedEventSelector
import Amazonka.CloudTrail.Types.AdvancedFieldSelector
import Amazonka.CloudTrail.Types.Channel
import Amazonka.CloudTrail.Types.DataResource
import Amazonka.CloudTrail.Types.Destination
import Amazonka.CloudTrail.Types.Event
import Amazonka.CloudTrail.Types.EventDataStore
import Amazonka.CloudTrail.Types.EventSelector
import Amazonka.CloudTrail.Types.ImportFailureListItem
import Amazonka.CloudTrail.Types.ImportSource
import Amazonka.CloudTrail.Types.ImportStatistics
import Amazonka.CloudTrail.Types.ImportsListItem
import Amazonka.CloudTrail.Types.InsightSelector
import Amazonka.CloudTrail.Types.LookupAttribute
import Amazonka.CloudTrail.Types.PublicKey
import Amazonka.CloudTrail.Types.Query
import Amazonka.CloudTrail.Types.QueryStatistics
import Amazonka.CloudTrail.Types.QueryStatisticsForDescribeQuery
import Amazonka.CloudTrail.Types.Resource
import Amazonka.CloudTrail.Types.ResourceTag
import Amazonka.CloudTrail.Types.S3ImportSource
import Amazonka.CloudTrail.Types.SourceConfig
import Amazonka.CloudTrail.Types.Tag
import Amazonka.CloudTrail.Types.Trail
import Amazonka.CloudTrail.Types.TrailInfo
import Amazonka.CloudTrail.UpdateEventDataStore
import Amazonka.CloudTrail.UpdateTrail

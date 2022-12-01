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
    createEventDataStore_multiRegionEnabled,
    createEventDataStore_tagsList,
    createEventDataStore_retentionPeriod,
    createEventDataStore_kmsKeyId,
    createEventDataStore_organizationEnabled,
    createEventDataStore_terminationProtectionEnabled,
    createEventDataStore_name,
    createEventDataStoreResponse_name,
    createEventDataStoreResponse_eventDataStoreArn,
    createEventDataStoreResponse_advancedEventSelectors,
    createEventDataStoreResponse_createdTimestamp,
    createEventDataStoreResponse_multiRegionEnabled,
    createEventDataStoreResponse_tagsList,
    createEventDataStoreResponse_updatedTimestamp,
    createEventDataStoreResponse_status,
    createEventDataStoreResponse_retentionPeriod,
    createEventDataStoreResponse_kmsKeyId,
    createEventDataStoreResponse_organizationEnabled,
    createEventDataStoreResponse_terminationProtectionEnabled,
    createEventDataStoreResponse_httpStatus,

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
    describeQueryResponse_queryStatistics,
    describeQueryResponse_queryStatus,
    describeQueryResponse_errorMessage,
    describeQueryResponse_queryId,
    describeQueryResponse_deliveryStatus,
    describeQueryResponse_deliveryS3Uri,
    describeQueryResponse_queryString,
    describeQueryResponse_httpStatus,

    -- ** DescribeTrails
    describeTrails_trailNameList,
    describeTrails_includeShadowTrails,
    describeTrailsResponse_trailList,
    describeTrailsResponse_httpStatus,

    -- ** GetChannel
    getChannel_channel,
    getChannelResponse_name,
    getChannelResponse_sourceConfig,
    getChannelResponse_channelArn,
    getChannelResponse_source,
    getChannelResponse_destinations,
    getChannelResponse_httpStatus,

    -- ** GetEventDataStore
    getEventDataStore_eventDataStore,
    getEventDataStoreResponse_name,
    getEventDataStoreResponse_eventDataStoreArn,
    getEventDataStoreResponse_advancedEventSelectors,
    getEventDataStoreResponse_createdTimestamp,
    getEventDataStoreResponse_multiRegionEnabled,
    getEventDataStoreResponse_updatedTimestamp,
    getEventDataStoreResponse_status,
    getEventDataStoreResponse_retentionPeriod,
    getEventDataStoreResponse_kmsKeyId,
    getEventDataStoreResponse_organizationEnabled,
    getEventDataStoreResponse_terminationProtectionEnabled,
    getEventDataStoreResponse_httpStatus,

    -- ** GetEventSelectors
    getEventSelectors_trailName,
    getEventSelectorsResponse_advancedEventSelectors,
    getEventSelectorsResponse_eventSelectors,
    getEventSelectorsResponse_trailARN,
    getEventSelectorsResponse_httpStatus,

    -- ** GetImport
    getImport_importId,
    getImportResponse_importSource,
    getImportResponse_endEventTime,
    getImportResponse_createdTimestamp,
    getImportResponse_updatedTimestamp,
    getImportResponse_startEventTime,
    getImportResponse_importStatistics,
    getImportResponse_importId,
    getImportResponse_importStatus,
    getImportResponse_destinations,
    getImportResponse_httpStatus,

    -- ** GetInsightSelectors
    getInsightSelectors_trailName,
    getInsightSelectorsResponse_insightSelectors,
    getInsightSelectorsResponse_trailARN,
    getInsightSelectorsResponse_httpStatus,

    -- ** GetQueryResults
    getQueryResults_nextToken,
    getQueryResults_eventDataStore,
    getQueryResults_maxQueryResults,
    getQueryResults_queryId,
    getQueryResultsResponse_nextToken,
    getQueryResultsResponse_queryStatistics,
    getQueryResultsResponse_queryResultRows,
    getQueryResultsResponse_queryStatus,
    getQueryResultsResponse_errorMessage,
    getQueryResultsResponse_httpStatus,

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

    -- ** ListChannels
    listChannels_nextToken,
    listChannels_maxResults,
    listChannelsResponse_nextToken,
    listChannelsResponse_channels,
    listChannelsResponse_httpStatus,

    -- ** ListEventDataStores
    listEventDataStores_nextToken,
    listEventDataStores_maxResults,
    listEventDataStoresResponse_nextToken,
    listEventDataStoresResponse_eventDataStores,
    listEventDataStoresResponse_httpStatus,

    -- ** ListImportFailures
    listImportFailures_nextToken,
    listImportFailures_maxResults,
    listImportFailures_importId,
    listImportFailuresResponse_nextToken,
    listImportFailuresResponse_failures,
    listImportFailuresResponse_httpStatus,

    -- ** ListImports
    listImports_destination,
    listImports_nextToken,
    listImports_maxResults,
    listImports_importStatus,
    listImportsResponse_imports,
    listImportsResponse_nextToken,
    listImportsResponse_httpStatus,

    -- ** ListPublicKeys
    listPublicKeys_nextToken,
    listPublicKeys_endTime,
    listPublicKeys_startTime,
    listPublicKeysResponse_nextToken,
    listPublicKeysResponse_publicKeyList,
    listPublicKeysResponse_httpStatus,

    -- ** ListQueries
    listQueries_nextToken,
    listQueries_queryStatus,
    listQueries_endTime,
    listQueries_maxResults,
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

    -- ** RegisterOrganizationDelegatedAdmin
    registerOrganizationDelegatedAdmin_memberAccountId,
    registerOrganizationDelegatedAdminResponse_httpStatus,

    -- ** RemoveTags
    removeTags_resourceId,
    removeTags_tagsList,
    removeTagsResponse_httpStatus,

    -- ** RestoreEventDataStore
    restoreEventDataStore_eventDataStore,
    restoreEventDataStoreResponse_name,
    restoreEventDataStoreResponse_eventDataStoreArn,
    restoreEventDataStoreResponse_advancedEventSelectors,
    restoreEventDataStoreResponse_createdTimestamp,
    restoreEventDataStoreResponse_multiRegionEnabled,
    restoreEventDataStoreResponse_updatedTimestamp,
    restoreEventDataStoreResponse_status,
    restoreEventDataStoreResponse_retentionPeriod,
    restoreEventDataStoreResponse_kmsKeyId,
    restoreEventDataStoreResponse_organizationEnabled,
    restoreEventDataStoreResponse_terminationProtectionEnabled,
    restoreEventDataStoreResponse_httpStatus,

    -- ** StartImport
    startImport_importSource,
    startImport_endEventTime,
    startImport_startEventTime,
    startImport_importId,
    startImport_destinations,
    startImportResponse_importSource,
    startImportResponse_endEventTime,
    startImportResponse_createdTimestamp,
    startImportResponse_updatedTimestamp,
    startImportResponse_startEventTime,
    startImportResponse_importId,
    startImportResponse_importStatus,
    startImportResponse_destinations,
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
    stopImportResponse_importSource,
    stopImportResponse_endEventTime,
    stopImportResponse_createdTimestamp,
    stopImportResponse_updatedTimestamp,
    stopImportResponse_startEventTime,
    stopImportResponse_importStatistics,
    stopImportResponse_importId,
    stopImportResponse_importStatus,
    stopImportResponse_destinations,
    stopImportResponse_httpStatus,

    -- ** StopLogging
    stopLogging_name,
    stopLoggingResponse_httpStatus,

    -- ** UpdateEventDataStore
    updateEventDataStore_name,
    updateEventDataStore_advancedEventSelectors,
    updateEventDataStore_multiRegionEnabled,
    updateEventDataStore_retentionPeriod,
    updateEventDataStore_kmsKeyId,
    updateEventDataStore_organizationEnabled,
    updateEventDataStore_terminationProtectionEnabled,
    updateEventDataStore_eventDataStore,
    updateEventDataStoreResponse_name,
    updateEventDataStoreResponse_eventDataStoreArn,
    updateEventDataStoreResponse_advancedEventSelectors,
    updateEventDataStoreResponse_createdTimestamp,
    updateEventDataStoreResponse_multiRegionEnabled,
    updateEventDataStoreResponse_updatedTimestamp,
    updateEventDataStoreResponse_status,
    updateEventDataStoreResponse_retentionPeriod,
    updateEventDataStoreResponse_kmsKeyId,
    updateEventDataStoreResponse_organizationEnabled,
    updateEventDataStoreResponse_terminationProtectionEnabled,
    updateEventDataStoreResponse_httpStatus,

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

    -- ** Channel
    channel_name,
    channel_channelArn,

    -- ** DataResource
    dataResource_type,
    dataResource_values,

    -- ** Destination
    destination_type,
    destination_location,

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

    -- ** EventDataStore
    eventDataStore_name,
    eventDataStore_eventDataStoreArn,
    eventDataStore_advancedEventSelectors,
    eventDataStore_createdTimestamp,
    eventDataStore_multiRegionEnabled,
    eventDataStore_updatedTimestamp,
    eventDataStore_status,
    eventDataStore_retentionPeriod,
    eventDataStore_organizationEnabled,
    eventDataStore_terminationProtectionEnabled,

    -- ** EventSelector
    eventSelector_excludeManagementEventSources,
    eventSelector_includeManagementEvents,
    eventSelector_dataResources,
    eventSelector_readWriteType,

    -- ** ImportFailureListItem
    importFailureListItem_errorMessage,
    importFailureListItem_status,
    importFailureListItem_lastUpdatedTime,
    importFailureListItem_location,
    importFailureListItem_errorType,

    -- ** ImportSource
    importSource_s3,

    -- ** ImportStatistics
    importStatistics_failedEntries,
    importStatistics_eventsCompleted,
    importStatistics_prefixesFound,
    importStatistics_prefixesCompleted,
    importStatistics_filesCompleted,

    -- ** ImportsListItem
    importsListItem_createdTimestamp,
    importsListItem_updatedTimestamp,
    importsListItem_importId,
    importsListItem_importStatus,
    importsListItem_destinations,

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

    -- ** Query
    query_queryStatus,
    query_queryId,
    query_creationTime,

    -- ** QueryStatistics
    queryStatistics_totalResultsCount,
    queryStatistics_bytesScanned,
    queryStatistics_resultsCount,

    -- ** QueryStatisticsForDescribeQuery
    queryStatisticsForDescribeQuery_eventsScanned,
    queryStatisticsForDescribeQuery_bytesScanned,
    queryStatisticsForDescribeQuery_executionTimeInMillis,
    queryStatisticsForDescribeQuery_eventsMatched,
    queryStatisticsForDescribeQuery_creationTime,

    -- ** Resource
    resource_resourceType,
    resource_resourceName,

    -- ** ResourceTag
    resourceTag_resourceId,
    resourceTag_tagsList,

    -- ** S3ImportSource
    s3ImportSource_s3LocationUri,
    s3ImportSource_s3BucketRegion,
    s3ImportSource_s3BucketAccessRoleArn,

    -- ** SourceConfig
    sourceConfig_applyToAllRegions,
    sourceConfig_advancedEventSelectors,

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

{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Rum.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rum.Lens
  ( -- * Operations

    -- ** BatchCreateRumMetricDefinitions
    batchCreateRumMetricDefinitions_destinationArn,
    batchCreateRumMetricDefinitions_appMonitorName,
    batchCreateRumMetricDefinitions_destination,
    batchCreateRumMetricDefinitions_metricDefinitions,
    batchCreateRumMetricDefinitionsResponse_metricDefinitions,
    batchCreateRumMetricDefinitionsResponse_httpStatus,
    batchCreateRumMetricDefinitionsResponse_errors,

    -- ** BatchDeleteRumMetricDefinitions
    batchDeleteRumMetricDefinitions_destinationArn,
    batchDeleteRumMetricDefinitions_appMonitorName,
    batchDeleteRumMetricDefinitions_destination,
    batchDeleteRumMetricDefinitions_metricDefinitionIds,
    batchDeleteRumMetricDefinitionsResponse_metricDefinitionIds,
    batchDeleteRumMetricDefinitionsResponse_httpStatus,
    batchDeleteRumMetricDefinitionsResponse_errors,

    -- ** BatchGetRumMetricDefinitions
    batchGetRumMetricDefinitions_destinationArn,
    batchGetRumMetricDefinitions_maxResults,
    batchGetRumMetricDefinitions_nextToken,
    batchGetRumMetricDefinitions_appMonitorName,
    batchGetRumMetricDefinitions_destination,
    batchGetRumMetricDefinitionsResponse_metricDefinitions,
    batchGetRumMetricDefinitionsResponse_nextToken,
    batchGetRumMetricDefinitionsResponse_httpStatus,

    -- ** CreateAppMonitor
    createAppMonitor_appMonitorConfiguration,
    createAppMonitor_customEvents,
    createAppMonitor_cwLogEnabled,
    createAppMonitor_tags,
    createAppMonitor_domain,
    createAppMonitor_name,
    createAppMonitorResponse_id,
    createAppMonitorResponse_httpStatus,

    -- ** DeleteAppMonitor
    deleteAppMonitor_name,
    deleteAppMonitorResponse_httpStatus,

    -- ** DeleteRumMetricsDestination
    deleteRumMetricsDestination_destinationArn,
    deleteRumMetricsDestination_appMonitorName,
    deleteRumMetricsDestination_destination,
    deleteRumMetricsDestinationResponse_httpStatus,

    -- ** GetAppMonitor
    getAppMonitor_name,
    getAppMonitorResponse_appMonitor,
    getAppMonitorResponse_httpStatus,

    -- ** GetAppMonitorData
    getAppMonitorData_filters,
    getAppMonitorData_maxResults,
    getAppMonitorData_nextToken,
    getAppMonitorData_name,
    getAppMonitorData_timeRange,
    getAppMonitorDataResponse_events,
    getAppMonitorDataResponse_nextToken,
    getAppMonitorDataResponse_httpStatus,

    -- ** ListAppMonitors
    listAppMonitors_maxResults,
    listAppMonitors_nextToken,
    listAppMonitorsResponse_appMonitorSummaries,
    listAppMonitorsResponse_nextToken,
    listAppMonitorsResponse_httpStatus,

    -- ** ListRumMetricsDestinations
    listRumMetricsDestinations_maxResults,
    listRumMetricsDestinations_nextToken,
    listRumMetricsDestinations_appMonitorName,
    listRumMetricsDestinationsResponse_destinations,
    listRumMetricsDestinationsResponse_nextToken,
    listRumMetricsDestinationsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_httpStatus,
    listTagsForResourceResponse_resourceArn,
    listTagsForResourceResponse_tags,

    -- ** PutRumEvents
    putRumEvents_appMonitorDetails,
    putRumEvents_batchId,
    putRumEvents_id,
    putRumEvents_rumEvents,
    putRumEvents_userDetails,
    putRumEventsResponse_httpStatus,

    -- ** PutRumMetricsDestination
    putRumMetricsDestination_destinationArn,
    putRumMetricsDestination_iamRoleArn,
    putRumMetricsDestination_appMonitorName,
    putRumMetricsDestination_destination,
    putRumMetricsDestinationResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateAppMonitor
    updateAppMonitor_appMonitorConfiguration,
    updateAppMonitor_customEvents,
    updateAppMonitor_cwLogEnabled,
    updateAppMonitor_domain,
    updateAppMonitor_name,
    updateAppMonitorResponse_httpStatus,

    -- ** UpdateRumMetricDefinition
    updateRumMetricDefinition_destinationArn,
    updateRumMetricDefinition_appMonitorName,
    updateRumMetricDefinition_destination,
    updateRumMetricDefinition_metricDefinition,
    updateRumMetricDefinition_metricDefinitionId,
    updateRumMetricDefinitionResponse_httpStatus,

    -- * Types

    -- ** AppMonitor
    appMonitor_appMonitorConfiguration,
    appMonitor_created,
    appMonitor_customEvents,
    appMonitor_dataStorage,
    appMonitor_domain,
    appMonitor_id,
    appMonitor_lastModified,
    appMonitor_name,
    appMonitor_state,
    appMonitor_tags,

    -- ** AppMonitorConfiguration
    appMonitorConfiguration_allowCookies,
    appMonitorConfiguration_enableXRay,
    appMonitorConfiguration_excludedPages,
    appMonitorConfiguration_favoritePages,
    appMonitorConfiguration_guestRoleArn,
    appMonitorConfiguration_identityPoolId,
    appMonitorConfiguration_includedPages,
    appMonitorConfiguration_sessionSampleRate,
    appMonitorConfiguration_telemetries,

    -- ** AppMonitorDetails
    appMonitorDetails_id,
    appMonitorDetails_name,
    appMonitorDetails_version,

    -- ** AppMonitorSummary
    appMonitorSummary_created,
    appMonitorSummary_id,
    appMonitorSummary_lastModified,
    appMonitorSummary_name,
    appMonitorSummary_state,

    -- ** BatchCreateRumMetricDefinitionsError
    batchCreateRumMetricDefinitionsError_errorCode,
    batchCreateRumMetricDefinitionsError_errorMessage,
    batchCreateRumMetricDefinitionsError_metricDefinition,

    -- ** BatchDeleteRumMetricDefinitionsError
    batchDeleteRumMetricDefinitionsError_errorCode,
    batchDeleteRumMetricDefinitionsError_errorMessage,
    batchDeleteRumMetricDefinitionsError_metricDefinitionId,

    -- ** CustomEvents
    customEvents_status,

    -- ** CwLog
    cwLog_cwLogEnabled,
    cwLog_cwLogGroup,

    -- ** DataStorage
    dataStorage_cwLog,

    -- ** MetricDefinition
    metricDefinition_dimensionKeys,
    metricDefinition_eventPattern,
    metricDefinition_namespace,
    metricDefinition_unitLabel,
    metricDefinition_valueKey,
    metricDefinition_metricDefinitionId,
    metricDefinition_name,

    -- ** MetricDefinitionRequest
    metricDefinitionRequest_dimensionKeys,
    metricDefinitionRequest_eventPattern,
    metricDefinitionRequest_namespace,
    metricDefinitionRequest_unitLabel,
    metricDefinitionRequest_valueKey,
    metricDefinitionRequest_name,

    -- ** MetricDestinationSummary
    metricDestinationSummary_destination,
    metricDestinationSummary_destinationArn,
    metricDestinationSummary_iamRoleArn,

    -- ** QueryFilter
    queryFilter_name,
    queryFilter_values,

    -- ** RumEvent
    rumEvent_metadata,
    rumEvent_details,
    rumEvent_id,
    rumEvent_timestamp,
    rumEvent_type,

    -- ** TimeRange
    timeRange_before,
    timeRange_after,

    -- ** UserDetails
    userDetails_sessionId,
    userDetails_userId,
  )
where

import Amazonka.Rum.BatchCreateRumMetricDefinitions
import Amazonka.Rum.BatchDeleteRumMetricDefinitions
import Amazonka.Rum.BatchGetRumMetricDefinitions
import Amazonka.Rum.CreateAppMonitor
import Amazonka.Rum.DeleteAppMonitor
import Amazonka.Rum.DeleteRumMetricsDestination
import Amazonka.Rum.GetAppMonitor
import Amazonka.Rum.GetAppMonitorData
import Amazonka.Rum.ListAppMonitors
import Amazonka.Rum.ListRumMetricsDestinations
import Amazonka.Rum.ListTagsForResource
import Amazonka.Rum.PutRumEvents
import Amazonka.Rum.PutRumMetricsDestination
import Amazonka.Rum.TagResource
import Amazonka.Rum.Types.AppMonitor
import Amazonka.Rum.Types.AppMonitorConfiguration
import Amazonka.Rum.Types.AppMonitorDetails
import Amazonka.Rum.Types.AppMonitorSummary
import Amazonka.Rum.Types.BatchCreateRumMetricDefinitionsError
import Amazonka.Rum.Types.BatchDeleteRumMetricDefinitionsError
import Amazonka.Rum.Types.CustomEvents
import Amazonka.Rum.Types.CwLog
import Amazonka.Rum.Types.DataStorage
import Amazonka.Rum.Types.MetricDefinition
import Amazonka.Rum.Types.MetricDefinitionRequest
import Amazonka.Rum.Types.MetricDestinationSummary
import Amazonka.Rum.Types.QueryFilter
import Amazonka.Rum.Types.RumEvent
import Amazonka.Rum.Types.TimeRange
import Amazonka.Rum.Types.UserDetails
import Amazonka.Rum.UntagResource
import Amazonka.Rum.UpdateAppMonitor
import Amazonka.Rum.UpdateRumMetricDefinition

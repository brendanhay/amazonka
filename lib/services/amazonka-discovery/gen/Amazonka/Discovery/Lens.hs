{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Discovery.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Discovery.Lens
  ( -- * Operations

    -- ** AssociateConfigurationItemsToApplication
    associateConfigurationItemsToApplication_applicationConfigurationId,
    associateConfigurationItemsToApplication_configurationIds,
    associateConfigurationItemsToApplicationResponse_httpStatus,

    -- ** BatchDeleteImportData
    batchDeleteImportData_importTaskIds,
    batchDeleteImportDataResponse_errors,
    batchDeleteImportDataResponse_httpStatus,

    -- ** CreateApplication
    createApplication_description,
    createApplication_name,
    createApplicationResponse_configurationId,
    createApplicationResponse_httpStatus,

    -- ** CreateTags
    createTags_configurationIds,
    createTags_tags,
    createTagsResponse_httpStatus,

    -- ** DeleteApplications
    deleteApplications_configurationIds,
    deleteApplicationsResponse_httpStatus,

    -- ** DeleteTags
    deleteTags_tags,
    deleteTags_configurationIds,
    deleteTagsResponse_httpStatus,

    -- ** DescribeAgents
    describeAgents_agentIds,
    describeAgents_filters,
    describeAgents_maxResults,
    describeAgents_nextToken,
    describeAgentsResponse_agentsInfo,
    describeAgentsResponse_nextToken,
    describeAgentsResponse_httpStatus,

    -- ** DescribeConfigurations
    describeConfigurations_configurationIds,
    describeConfigurationsResponse_configurations,
    describeConfigurationsResponse_httpStatus,

    -- ** DescribeContinuousExports
    describeContinuousExports_exportIds,
    describeContinuousExports_maxResults,
    describeContinuousExports_nextToken,
    describeContinuousExportsResponse_descriptions,
    describeContinuousExportsResponse_nextToken,
    describeContinuousExportsResponse_httpStatus,

    -- ** DescribeExportTasks
    describeExportTasks_exportIds,
    describeExportTasks_filters,
    describeExportTasks_maxResults,
    describeExportTasks_nextToken,
    describeExportTasksResponse_exportsInfo,
    describeExportTasksResponse_nextToken,
    describeExportTasksResponse_httpStatus,

    -- ** DescribeImportTasks
    describeImportTasks_filters,
    describeImportTasks_maxResults,
    describeImportTasks_nextToken,
    describeImportTasksResponse_nextToken,
    describeImportTasksResponse_tasks,
    describeImportTasksResponse_httpStatus,

    -- ** DescribeTags
    describeTags_filters,
    describeTags_maxResults,
    describeTags_nextToken,
    describeTagsResponse_nextToken,
    describeTagsResponse_tags,
    describeTagsResponse_httpStatus,

    -- ** DisassociateConfigurationItemsFromApplication
    disassociateConfigurationItemsFromApplication_applicationConfigurationId,
    disassociateConfigurationItemsFromApplication_configurationIds,
    disassociateConfigurationItemsFromApplicationResponse_httpStatus,

    -- ** GetDiscoverySummary
    getDiscoverySummaryResponse_agentSummary,
    getDiscoverySummaryResponse_agentlessCollectorSummary,
    getDiscoverySummaryResponse_applications,
    getDiscoverySummaryResponse_connectorSummary,
    getDiscoverySummaryResponse_meCollectorSummary,
    getDiscoverySummaryResponse_servers,
    getDiscoverySummaryResponse_serversMappedToApplications,
    getDiscoverySummaryResponse_serversMappedtoTags,
    getDiscoverySummaryResponse_httpStatus,

    -- ** ListConfigurations
    listConfigurations_filters,
    listConfigurations_maxResults,
    listConfigurations_nextToken,
    listConfigurations_orderBy,
    listConfigurations_configurationType,
    listConfigurationsResponse_configurations,
    listConfigurationsResponse_nextToken,
    listConfigurationsResponse_httpStatus,

    -- ** ListServerNeighbors
    listServerNeighbors_maxResults,
    listServerNeighbors_neighborConfigurationIds,
    listServerNeighbors_nextToken,
    listServerNeighbors_portInformationNeeded,
    listServerNeighbors_configurationId,
    listServerNeighborsResponse_knownDependencyCount,
    listServerNeighborsResponse_nextToken,
    listServerNeighborsResponse_httpStatus,
    listServerNeighborsResponse_neighbors,

    -- ** StartContinuousExport
    startContinuousExportResponse_dataSource,
    startContinuousExportResponse_exportId,
    startContinuousExportResponse_s3Bucket,
    startContinuousExportResponse_schemaStorageConfig,
    startContinuousExportResponse_startTime,
    startContinuousExportResponse_httpStatus,

    -- ** StartDataCollectionByAgentIds
    startDataCollectionByAgentIds_agentIds,
    startDataCollectionByAgentIdsResponse_agentsConfigurationStatus,
    startDataCollectionByAgentIdsResponse_httpStatus,

    -- ** StartExportTask
    startExportTask_endTime,
    startExportTask_exportDataFormat,
    startExportTask_filters,
    startExportTask_startTime,
    startExportTaskResponse_exportId,
    startExportTaskResponse_httpStatus,

    -- ** StartImportTask
    startImportTask_clientRequestToken,
    startImportTask_name,
    startImportTask_importUrl,
    startImportTaskResponse_task,
    startImportTaskResponse_httpStatus,

    -- ** StopContinuousExport
    stopContinuousExport_exportId,
    stopContinuousExportResponse_startTime,
    stopContinuousExportResponse_stopTime,
    stopContinuousExportResponse_httpStatus,

    -- ** StopDataCollectionByAgentIds
    stopDataCollectionByAgentIds_agentIds,
    stopDataCollectionByAgentIdsResponse_agentsConfigurationStatus,
    stopDataCollectionByAgentIdsResponse_httpStatus,

    -- ** UpdateApplication
    updateApplication_description,
    updateApplication_name,
    updateApplication_configurationId,
    updateApplicationResponse_httpStatus,

    -- * Types

    -- ** AgentConfigurationStatus
    agentConfigurationStatus_agentId,
    agentConfigurationStatus_description,
    agentConfigurationStatus_operationSucceeded,

    -- ** AgentInfo
    agentInfo_agentId,
    agentInfo_agentNetworkInfoList,
    agentInfo_agentType,
    agentInfo_collectionStatus,
    agentInfo_connectorId,
    agentInfo_health,
    agentInfo_hostName,
    agentInfo_lastHealthPingTime,
    agentInfo_registeredTime,
    agentInfo_version,

    -- ** AgentNetworkInfo
    agentNetworkInfo_ipAddress,
    agentNetworkInfo_macAddress,

    -- ** BatchDeleteImportDataError
    batchDeleteImportDataError_errorCode,
    batchDeleteImportDataError_errorDescription,
    batchDeleteImportDataError_importTaskId,

    -- ** ConfigurationTag
    configurationTag_configurationId,
    configurationTag_configurationType,
    configurationTag_key,
    configurationTag_timeOfCreation,
    configurationTag_value,

    -- ** ContinuousExportDescription
    continuousExportDescription_dataSource,
    continuousExportDescription_exportId,
    continuousExportDescription_s3Bucket,
    continuousExportDescription_schemaStorageConfig,
    continuousExportDescription_startTime,
    continuousExportDescription_status,
    continuousExportDescription_statusDetail,
    continuousExportDescription_stopTime,

    -- ** CustomerAgentInfo
    customerAgentInfo_activeAgents,
    customerAgentInfo_healthyAgents,
    customerAgentInfo_blackListedAgents,
    customerAgentInfo_shutdownAgents,
    customerAgentInfo_unhealthyAgents,
    customerAgentInfo_totalAgents,
    customerAgentInfo_unknownAgents,

    -- ** CustomerAgentlessCollectorInfo
    customerAgentlessCollectorInfo_activeAgentlessCollectors,
    customerAgentlessCollectorInfo_healthyAgentlessCollectors,
    customerAgentlessCollectorInfo_denyListedAgentlessCollectors,
    customerAgentlessCollectorInfo_shutdownAgentlessCollectors,
    customerAgentlessCollectorInfo_unhealthyAgentlessCollectors,
    customerAgentlessCollectorInfo_totalAgentlessCollectors,
    customerAgentlessCollectorInfo_unknownAgentlessCollectors,

    -- ** CustomerConnectorInfo
    customerConnectorInfo_activeConnectors,
    customerConnectorInfo_healthyConnectors,
    customerConnectorInfo_blackListedConnectors,
    customerConnectorInfo_shutdownConnectors,
    customerConnectorInfo_unhealthyConnectors,
    customerConnectorInfo_totalConnectors,
    customerConnectorInfo_unknownConnectors,

    -- ** CustomerMeCollectorInfo
    customerMeCollectorInfo_activeMeCollectors,
    customerMeCollectorInfo_healthyMeCollectors,
    customerMeCollectorInfo_denyListedMeCollectors,
    customerMeCollectorInfo_shutdownMeCollectors,
    customerMeCollectorInfo_unhealthyMeCollectors,
    customerMeCollectorInfo_totalMeCollectors,
    customerMeCollectorInfo_unknownMeCollectors,

    -- ** ExportFilter
    exportFilter_name,
    exportFilter_values,
    exportFilter_condition,

    -- ** ExportInfo
    exportInfo_configurationsDownloadUrl,
    exportInfo_isTruncated,
    exportInfo_requestedEndTime,
    exportInfo_requestedStartTime,
    exportInfo_exportId,
    exportInfo_exportStatus,
    exportInfo_statusMessage,
    exportInfo_exportRequestTime,

    -- ** Filter
    filter_name,
    filter_values,
    filter_condition,

    -- ** ImportTask
    importTask_applicationImportFailure,
    importTask_applicationImportSuccess,
    importTask_clientRequestToken,
    importTask_errorsAndFailedEntriesZip,
    importTask_importCompletionTime,
    importTask_importDeletedTime,
    importTask_importRequestTime,
    importTask_importTaskId,
    importTask_importUrl,
    importTask_name,
    importTask_serverImportFailure,
    importTask_serverImportSuccess,
    importTask_status,

    -- ** ImportTaskFilter
    importTaskFilter_name,
    importTaskFilter_values,

    -- ** NeighborConnectionDetail
    neighborConnectionDetail_destinationPort,
    neighborConnectionDetail_transportProtocol,
    neighborConnectionDetail_sourceServerId,
    neighborConnectionDetail_destinationServerId,
    neighborConnectionDetail_connectionsCount,

    -- ** OrderByElement
    orderByElement_sortOrder,
    orderByElement_fieldName,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TagFilter
    tagFilter_name,
    tagFilter_values,
  )
where

import Amazonka.Discovery.AssociateConfigurationItemsToApplication
import Amazonka.Discovery.BatchDeleteImportData
import Amazonka.Discovery.CreateApplication
import Amazonka.Discovery.CreateTags
import Amazonka.Discovery.DeleteApplications
import Amazonka.Discovery.DeleteTags
import Amazonka.Discovery.DescribeAgents
import Amazonka.Discovery.DescribeConfigurations
import Amazonka.Discovery.DescribeContinuousExports
import Amazonka.Discovery.DescribeExportTasks
import Amazonka.Discovery.DescribeImportTasks
import Amazonka.Discovery.DescribeTags
import Amazonka.Discovery.DisassociateConfigurationItemsFromApplication
import Amazonka.Discovery.GetDiscoverySummary
import Amazonka.Discovery.ListConfigurations
import Amazonka.Discovery.ListServerNeighbors
import Amazonka.Discovery.StartContinuousExport
import Amazonka.Discovery.StartDataCollectionByAgentIds
import Amazonka.Discovery.StartExportTask
import Amazonka.Discovery.StartImportTask
import Amazonka.Discovery.StopContinuousExport
import Amazonka.Discovery.StopDataCollectionByAgentIds
import Amazonka.Discovery.Types.AgentConfigurationStatus
import Amazonka.Discovery.Types.AgentInfo
import Amazonka.Discovery.Types.AgentNetworkInfo
import Amazonka.Discovery.Types.BatchDeleteImportDataError
import Amazonka.Discovery.Types.ConfigurationTag
import Amazonka.Discovery.Types.ContinuousExportDescription
import Amazonka.Discovery.Types.CustomerAgentInfo
import Amazonka.Discovery.Types.CustomerAgentlessCollectorInfo
import Amazonka.Discovery.Types.CustomerConnectorInfo
import Amazonka.Discovery.Types.CustomerMeCollectorInfo
import Amazonka.Discovery.Types.ExportFilter
import Amazonka.Discovery.Types.ExportInfo
import Amazonka.Discovery.Types.Filter
import Amazonka.Discovery.Types.ImportTask
import Amazonka.Discovery.Types.ImportTaskFilter
import Amazonka.Discovery.Types.NeighborConnectionDetail
import Amazonka.Discovery.Types.OrderByElement
import Amazonka.Discovery.Types.Tag
import Amazonka.Discovery.Types.TagFilter
import Amazonka.Discovery.UpdateApplication

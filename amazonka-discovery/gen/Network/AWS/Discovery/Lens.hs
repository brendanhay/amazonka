{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Lens
  ( -- * Operations

    -- ** DescribeExportTasks
    describeExportTasks_nextToken,
    describeExportTasks_maxResults,
    describeExportTasks_exportIds,
    describeExportTasks_filters,
    describeExportTasksResponse_nextToken,
    describeExportTasksResponse_exportsInfo,
    describeExportTasksResponse_httpStatus,

    -- ** DescribeTags
    describeTags_nextToken,
    describeTags_maxResults,
    describeTags_filters,
    describeTagsResponse_nextToken,
    describeTagsResponse_tags,
    describeTagsResponse_httpStatus,

    -- ** StartExportTask
    startExportTask_exportDataFormat,
    startExportTask_startTime,
    startExportTask_endTime,
    startExportTask_filters,
    startExportTaskResponse_exportId,
    startExportTaskResponse_httpStatus,

    -- ** ListConfigurations
    listConfigurations_nextToken,
    listConfigurations_maxResults,
    listConfigurations_orderBy,
    listConfigurations_filters,
    listConfigurations_configurationType,
    listConfigurationsResponse_nextToken,
    listConfigurationsResponse_configurations,
    listConfigurationsResponse_httpStatus,

    -- ** CreateApplication
    createApplication_description,
    createApplication_name,
    createApplicationResponse_configurationId,
    createApplicationResponse_httpStatus,

    -- ** DescribeImportTasks
    describeImportTasks_nextToken,
    describeImportTasks_maxResults,
    describeImportTasks_filters,
    describeImportTasksResponse_nextToken,
    describeImportTasksResponse_tasks,
    describeImportTasksResponse_httpStatus,

    -- ** DeleteApplications
    deleteApplications_configurationIds,
    deleteApplicationsResponse_httpStatus,

    -- ** GetDiscoverySummary
    getDiscoverySummaryResponse_servers,
    getDiscoverySummaryResponse_agentSummary,
    getDiscoverySummaryResponse_connectorSummary,
    getDiscoverySummaryResponse_serversMappedToApplications,
    getDiscoverySummaryResponse_applications,
    getDiscoverySummaryResponse_serversMappedtoTags,
    getDiscoverySummaryResponse_httpStatus,

    -- ** DeleteTags
    deleteTags_tags,
    deleteTags_configurationIds,
    deleteTagsResponse_httpStatus,

    -- ** DisassociateConfigurationItemsFromApplication
    disassociateConfigurationItemsFromApplication_applicationConfigurationId,
    disassociateConfigurationItemsFromApplication_configurationIds,
    disassociateConfigurationItemsFromApplicationResponse_httpStatus,

    -- ** StopDataCollectionByAgentIds
    stopDataCollectionByAgentIds_agentIds,
    stopDataCollectionByAgentIdsResponse_agentsConfigurationStatus,
    stopDataCollectionByAgentIdsResponse_httpStatus,

    -- ** StartDataCollectionByAgentIds
    startDataCollectionByAgentIds_agentIds,
    startDataCollectionByAgentIdsResponse_agentsConfigurationStatus,
    startDataCollectionByAgentIdsResponse_httpStatus,

    -- ** DescribeAgents
    describeAgents_agentIds,
    describeAgents_nextToken,
    describeAgents_maxResults,
    describeAgents_filters,
    describeAgentsResponse_nextToken,
    describeAgentsResponse_agentsInfo,
    describeAgentsResponse_httpStatus,

    -- ** DescribeContinuousExports
    describeContinuousExports_nextToken,
    describeContinuousExports_maxResults,
    describeContinuousExports_exportIds,
    describeContinuousExportsResponse_nextToken,
    describeContinuousExportsResponse_descriptions,
    describeContinuousExportsResponse_httpStatus,

    -- ** StopContinuousExport
    stopContinuousExport_exportId,
    stopContinuousExportResponse_startTime,
    stopContinuousExportResponse_stopTime,
    stopContinuousExportResponse_httpStatus,

    -- ** StartContinuousExport
    startContinuousExportResponse_s3Bucket,
    startContinuousExportResponse_dataSource,
    startContinuousExportResponse_startTime,
    startContinuousExportResponse_schemaStorageConfig,
    startContinuousExportResponse_exportId,
    startContinuousExportResponse_httpStatus,

    -- ** DescribeConfigurations
    describeConfigurations_configurationIds,
    describeConfigurationsResponse_configurations,
    describeConfigurationsResponse_httpStatus,

    -- ** ListServerNeighbors
    listServerNeighbors_nextToken,
    listServerNeighbors_maxResults,
    listServerNeighbors_portInformationNeeded,
    listServerNeighbors_neighborConfigurationIds,
    listServerNeighbors_configurationId,
    listServerNeighborsResponse_nextToken,
    listServerNeighborsResponse_knownDependencyCount,
    listServerNeighborsResponse_httpStatus,
    listServerNeighborsResponse_neighbors,

    -- ** AssociateConfigurationItemsToApplication
    associateConfigurationItemsToApplication_applicationConfigurationId,
    associateConfigurationItemsToApplication_configurationIds,
    associateConfigurationItemsToApplicationResponse_httpStatus,

    -- ** UpdateApplication
    updateApplication_name,
    updateApplication_description,
    updateApplication_configurationId,
    updateApplicationResponse_httpStatus,

    -- ** StartImportTask
    startImportTask_clientRequestToken,
    startImportTask_name,
    startImportTask_importUrl,
    startImportTaskResponse_task,
    startImportTaskResponse_httpStatus,

    -- ** BatchDeleteImportData
    batchDeleteImportData_importTaskIds,
    batchDeleteImportDataResponse_errors,
    batchDeleteImportDataResponse_httpStatus,

    -- ** CreateTags
    createTags_configurationIds,
    createTags_tags,
    createTagsResponse_httpStatus,

    -- * Types

    -- ** AgentConfigurationStatus
    agentConfigurationStatus_agentId,
    agentConfigurationStatus_operationSucceeded,
    agentConfigurationStatus_description,

    -- ** AgentInfo
    agentInfo_hostName,
    agentInfo_agentId,
    agentInfo_agentType,
    agentInfo_connectorId,
    agentInfo_agentNetworkInfoList,
    agentInfo_lastHealthPingTime,
    agentInfo_registeredTime,
    agentInfo_version,
    agentInfo_health,
    agentInfo_collectionStatus,

    -- ** AgentNetworkInfo
    agentNetworkInfo_macAddress,
    agentNetworkInfo_ipAddress,

    -- ** BatchDeleteImportDataError
    batchDeleteImportDataError_errorDescription,
    batchDeleteImportDataError_importTaskId,
    batchDeleteImportDataError_errorCode,

    -- ** ConfigurationTag
    configurationTag_key,
    configurationTag_configurationId,
    configurationTag_value,
    configurationTag_configurationType,
    configurationTag_timeOfCreation,

    -- ** ContinuousExportDescription
    continuousExportDescription_status,
    continuousExportDescription_s3Bucket,
    continuousExportDescription_dataSource,
    continuousExportDescription_startTime,
    continuousExportDescription_statusDetail,
    continuousExportDescription_stopTime,
    continuousExportDescription_schemaStorageConfig,
    continuousExportDescription_exportId,

    -- ** CustomerAgentInfo
    customerAgentInfo_activeAgents,
    customerAgentInfo_healthyAgents,
    customerAgentInfo_blackListedAgents,
    customerAgentInfo_shutdownAgents,
    customerAgentInfo_unhealthyAgents,
    customerAgentInfo_totalAgents,
    customerAgentInfo_unknownAgents,

    -- ** CustomerConnectorInfo
    customerConnectorInfo_activeConnectors,
    customerConnectorInfo_healthyConnectors,
    customerConnectorInfo_blackListedConnectors,
    customerConnectorInfo_shutdownConnectors,
    customerConnectorInfo_unhealthyConnectors,
    customerConnectorInfo_totalConnectors,
    customerConnectorInfo_unknownConnectors,

    -- ** ExportFilter
    exportFilter_name,
    exportFilter_values,
    exportFilter_condition,

    -- ** ExportInfo
    exportInfo_isTruncated,
    exportInfo_configurationsDownloadUrl,
    exportInfo_requestedStartTime,
    exportInfo_requestedEndTime,
    exportInfo_exportId,
    exportInfo_exportStatus,
    exportInfo_statusMessage,
    exportInfo_exportRequestTime,

    -- ** Filter
    filter_name,
    filter_values,
    filter_condition,

    -- ** ImportTask
    importTask_status,
    importTask_applicationImportSuccess,
    importTask_importRequestTime,
    importTask_serverImportFailure,
    importTask_importTaskId,
    importTask_errorsAndFailedEntriesZip,
    importTask_name,
    importTask_applicationImportFailure,
    importTask_importCompletionTime,
    importTask_importUrl,
    importTask_clientRequestToken,
    importTask_serverImportSuccess,
    importTask_importDeletedTime,

    -- ** ImportTaskFilter
    importTaskFilter_values,
    importTaskFilter_name,

    -- ** NeighborConnectionDetail
    neighborConnectionDetail_transportProtocol,
    neighborConnectionDetail_destinationPort,
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

import Network.AWS.Discovery.AssociateConfigurationItemsToApplication
import Network.AWS.Discovery.BatchDeleteImportData
import Network.AWS.Discovery.CreateApplication
import Network.AWS.Discovery.CreateTags
import Network.AWS.Discovery.DeleteApplications
import Network.AWS.Discovery.DeleteTags
import Network.AWS.Discovery.DescribeAgents
import Network.AWS.Discovery.DescribeConfigurations
import Network.AWS.Discovery.DescribeContinuousExports
import Network.AWS.Discovery.DescribeExportTasks
import Network.AWS.Discovery.DescribeImportTasks
import Network.AWS.Discovery.DescribeTags
import Network.AWS.Discovery.DisassociateConfigurationItemsFromApplication
import Network.AWS.Discovery.GetDiscoverySummary
import Network.AWS.Discovery.ListConfigurations
import Network.AWS.Discovery.ListServerNeighbors
import Network.AWS.Discovery.StartContinuousExport
import Network.AWS.Discovery.StartDataCollectionByAgentIds
import Network.AWS.Discovery.StartExportTask
import Network.AWS.Discovery.StartImportTask
import Network.AWS.Discovery.StopContinuousExport
import Network.AWS.Discovery.StopDataCollectionByAgentIds
import Network.AWS.Discovery.Types.AgentConfigurationStatus
import Network.AWS.Discovery.Types.AgentInfo
import Network.AWS.Discovery.Types.AgentNetworkInfo
import Network.AWS.Discovery.Types.BatchDeleteImportDataError
import Network.AWS.Discovery.Types.ConfigurationTag
import Network.AWS.Discovery.Types.ContinuousExportDescription
import Network.AWS.Discovery.Types.CustomerAgentInfo
import Network.AWS.Discovery.Types.CustomerConnectorInfo
import Network.AWS.Discovery.Types.ExportFilter
import Network.AWS.Discovery.Types.ExportInfo
import Network.AWS.Discovery.Types.Filter
import Network.AWS.Discovery.Types.ImportTask
import Network.AWS.Discovery.Types.ImportTaskFilter
import Network.AWS.Discovery.Types.NeighborConnectionDetail
import Network.AWS.Discovery.Types.OrderByElement
import Network.AWS.Discovery.Types.Tag
import Network.AWS.Discovery.Types.TagFilter
import Network.AWS.Discovery.UpdateApplication

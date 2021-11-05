{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Discovery.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Discovery.Lens
  ( -- * Operations

    -- ** DescribeTags
    describeTags_filters,
    describeTags_nextToken,
    describeTags_maxResults,
    describeTagsResponse_nextToken,
    describeTagsResponse_tags,
    describeTagsResponse_httpStatus,

    -- ** DescribeContinuousExports
    describeContinuousExports_nextToken,
    describeContinuousExports_exportIds,
    describeContinuousExports_maxResults,
    describeContinuousExportsResponse_nextToken,
    describeContinuousExportsResponse_descriptions,
    describeContinuousExportsResponse_httpStatus,

    -- ** StopDataCollectionByAgentIds
    stopDataCollectionByAgentIds_agentIds,
    stopDataCollectionByAgentIdsResponse_agentsConfigurationStatus,
    stopDataCollectionByAgentIdsResponse_httpStatus,

    -- ** CreateTags
    createTags_configurationIds,
    createTags_tags,
    createTagsResponse_httpStatus,

    -- ** BatchDeleteImportData
    batchDeleteImportData_importTaskIds,
    batchDeleteImportDataResponse_errors,
    batchDeleteImportDataResponse_httpStatus,

    -- ** DeleteTags
    deleteTags_tags,
    deleteTags_configurationIds,
    deleteTagsResponse_httpStatus,

    -- ** StartImportTask
    startImportTask_clientRequestToken,
    startImportTask_name,
    startImportTask_importUrl,
    startImportTaskResponse_task,
    startImportTaskResponse_httpStatus,

    -- ** DeleteApplications
    deleteApplications_configurationIds,
    deleteApplicationsResponse_httpStatus,

    -- ** UpdateApplication
    updateApplication_name,
    updateApplication_description,
    updateApplication_configurationId,
    updateApplicationResponse_httpStatus,

    -- ** DescribeConfigurations
    describeConfigurations_configurationIds,
    describeConfigurationsResponse_configurations,
    describeConfigurationsResponse_httpStatus,

    -- ** DescribeImportTasks
    describeImportTasks_filters,
    describeImportTasks_nextToken,
    describeImportTasks_maxResults,
    describeImportTasksResponse_tasks,
    describeImportTasksResponse_nextToken,
    describeImportTasksResponse_httpStatus,

    -- ** CreateApplication
    createApplication_description,
    createApplication_name,
    createApplicationResponse_configurationId,
    createApplicationResponse_httpStatus,

    -- ** ListConfigurations
    listConfigurations_orderBy,
    listConfigurations_filters,
    listConfigurations_nextToken,
    listConfigurations_maxResults,
    listConfigurations_configurationType,
    listConfigurationsResponse_configurations,
    listConfigurationsResponse_nextToken,
    listConfigurationsResponse_httpStatus,

    -- ** StartContinuousExport
    startContinuousExportResponse_startTime,
    startContinuousExportResponse_schemaStorageConfig,
    startContinuousExportResponse_dataSource,
    startContinuousExportResponse_s3Bucket,
    startContinuousExportResponse_exportId,
    startContinuousExportResponse_httpStatus,

    -- ** DescribeAgents
    describeAgents_agentIds,
    describeAgents_filters,
    describeAgents_nextToken,
    describeAgents_maxResults,
    describeAgentsResponse_agentsInfo,
    describeAgentsResponse_nextToken,
    describeAgentsResponse_httpStatus,

    -- ** DescribeExportTasks
    describeExportTasks_filters,
    describeExportTasks_nextToken,
    describeExportTasks_exportIds,
    describeExportTasks_maxResults,
    describeExportTasksResponse_nextToken,
    describeExportTasksResponse_exportsInfo,
    describeExportTasksResponse_httpStatus,

    -- ** StartDataCollectionByAgentIds
    startDataCollectionByAgentIds_agentIds,
    startDataCollectionByAgentIdsResponse_agentsConfigurationStatus,
    startDataCollectionByAgentIdsResponse_httpStatus,

    -- ** GetDiscoverySummary
    getDiscoverySummaryResponse_servers,
    getDiscoverySummaryResponse_serversMappedtoTags,
    getDiscoverySummaryResponse_serversMappedToApplications,
    getDiscoverySummaryResponse_connectorSummary,
    getDiscoverySummaryResponse_agentSummary,
    getDiscoverySummaryResponse_applications,
    getDiscoverySummaryResponse_httpStatus,

    -- ** DisassociateConfigurationItemsFromApplication
    disassociateConfigurationItemsFromApplication_applicationConfigurationId,
    disassociateConfigurationItemsFromApplication_configurationIds,
    disassociateConfigurationItemsFromApplicationResponse_httpStatus,

    -- ** AssociateConfigurationItemsToApplication
    associateConfigurationItemsToApplication_applicationConfigurationId,
    associateConfigurationItemsToApplication_configurationIds,
    associateConfigurationItemsToApplicationResponse_httpStatus,

    -- ** ListServerNeighbors
    listServerNeighbors_portInformationNeeded,
    listServerNeighbors_neighborConfigurationIds,
    listServerNeighbors_nextToken,
    listServerNeighbors_maxResults,
    listServerNeighbors_configurationId,
    listServerNeighborsResponse_nextToken,
    listServerNeighborsResponse_knownDependencyCount,
    listServerNeighborsResponse_httpStatus,
    listServerNeighborsResponse_neighbors,

    -- ** StopContinuousExport
    stopContinuousExport_exportId,
    stopContinuousExportResponse_startTime,
    stopContinuousExportResponse_stopTime,
    stopContinuousExportResponse_httpStatus,

    -- ** StartExportTask
    startExportTask_exportDataFormat,
    startExportTask_startTime,
    startExportTask_filters,
    startExportTask_endTime,
    startExportTaskResponse_exportId,
    startExportTaskResponse_httpStatus,

    -- * Types

    -- ** AgentConfigurationStatus
    agentConfigurationStatus_agentId,
    agentConfigurationStatus_operationSucceeded,
    agentConfigurationStatus_description,

    -- ** AgentInfo
    agentInfo_hostName,
    agentInfo_lastHealthPingTime,
    agentInfo_agentNetworkInfoList,
    agentInfo_connectorId,
    agentInfo_health,
    agentInfo_agentId,
    agentInfo_version,
    agentInfo_collectionStatus,
    agentInfo_registeredTime,
    agentInfo_agentType,

    -- ** AgentNetworkInfo
    agentNetworkInfo_ipAddress,
    agentNetworkInfo_macAddress,

    -- ** BatchDeleteImportDataError
    batchDeleteImportDataError_importTaskId,
    batchDeleteImportDataError_errorCode,
    batchDeleteImportDataError_errorDescription,

    -- ** ConfigurationTag
    configurationTag_timeOfCreation,
    configurationTag_configurationId,
    configurationTag_configurationType,
    configurationTag_value,
    configurationTag_key,

    -- ** ContinuousExportDescription
    continuousExportDescription_status,
    continuousExportDescription_startTime,
    continuousExportDescription_schemaStorageConfig,
    continuousExportDescription_statusDetail,
    continuousExportDescription_stopTime,
    continuousExportDescription_dataSource,
    continuousExportDescription_s3Bucket,
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
    exportInfo_configurationsDownloadUrl,
    exportInfo_requestedStartTime,
    exportInfo_requestedEndTime,
    exportInfo_isTruncated,
    exportInfo_exportId,
    exportInfo_exportStatus,
    exportInfo_statusMessage,
    exportInfo_exportRequestTime,

    -- ** Filter
    filter_name,
    filter_values,
    filter_condition,

    -- ** ImportTask
    importTask_applicationImportSuccess,
    importTask_status,
    importTask_serverImportSuccess,
    importTask_importCompletionTime,
    importTask_name,
    importTask_applicationImportFailure,
    importTask_errorsAndFailedEntriesZip,
    importTask_importTaskId,
    importTask_importDeletedTime,
    importTask_serverImportFailure,
    importTask_clientRequestToken,
    importTask_importUrl,
    importTask_importRequestTime,

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
import Amazonka.Discovery.Types.CustomerConnectorInfo
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

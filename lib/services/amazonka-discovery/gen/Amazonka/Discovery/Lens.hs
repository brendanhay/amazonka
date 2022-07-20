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
    describeAgents_nextToken,
    describeAgents_filters,
    describeAgents_maxResults,
    describeAgents_agentIds,
    describeAgentsResponse_nextToken,
    describeAgentsResponse_agentsInfo,
    describeAgentsResponse_httpStatus,

    -- ** DescribeConfigurations
    describeConfigurations_configurationIds,
    describeConfigurationsResponse_configurations,
    describeConfigurationsResponse_httpStatus,

    -- ** DescribeContinuousExports
    describeContinuousExports_exportIds,
    describeContinuousExports_nextToken,
    describeContinuousExports_maxResults,
    describeContinuousExportsResponse_nextToken,
    describeContinuousExportsResponse_descriptions,
    describeContinuousExportsResponse_httpStatus,

    -- ** DescribeExportTasks
    describeExportTasks_exportIds,
    describeExportTasks_nextToken,
    describeExportTasks_filters,
    describeExportTasks_maxResults,
    describeExportTasksResponse_nextToken,
    describeExportTasksResponse_exportsInfo,
    describeExportTasksResponse_httpStatus,

    -- ** DescribeImportTasks
    describeImportTasks_nextToken,
    describeImportTasks_filters,
    describeImportTasks_maxResults,
    describeImportTasksResponse_tasks,
    describeImportTasksResponse_nextToken,
    describeImportTasksResponse_httpStatus,

    -- ** DescribeTags
    describeTags_nextToken,
    describeTags_filters,
    describeTags_maxResults,
    describeTagsResponse_tags,
    describeTagsResponse_nextToken,
    describeTagsResponse_httpStatus,

    -- ** DisassociateConfigurationItemsFromApplication
    disassociateConfigurationItemsFromApplication_applicationConfigurationId,
    disassociateConfigurationItemsFromApplication_configurationIds,
    disassociateConfigurationItemsFromApplicationResponse_httpStatus,

    -- ** GetDiscoverySummary
    getDiscoverySummaryResponse_servers,
    getDiscoverySummaryResponse_applications,
    getDiscoverySummaryResponse_agentSummary,
    getDiscoverySummaryResponse_connectorSummary,
    getDiscoverySummaryResponse_serversMappedtoTags,
    getDiscoverySummaryResponse_serversMappedToApplications,
    getDiscoverySummaryResponse_httpStatus,

    -- ** ListConfigurations
    listConfigurations_nextToken,
    listConfigurations_filters,
    listConfigurations_maxResults,
    listConfigurations_orderBy,
    listConfigurations_configurationType,
    listConfigurationsResponse_nextToken,
    listConfigurationsResponse_configurations,
    listConfigurationsResponse_httpStatus,

    -- ** ListServerNeighbors
    listServerNeighbors_portInformationNeeded,
    listServerNeighbors_nextToken,
    listServerNeighbors_neighborConfigurationIds,
    listServerNeighbors_maxResults,
    listServerNeighbors_configurationId,
    listServerNeighborsResponse_nextToken,
    listServerNeighborsResponse_knownDependencyCount,
    listServerNeighborsResponse_httpStatus,
    listServerNeighborsResponse_neighbors,

    -- ** StartContinuousExport
    startContinuousExportResponse_s3Bucket,
    startContinuousExportResponse_schemaStorageConfig,
    startContinuousExportResponse_dataSource,
    startContinuousExportResponse_exportId,
    startContinuousExportResponse_startTime,
    startContinuousExportResponse_httpStatus,

    -- ** StartDataCollectionByAgentIds
    startDataCollectionByAgentIds_agentIds,
    startDataCollectionByAgentIdsResponse_agentsConfigurationStatus,
    startDataCollectionByAgentIdsResponse_httpStatus,

    -- ** StartExportTask
    startExportTask_filters,
    startExportTask_endTime,
    startExportTask_exportDataFormat,
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
    stopContinuousExportResponse_stopTime,
    stopContinuousExportResponse_startTime,
    stopContinuousExportResponse_httpStatus,

    -- ** StopDataCollectionByAgentIds
    stopDataCollectionByAgentIds_agentIds,
    stopDataCollectionByAgentIdsResponse_agentsConfigurationStatus,
    stopDataCollectionByAgentIdsResponse_httpStatus,

    -- ** UpdateApplication
    updateApplication_name,
    updateApplication_description,
    updateApplication_configurationId,
    updateApplicationResponse_httpStatus,

    -- * Types

    -- ** AgentConfigurationStatus
    agentConfigurationStatus_description,
    agentConfigurationStatus_agentId,
    agentConfigurationStatus_operationSucceeded,

    -- ** AgentInfo
    agentInfo_agentType,
    agentInfo_collectionStatus,
    agentInfo_connectorId,
    agentInfo_lastHealthPingTime,
    agentInfo_hostName,
    agentInfo_agentId,
    agentInfo_health,
    agentInfo_agentNetworkInfoList,
    agentInfo_registeredTime,
    agentInfo_version,

    -- ** AgentNetworkInfo
    agentNetworkInfo_macAddress,
    agentNetworkInfo_ipAddress,

    -- ** BatchDeleteImportDataError
    batchDeleteImportDataError_importTaskId,
    batchDeleteImportDataError_errorCode,
    batchDeleteImportDataError_errorDescription,

    -- ** ConfigurationTag
    configurationTag_key,
    configurationTag_timeOfCreation,
    configurationTag_configurationId,
    configurationTag_configurationType,
    configurationTag_value,

    -- ** ContinuousExportDescription
    continuousExportDescription_s3Bucket,
    continuousExportDescription_stopTime,
    continuousExportDescription_statusDetail,
    continuousExportDescription_status,
    continuousExportDescription_schemaStorageConfig,
    continuousExportDescription_dataSource,
    continuousExportDescription_exportId,
    continuousExportDescription_startTime,

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
    exportInfo_requestedEndTime,
    exportInfo_isTruncated,
    exportInfo_requestedStartTime,
    exportInfo_configurationsDownloadUrl,
    exportInfo_exportId,
    exportInfo_exportStatus,
    exportInfo_statusMessage,
    exportInfo_exportRequestTime,

    -- ** Filter
    filter_name,
    filter_values,
    filter_condition,

    -- ** ImportTask
    importTask_name,
    importTask_serverImportFailure,
    importTask_clientRequestToken,
    importTask_importTaskId,
    importTask_status,
    importTask_serverImportSuccess,
    importTask_errorsAndFailedEntriesZip,
    importTask_applicationImportSuccess,
    importTask_applicationImportFailure,
    importTask_importCompletionTime,
    importTask_importRequestTime,
    importTask_importUrl,
    importTask_importDeletedTime,

    -- ** ImportTaskFilter
    importTaskFilter_name,
    importTaskFilter_values,

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

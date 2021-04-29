{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Application Discovery Service
--
-- AWS Application Discovery Service helps you plan application migration
-- projects. It automatically identifies servers, virtual machines (VMs),
-- and network dependencies in your on-premises data centers. For more
-- information, see the
-- <http://aws.amazon.com/application-discovery/faqs/ AWS Application Discovery Service FAQ>.
-- Application Discovery Service offers three ways of performing discovery
-- and collecting data about your on-premises servers:
--
-- -   __Agentless discovery__ is recommended for environments that use
--     VMware vCenter Server. This mode doesn\'t require you to install an
--     agent on each host. It does not work in non-VMware environments.
--
--     -   Agentless discovery gathers server information regardless of the
--         operating systems, which minimizes the time required for initial
--         on-premises infrastructure assessment.
--
--     -   Agentless discovery doesn\'t collect information about network
--         dependencies, only agent-based discovery collects that
--         information.
--
-- -   __Agent-based discovery__ collects a richer set of data than
--     agentless discovery by using the AWS Application Discovery Agent,
--     which you install on one or more hosts in your data center.
--
--     -   The agent captures infrastructure and application information,
--         including an inventory of running processes, system performance
--         information, resource utilization, and network dependencies.
--
--     -   The information collected by agents is secured at rest and in
--         transit to the Application Discovery Service database in the
--         cloud.
--
-- -   __AWS Partner Network (APN) solutions__ integrate with Application
--     Discovery Service, enabling you to import details of your
--     on-premises environment directly into Migration Hub without using
--     the discovery connector or discovery agent.
--
--     -   Third-party application discovery tools can query AWS
--         Application Discovery Service, and they can write to the
--         Application Discovery Service database using the public API.
--
--     -   In this way, you can import data into Migration Hub and view it,
--         so that you can associate applications with servers and track
--         migrations.
--
-- __Recommendations__
--
-- We recommend that you use agent-based discovery for non-VMware
-- environments, and whenever you want to collect information about network
-- dependencies. You can run agent-based and agentless discovery
-- simultaneously. Use agentless discovery to complete the initial
-- infrastructure assessment quickly, and then install agents on select
-- hosts to collect additional information.
--
-- __Working With This Guide__
--
-- This API reference provides descriptions, syntax, and usage examples for
-- each of the actions and data types for Application Discovery Service.
-- The topic for each action shows the API request parameters and the
-- response. Alternatively, you can use one of the AWS SDKs to access an
-- API that is tailored to the programming language or platform that
-- you\'re using. For more information, see
-- <http://aws.amazon.com/tools/#SDKs AWS SDKs>.
--
-- -   Remember that you must set your Migration Hub home region before you
--     call any of these APIs.
--
-- -   You must make API calls for write actions (create, notify,
--     associate, disassociate, import, or put) while in your home region,
--     or a @HomeRegionNotSetException@ error is returned.
--
-- -   API calls for read actions (list, describe, stop, and delete) are
--     permitted outside of your home region.
--
-- -   Although it is unlikely, the Migration Hub home region could change.
--     If you call APIs outside the home region, an @InvalidInputException@
--     is returned.
--
-- -   You must call @GetHomeRegion@ to obtain the latest Migration Hub
--     home region.
--
-- This guide is intended for use with the
-- <http://docs.aws.amazon.com/application-discovery/latest/userguide/ AWS Application Discovery Service User Guide>.
--
-- All data is handled according to the
-- <http://aws.amazon.com/privacy/ AWS Privacy Policy>. You can operate
-- Application Discovery Service offline to inspect collected data before
-- it is shared with the service.
module Network.AWS.Discovery
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ConflictErrorException
    _ConflictErrorException,

    -- ** OperationNotPermittedException
    _OperationNotPermittedException,

    -- ** ServerInternalErrorException
    _ServerInternalErrorException,

    -- ** HomeRegionNotSetException
    _HomeRegionNotSetException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** InvalidParameterValueException
    _InvalidParameterValueException,

    -- ** AuthorizationErrorException
    _AuthorizationErrorException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeExportTasks (Paginated)
    DescribeExportTasks (DescribeExportTasks'),
    newDescribeExportTasks,
    DescribeExportTasksResponse (DescribeExportTasksResponse'),
    newDescribeExportTasksResponse,

    -- ** DescribeTags (Paginated)
    DescribeTags (DescribeTags'),
    newDescribeTags,
    DescribeTagsResponse (DescribeTagsResponse'),
    newDescribeTagsResponse,

    -- ** StartExportTask
    StartExportTask (StartExportTask'),
    newStartExportTask,
    StartExportTaskResponse (StartExportTaskResponse'),
    newStartExportTaskResponse,

    -- ** ListConfigurations (Paginated)
    ListConfigurations (ListConfigurations'),
    newListConfigurations,
    ListConfigurationsResponse (ListConfigurationsResponse'),
    newListConfigurationsResponse,

    -- ** CreateApplication
    CreateApplication (CreateApplication'),
    newCreateApplication,
    CreateApplicationResponse (CreateApplicationResponse'),
    newCreateApplicationResponse,

    -- ** DescribeImportTasks
    DescribeImportTasks (DescribeImportTasks'),
    newDescribeImportTasks,
    DescribeImportTasksResponse (DescribeImportTasksResponse'),
    newDescribeImportTasksResponse,

    -- ** DeleteApplications
    DeleteApplications (DeleteApplications'),
    newDeleteApplications,
    DeleteApplicationsResponse (DeleteApplicationsResponse'),
    newDeleteApplicationsResponse,

    -- ** GetDiscoverySummary
    GetDiscoverySummary (GetDiscoverySummary'),
    newGetDiscoverySummary,
    GetDiscoverySummaryResponse (GetDiscoverySummaryResponse'),
    newGetDiscoverySummaryResponse,

    -- ** DeleteTags
    DeleteTags (DeleteTags'),
    newDeleteTags,
    DeleteTagsResponse (DeleteTagsResponse'),
    newDeleteTagsResponse,

    -- ** DisassociateConfigurationItemsFromApplication
    DisassociateConfigurationItemsFromApplication (DisassociateConfigurationItemsFromApplication'),
    newDisassociateConfigurationItemsFromApplication,
    DisassociateConfigurationItemsFromApplicationResponse (DisassociateConfigurationItemsFromApplicationResponse'),
    newDisassociateConfigurationItemsFromApplicationResponse,

    -- ** StopDataCollectionByAgentIds
    StopDataCollectionByAgentIds (StopDataCollectionByAgentIds'),
    newStopDataCollectionByAgentIds,
    StopDataCollectionByAgentIdsResponse (StopDataCollectionByAgentIdsResponse'),
    newStopDataCollectionByAgentIdsResponse,

    -- ** StartDataCollectionByAgentIds
    StartDataCollectionByAgentIds (StartDataCollectionByAgentIds'),
    newStartDataCollectionByAgentIds,
    StartDataCollectionByAgentIdsResponse (StartDataCollectionByAgentIdsResponse'),
    newStartDataCollectionByAgentIdsResponse,

    -- ** DescribeAgents (Paginated)
    DescribeAgents (DescribeAgents'),
    newDescribeAgents,
    DescribeAgentsResponse (DescribeAgentsResponse'),
    newDescribeAgentsResponse,

    -- ** DescribeContinuousExports (Paginated)
    DescribeContinuousExports (DescribeContinuousExports'),
    newDescribeContinuousExports,
    DescribeContinuousExportsResponse (DescribeContinuousExportsResponse'),
    newDescribeContinuousExportsResponse,

    -- ** StopContinuousExport
    StopContinuousExport (StopContinuousExport'),
    newStopContinuousExport,
    StopContinuousExportResponse (StopContinuousExportResponse'),
    newStopContinuousExportResponse,

    -- ** StartContinuousExport
    StartContinuousExport (StartContinuousExport'),
    newStartContinuousExport,
    StartContinuousExportResponse (StartContinuousExportResponse'),
    newStartContinuousExportResponse,

    -- ** DescribeConfigurations
    DescribeConfigurations (DescribeConfigurations'),
    newDescribeConfigurations,
    DescribeConfigurationsResponse (DescribeConfigurationsResponse'),
    newDescribeConfigurationsResponse,

    -- ** ListServerNeighbors
    ListServerNeighbors (ListServerNeighbors'),
    newListServerNeighbors,
    ListServerNeighborsResponse (ListServerNeighborsResponse'),
    newListServerNeighborsResponse,

    -- ** AssociateConfigurationItemsToApplication
    AssociateConfigurationItemsToApplication (AssociateConfigurationItemsToApplication'),
    newAssociateConfigurationItemsToApplication,
    AssociateConfigurationItemsToApplicationResponse (AssociateConfigurationItemsToApplicationResponse'),
    newAssociateConfigurationItemsToApplicationResponse,

    -- ** UpdateApplication
    UpdateApplication (UpdateApplication'),
    newUpdateApplication,
    UpdateApplicationResponse (UpdateApplicationResponse'),
    newUpdateApplicationResponse,

    -- ** StartImportTask
    StartImportTask (StartImportTask'),
    newStartImportTask,
    StartImportTaskResponse (StartImportTaskResponse'),
    newStartImportTaskResponse,

    -- ** BatchDeleteImportData
    BatchDeleteImportData (BatchDeleteImportData'),
    newBatchDeleteImportData,
    BatchDeleteImportDataResponse (BatchDeleteImportDataResponse'),
    newBatchDeleteImportDataResponse,

    -- ** CreateTags
    CreateTags (CreateTags'),
    newCreateTags,
    CreateTagsResponse (CreateTagsResponse'),
    newCreateTagsResponse,

    -- * Types

    -- ** AgentStatus
    AgentStatus (..),

    -- ** BatchDeleteImportDataErrorCode
    BatchDeleteImportDataErrorCode (..),

    -- ** ConfigurationItemType
    ConfigurationItemType (..),

    -- ** ContinuousExportStatus
    ContinuousExportStatus (..),

    -- ** DataSource
    DataSource (..),

    -- ** ExportDataFormat
    ExportDataFormat (..),

    -- ** ExportStatus
    ExportStatus (..),

    -- ** ImportStatus
    ImportStatus (..),

    -- ** ImportTaskFilterName
    ImportTaskFilterName (..),

    -- ** OrderString
    OrderString (..),

    -- ** AgentConfigurationStatus
    AgentConfigurationStatus (AgentConfigurationStatus'),
    newAgentConfigurationStatus,

    -- ** AgentInfo
    AgentInfo (AgentInfo'),
    newAgentInfo,

    -- ** AgentNetworkInfo
    AgentNetworkInfo (AgentNetworkInfo'),
    newAgentNetworkInfo,

    -- ** BatchDeleteImportDataError
    BatchDeleteImportDataError (BatchDeleteImportDataError'),
    newBatchDeleteImportDataError,

    -- ** ConfigurationTag
    ConfigurationTag (ConfigurationTag'),
    newConfigurationTag,

    -- ** ContinuousExportDescription
    ContinuousExportDescription (ContinuousExportDescription'),
    newContinuousExportDescription,

    -- ** CustomerAgentInfo
    CustomerAgentInfo (CustomerAgentInfo'),
    newCustomerAgentInfo,

    -- ** CustomerConnectorInfo
    CustomerConnectorInfo (CustomerConnectorInfo'),
    newCustomerConnectorInfo,

    -- ** ExportFilter
    ExportFilter (ExportFilter'),
    newExportFilter,

    -- ** ExportInfo
    ExportInfo (ExportInfo'),
    newExportInfo,

    -- ** Filter
    Filter (Filter'),
    newFilter,

    -- ** ImportTask
    ImportTask (ImportTask'),
    newImportTask,

    -- ** ImportTaskFilter
    ImportTaskFilter (ImportTaskFilter'),
    newImportTaskFilter,

    -- ** NeighborConnectionDetail
    NeighborConnectionDetail (NeighborConnectionDetail'),
    newNeighborConnectionDetail,

    -- ** OrderByElement
    OrderByElement (OrderByElement'),
    newOrderByElement,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TagFilter
    TagFilter (TagFilter'),
    newTagFilter,
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
import Network.AWS.Discovery.Lens
import Network.AWS.Discovery.ListConfigurations
import Network.AWS.Discovery.ListServerNeighbors
import Network.AWS.Discovery.StartContinuousExport
import Network.AWS.Discovery.StartDataCollectionByAgentIds
import Network.AWS.Discovery.StartExportTask
import Network.AWS.Discovery.StartImportTask
import Network.AWS.Discovery.StopContinuousExport
import Network.AWS.Discovery.StopDataCollectionByAgentIds
import Network.AWS.Discovery.Types
import Network.AWS.Discovery.UpdateApplication
import Network.AWS.Discovery.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Discovery'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.

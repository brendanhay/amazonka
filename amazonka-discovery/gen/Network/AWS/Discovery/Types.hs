{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ConflictErrorException,
    _OperationNotPermittedException,
    _ServerInternalErrorException,
    _HomeRegionNotSetException,
    _InvalidParameterException,
    _InvalidParameterValueException,
    _AuthorizationErrorException,
    _ResourceInUseException,
    _ResourceNotFoundException,

    -- * AgentStatus
    AgentStatus (..),

    -- * BatchDeleteImportDataErrorCode
    BatchDeleteImportDataErrorCode (..),

    -- * ConfigurationItemType
    ConfigurationItemType (..),

    -- * ContinuousExportStatus
    ContinuousExportStatus (..),

    -- * DataSource
    DataSource (..),

    -- * ExportDataFormat
    ExportDataFormat (..),

    -- * ExportStatus
    ExportStatus (..),

    -- * ImportStatus
    ImportStatus (..),

    -- * ImportTaskFilterName
    ImportTaskFilterName (..),

    -- * OrderString
    OrderString (..),

    -- * AgentConfigurationStatus
    AgentConfigurationStatus (..),
    newAgentConfigurationStatus,
    agentConfigurationStatus_agentId,
    agentConfigurationStatus_operationSucceeded,
    agentConfigurationStatus_description,

    -- * AgentInfo
    AgentInfo (..),
    newAgentInfo,
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

    -- * AgentNetworkInfo
    AgentNetworkInfo (..),
    newAgentNetworkInfo,
    agentNetworkInfo_macAddress,
    agentNetworkInfo_ipAddress,

    -- * BatchDeleteImportDataError
    BatchDeleteImportDataError (..),
    newBatchDeleteImportDataError,
    batchDeleteImportDataError_errorDescription,
    batchDeleteImportDataError_importTaskId,
    batchDeleteImportDataError_errorCode,

    -- * ConfigurationTag
    ConfigurationTag (..),
    newConfigurationTag,
    configurationTag_key,
    configurationTag_configurationId,
    configurationTag_value,
    configurationTag_configurationType,
    configurationTag_timeOfCreation,

    -- * ContinuousExportDescription
    ContinuousExportDescription (..),
    newContinuousExportDescription,
    continuousExportDescription_status,
    continuousExportDescription_s3Bucket,
    continuousExportDescription_dataSource,
    continuousExportDescription_startTime,
    continuousExportDescription_statusDetail,
    continuousExportDescription_stopTime,
    continuousExportDescription_schemaStorageConfig,
    continuousExportDescription_exportId,

    -- * CustomerAgentInfo
    CustomerAgentInfo (..),
    newCustomerAgentInfo,
    customerAgentInfo_activeAgents,
    customerAgentInfo_healthyAgents,
    customerAgentInfo_blackListedAgents,
    customerAgentInfo_shutdownAgents,
    customerAgentInfo_unhealthyAgents,
    customerAgentInfo_totalAgents,
    customerAgentInfo_unknownAgents,

    -- * CustomerConnectorInfo
    CustomerConnectorInfo (..),
    newCustomerConnectorInfo,
    customerConnectorInfo_activeConnectors,
    customerConnectorInfo_healthyConnectors,
    customerConnectorInfo_blackListedConnectors,
    customerConnectorInfo_shutdownConnectors,
    customerConnectorInfo_unhealthyConnectors,
    customerConnectorInfo_totalConnectors,
    customerConnectorInfo_unknownConnectors,

    -- * ExportFilter
    ExportFilter (..),
    newExportFilter,
    exportFilter_name,
    exportFilter_values,
    exportFilter_condition,

    -- * ExportInfo
    ExportInfo (..),
    newExportInfo,
    exportInfo_isTruncated,
    exportInfo_configurationsDownloadUrl,
    exportInfo_requestedStartTime,
    exportInfo_requestedEndTime,
    exportInfo_exportId,
    exportInfo_exportStatus,
    exportInfo_statusMessage,
    exportInfo_exportRequestTime,

    -- * Filter
    Filter (..),
    newFilter,
    filter_name,
    filter_values,
    filter_condition,

    -- * ImportTask
    ImportTask (..),
    newImportTask,
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

    -- * ImportTaskFilter
    ImportTaskFilter (..),
    newImportTaskFilter,
    importTaskFilter_values,
    importTaskFilter_name,

    -- * NeighborConnectionDetail
    NeighborConnectionDetail (..),
    newNeighborConnectionDetail,
    neighborConnectionDetail_transportProtocol,
    neighborConnectionDetail_destinationPort,
    neighborConnectionDetail_sourceServerId,
    neighborConnectionDetail_destinationServerId,
    neighborConnectionDetail_connectionsCount,

    -- * OrderByElement
    OrderByElement (..),
    newOrderByElement,
    orderByElement_sortOrder,
    orderByElement_fieldName,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TagFilter
    TagFilter (..),
    newTagFilter,
    tagFilter_name,
    tagFilter_values,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Discovery.Types.AgentConfigurationStatus
import Network.AWS.Discovery.Types.AgentInfo
import Network.AWS.Discovery.Types.AgentNetworkInfo
import Network.AWS.Discovery.Types.AgentStatus
import Network.AWS.Discovery.Types.BatchDeleteImportDataError
import Network.AWS.Discovery.Types.BatchDeleteImportDataErrorCode
import Network.AWS.Discovery.Types.ConfigurationItemType
import Network.AWS.Discovery.Types.ConfigurationTag
import Network.AWS.Discovery.Types.ContinuousExportDescription
import Network.AWS.Discovery.Types.ContinuousExportStatus
import Network.AWS.Discovery.Types.CustomerAgentInfo
import Network.AWS.Discovery.Types.CustomerConnectorInfo
import Network.AWS.Discovery.Types.DataSource
import Network.AWS.Discovery.Types.ExportDataFormat
import Network.AWS.Discovery.Types.ExportFilter
import Network.AWS.Discovery.Types.ExportInfo
import Network.AWS.Discovery.Types.ExportStatus
import Network.AWS.Discovery.Types.Filter
import Network.AWS.Discovery.Types.ImportStatus
import Network.AWS.Discovery.Types.ImportTask
import Network.AWS.Discovery.Types.ImportTaskFilter
import Network.AWS.Discovery.Types.ImportTaskFilterName
import Network.AWS.Discovery.Types.NeighborConnectionDetail
import Network.AWS.Discovery.Types.OrderByElement
import Network.AWS.Discovery.Types.OrderString
import Network.AWS.Discovery.Types.Tag
import Network.AWS.Discovery.Types.TagFilter
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-11-01@ of the Amazon Application Discovery Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "Discovery",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "discovery",
      Core._serviceSigningName = "discovery",
      Core._serviceVersion = "2015-11-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "Discovery",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- |
_ConflictErrorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictErrorException =
  Core._MatchServiceError
    defaultService
    "ConflictErrorException"

-- | This operation is not permitted.
_OperationNotPermittedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OperationNotPermittedException =
  Core._MatchServiceError
    defaultService
    "OperationNotPermittedException"

-- | The server experienced an internal error. Try again.
_ServerInternalErrorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServerInternalErrorException =
  Core._MatchServiceError
    defaultService
    "ServerInternalErrorException"

-- | The home region is not set. Set the home region to continue.
_HomeRegionNotSetException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_HomeRegionNotSetException =
  Core._MatchServiceError
    defaultService
    "HomeRegionNotSetException"

-- | One or more parameters are not valid. Verify the parameters and try
-- again.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"

-- | The value of one or more parameters are either invalid or out of range.
-- Verify the parameter values and try again.
_InvalidParameterValueException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterValueException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterValueException"

-- | The AWS user account does not have permission to perform the action.
-- Check the IAM policy associated with this account.
_AuthorizationErrorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AuthorizationErrorException =
  Core._MatchServiceError
    defaultService
    "AuthorizationErrorException"

-- | This issue occurs when the same @clientRequestToken@ is used with the
-- @StartImportTask@ action, but with different parameters. For example,
-- you use the same request token but have two different import URLs, you
-- can encounter this issue. If the import tasks are meant to be different,
-- use a different @clientRequestToken@, and try again.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"

-- | The specified configuration ID was not located. Verify the configuration
-- ID and try again.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

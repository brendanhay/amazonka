-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _AuthorizationErrorException,
    _HomeRegionNotSetException,
    _InvalidParameterException,
    _ConflictErrorException,
    _InvalidParameterValueException,
    _ServerInternalErrorException,
    _OperationNotPermittedException,
    _ResourceNotFoundException,
    _ResourceInUseException,

    -- * ConfigurationsDownloadUrl
    ConfigurationsDownloadUrl (..),

    -- * AgentInfo
    AgentInfo (..),
    mkAgentInfo,
    aiAgentId,
    aiAgentNetworkInfoList,
    aiAgentType,
    aiCollectionStatus,
    aiConnectorId,
    aiHealth,
    aiHostName,
    aiLastHealthPingTime,
    aiRegisteredTime,
    aiVersion,

    -- * FilterName
    FilterName (..),

    -- * ImportTaskFilterName
    ImportTaskFilterName (..),

    -- * ExportDataFormat
    ExportDataFormat (..),

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * ExportInfo
    ExportInfo (..),
    mkExportInfo,
    eiExportId,
    eiExportStatus,
    eiStatusMessage,
    eiExportRequestTime,
    eiConfigurationsDownloadUrl,
    eiIsTruncated,
    eiRequestedEndTime,
    eiRequestedStartTime,

    -- * AgentConfigurationStatus
    AgentConfigurationStatus (..),
    mkAgentConfigurationStatus,
    acsAgentId,
    acsDescription,
    acsOperationSucceeded,

    -- * String
    String (..),

    -- * ConfigurationId
    ConfigurationId (..),

    -- * AgentStatus
    AgentStatus (..),

    -- * ContinuousExportStatus
    ContinuousExportStatus (..),

    -- * ExportStatus
    ExportStatus (..),

    -- * BatchDeleteImportDataError
    BatchDeleteImportDataError (..),
    mkBatchDeleteImportDataError,
    bdideErrorCode,
    bdideErrorDescription,
    bdideImportTaskId,

    -- * ImportTask
    ImportTask (..),
    mkImportTask,
    itApplicationImportFailure,
    itApplicationImportSuccess,
    itClientRequestToken,
    itErrorsAndFailedEntriesZip,
    itImportCompletionTime,
    itImportDeletedTime,
    itImportRequestTime,
    itImportTaskId,
    itImportUrl,
    itName,
    itServerImportFailure,
    itServerImportSuccess,
    itStatus,

    -- * ImportTaskIdentifier
    ImportTaskIdentifier (..),

    -- * StringMax255
    StringMax255 (..),

    -- * OrderString
    OrderString (..),

    -- * TagValue
    TagValue (..),

    -- * AgentNetworkInfo
    AgentNetworkInfo (..),
    mkAgentNetworkInfo,
    aniIpAddress,
    aniMacAddress,

    -- * NextToken
    NextToken (..),

    -- * AgentId
    AgentId (..),

    -- * ExportFilter
    ExportFilter (..),
    mkExportFilter,
    efName,
    efValues,
    efCondition,

    -- * ImportTaskName
    ImportTaskName (..),

    -- * ApplicationId
    ApplicationId (..),

    -- * ContinuousExportDescription
    ContinuousExportDescription (..),
    mkContinuousExportDescription,
    cedDataSource,
    cedExportId,
    cedS3Bucket,
    cedSchemaStorageConfig,
    cedStartTime,
    cedStatus,
    cedStatusDetail,
    cedStopTime,

    -- * ConfigurationsExportId
    ConfigurationsExportId (..),

    -- * TagFilter
    TagFilter (..),
    mkTagFilter,
    tfName,
    tfValues,

    -- * ConfigurationTag
    ConfigurationTag (..),
    mkConfigurationTag,
    ctConfigurationId,
    ctConfigurationType,
    ctKey,
    ctTimeOfCreation,
    ctValue,

    -- * DatabaseName
    DatabaseName (..),

    -- * ConfigurationItemType
    ConfigurationItemType (..),

    -- * NeighborConnectionDetail
    NeighborConnectionDetail (..),
    mkNeighborConnectionDetail,
    ncdSourceServerId,
    ncdDestinationServerId,
    ncdConnectionsCount,
    ncdDestinationPort,
    ncdTransportProtocol,

    -- * DataSource
    DataSource (..),

    -- * BatchDeleteImportDataErrorCode
    BatchDeleteImportDataErrorCode (..),

    -- * Filter
    Filter (..),
    mkFilter,
    fName,
    fValues,
    fCondition,

    -- * ImportTaskFilter
    ImportTaskFilter (..),
    mkImportTaskFilter,
    itfName,
    itfValues,

    -- * Condition
    Condition (..),

    -- * FilterValue
    FilterValue (..),

    -- * CustomerAgentInfo
    CustomerAgentInfo (..),
    mkCustomerAgentInfo,
    caiActiveAgents,
    caiHealthyAgents,
    caiBlackListedAgents,
    caiShutdownAgents,
    caiUnhealthyAgents,
    caiTotalAgents,
    caiUnknownAgents,

    -- * ImportTaskFilterValue
    ImportTaskFilterValue (..),

    -- * ClientRequestToken
    ClientRequestToken (..),

    -- * OrderByElement
    OrderByElement (..),
    mkOrderByElement,
    obeFieldName,
    obeSortOrder,

    -- * ImportURL
    ImportURL (..),

    -- * S3Bucket
    S3Bucket (..),

    -- * CustomerConnectorInfo
    CustomerConnectorInfo (..),
    mkCustomerConnectorInfo,
    cciActiveConnectors,
    cciHealthyConnectors,
    cciBlackListedConnectors,
    cciShutdownConnectors,
    cciUnhealthyConnectors,
    cciTotalConnectors,
    cciUnknownConnectors,

    -- * ImportStatus
    ImportStatus (..),

    -- * ExportId
    ExportId (..),

    -- * Name
    Name (..),

    -- * Description
    Description (..),

    -- * AgentType
    AgentType (..),

    -- * CollectionStatus
    CollectionStatus (..),

    -- * ConnectorId
    ConnectorId (..),

    -- * HostName
    HostName (..),

    -- * LastHealthPingTime
    LastHealthPingTime (..),

    -- * RegisteredTime
    RegisteredTime (..),

    -- * Version
    Version (..),

    -- * Key
    Key (..),

    -- * Value
    Value (..),

    -- * StatusMessage
    StatusMessage (..),

    -- * ErrorDescription
    ErrorDescription (..),

    -- * ImportTaskId
    ImportTaskId (..),

    -- * ErrorsAndFailedEntriesZip
    ErrorsAndFailedEntriesZip (..),

    -- * ImportUrl
    ImportUrl (..),

    -- * ApplicationConfigurationId
    ApplicationConfigurationId (..),
  )
where

import Network.AWS.Discovery.Types.AgentConfigurationStatus
import Network.AWS.Discovery.Types.AgentId
import Network.AWS.Discovery.Types.AgentInfo
import Network.AWS.Discovery.Types.AgentNetworkInfo
import Network.AWS.Discovery.Types.AgentStatus
import Network.AWS.Discovery.Types.AgentType
import Network.AWS.Discovery.Types.ApplicationConfigurationId
import Network.AWS.Discovery.Types.ApplicationId
import Network.AWS.Discovery.Types.BatchDeleteImportDataError
import Network.AWS.Discovery.Types.BatchDeleteImportDataErrorCode
import Network.AWS.Discovery.Types.ClientRequestToken
import Network.AWS.Discovery.Types.CollectionStatus
import Network.AWS.Discovery.Types.Condition
import Network.AWS.Discovery.Types.ConfigurationId
import Network.AWS.Discovery.Types.ConfigurationItemType
import Network.AWS.Discovery.Types.ConfigurationTag
import Network.AWS.Discovery.Types.ConfigurationsDownloadUrl
import Network.AWS.Discovery.Types.ConfigurationsExportId
import Network.AWS.Discovery.Types.ConnectorId
import Network.AWS.Discovery.Types.ContinuousExportDescription
import Network.AWS.Discovery.Types.ContinuousExportStatus
import Network.AWS.Discovery.Types.CustomerAgentInfo
import Network.AWS.Discovery.Types.CustomerConnectorInfo
import Network.AWS.Discovery.Types.DataSource
import Network.AWS.Discovery.Types.DatabaseName
import Network.AWS.Discovery.Types.Description
import Network.AWS.Discovery.Types.ErrorDescription
import Network.AWS.Discovery.Types.ErrorsAndFailedEntriesZip
import Network.AWS.Discovery.Types.ExportDataFormat
import Network.AWS.Discovery.Types.ExportFilter
import Network.AWS.Discovery.Types.ExportId
import Network.AWS.Discovery.Types.ExportInfo
import Network.AWS.Discovery.Types.ExportStatus
import Network.AWS.Discovery.Types.Filter
import Network.AWS.Discovery.Types.FilterName
import Network.AWS.Discovery.Types.FilterValue
import Network.AWS.Discovery.Types.HostName
import Network.AWS.Discovery.Types.ImportStatus
import Network.AWS.Discovery.Types.ImportTask
import Network.AWS.Discovery.Types.ImportTaskFilter
import Network.AWS.Discovery.Types.ImportTaskFilterName
import Network.AWS.Discovery.Types.ImportTaskFilterValue
import Network.AWS.Discovery.Types.ImportTaskId
import Network.AWS.Discovery.Types.ImportTaskIdentifier
import Network.AWS.Discovery.Types.ImportTaskName
import Network.AWS.Discovery.Types.ImportURL
import Network.AWS.Discovery.Types.ImportUrl
import Network.AWS.Discovery.Types.Key
import Network.AWS.Discovery.Types.LastHealthPingTime
import Network.AWS.Discovery.Types.Name
import Network.AWS.Discovery.Types.NeighborConnectionDetail
import Network.AWS.Discovery.Types.NextToken
import Network.AWS.Discovery.Types.OrderByElement
import Network.AWS.Discovery.Types.OrderString
import Network.AWS.Discovery.Types.RegisteredTime
import Network.AWS.Discovery.Types.S3Bucket
import Network.AWS.Discovery.Types.StatusMessage
import Network.AWS.Discovery.Types.String
import Network.AWS.Discovery.Types.StringMax255
import Network.AWS.Discovery.Types.Tag
import Network.AWS.Discovery.Types.TagFilter
import Network.AWS.Discovery.Types.TagValue
import Network.AWS.Discovery.Types.Value
import Network.AWS.Discovery.Types.Version
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-11-01@ of the Amazon Application Discovery Service SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "Discovery",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "discovery",
      Core._svcVersion = "2015-11-01",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "Discovery",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
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
      | Lens.has
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | The AWS user account does not have permission to perform the action. Check the IAM policy associated with this account.
_AuthorizationErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AuthorizationErrorException =
  Core._MatchServiceError
    mkServiceConfig
    "AuthorizationErrorException"
{-# DEPRECATED _AuthorizationErrorException "Use generic-lens or generic-optics instead." #-}

-- | The home region is not set. Set the home region to continue.
_HomeRegionNotSetException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_HomeRegionNotSetException =
  Core._MatchServiceError
    mkServiceConfig
    "HomeRegionNotSetException"
{-# DEPRECATED _HomeRegionNotSetException "Use generic-lens or generic-optics instead." #-}

-- | One or more parameters are not valid. Verify the parameters and try again.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidParameterException"
{-# DEPRECATED _InvalidParameterException "Use generic-lens or generic-optics instead." #-}

-- |
_ConflictErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConflictErrorException =
  Core._MatchServiceError mkServiceConfig "ConflictErrorException"
{-# DEPRECATED _ConflictErrorException "Use generic-lens or generic-optics instead." #-}

-- | The value of one or more parameters are either invalid or out of range. Verify the parameter values and try again.
_InvalidParameterValueException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterValueException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidParameterValueException"
{-# DEPRECATED _InvalidParameterValueException "Use generic-lens or generic-optics instead." #-}

-- | The server experienced an internal error. Try again.
_ServerInternalErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServerInternalErrorException =
  Core._MatchServiceError
    mkServiceConfig
    "ServerInternalErrorException"
{-# DEPRECATED _ServerInternalErrorException "Use generic-lens or generic-optics instead." #-}

-- | This operation is not permitted.
_OperationNotPermittedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OperationNotPermittedException =
  Core._MatchServiceError
    mkServiceConfig
    "OperationNotPermittedException"
{-# DEPRECATED _OperationNotPermittedException "Use generic-lens or generic-optics instead." #-}

-- | The specified configuration ID was not located. Verify the configuration ID and try again.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "ResourceNotFoundException"
{-# DEPRECATED _ResourceNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | This issue occurs when the same @clientRequestToken@ is used with the @StartImportTask@ action, but with different parameters. For example, you use the same request token but have two different import URLs, you can encounter this issue. If the import tasks are meant to be different, use a different @clientRequestToken@ , and try again.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError mkServiceConfig "ResourceInUseException"
{-# DEPRECATED _ResourceInUseException "Use generic-lens or generic-optics instead." #-}

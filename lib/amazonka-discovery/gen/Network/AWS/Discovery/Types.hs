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
    discoveryService,

    -- * Errors

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
    mkAgentConfigurationStatus,
    acsAgentId,
    acsOperationSucceeded,
    acsDescription,

    -- * AgentInfo
    AgentInfo (..),
    mkAgentInfo,
    aiHostName,
    aiLastHealthPingTime,
    aiAgentNetworkInfoList,
    aiConnectorId,
    aiHealth,
    aiAgentId,
    aiVersion,
    aiCollectionStatus,
    aiRegisteredTime,
    aiAgentType,

    -- * AgentNetworkInfo
    AgentNetworkInfo (..),
    mkAgentNetworkInfo,
    aniIpAddress,
    aniMacAddress,

    -- * BatchDeleteImportDataError
    BatchDeleteImportDataError (..),
    mkBatchDeleteImportDataError,
    bdideImportTaskId,
    bdideErrorCode,
    bdideErrorDescription,

    -- * ConfigurationTag
    ConfigurationTag (..),
    mkConfigurationTag,
    ctTimeOfCreation,
    ctConfigurationId,
    ctConfigurationType,
    ctValue,
    ctKey,

    -- * ContinuousExportDescription
    ContinuousExportDescription (..),
    mkContinuousExportDescription,
    cedStatus,
    cedStartTime,
    cedSchemaStorageConfig,
    cedStatusDetail,
    cedStopTime,
    cedDataSource,
    cedS3Bucket,
    cedExportId,

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

    -- * ExportFilter
    ExportFilter (..),
    mkExportFilter,
    efName,
    efValues,
    efCondition,

    -- * ExportInfo
    ExportInfo (..),
    mkExportInfo,
    eiConfigurationsDownloadURL,
    eiRequestedStartTime,
    eiRequestedEndTime,
    eiIsTruncated,
    eiExportId,
    eiExportStatus,
    eiStatusMessage,
    eiExportRequestTime,

    -- * Filter
    Filter (..),
    mkFilter,
    fName,
    fValues,
    fCondition,

    -- * ImportTask
    ImportTask (..),
    mkImportTask,
    itApplicationImportSuccess,
    itStatus,
    itServerImportSuccess,
    itImportCompletionTime,
    itName,
    itApplicationImportFailure,
    itErrorsAndFailedEntriesZip,
    itImportTaskId,
    itImportDeletedTime,
    itServerImportFailure,
    itClientRequestToken,
    itImportURL,
    itImportRequestTime,

    -- * ImportTaskFilter
    ImportTaskFilter (..),
    mkImportTaskFilter,
    itfValues,
    itfName,

    -- * NeighborConnectionDetail
    NeighborConnectionDetail (..),
    mkNeighborConnectionDetail,
    ncdTransportProtocol,
    ncdDestinationPort,
    ncdSourceServerId,
    ncdDestinationServerId,
    ncdConnectionsCount,

    -- * OrderByElement
    OrderByElement (..),
    mkOrderByElement,
    obeSortOrder,
    obeFieldName,

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * TagFilter
    TagFilter (..),
    mkTagFilter,
    tfName,
    tfValues,
  )
where

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
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-11-01@ of the Amazon Application Discovery Service SDK configuration.
discoveryService :: Lude.Service
discoveryService =
  Lude.Service
    { Lude._svcAbbrev = "Discovery",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "discovery",
      Lude._svcVersion = "2015-11-01",
      Lude._svcEndpoint = Lude.defaultEndpoint discoveryService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "Discovery",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing

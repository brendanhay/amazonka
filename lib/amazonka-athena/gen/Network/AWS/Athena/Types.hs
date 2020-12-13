-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types
  ( -- * Service configuration
    athenaService,

    -- * Errors

    -- * ColumnNullable
    ColumnNullable (..),

    -- * DataCatalogType
    DataCatalogType (..),

    -- * EncryptionOption
    EncryptionOption (..),

    -- * QueryExecutionState
    QueryExecutionState (..),

    -- * StatementType
    StatementType (..),

    -- * WorkGroupState
    WorkGroupState (..),

    -- * Column
    Column (..),
    mkColumn,
    cName,
    cType,
    cComment,

    -- * ColumnInfo
    ColumnInfo (..),
    mkColumnInfo,
    ciScale,
    ciPrecision,
    ciSchemaName,
    ciCatalogName,
    ciName,
    ciType,
    ciCaseSensitive,
    ciLabel,
    ciTableName,
    ciNullable,

    -- * DataCatalog
    DataCatalog (..),
    mkDataCatalog,
    dcName,
    dcParameters,
    dcType,
    dcDescription,

    -- * DataCatalogSummary
    DataCatalogSummary (..),
    mkDataCatalogSummary,
    dcsCatalogName,
    dcsType,

    -- * Database
    Database (..),
    mkDatabase,
    dName,
    dParameters,
    dDescription,

    -- * Datum
    Datum (..),
    mkDatum,
    dVarCharValue,

    -- * EncryptionConfiguration
    EncryptionConfiguration (..),
    mkEncryptionConfiguration,
    ecEncryptionOption,
    ecKMSKey,

    -- * NamedQuery
    NamedQuery (..),
    mkNamedQuery,
    nqNamedQueryId,
    nqDatabase,
    nqName,
    nqQueryString,
    nqDescription,
    nqWorkGroup,

    -- * QueryExecution
    QueryExecution (..),
    mkQueryExecution,
    qeStatus,
    qeQueryExecutionContext,
    qeResultConfiguration,
    qeQuery,
    qeStatementType,
    qeStatistics,
    qeQueryExecutionId,
    qeWorkGroup,

    -- * QueryExecutionContext
    QueryExecutionContext (..),
    mkQueryExecutionContext,
    qecDatabase,
    qecCatalog,

    -- * QueryExecutionStatistics
    QueryExecutionStatistics (..),
    mkQueryExecutionStatistics,
    qesTotalExecutionTimeInMillis,
    qesEngineExecutionTimeInMillis,
    qesQueryPlanningTimeInMillis,
    qesDataScannedInBytes,
    qesQueryQueueTimeInMillis,
    qesDataManifestLocation,
    qesServiceProcessingTimeInMillis,

    -- * QueryExecutionStatus
    QueryExecutionStatus (..),
    mkQueryExecutionStatus,
    qesState,
    qesStateChangeReason,
    qesSubmissionDateTime,
    qesCompletionDateTime,

    -- * ResultConfiguration
    ResultConfiguration (..),
    mkResultConfiguration,
    rcEncryptionConfiguration,
    rcOutputLocation,

    -- * ResultConfigurationUpdates
    ResultConfigurationUpdates (..),
    mkResultConfigurationUpdates,
    rcuRemoveOutputLocation,
    rcuRemoveEncryptionConfiguration,
    rcuEncryptionConfiguration,
    rcuOutputLocation,

    -- * ResultSet
    ResultSet (..),
    mkResultSet,
    rsRows,
    rsResultSetMetadata,

    -- * ResultSetMetadata
    ResultSetMetadata (..),
    mkResultSetMetadata,
    rsmColumnInfo,

    -- * Row
    Row (..),
    mkRow,
    rData,

    -- * TableMetadata
    TableMetadata (..),
    mkTableMetadata,
    tmTableType,
    tmName,
    tmParameters,
    tmColumns,
    tmLastAccessTime,
    tmPartitionKeys,
    tmCreateTime,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- * UnprocessedNamedQueryId
    UnprocessedNamedQueryId (..),
    mkUnprocessedNamedQueryId,
    unqiNamedQueryId,
    unqiErrorCode,
    unqiErrorMessage,

    -- * UnprocessedQueryExecutionId
    UnprocessedQueryExecutionId (..),
    mkUnprocessedQueryExecutionId,
    uqeiErrorCode,
    uqeiQueryExecutionId,
    uqeiErrorMessage,

    -- * WorkGroup
    WorkGroup (..),
    mkWorkGroup,
    wgCreationTime,
    wgState,
    wgName,
    wgConfiguration,
    wgDescription,

    -- * WorkGroupConfiguration
    WorkGroupConfiguration (..),
    mkWorkGroupConfiguration,
    wgcRequesterPaysEnabled,
    wgcResultConfiguration,
    wgcBytesScannedCutoffPerQuery,
    wgcEnforceWorkGroupConfiguration,
    wgcPublishCloudWatchMetricsEnabled,

    -- * WorkGroupConfigurationUpdates
    WorkGroupConfigurationUpdates (..),
    mkWorkGroupConfigurationUpdates,
    wgcuRequesterPaysEnabled,
    wgcuResultConfigurationUpdates,
    wgcuBytesScannedCutoffPerQuery,
    wgcuRemoveBytesScannedCutoffPerQuery,
    wgcuEnforceWorkGroupConfiguration,
    wgcuPublishCloudWatchMetricsEnabled,

    -- * WorkGroupSummary
    WorkGroupSummary (..),
    mkWorkGroupSummary,
    wgsCreationTime,
    wgsState,
    wgsName,
    wgsDescription,
  )
where

import Network.AWS.Athena.Types.Column
import Network.AWS.Athena.Types.ColumnInfo
import Network.AWS.Athena.Types.ColumnNullable
import Network.AWS.Athena.Types.DataCatalog
import Network.AWS.Athena.Types.DataCatalogSummary
import Network.AWS.Athena.Types.DataCatalogType
import Network.AWS.Athena.Types.Database
import Network.AWS.Athena.Types.Datum
import Network.AWS.Athena.Types.EncryptionConfiguration
import Network.AWS.Athena.Types.EncryptionOption
import Network.AWS.Athena.Types.NamedQuery
import Network.AWS.Athena.Types.QueryExecution
import Network.AWS.Athena.Types.QueryExecutionContext
import Network.AWS.Athena.Types.QueryExecutionState
import Network.AWS.Athena.Types.QueryExecutionStatistics
import Network.AWS.Athena.Types.QueryExecutionStatus
import Network.AWS.Athena.Types.ResultConfiguration
import Network.AWS.Athena.Types.ResultConfigurationUpdates
import Network.AWS.Athena.Types.ResultSet
import Network.AWS.Athena.Types.ResultSetMetadata
import Network.AWS.Athena.Types.Row
import Network.AWS.Athena.Types.StatementType
import Network.AWS.Athena.Types.TableMetadata
import Network.AWS.Athena.Types.Tag
import Network.AWS.Athena.Types.UnprocessedNamedQueryId
import Network.AWS.Athena.Types.UnprocessedQueryExecutionId
import Network.AWS.Athena.Types.WorkGroup
import Network.AWS.Athena.Types.WorkGroupConfiguration
import Network.AWS.Athena.Types.WorkGroupConfigurationUpdates
import Network.AWS.Athena.Types.WorkGroupState
import Network.AWS.Athena.Types.WorkGroupSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-05-18@ of the Amazon Athena SDK configuration.
athenaService :: Lude.Service
athenaService =
  Lude.Service
    { Lude._svcAbbrev = "Athena",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "athena",
      Lude._svcVersion = "2017-05-18",
      Lude._svcEndpoint = Lude.defaultEndpoint athenaService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "Athena",
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

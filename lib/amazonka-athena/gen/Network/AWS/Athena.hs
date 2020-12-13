{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Athena is an interactive query service that lets you use standard SQL to analyze data directly in Amazon S3. You can point Athena at your data in Amazon S3 and run ad-hoc queries and get results in seconds. Athena is serverless, so there is no infrastructure to set up or manage. You pay only for the queries you run. Athena scales automatically—executing queries in parallel—so results are fast, even with large datasets and complex queries. For more information, see <http://docs.aws.amazon.com/athena/latest/ug/what-is.html What is Amazon Athena> in the /Amazon Athena User Guide/ .
--
-- If you connect to Athena using the JDBC driver, use version 1.1.0 of the driver or later with the Amazon Athena API. Earlier version drivers do not support the API. For more information and to download the driver, see <https://docs.aws.amazon.com/athena/latest/ug/connect-with-jdbc.html Accessing Amazon Athena with JDBC> .
-- For code samples using the AWS SDK for Java, see <https://docs.aws.amazon.com/athena/latest/ug/code-samples.html Examples and Code Samples> in the /Amazon Athena User Guide/ .
module Network.AWS.Athena
  ( -- * Service configuration
    athenaService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListDatabases (Paginated)
    module Network.AWS.Athena.ListDatabases,

    -- ** DeleteWorkGroup
    module Network.AWS.Athena.DeleteWorkGroup,

    -- ** UpdateWorkGroup
    module Network.AWS.Athena.UpdateWorkGroup,

    -- ** GetNamedQuery
    module Network.AWS.Athena.GetNamedQuery,

    -- ** ListTagsForResource (Paginated)
    module Network.AWS.Athena.ListTagsForResource,

    -- ** DeleteDataCatalog
    module Network.AWS.Athena.DeleteDataCatalog,

    -- ** UpdateDataCatalog
    module Network.AWS.Athena.UpdateDataCatalog,

    -- ** ListDataCatalogs (Paginated)
    module Network.AWS.Athena.ListDataCatalogs,

    -- ** CreateNamedQuery
    module Network.AWS.Athena.CreateNamedQuery,

    -- ** GetTableMetadata
    module Network.AWS.Athena.GetTableMetadata,

    -- ** ListNamedQueries (Paginated)
    module Network.AWS.Athena.ListNamedQueries,

    -- ** DeleteNamedQuery
    module Network.AWS.Athena.DeleteNamedQuery,

    -- ** StartQueryExecution
    module Network.AWS.Athena.StartQueryExecution,

    -- ** BatchGetNamedQuery
    module Network.AWS.Athena.BatchGetNamedQuery,

    -- ** GetQueryExecution
    module Network.AWS.Athena.GetQueryExecution,

    -- ** CreateDataCatalog
    module Network.AWS.Athena.CreateDataCatalog,

    -- ** ListWorkGroups
    module Network.AWS.Athena.ListWorkGroups,

    -- ** CreateWorkGroup
    module Network.AWS.Athena.CreateWorkGroup,

    -- ** BatchGetQueryExecution
    module Network.AWS.Athena.BatchGetQueryExecution,

    -- ** GetDataCatalog
    module Network.AWS.Athena.GetDataCatalog,

    -- ** StopQueryExecution
    module Network.AWS.Athena.StopQueryExecution,

    -- ** TagResource
    module Network.AWS.Athena.TagResource,

    -- ** GetWorkGroup
    module Network.AWS.Athena.GetWorkGroup,

    -- ** GetDatabase
    module Network.AWS.Athena.GetDatabase,

    -- ** UntagResource
    module Network.AWS.Athena.UntagResource,

    -- ** GetQueryResults (Paginated)
    module Network.AWS.Athena.GetQueryResults,

    -- ** ListTableMetadata (Paginated)
    module Network.AWS.Athena.ListTableMetadata,

    -- ** ListQueryExecutions (Paginated)
    module Network.AWS.Athena.ListQueryExecutions,

    -- * Types

    -- ** ColumnNullable
    ColumnNullable (..),

    -- ** DataCatalogType
    DataCatalogType (..),

    -- ** EncryptionOption
    EncryptionOption (..),

    -- ** QueryExecutionState
    QueryExecutionState (..),

    -- ** StatementType
    StatementType (..),

    -- ** WorkGroupState
    WorkGroupState (..),

    -- ** Column
    Column (..),
    mkColumn,
    cName,
    cType,
    cComment,

    -- ** ColumnInfo
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

    -- ** DataCatalog
    DataCatalog (..),
    mkDataCatalog,
    dcName,
    dcParameters,
    dcType,
    dcDescription,

    -- ** DataCatalogSummary
    DataCatalogSummary (..),
    mkDataCatalogSummary,
    dcsCatalogName,
    dcsType,

    -- ** Database
    Database (..),
    mkDatabase,
    dName,
    dParameters,
    dDescription,

    -- ** Datum
    Datum (..),
    mkDatum,
    dVarCharValue,

    -- ** EncryptionConfiguration
    EncryptionConfiguration (..),
    mkEncryptionConfiguration,
    ecEncryptionOption,
    ecKMSKey,

    -- ** NamedQuery
    NamedQuery (..),
    mkNamedQuery,
    nqNamedQueryId,
    nqDatabase,
    nqName,
    nqQueryString,
    nqDescription,
    nqWorkGroup,

    -- ** QueryExecution
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

    -- ** QueryExecutionContext
    QueryExecutionContext (..),
    mkQueryExecutionContext,
    qecDatabase,
    qecCatalog,

    -- ** QueryExecutionStatistics
    QueryExecutionStatistics (..),
    mkQueryExecutionStatistics,
    qesTotalExecutionTimeInMillis,
    qesEngineExecutionTimeInMillis,
    qesQueryPlanningTimeInMillis,
    qesDataScannedInBytes,
    qesQueryQueueTimeInMillis,
    qesDataManifestLocation,
    qesServiceProcessingTimeInMillis,

    -- ** QueryExecutionStatus
    QueryExecutionStatus (..),
    mkQueryExecutionStatus,
    qesState,
    qesStateChangeReason,
    qesSubmissionDateTime,
    qesCompletionDateTime,

    -- ** ResultConfiguration
    ResultConfiguration (..),
    mkResultConfiguration,
    rcEncryptionConfiguration,
    rcOutputLocation,

    -- ** ResultConfigurationUpdates
    ResultConfigurationUpdates (..),
    mkResultConfigurationUpdates,
    rcuRemoveOutputLocation,
    rcuRemoveEncryptionConfiguration,
    rcuEncryptionConfiguration,
    rcuOutputLocation,

    -- ** ResultSet
    ResultSet (..),
    mkResultSet,
    rsRows,
    rsResultSetMetadata,

    -- ** ResultSetMetadata
    ResultSetMetadata (..),
    mkResultSetMetadata,
    rsmColumnInfo,

    -- ** Row
    Row (..),
    mkRow,
    rData,

    -- ** TableMetadata
    TableMetadata (..),
    mkTableMetadata,
    tmTableType,
    tmName,
    tmParameters,
    tmColumns,
    tmLastAccessTime,
    tmPartitionKeys,
    tmCreateTime,

    -- ** Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- ** UnprocessedNamedQueryId
    UnprocessedNamedQueryId (..),
    mkUnprocessedNamedQueryId,
    unqiNamedQueryId,
    unqiErrorCode,
    unqiErrorMessage,

    -- ** UnprocessedQueryExecutionId
    UnprocessedQueryExecutionId (..),
    mkUnprocessedQueryExecutionId,
    uqeiErrorCode,
    uqeiQueryExecutionId,
    uqeiErrorMessage,

    -- ** WorkGroup
    WorkGroup (..),
    mkWorkGroup,
    wgCreationTime,
    wgState,
    wgName,
    wgConfiguration,
    wgDescription,

    -- ** WorkGroupConfiguration
    WorkGroupConfiguration (..),
    mkWorkGroupConfiguration,
    wgcRequesterPaysEnabled,
    wgcResultConfiguration,
    wgcBytesScannedCutoffPerQuery,
    wgcEnforceWorkGroupConfiguration,
    wgcPublishCloudWatchMetricsEnabled,

    -- ** WorkGroupConfigurationUpdates
    WorkGroupConfigurationUpdates (..),
    mkWorkGroupConfigurationUpdates,
    wgcuRequesterPaysEnabled,
    wgcuResultConfigurationUpdates,
    wgcuBytesScannedCutoffPerQuery,
    wgcuRemoveBytesScannedCutoffPerQuery,
    wgcuEnforceWorkGroupConfiguration,
    wgcuPublishCloudWatchMetricsEnabled,

    -- ** WorkGroupSummary
    WorkGroupSummary (..),
    mkWorkGroupSummary,
    wgsCreationTime,
    wgsState,
    wgsName,
    wgsDescription,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.DateTime,
    Lude.Timestamp,
  )
where

import Network.AWS.Athena.BatchGetNamedQuery
import Network.AWS.Athena.BatchGetQueryExecution
import Network.AWS.Athena.CreateDataCatalog
import Network.AWS.Athena.CreateNamedQuery
import Network.AWS.Athena.CreateWorkGroup
import Network.AWS.Athena.DeleteDataCatalog
import Network.AWS.Athena.DeleteNamedQuery
import Network.AWS.Athena.DeleteWorkGroup
import Network.AWS.Athena.GetDataCatalog
import Network.AWS.Athena.GetDatabase
import Network.AWS.Athena.GetNamedQuery
import Network.AWS.Athena.GetQueryExecution
import Network.AWS.Athena.GetQueryResults
import Network.AWS.Athena.GetTableMetadata
import Network.AWS.Athena.GetWorkGroup
import Network.AWS.Athena.ListDataCatalogs
import Network.AWS.Athena.ListDatabases
import Network.AWS.Athena.ListNamedQueries
import Network.AWS.Athena.ListQueryExecutions
import Network.AWS.Athena.ListTableMetadata
import Network.AWS.Athena.ListTagsForResource
import Network.AWS.Athena.ListWorkGroups
import Network.AWS.Athena.StartQueryExecution
import Network.AWS.Athena.StopQueryExecution
import Network.AWS.Athena.TagResource
import Network.AWS.Athena.Types
import Network.AWS.Athena.UntagResource
import Network.AWS.Athena.UpdateDataCatalog
import Network.AWS.Athena.UpdateWorkGroup
import Network.AWS.Athena.Waiters
import qualified Network.AWS.Prelude as Lude

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Athena'.

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

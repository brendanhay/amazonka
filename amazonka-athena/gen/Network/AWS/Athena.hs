{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Athena is an interactive query service that lets you use standard SQL to analyze data directly in Amazon S3. You can point Athena at your data in Amazon S3 and run ad-hoc queries and get results in seconds. Athena is serverless, so there is no infrastructure to set up or manage. You pay only for the queries you run. Athena scales automatically—executing queries in parallel—so results are fast, even with large datasets and complex queries. For more information, see <http://docs.aws.amazon.com/athena/latest/ug/what-is.html What is Amazon Athena> in the /Amazon Athena User Guide/ .
--
--
-- For code samples using the AWS SDK for Java, see <http://docs.aws.amazon.com/athena/latest/ug/code-samples.html Examples and Code Samples> in the /Amazon Athena User Guide/ .
--
module Network.AWS.Athena
    (
    -- * Service Configuration
      athena

    -- * Errors
    -- $errors

    -- ** InvalidRequestException
    , _InvalidRequestException

    -- ** TooManyRequestsException
    , _TooManyRequestsException

    -- ** InternalServerException
    , _InternalServerException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetNamedQuery
    , module Network.AWS.Athena.GetNamedQuery

    -- ** CreateNamedQuery
    , module Network.AWS.Athena.CreateNamedQuery

    -- ** ListNamedQueries (Paginated)
    , module Network.AWS.Athena.ListNamedQueries

    -- ** DeleteNamedQuery
    , module Network.AWS.Athena.DeleteNamedQuery

    -- ** StartQueryExecution
    , module Network.AWS.Athena.StartQueryExecution

    -- ** BatchGetNamedQuery
    , module Network.AWS.Athena.BatchGetNamedQuery

    -- ** GetQueryExecution
    , module Network.AWS.Athena.GetQueryExecution

    -- ** BatchGetQueryExecution
    , module Network.AWS.Athena.BatchGetQueryExecution

    -- ** StopQueryExecution
    , module Network.AWS.Athena.StopQueryExecution

    -- ** GetQueryResults (Paginated)
    , module Network.AWS.Athena.GetQueryResults

    -- ** ListQueryExecutions (Paginated)
    , module Network.AWS.Athena.ListQueryExecutions

    -- * Types

    -- ** ColumnNullable
    , ColumnNullable (..)

    -- ** EncryptionOption
    , EncryptionOption (..)

    -- ** QueryExecutionState
    , QueryExecutionState (..)

    -- ** ColumnInfo
    , ColumnInfo
    , columnInfo
    , ciScale
    , ciPrecision
    , ciSchemaName
    , ciCatalogName
    , ciCaseSensitive
    , ciLabel
    , ciTableName
    , ciNullable
    , ciName
    , ciType

    -- ** Datum
    , Datum
    , datum
    , dVarCharValue

    -- ** EncryptionConfiguration
    , EncryptionConfiguration
    , encryptionConfiguration
    , ecKMSKey
    , ecEncryptionOption

    -- ** NamedQuery
    , NamedQuery
    , namedQuery
    , nqNamedQueryId
    , nqDescription
    , nqName
    , nqDatabase
    , nqQueryString

    -- ** QueryExecution
    , QueryExecution
    , queryExecution
    , qeStatus
    , qeQueryExecutionContext
    , qeResultConfiguration
    , qeQuery
    , qeStatistics
    , qeQueryExecutionId

    -- ** QueryExecutionContext
    , QueryExecutionContext
    , queryExecutionContext
    , qecDatabase

    -- ** QueryExecutionStatistics
    , QueryExecutionStatistics
    , queryExecutionStatistics
    , qesEngineExecutionTimeInMillis
    , qesDataScannedInBytes

    -- ** QueryExecutionStatus
    , QueryExecutionStatus
    , queryExecutionStatus
    , qesState
    , qesStateChangeReason
    , qesSubmissionDateTime
    , qesCompletionDateTime

    -- ** ResultConfiguration
    , ResultConfiguration
    , resultConfiguration
    , rcEncryptionConfiguration
    , rcOutputLocation

    -- ** ResultSet
    , ResultSet
    , resultSet
    , rsRows
    , rsResultSetMetadata

    -- ** ResultSetMetadata
    , ResultSetMetadata
    , resultSetMetadata
    , rsmColumnInfo

    -- ** Row
    , Row
    , row
    , rowData

    -- ** UnprocessedNamedQueryId
    , UnprocessedNamedQueryId
    , unprocessedNamedQueryId
    , unqiNamedQueryId
    , unqiErrorCode
    , unqiErrorMessage

    -- ** UnprocessedQueryExecutionId
    , UnprocessedQueryExecutionId
    , unprocessedQueryExecutionId
    , uqeiErrorCode
    , uqeiQueryExecutionId
    , uqeiErrorMessage
    ) where

import Network.AWS.Athena.BatchGetNamedQuery
import Network.AWS.Athena.BatchGetQueryExecution
import Network.AWS.Athena.CreateNamedQuery
import Network.AWS.Athena.DeleteNamedQuery
import Network.AWS.Athena.GetNamedQuery
import Network.AWS.Athena.GetQueryExecution
import Network.AWS.Athena.GetQueryResults
import Network.AWS.Athena.ListNamedQueries
import Network.AWS.Athena.ListQueryExecutions
import Network.AWS.Athena.StartQueryExecution
import Network.AWS.Athena.StopQueryExecution
import Network.AWS.Athena.Types
import Network.AWS.Athena.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'Athena'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}

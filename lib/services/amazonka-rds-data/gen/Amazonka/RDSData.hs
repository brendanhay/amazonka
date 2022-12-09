{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.RDSData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-08-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon RDS Data Service
--
-- Amazon RDS provides an HTTP endpoint to run SQL statements on an Amazon
-- Aurora Serverless v1 DB cluster. To run these statements, you work with
-- the Data Service API.
--
-- The Data Service API isn\'t supported on Amazon Aurora Serverless v2 DB
-- clusters.
--
-- For more information about the Data Service API, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/data-api.html Using the Data API>
-- in the /Amazon Aurora User Guide/.
module Amazonka.RDSData
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** BadRequestException
    _BadRequestException,

    -- ** ForbiddenException
    _ForbiddenException,

    -- ** InternalServerErrorException
    _InternalServerErrorException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** ServiceUnavailableError
    _ServiceUnavailableError,

    -- ** StatementTimeoutException
    _StatementTimeoutException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** BatchExecuteStatement
    BatchExecuteStatement (BatchExecuteStatement'),
    newBatchExecuteStatement,
    BatchExecuteStatementResponse (BatchExecuteStatementResponse'),
    newBatchExecuteStatementResponse,

    -- ** BeginTransaction
    BeginTransaction (BeginTransaction'),
    newBeginTransaction,
    BeginTransactionResponse (BeginTransactionResponse'),
    newBeginTransactionResponse,

    -- ** CommitTransaction
    CommitTransaction (CommitTransaction'),
    newCommitTransaction,
    CommitTransactionResponse (CommitTransactionResponse'),
    newCommitTransactionResponse,

    -- ** ExecuteStatement
    ExecuteStatement (ExecuteStatement'),
    newExecuteStatement,
    ExecuteStatementResponse (ExecuteStatementResponse'),
    newExecuteStatementResponse,

    -- ** RollbackTransaction
    RollbackTransaction (RollbackTransaction'),
    newRollbackTransaction,
    RollbackTransactionResponse (RollbackTransactionResponse'),
    newRollbackTransactionResponse,

    -- * Types

    -- ** DecimalReturnType
    DecimalReturnType (..),

    -- ** LongReturnType
    LongReturnType (..),

    -- ** RecordsFormatType
    RecordsFormatType (..),

    -- ** TypeHint
    TypeHint (..),

    -- ** ArrayValue
    ArrayValue (ArrayValue'),
    newArrayValue,

    -- ** ColumnMetadata
    ColumnMetadata (ColumnMetadata'),
    newColumnMetadata,

    -- ** Field
    Field (Field'),
    newField,

    -- ** ResultSetOptions
    ResultSetOptions (ResultSetOptions'),
    newResultSetOptions,

    -- ** SqlParameter
    SqlParameter (SqlParameter'),
    newSqlParameter,

    -- ** UpdateResult
    UpdateResult (UpdateResult'),
    newUpdateResult,
  )
where

import Amazonka.RDSData.BatchExecuteStatement
import Amazonka.RDSData.BeginTransaction
import Amazonka.RDSData.CommitTransaction
import Amazonka.RDSData.ExecuteStatement
import Amazonka.RDSData.Lens
import Amazonka.RDSData.RollbackTransaction
import Amazonka.RDSData.Types
import Amazonka.RDSData.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'RDSData'.

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

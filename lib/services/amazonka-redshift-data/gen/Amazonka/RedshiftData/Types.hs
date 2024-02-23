{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.RedshiftData.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RedshiftData.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ActiveStatementsExceededException,
    _BatchExecuteStatementException,
    _DatabaseConnectionException,
    _ExecuteStatementException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ValidationException,

    -- * StatementStatusString
    StatementStatusString (..),

    -- * StatusString
    StatusString (..),

    -- * ColumnMetadata
    ColumnMetadata (..),
    newColumnMetadata,
    columnMetadata_columnDefault,
    columnMetadata_isCaseSensitive,
    columnMetadata_isCurrency,
    columnMetadata_isSigned,
    columnMetadata_label,
    columnMetadata_length,
    columnMetadata_name,
    columnMetadata_nullable,
    columnMetadata_precision,
    columnMetadata_scale,
    columnMetadata_schemaName,
    columnMetadata_tableName,
    columnMetadata_typeName,

    -- * Field
    Field (..),
    newField,
    field_blobValue,
    field_booleanValue,
    field_doubleValue,
    field_isNull,
    field_longValue,
    field_stringValue,

    -- * SqlParameter
    SqlParameter (..),
    newSqlParameter,
    sqlParameter_name,
    sqlParameter_value,

    -- * StatementData
    StatementData (..),
    newStatementData,
    statementData_createdAt,
    statementData_isBatchStatement,
    statementData_queryParameters,
    statementData_queryString,
    statementData_queryStrings,
    statementData_secretArn,
    statementData_statementName,
    statementData_status,
    statementData_updatedAt,
    statementData_id,

    -- * SubStatementData
    SubStatementData (..),
    newSubStatementData,
    subStatementData_createdAt,
    subStatementData_duration,
    subStatementData_error,
    subStatementData_hasResultSet,
    subStatementData_queryString,
    subStatementData_redshiftQueryId,
    subStatementData_resultRows,
    subStatementData_resultSize,
    subStatementData_status,
    subStatementData_updatedAt,
    subStatementData_id,

    -- * TableMember
    TableMember (..),
    newTableMember,
    tableMember_name,
    tableMember_schema,
    tableMember_type,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftData.Types.ColumnMetadata
import Amazonka.RedshiftData.Types.Field
import Amazonka.RedshiftData.Types.SqlParameter
import Amazonka.RedshiftData.Types.StatementData
import Amazonka.RedshiftData.Types.StatementStatusString
import Amazonka.RedshiftData.Types.StatusString
import Amazonka.RedshiftData.Types.SubStatementData
import Amazonka.RedshiftData.Types.TableMember
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2019-12-20@ of the Amazon Redshift Data API Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "RedshiftData",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "redshift-data",
      Core.signingName = "redshift-data",
      Core.version = "2019-12-20",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "RedshiftData",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | The number of active statements exceeds the limit.
_ActiveStatementsExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ActiveStatementsExceededException =
  Core._MatchServiceError
    defaultService
    "ActiveStatementsExceededException"

-- | An SQL statement encountered an environmental error while running.
_BatchExecuteStatementException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_BatchExecuteStatementException =
  Core._MatchServiceError
    defaultService
    "BatchExecuteStatementException"

-- | Connection to a database failed.
_DatabaseConnectionException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DatabaseConnectionException =
  Core._MatchServiceError
    defaultService
    "DatabaseConnectionException"

-- | The SQL statement encountered an environmental error while running.
_ExecuteStatementException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ExecuteStatementException =
  Core._MatchServiceError
    defaultService
    "ExecuteStatementException"

-- | The Amazon Redshift Data API operation failed due to invalid input.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | The Amazon Redshift Data API operation failed due to a missing resource.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The Amazon Redshift Data API operation failed due to invalid input.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"

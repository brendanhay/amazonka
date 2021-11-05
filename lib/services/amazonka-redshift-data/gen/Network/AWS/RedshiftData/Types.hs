{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.RedshiftData.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RedshiftData.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _ExecuteStatementException,
    _InternalServerException,
    _ActiveStatementsExceededException,
    _ResourceNotFoundException,
    _BatchExecuteStatementException,

    -- * StatementStatusString
    StatementStatusString (..),

    -- * StatusString
    StatusString (..),

    -- * ColumnMetadata
    ColumnMetadata (..),
    newColumnMetadata,
    columnMetadata_length,
    columnMetadata_typeName,
    columnMetadata_isCaseSensitive,
    columnMetadata_columnDefault,
    columnMetadata_isCurrency,
    columnMetadata_scale,
    columnMetadata_precision,
    columnMetadata_schemaName,
    columnMetadata_name,
    columnMetadata_isSigned,
    columnMetadata_label,
    columnMetadata_nullable,
    columnMetadata_tableName,

    -- * Field
    Field (..),
    newField,
    field_doubleValue,
    field_stringValue,
    field_longValue,
    field_booleanValue,
    field_blobValue,
    field_isNull,

    -- * SqlParameter
    SqlParameter (..),
    newSqlParameter,
    sqlParameter_name,
    sqlParameter_value,

    -- * StatementData
    StatementData (..),
    newStatementData,
    statementData_status,
    statementData_createdAt,
    statementData_queryParameters,
    statementData_queryStrings,
    statementData_queryString,
    statementData_statementName,
    statementData_updatedAt,
    statementData_secretArn,
    statementData_isBatchStatement,
    statementData_id,

    -- * SubStatementData
    SubStatementData (..),
    newSubStatementData,
    subStatementData_status,
    subStatementData_redshiftQueryId,
    subStatementData_resultSize,
    subStatementData_createdAt,
    subStatementData_error,
    subStatementData_resultRows,
    subStatementData_hasResultSet,
    subStatementData_queryString,
    subStatementData_updatedAt,
    subStatementData_duration,
    subStatementData_id,

    -- * TableMember
    TableMember (..),
    newTableMember,
    tableMember_schema,
    tableMember_name,
    tableMember_type,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
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
    { Core._serviceAbbrev = "RedshiftData",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "redshift-data",
      Core._serviceSigningName = "redshift-data",
      Core._serviceVersion = "2019-12-20",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "RedshiftData",
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
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
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
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The Amazon Redshift Data API operation failed due to invalid input.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"

-- | The SQL statement encountered an environmental error while running.
_ExecuteStatementException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ExecuteStatementException =
  Core._MatchServiceError
    defaultService
    "ExecuteStatementException"

-- | The Amazon Redshift Data API operation failed due to invalid input.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | The number of active statements exceeds the limit.
_ActiveStatementsExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ActiveStatementsExceededException =
  Core._MatchServiceError
    defaultService
    "ActiveStatementsExceededException"

-- | The Amazon Redshift Data API operation failed due to a missing resource.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | An SQL statement encountered an environmental error while running.
_BatchExecuteStatementException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BatchExecuteStatementException =
  Core._MatchServiceError
    defaultService
    "BatchExecuteStatementException"

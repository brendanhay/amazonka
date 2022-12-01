{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.RDSData.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDSData.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _NotFoundException,
    _InternalServerErrorException,
    _ForbiddenException,
    _ServiceUnavailableError,
    _BadRequestException,
    _StatementTimeoutException,

    -- * DecimalReturnType
    DecimalReturnType (..),

    -- * LongReturnType
    LongReturnType (..),

    -- * RecordsFormatType
    RecordsFormatType (..),

    -- * TypeHint
    TypeHint (..),

    -- * ArrayValue
    ArrayValue (..),
    newArrayValue,
    arrayValue_stringValues,
    arrayValue_booleanValues,
    arrayValue_longValues,
    arrayValue_doubleValues,
    arrayValue_arrayValues,

    -- * ColumnMetadata
    ColumnMetadata (..),
    newColumnMetadata,
    columnMetadata_tableName,
    columnMetadata_name,
    columnMetadata_type,
    columnMetadata_label,
    columnMetadata_schemaName,
    columnMetadata_nullable,
    columnMetadata_isCaseSensitive,
    columnMetadata_typeName,
    columnMetadata_arrayBaseColumnType,
    columnMetadata_precision,
    columnMetadata_scale,
    columnMetadata_isAutoIncrement,
    columnMetadata_isCurrency,
    columnMetadata_isSigned,

    -- * Field
    Field (..),
    newField,
    field_doubleValue,
    field_booleanValue,
    field_isNull,
    field_stringValue,
    field_longValue,
    field_blobValue,
    field_arrayValue,

    -- * ResultSetOptions
    ResultSetOptions (..),
    newResultSetOptions,
    resultSetOptions_decimalReturnType,
    resultSetOptions_longReturnType,

    -- * SqlParameter
    SqlParameter (..),
    newSqlParameter,
    sqlParameter_name,
    sqlParameter_typeHint,
    sqlParameter_value,

    -- * UpdateResult
    UpdateResult (..),
    newUpdateResult,
    updateResult_generatedFields,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDSData.Types.ArrayValue
import Amazonka.RDSData.Types.ColumnMetadata
import Amazonka.RDSData.Types.DecimalReturnType
import Amazonka.RDSData.Types.Field
import Amazonka.RDSData.Types.LongReturnType
import Amazonka.RDSData.Types.RecordsFormatType
import Amazonka.RDSData.Types.ResultSetOptions
import Amazonka.RDSData.Types.SqlParameter
import Amazonka.RDSData.Types.TypeHint
import Amazonka.RDSData.Types.UpdateResult
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-08-01@ of the Amazon RDS DataService SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "RDSData",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "rds-data",
      Core.signingName = "rds-data",
      Core.version = "2018-08-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "RDSData",
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
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | You do not have sufficient access to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The @resourceArn@, @secretArn@, or @transactionId@ value can\'t be
-- found.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | An internal error occurred.
_InternalServerErrorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServerErrorException"
    Prelude.. Core.hasStatus 500

-- | There are insufficient privileges to make the call.
_ForbiddenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ForbiddenException =
  Core._MatchServiceError
    defaultService
    "ForbiddenException"
    Prelude.. Core.hasStatus 403

-- | The service specified by the @resourceArn@ parameter is not available.
_ServiceUnavailableError :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableError =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableError"
    Prelude.. Core.hasStatus 503

-- | There is an error in the call or in a SQL statement.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400

-- | The execution of the SQL statement timed out.
_StatementTimeoutException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_StatementTimeoutException =
  Core._MatchServiceError
    defaultService
    "StatementTimeoutException"
    Prelude.. Core.hasStatus 400

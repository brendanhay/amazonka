{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.TimeStreamWrite.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamWrite.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _InvalidEndpointException,
    _RejectedRecordsException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _ValidationException,

    -- * DimensionValueType
    DimensionValueType (..),

    -- * MeasureValueType
    MeasureValueType (..),

    -- * S3EncryptionOption
    S3EncryptionOption (..),

    -- * TableStatus
    TableStatus (..),

    -- * TimeUnit
    TimeUnit (..),

    -- * Database
    Database (..),
    newDatabase,
    database_arn,
    database_creationTime,
    database_databaseName,
    database_kmsKeyId,
    database_lastUpdatedTime,
    database_tableCount,

    -- * Dimension
    Dimension (..),
    newDimension,
    dimension_dimensionValueType,
    dimension_name,
    dimension_value,

    -- * Endpoint
    Endpoint (..),
    newEndpoint,
    endpoint_address,
    endpoint_cachePeriodInMinutes,

    -- * MagneticStoreRejectedDataLocation
    MagneticStoreRejectedDataLocation (..),
    newMagneticStoreRejectedDataLocation,
    magneticStoreRejectedDataLocation_s3Configuration,

    -- * MagneticStoreWriteProperties
    MagneticStoreWriteProperties (..),
    newMagneticStoreWriteProperties,
    magneticStoreWriteProperties_magneticStoreRejectedDataLocation,
    magneticStoreWriteProperties_enableMagneticStoreWrites,

    -- * MeasureValue
    MeasureValue (..),
    newMeasureValue,
    measureValue_name,
    measureValue_value,
    measureValue_type,

    -- * Record
    Record (..),
    newRecord,
    record_dimensions,
    record_measureName,
    record_measureValue,
    record_measureValueType,
    record_measureValues,
    record_time,
    record_timeUnit,
    record_version,

    -- * RecordsIngested
    RecordsIngested (..),
    newRecordsIngested,
    recordsIngested_magneticStore,
    recordsIngested_memoryStore,
    recordsIngested_total,

    -- * RetentionProperties
    RetentionProperties (..),
    newRetentionProperties,
    retentionProperties_memoryStoreRetentionPeriodInHours,
    retentionProperties_magneticStoreRetentionPeriodInDays,

    -- * S3Configuration
    S3Configuration (..),
    newS3Configuration,
    s3Configuration_bucketName,
    s3Configuration_encryptionOption,
    s3Configuration_kmsKeyId,
    s3Configuration_objectKeyPrefix,

    -- * Table
    Table (..),
    newTable,
    table_arn,
    table_creationTime,
    table_databaseName,
    table_lastUpdatedTime,
    table_magneticStoreWriteProperties,
    table_retentionProperties,
    table_tableName,
    table_tableStatus,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign
import Amazonka.TimeStreamWrite.Types.Database
import Amazonka.TimeStreamWrite.Types.Dimension
import Amazonka.TimeStreamWrite.Types.DimensionValueType
import Amazonka.TimeStreamWrite.Types.Endpoint
import Amazonka.TimeStreamWrite.Types.MagneticStoreRejectedDataLocation
import Amazonka.TimeStreamWrite.Types.MagneticStoreWriteProperties
import Amazonka.TimeStreamWrite.Types.MeasureValue
import Amazonka.TimeStreamWrite.Types.MeasureValueType
import Amazonka.TimeStreamWrite.Types.Record
import Amazonka.TimeStreamWrite.Types.RecordsIngested
import Amazonka.TimeStreamWrite.Types.RetentionProperties
import Amazonka.TimeStreamWrite.Types.S3Configuration
import Amazonka.TimeStreamWrite.Types.S3EncryptionOption
import Amazonka.TimeStreamWrite.Types.Table
import Amazonka.TimeStreamWrite.Types.TableStatus
import Amazonka.TimeStreamWrite.Types.Tag
import Amazonka.TimeStreamWrite.Types.TimeUnit

-- | API version @2018-11-01@ of the Amazon Timestream Write SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "TimeStreamWrite",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "ingest.timestream",
      Core.signingName = "timestream",
      Core.version = "2018-11-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "TimeStreamWrite",
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

-- | You are not authorized to perform this action.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | Timestream was unable to process this request because it contains
-- resource that already exists.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | Timestream was unable to fully process this request because of an
-- internal server error.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | The requested endpoint was invalid.
_InvalidEndpointException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidEndpointException =
  Core._MatchServiceError
    defaultService
    "InvalidEndpointException"

-- | WriteRecords would throw this exception in the following cases:
--
-- -   Records with duplicate data where there are multiple records with
--     the same dimensions, timestamps, and measure names but:
--
--     -   Measure values are different
--
--     -   Version is not present in the request /or/ the value of version
--         in the new record is equal to or lower than the existing value
--
--     In this case, if Timestream rejects data, the @ExistingVersion@
--     field in the @RejectedRecords@ response will indicate the current
--     record’s version. To force an update, you can resend the request
--     with a version for the record set to a value greater than the
--     @ExistingVersion@.
--
-- -   Records with timestamps that lie outside the retention duration of
--     the memory store
--
-- -   Records with dimensions or measures that exceed the Timestream
--     defined limits.
--
-- For more information, see
-- <https://docs.aws.amazon.com/timestream/latest/developerguide/ts-limits.html Quotas>
-- in the Timestream Developer Guide.
_RejectedRecordsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_RejectedRecordsException =
  Core._MatchServiceError
    defaultService
    "RejectedRecordsException"

-- | The operation tried to access a nonexistent resource. The resource might
-- not be specified correctly, or its status might not be ACTIVE.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | Instance quota of resource exceeded for this account.
_ServiceQuotaExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"

-- | Too many requests were made by a user exceeding service quotas. The
-- request was throttled.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

-- | Invalid or malformed request.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"

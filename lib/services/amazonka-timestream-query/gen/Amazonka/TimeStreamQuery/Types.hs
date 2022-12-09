{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.TimeStreamQuery.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamQuery.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _InvalidEndpointException,
    _QueryExecutionException,
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

    -- * ScalarMeasureValueType
    ScalarMeasureValueType (..),

    -- * ScalarType
    ScalarType (..),

    -- * ScheduledQueryRunStatus
    ScheduledQueryRunStatus (..),

    -- * ScheduledQueryState
    ScheduledQueryState (..),

    -- * ColumnInfo
    ColumnInfo (..),
    newColumnInfo,
    columnInfo_name,
    columnInfo_type,

    -- * Datum
    Datum (..),
    newDatum,
    datum_arrayValue,
    datum_nullValue,
    datum_rowValue,
    datum_scalarValue,
    datum_timeSeriesValue,

    -- * DimensionMapping
    DimensionMapping (..),
    newDimensionMapping,
    dimensionMapping_name,
    dimensionMapping_dimensionValueType,

    -- * Endpoint
    Endpoint (..),
    newEndpoint,
    endpoint_address,
    endpoint_cachePeriodInMinutes,

    -- * ErrorReportConfiguration
    ErrorReportConfiguration (..),
    newErrorReportConfiguration,
    errorReportConfiguration_s3Configuration,

    -- * ErrorReportLocation
    ErrorReportLocation (..),
    newErrorReportLocation,
    errorReportLocation_s3ReportLocation,

    -- * ExecutionStats
    ExecutionStats (..),
    newExecutionStats,
    executionStats_bytesMetered,
    executionStats_dataWrites,
    executionStats_executionTimeInMillis,
    executionStats_queryResultRows,
    executionStats_recordsIngested,

    -- * MixedMeasureMapping
    MixedMeasureMapping (..),
    newMixedMeasureMapping,
    mixedMeasureMapping_measureName,
    mixedMeasureMapping_multiMeasureAttributeMappings,
    mixedMeasureMapping_sourceColumn,
    mixedMeasureMapping_targetMeasureName,
    mixedMeasureMapping_measureValueType,

    -- * MultiMeasureAttributeMapping
    MultiMeasureAttributeMapping (..),
    newMultiMeasureAttributeMapping,
    multiMeasureAttributeMapping_targetMultiMeasureAttributeName,
    multiMeasureAttributeMapping_sourceColumn,
    multiMeasureAttributeMapping_measureValueType,

    -- * MultiMeasureMappings
    MultiMeasureMappings (..),
    newMultiMeasureMappings,
    multiMeasureMappings_targetMultiMeasureName,
    multiMeasureMappings_multiMeasureAttributeMappings,

    -- * NotificationConfiguration
    NotificationConfiguration (..),
    newNotificationConfiguration,
    notificationConfiguration_snsConfiguration,

    -- * ParameterMapping
    ParameterMapping (..),
    newParameterMapping,
    parameterMapping_name,
    parameterMapping_type,

    -- * QueryStatus
    QueryStatus (..),
    newQueryStatus,
    queryStatus_cumulativeBytesMetered,
    queryStatus_cumulativeBytesScanned,
    queryStatus_progressPercentage,

    -- * Row
    Row (..),
    newRow,
    row_data,

    -- * S3Configuration
    S3Configuration (..),
    newS3Configuration,
    s3Configuration_encryptionOption,
    s3Configuration_objectKeyPrefix,
    s3Configuration_bucketName,

    -- * S3ReportLocation
    S3ReportLocation (..),
    newS3ReportLocation,
    s3ReportLocation_bucketName,
    s3ReportLocation_objectKey,

    -- * ScheduleConfiguration
    ScheduleConfiguration (..),
    newScheduleConfiguration,
    scheduleConfiguration_scheduleExpression,

    -- * ScheduledQuery
    ScheduledQuery (..),
    newScheduledQuery,
    scheduledQuery_creationTime,
    scheduledQuery_errorReportConfiguration,
    scheduledQuery_lastRunStatus,
    scheduledQuery_nextInvocationTime,
    scheduledQuery_previousInvocationTime,
    scheduledQuery_targetDestination,
    scheduledQuery_arn,
    scheduledQuery_name,
    scheduledQuery_state,

    -- * ScheduledQueryDescription
    ScheduledQueryDescription (..),
    newScheduledQueryDescription,
    scheduledQueryDescription_creationTime,
    scheduledQueryDescription_errorReportConfiguration,
    scheduledQueryDescription_kmsKeyId,
    scheduledQueryDescription_lastRunSummary,
    scheduledQueryDescription_nextInvocationTime,
    scheduledQueryDescription_previousInvocationTime,
    scheduledQueryDescription_recentlyFailedRuns,
    scheduledQueryDescription_scheduledQueryExecutionRoleArn,
    scheduledQueryDescription_targetConfiguration,
    scheduledQueryDescription_arn,
    scheduledQueryDescription_name,
    scheduledQueryDescription_queryString,
    scheduledQueryDescription_state,
    scheduledQueryDescription_scheduleConfiguration,
    scheduledQueryDescription_notificationConfiguration,

    -- * ScheduledQueryRunSummary
    ScheduledQueryRunSummary (..),
    newScheduledQueryRunSummary,
    scheduledQueryRunSummary_errorReportLocation,
    scheduledQueryRunSummary_executionStats,
    scheduledQueryRunSummary_failureReason,
    scheduledQueryRunSummary_invocationTime,
    scheduledQueryRunSummary_runStatus,
    scheduledQueryRunSummary_triggerTime,

    -- * SelectColumn
    SelectColumn (..),
    newSelectColumn,
    selectColumn_aliased,
    selectColumn_databaseName,
    selectColumn_name,
    selectColumn_tableName,
    selectColumn_type,

    -- * SnsConfiguration
    SnsConfiguration (..),
    newSnsConfiguration,
    snsConfiguration_topicArn,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TargetConfiguration
    TargetConfiguration (..),
    newTargetConfiguration,
    targetConfiguration_timestreamConfiguration,

    -- * TargetDestination
    TargetDestination (..),
    newTargetDestination,
    targetDestination_timestreamDestination,

    -- * TimeSeriesDataPoint
    TimeSeriesDataPoint (..),
    newTimeSeriesDataPoint,
    timeSeriesDataPoint_time,
    timeSeriesDataPoint_value,

    -- * TimestreamConfiguration
    TimestreamConfiguration (..),
    newTimestreamConfiguration,
    timestreamConfiguration_measureNameColumn,
    timestreamConfiguration_mixedMeasureMappings,
    timestreamConfiguration_multiMeasureMappings,
    timestreamConfiguration_databaseName,
    timestreamConfiguration_tableName,
    timestreamConfiguration_timeColumn,
    timestreamConfiguration_dimensionMappings,

    -- * TimestreamDestination
    TimestreamDestination (..),
    newTimestreamDestination,
    timestreamDestination_databaseName,
    timestreamDestination_tableName,

    -- * Type
    Type (..),
    newType,
    type_arrayColumnInfo,
    type_rowColumnInfo,
    type_scalarType,
    type_timeSeriesMeasureValueColumnInfo,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign
import Amazonka.TimeStreamQuery.Types.ColumnInfo
import Amazonka.TimeStreamQuery.Types.Datum
import Amazonka.TimeStreamQuery.Types.DimensionMapping
import Amazonka.TimeStreamQuery.Types.DimensionValueType
import Amazonka.TimeStreamQuery.Types.Endpoint
import Amazonka.TimeStreamQuery.Types.ErrorReportConfiguration
import Amazonka.TimeStreamQuery.Types.ErrorReportLocation
import Amazonka.TimeStreamQuery.Types.ExecutionStats
import Amazonka.TimeStreamQuery.Types.MeasureValueType
import Amazonka.TimeStreamQuery.Types.MixedMeasureMapping
import Amazonka.TimeStreamQuery.Types.MultiMeasureAttributeMapping
import Amazonka.TimeStreamQuery.Types.MultiMeasureMappings
import Amazonka.TimeStreamQuery.Types.NotificationConfiguration
import Amazonka.TimeStreamQuery.Types.ParameterMapping
import Amazonka.TimeStreamQuery.Types.QueryStatus
import Amazonka.TimeStreamQuery.Types.Row
import Amazonka.TimeStreamQuery.Types.S3Configuration
import Amazonka.TimeStreamQuery.Types.S3EncryptionOption
import Amazonka.TimeStreamQuery.Types.S3ReportLocation
import Amazonka.TimeStreamQuery.Types.ScalarMeasureValueType
import Amazonka.TimeStreamQuery.Types.ScalarType
import Amazonka.TimeStreamQuery.Types.ScheduleConfiguration
import Amazonka.TimeStreamQuery.Types.ScheduledQuery
import Amazonka.TimeStreamQuery.Types.ScheduledQueryDescription
import Amazonka.TimeStreamQuery.Types.ScheduledQueryRunStatus
import Amazonka.TimeStreamQuery.Types.ScheduledQueryRunSummary
import Amazonka.TimeStreamQuery.Types.ScheduledQueryState
import Amazonka.TimeStreamQuery.Types.SelectColumn
import Amazonka.TimeStreamQuery.Types.SnsConfiguration
import Amazonka.TimeStreamQuery.Types.Tag
import Amazonka.TimeStreamQuery.Types.TargetConfiguration
import Amazonka.TimeStreamQuery.Types.TargetDestination
import Amazonka.TimeStreamQuery.Types.TimeSeriesDataPoint
import Amazonka.TimeStreamQuery.Types.TimestreamConfiguration
import Amazonka.TimeStreamQuery.Types.TimestreamDestination
import Amazonka.TimeStreamQuery.Types.Type

-- | API version @2018-11-01@ of the Amazon Timestream Query SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "TimeStreamQuery",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "query.timestream",
      Core.signingName = "timestream",
      Core.version = "2018-11-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "TimeStreamQuery",
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
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | Unable to poll results for a cancelled query.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | Timestream was unable to fully process this request because of an
-- internal server error.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | The requested endpoint was not valid.
_InvalidEndpointException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidEndpointException =
  Core._MatchServiceError
    defaultService
    "InvalidEndpointException"

-- | Timestream was unable to run the query successfully.
_QueryExecutionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_QueryExecutionException =
  Core._MatchServiceError
    defaultService
    "QueryExecutionException"

-- | The requested resource could not be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | You have exceeded the service quota.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"

-- | The request was denied due to request throttling.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

-- | Invalid or malformed request.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"

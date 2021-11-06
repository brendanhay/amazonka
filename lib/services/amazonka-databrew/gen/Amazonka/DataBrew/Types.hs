{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DataBrew.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _AccessDeniedException,
    _ConflictException,
    _ServiceQuotaExceededException,
    _InternalServerException,
    _ResourceNotFoundException,

    -- * CompressionFormat
    CompressionFormat (..),

    -- * DatabaseOutputMode
    DatabaseOutputMode (..),

    -- * EncryptionMode
    EncryptionMode (..),

    -- * InputFormat
    InputFormat (..),

    -- * JobRunState
    JobRunState (..),

    -- * JobType
    JobType (..),

    -- * LogSubscription
    LogSubscription (..),

    -- * Order
    Order (..),

    -- * OrderedBy
    OrderedBy (..),

    -- * OutputFormat
    OutputFormat (..),

    -- * ParameterType
    ParameterType (..),

    -- * SampleMode
    SampleMode (..),

    -- * SampleType
    SampleType (..),

    -- * SessionStatus
    SessionStatus (..),

    -- * Source
    Source (..),

    -- * ColumnSelector
    ColumnSelector (..),
    newColumnSelector,
    columnSelector_regex,
    columnSelector_name,

    -- * ColumnStatisticsConfiguration
    ColumnStatisticsConfiguration (..),
    newColumnStatisticsConfiguration,
    columnStatisticsConfiguration_selectors,
    columnStatisticsConfiguration_statistics,

    -- * ConditionExpression
    ConditionExpression (..),
    newConditionExpression,
    conditionExpression_value,
    conditionExpression_condition,
    conditionExpression_targetColumn,

    -- * CsvOptions
    CsvOptions (..),
    newCsvOptions,
    csvOptions_headerRow,
    csvOptions_delimiter,

    -- * CsvOutputOptions
    CsvOutputOptions (..),
    newCsvOutputOptions,
    csvOutputOptions_delimiter,

    -- * DataCatalogInputDefinition
    DataCatalogInputDefinition (..),
    newDataCatalogInputDefinition,
    dataCatalogInputDefinition_tempDirectory,
    dataCatalogInputDefinition_catalogId,
    dataCatalogInputDefinition_databaseName,
    dataCatalogInputDefinition_tableName,

    -- * DataCatalogOutput
    DataCatalogOutput (..),
    newDataCatalogOutput,
    dataCatalogOutput_databaseOptions,
    dataCatalogOutput_s3Options,
    dataCatalogOutput_catalogId,
    dataCatalogOutput_overwrite,
    dataCatalogOutput_databaseName,
    dataCatalogOutput_tableName,

    -- * DatabaseInputDefinition
    DatabaseInputDefinition (..),
    newDatabaseInputDefinition,
    databaseInputDefinition_tempDirectory,
    databaseInputDefinition_glueConnectionName,
    databaseInputDefinition_databaseTableName,

    -- * DatabaseOutput
    DatabaseOutput (..),
    newDatabaseOutput,
    databaseOutput_databaseOutputMode,
    databaseOutput_glueConnectionName,
    databaseOutput_databaseOptions,

    -- * DatabaseTableOutputOptions
    DatabaseTableOutputOptions (..),
    newDatabaseTableOutputOptions,
    databaseTableOutputOptions_tempDirectory,
    databaseTableOutputOptions_tableName,

    -- * Dataset
    Dataset (..),
    newDataset,
    dataset_lastModifiedDate,
    dataset_pathOptions,
    dataset_createDate,
    dataset_formatOptions,
    dataset_format,
    dataset_createdBy,
    dataset_accountId,
    dataset_resourceArn,
    dataset_source,
    dataset_lastModifiedBy,
    dataset_tags,
    dataset_name,
    dataset_input,

    -- * DatasetParameter
    DatasetParameter (..),
    newDatasetParameter,
    datasetParameter_createColumn,
    datasetParameter_filter,
    datasetParameter_datetimeOptions,
    datasetParameter_name,
    datasetParameter_type,

    -- * DatetimeOptions
    DatetimeOptions (..),
    newDatetimeOptions,
    datetimeOptions_timezoneOffset,
    datetimeOptions_localeCode,
    datetimeOptions_format,

    -- * ExcelOptions
    ExcelOptions (..),
    newExcelOptions,
    excelOptions_sheetIndexes,
    excelOptions_sheetNames,
    excelOptions_headerRow,

    -- * FilesLimit
    FilesLimit (..),
    newFilesLimit,
    filesLimit_orderedBy,
    filesLimit_order,
    filesLimit_maxFiles,

    -- * FilterExpression
    FilterExpression (..),
    newFilterExpression,
    filterExpression_expression,
    filterExpression_valuesMap,

    -- * FormatOptions
    FormatOptions (..),
    newFormatOptions,
    formatOptions_json,
    formatOptions_csv,
    formatOptions_excel,

    -- * Input
    Input (..),
    newInput,
    input_dataCatalogInputDefinition,
    input_s3InputDefinition,
    input_databaseInputDefinition,

    -- * Job
    Job (..),
    newJob,
    job_dataCatalogOutputs,
    job_lastModifiedDate,
    job_createDate,
    job_recipeReference,
    job_createdBy,
    job_databaseOutputs,
    job_accountId,
    job_encryptionMode,
    job_outputs,
    job_resourceArn,
    job_logSubscription,
    job_projectName,
    job_maxRetries,
    job_datasetName,
    job_encryptionKeyArn,
    job_maxCapacity,
    job_lastModifiedBy,
    job_type,
    job_timeout,
    job_tags,
    job_roleArn,
    job_jobSample,
    job_name,

    -- * JobRun
    JobRun (..),
    newJobRun,
    jobRun_completedOn,
    jobRun_state,
    jobRun_dataCatalogOutputs,
    jobRun_jobName,
    jobRun_startedOn,
    jobRun_recipeReference,
    jobRun_databaseOutputs,
    jobRun_logGroupName,
    jobRun_outputs,
    jobRun_runId,
    jobRun_executionTime,
    jobRun_logSubscription,
    jobRun_startedBy,
    jobRun_datasetName,
    jobRun_attempt,
    jobRun_errorMessage,
    jobRun_jobSample,

    -- * JobSample
    JobSample (..),
    newJobSample,
    jobSample_size,
    jobSample_mode,

    -- * JsonOptions
    JsonOptions (..),
    newJsonOptions,
    jsonOptions_multiLine,

    -- * Output
    Output (..),
    newOutput,
    output_partitionColumns,
    output_formatOptions,
    output_format,
    output_compressionFormat,
    output_overwrite,
    output_location,

    -- * OutputFormatOptions
    OutputFormatOptions (..),
    newOutputFormatOptions,
    outputFormatOptions_csv,

    -- * PathOptions
    PathOptions (..),
    newPathOptions,
    pathOptions_lastModifiedDateCondition,
    pathOptions_parameters,
    pathOptions_filesLimit,

    -- * ProfileConfiguration
    ProfileConfiguration (..),
    newProfileConfiguration,
    profileConfiguration_datasetStatisticsConfiguration,
    profileConfiguration_columnStatisticsConfigurations,
    profileConfiguration_profileColumns,

    -- * Project
    Project (..),
    newProject,
    project_lastModifiedDate,
    project_openDate,
    project_createDate,
    project_createdBy,
    project_accountId,
    project_resourceArn,
    project_datasetName,
    project_lastModifiedBy,
    project_sample,
    project_openedBy,
    project_tags,
    project_roleArn,
    project_name,
    project_recipeName,

    -- * Recipe
    Recipe (..),
    newRecipe,
    recipe_lastModifiedDate,
    recipe_createDate,
    recipe_publishedBy,
    recipe_createdBy,
    recipe_steps,
    recipe_publishedDate,
    recipe_resourceArn,
    recipe_recipeVersion,
    recipe_projectName,
    recipe_lastModifiedBy,
    recipe_description,
    recipe_tags,
    recipe_name,

    -- * RecipeAction
    RecipeAction (..),
    newRecipeAction,
    recipeAction_parameters,
    recipeAction_operation,

    -- * RecipeReference
    RecipeReference (..),
    newRecipeReference,
    recipeReference_recipeVersion,
    recipeReference_name,

    -- * RecipeStep
    RecipeStep (..),
    newRecipeStep,
    recipeStep_conditionExpressions,
    recipeStep_action,

    -- * RecipeVersionErrorDetail
    RecipeVersionErrorDetail (..),
    newRecipeVersionErrorDetail,
    recipeVersionErrorDetail_recipeVersion,
    recipeVersionErrorDetail_errorCode,
    recipeVersionErrorDetail_errorMessage,

    -- * S3Location
    S3Location (..),
    newS3Location,
    s3Location_key,
    s3Location_bucket,

    -- * S3TableOutputOptions
    S3TableOutputOptions (..),
    newS3TableOutputOptions,
    s3TableOutputOptions_location,

    -- * Sample
    Sample (..),
    newSample,
    sample_size,
    sample_type,

    -- * Schedule
    Schedule (..),
    newSchedule,
    schedule_lastModifiedDate,
    schedule_createDate,
    schedule_createdBy,
    schedule_accountId,
    schedule_resourceArn,
    schedule_cronExpression,
    schedule_lastModifiedBy,
    schedule_jobNames,
    schedule_tags,
    schedule_name,

    -- * StatisticOverride
    StatisticOverride (..),
    newStatisticOverride,
    statisticOverride_statistic,
    statisticOverride_parameters,

    -- * StatisticsConfiguration
    StatisticsConfiguration (..),
    newStatisticsConfiguration,
    statisticsConfiguration_overrides,
    statisticsConfiguration_includedStatistics,

    -- * ViewFrame
    ViewFrame (..),
    newViewFrame,
    viewFrame_hiddenColumns,
    viewFrame_columnRange,
    viewFrame_startColumnIndex,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.DataBrew.Types.ColumnSelector
import Amazonka.DataBrew.Types.ColumnStatisticsConfiguration
import Amazonka.DataBrew.Types.CompressionFormat
import Amazonka.DataBrew.Types.ConditionExpression
import Amazonka.DataBrew.Types.CsvOptions
import Amazonka.DataBrew.Types.CsvOutputOptions
import Amazonka.DataBrew.Types.DataCatalogInputDefinition
import Amazonka.DataBrew.Types.DataCatalogOutput
import Amazonka.DataBrew.Types.DatabaseInputDefinition
import Amazonka.DataBrew.Types.DatabaseOutput
import Amazonka.DataBrew.Types.DatabaseOutputMode
import Amazonka.DataBrew.Types.DatabaseTableOutputOptions
import Amazonka.DataBrew.Types.Dataset
import Amazonka.DataBrew.Types.DatasetParameter
import Amazonka.DataBrew.Types.DatetimeOptions
import Amazonka.DataBrew.Types.EncryptionMode
import Amazonka.DataBrew.Types.ExcelOptions
import Amazonka.DataBrew.Types.FilesLimit
import Amazonka.DataBrew.Types.FilterExpression
import Amazonka.DataBrew.Types.FormatOptions
import Amazonka.DataBrew.Types.Input
import Amazonka.DataBrew.Types.InputFormat
import Amazonka.DataBrew.Types.Job
import Amazonka.DataBrew.Types.JobRun
import Amazonka.DataBrew.Types.JobRunState
import Amazonka.DataBrew.Types.JobSample
import Amazonka.DataBrew.Types.JobType
import Amazonka.DataBrew.Types.JsonOptions
import Amazonka.DataBrew.Types.LogSubscription
import Amazonka.DataBrew.Types.Order
import Amazonka.DataBrew.Types.OrderedBy
import Amazonka.DataBrew.Types.Output
import Amazonka.DataBrew.Types.OutputFormat
import Amazonka.DataBrew.Types.OutputFormatOptions
import Amazonka.DataBrew.Types.ParameterType
import Amazonka.DataBrew.Types.PathOptions
import Amazonka.DataBrew.Types.ProfileConfiguration
import Amazonka.DataBrew.Types.Project
import Amazonka.DataBrew.Types.Recipe
import Amazonka.DataBrew.Types.RecipeAction
import Amazonka.DataBrew.Types.RecipeReference
import Amazonka.DataBrew.Types.RecipeStep
import Amazonka.DataBrew.Types.RecipeVersionErrorDetail
import Amazonka.DataBrew.Types.S3Location
import Amazonka.DataBrew.Types.S3TableOutputOptions
import Amazonka.DataBrew.Types.Sample
import Amazonka.DataBrew.Types.SampleMode
import Amazonka.DataBrew.Types.SampleType
import Amazonka.DataBrew.Types.Schedule
import Amazonka.DataBrew.Types.SessionStatus
import Amazonka.DataBrew.Types.Source
import Amazonka.DataBrew.Types.StatisticOverride
import Amazonka.DataBrew.Types.StatisticsConfiguration
import Amazonka.DataBrew.Types.ViewFrame
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2017-07-25@ of the Amazon Glue DataBrew SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "DataBrew",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "databrew",
      Core._serviceSigningName = "databrew",
      Core._serviceVersion = "2017-07-25",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "DataBrew",
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

-- | The input parameters for this request failed validation.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- | Access to the specified resource was denied.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | Updating or deleting a resource can cause an inconsistent state.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | A service quota is exceeded.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | An internal service failure occurred.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | One or more resources can\'t be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

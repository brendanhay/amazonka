{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DataBrew.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _InternalServerException,
    _ServiceQuotaExceededException,
    _ResourceNotFoundException,
    _ConflictException,
    _ValidationException,

    -- * AnalyticsMode
    AnalyticsMode (..),

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

    -- * ThresholdType
    ThresholdType (..),

    -- * ThresholdUnit
    ThresholdUnit (..),

    -- * ValidationMode
    ValidationMode (..),

    -- * AllowedStatistics
    AllowedStatistics (..),
    newAllowedStatistics,
    allowedStatistics_statistics,

    -- * ColumnSelector
    ColumnSelector (..),
    newColumnSelector,
    columnSelector_name,
    columnSelector_regex,

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
    csvOptions_delimiter,
    csvOptions_headerRow,

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
    dataCatalogOutput_s3Options,
    dataCatalogOutput_databaseOptions,
    dataCatalogOutput_overwrite,
    dataCatalogOutput_catalogId,
    dataCatalogOutput_databaseName,
    dataCatalogOutput_tableName,

    -- * DatabaseInputDefinition
    DatabaseInputDefinition (..),
    newDatabaseInputDefinition,
    databaseInputDefinition_tempDirectory,
    databaseInputDefinition_databaseTableName,
    databaseInputDefinition_queryString,
    databaseInputDefinition_glueConnectionName,

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
    dataset_tags,
    dataset_pathOptions,
    dataset_lastModifiedDate,
    dataset_format,
    dataset_accountId,
    dataset_source,
    dataset_createDate,
    dataset_formatOptions,
    dataset_lastModifiedBy,
    dataset_resourceArn,
    dataset_createdBy,
    dataset_name,
    dataset_input,

    -- * DatasetParameter
    DatasetParameter (..),
    newDatasetParameter,
    datasetParameter_datetimeOptions,
    datasetParameter_filter,
    datasetParameter_createColumn,
    datasetParameter_name,
    datasetParameter_type,

    -- * DatetimeOptions
    DatetimeOptions (..),
    newDatetimeOptions,
    datetimeOptions_localeCode,
    datetimeOptions_timezoneOffset,
    datetimeOptions_format,

    -- * EntityDetectorConfiguration
    EntityDetectorConfiguration (..),
    newEntityDetectorConfiguration,
    entityDetectorConfiguration_allowedStatistics,
    entityDetectorConfiguration_entityTypes,

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
    formatOptions_excel,
    formatOptions_json,
    formatOptions_csv,

    -- * Input
    Input (..),
    newInput,
    input_metadata,
    input_s3InputDefinition,
    input_dataCatalogInputDefinition,
    input_databaseInputDefinition,

    -- * Job
    Job (..),
    newJob,
    job_tags,
    job_encryptionKeyArn,
    job_jobSample,
    job_timeout,
    job_type,
    job_roleArn,
    job_databaseOutputs,
    job_lastModifiedDate,
    job_dataCatalogOutputs,
    job_datasetName,
    job_logSubscription,
    job_maxRetries,
    job_recipeReference,
    job_accountId,
    job_outputs,
    job_createDate,
    job_lastModifiedBy,
    job_resourceArn,
    job_projectName,
    job_createdBy,
    job_maxCapacity,
    job_encryptionMode,
    job_validationConfigurations,
    job_name,

    -- * JobRun
    JobRun (..),
    newJobRun,
    jobRun_jobSample,
    jobRun_databaseOutputs,
    jobRun_startedOn,
    jobRun_errorMessage,
    jobRun_attempt,
    jobRun_dataCatalogOutputs,
    jobRun_jobName,
    jobRun_datasetName,
    jobRun_state,
    jobRun_executionTime,
    jobRun_logSubscription,
    jobRun_completedOn,
    jobRun_startedBy,
    jobRun_recipeReference,
    jobRun_outputs,
    jobRun_runId,
    jobRun_logGroupName,
    jobRun_validationConfigurations,

    -- * JobSample
    JobSample (..),
    newJobSample,
    jobSample_size,
    jobSample_mode,

    -- * JsonOptions
    JsonOptions (..),
    newJsonOptions,
    jsonOptions_multiLine,

    -- * Metadata
    Metadata (..),
    newMetadata,
    metadata_sourceArn,

    -- * Output
    Output (..),
    newOutput,
    output_format,
    output_overwrite,
    output_partitionColumns,
    output_formatOptions,
    output_compressionFormat,
    output_maxOutputFiles,
    output_location,

    -- * OutputFormatOptions
    OutputFormatOptions (..),
    newOutputFormatOptions,
    outputFormatOptions_csv,

    -- * PathOptions
    PathOptions (..),
    newPathOptions,
    pathOptions_lastModifiedDateCondition,
    pathOptions_filesLimit,
    pathOptions_parameters,

    -- * ProfileConfiguration
    ProfileConfiguration (..),
    newProfileConfiguration,
    profileConfiguration_columnStatisticsConfigurations,
    profileConfiguration_datasetStatisticsConfiguration,
    profileConfiguration_profileColumns,
    profileConfiguration_entityDetectorConfiguration,

    -- * Project
    Project (..),
    newProject,
    project_tags,
    project_openedBy,
    project_roleArn,
    project_lastModifiedDate,
    project_datasetName,
    project_accountId,
    project_createDate,
    project_openDate,
    project_lastModifiedBy,
    project_resourceArn,
    project_createdBy,
    project_sample,
    project_name,
    project_recipeName,

    -- * Recipe
    Recipe (..),
    newRecipe,
    recipe_tags,
    recipe_publishedBy,
    recipe_lastModifiedDate,
    recipe_steps,
    recipe_description,
    recipe_createDate,
    recipe_publishedDate,
    recipe_lastModifiedBy,
    recipe_resourceArn,
    recipe_projectName,
    recipe_createdBy,
    recipe_recipeVersion,
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
    recipeVersionErrorDetail_errorMessage,
    recipeVersionErrorDetail_errorCode,
    recipeVersionErrorDetail_recipeVersion,

    -- * Rule
    Rule (..),
    newRule,
    rule_substitutionMap,
    rule_columnSelectors,
    rule_disabled,
    rule_threshold,
    rule_name,
    rule_checkExpression,

    -- * RulesetItem
    RulesetItem (..),
    newRulesetItem,
    rulesetItem_tags,
    rulesetItem_lastModifiedDate,
    rulesetItem_description,
    rulesetItem_accountId,
    rulesetItem_createDate,
    rulesetItem_ruleCount,
    rulesetItem_lastModifiedBy,
    rulesetItem_resourceArn,
    rulesetItem_createdBy,
    rulesetItem_name,
    rulesetItem_targetArn,

    -- * S3Location
    S3Location (..),
    newS3Location,
    s3Location_key,
    s3Location_bucketOwner,
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
    schedule_tags,
    schedule_lastModifiedDate,
    schedule_accountId,
    schedule_createDate,
    schedule_lastModifiedBy,
    schedule_resourceArn,
    schedule_jobNames,
    schedule_cronExpression,
    schedule_createdBy,
    schedule_name,

    -- * StatisticOverride
    StatisticOverride (..),
    newStatisticOverride,
    statisticOverride_statistic,
    statisticOverride_parameters,

    -- * StatisticsConfiguration
    StatisticsConfiguration (..),
    newStatisticsConfiguration,
    statisticsConfiguration_includedStatistics,
    statisticsConfiguration_overrides,

    -- * Threshold
    Threshold (..),
    newThreshold,
    threshold_type,
    threshold_unit,
    threshold_value,

    -- * ValidationConfiguration
    ValidationConfiguration (..),
    newValidationConfiguration,
    validationConfiguration_validationMode,
    validationConfiguration_rulesetArn,

    -- * ViewFrame
    ViewFrame (..),
    newViewFrame,
    viewFrame_analytics,
    viewFrame_startRowIndex,
    viewFrame_columnRange,
    viewFrame_rowRange,
    viewFrame_hiddenColumns,
    viewFrame_startColumnIndex,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DataBrew.Types.AllowedStatistics
import Amazonka.DataBrew.Types.AnalyticsMode
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
import Amazonka.DataBrew.Types.EntityDetectorConfiguration
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
import Amazonka.DataBrew.Types.Metadata
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
import Amazonka.DataBrew.Types.Rule
import Amazonka.DataBrew.Types.RulesetItem
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
import Amazonka.DataBrew.Types.Threshold
import Amazonka.DataBrew.Types.ThresholdType
import Amazonka.DataBrew.Types.ThresholdUnit
import Amazonka.DataBrew.Types.ValidationConfiguration
import Amazonka.DataBrew.Types.ValidationMode
import Amazonka.DataBrew.Types.ViewFrame
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2017-07-25@ of the Amazon Glue DataBrew SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "DataBrew",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "databrew",
      Core.signingName = "databrew",
      Core.version = "2017-07-25",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "DataBrew",
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

-- | Access to the specified resource was denied.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | An internal service failure occurred.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | A service quota is exceeded.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | One or more resources can\'t be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | Updating or deleting a resource can cause an inconsistent state.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The input parameters for this request failed validation.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

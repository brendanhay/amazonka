{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DataBrew.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
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
    dataCatalogInputDefinition_catalogId,
    dataCatalogInputDefinition_tempDirectory,
    dataCatalogInputDefinition_databaseName,
    dataCatalogInputDefinition_tableName,

    -- * DataCatalogOutput
    DataCatalogOutput (..),
    newDataCatalogOutput,
    dataCatalogOutput_catalogId,
    dataCatalogOutput_databaseOptions,
    dataCatalogOutput_overwrite,
    dataCatalogOutput_s3Options,
    dataCatalogOutput_databaseName,
    dataCatalogOutput_tableName,

    -- * DatabaseInputDefinition
    DatabaseInputDefinition (..),
    newDatabaseInputDefinition,
    databaseInputDefinition_databaseTableName,
    databaseInputDefinition_queryString,
    databaseInputDefinition_tempDirectory,
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
    dataset_accountId,
    dataset_createDate,
    dataset_createdBy,
    dataset_format,
    dataset_formatOptions,
    dataset_lastModifiedBy,
    dataset_lastModifiedDate,
    dataset_pathOptions,
    dataset_resourceArn,
    dataset_source,
    dataset_tags,
    dataset_name,
    dataset_input,

    -- * DatasetParameter
    DatasetParameter (..),
    newDatasetParameter,
    datasetParameter_createColumn,
    datasetParameter_datetimeOptions,
    datasetParameter_filter,
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
    excelOptions_headerRow,
    excelOptions_sheetIndexes,
    excelOptions_sheetNames,

    -- * FilesLimit
    FilesLimit (..),
    newFilesLimit,
    filesLimit_order,
    filesLimit_orderedBy,
    filesLimit_maxFiles,

    -- * FilterExpression
    FilterExpression (..),
    newFilterExpression,
    filterExpression_expression,
    filterExpression_valuesMap,

    -- * FormatOptions
    FormatOptions (..),
    newFormatOptions,
    formatOptions_csv,
    formatOptions_excel,
    formatOptions_json,

    -- * Input
    Input (..),
    newInput,
    input_dataCatalogInputDefinition,
    input_databaseInputDefinition,
    input_metadata,
    input_s3InputDefinition,

    -- * Job
    Job (..),
    newJob,
    job_accountId,
    job_createDate,
    job_createdBy,
    job_dataCatalogOutputs,
    job_databaseOutputs,
    job_datasetName,
    job_encryptionKeyArn,
    job_encryptionMode,
    job_jobSample,
    job_lastModifiedBy,
    job_lastModifiedDate,
    job_logSubscription,
    job_maxCapacity,
    job_maxRetries,
    job_outputs,
    job_projectName,
    job_recipeReference,
    job_resourceArn,
    job_roleArn,
    job_tags,
    job_timeout,
    job_type,
    job_validationConfigurations,
    job_name,

    -- * JobRun
    JobRun (..),
    newJobRun,
    jobRun_attempt,
    jobRun_completedOn,
    jobRun_dataCatalogOutputs,
    jobRun_databaseOutputs,
    jobRun_datasetName,
    jobRun_errorMessage,
    jobRun_executionTime,
    jobRun_jobName,
    jobRun_jobSample,
    jobRun_logGroupName,
    jobRun_logSubscription,
    jobRun_outputs,
    jobRun_recipeReference,
    jobRun_runId,
    jobRun_startedBy,
    jobRun_startedOn,
    jobRun_state,
    jobRun_validationConfigurations,

    -- * JobSample
    JobSample (..),
    newJobSample,
    jobSample_mode,
    jobSample_size,

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
    output_compressionFormat,
    output_format,
    output_formatOptions,
    output_maxOutputFiles,
    output_overwrite,
    output_partitionColumns,
    output_location,

    -- * OutputFormatOptions
    OutputFormatOptions (..),
    newOutputFormatOptions,
    outputFormatOptions_csv,

    -- * PathOptions
    PathOptions (..),
    newPathOptions,
    pathOptions_filesLimit,
    pathOptions_lastModifiedDateCondition,
    pathOptions_parameters,

    -- * ProfileConfiguration
    ProfileConfiguration (..),
    newProfileConfiguration,
    profileConfiguration_columnStatisticsConfigurations,
    profileConfiguration_datasetStatisticsConfiguration,
    profileConfiguration_entityDetectorConfiguration,
    profileConfiguration_profileColumns,

    -- * Project
    Project (..),
    newProject,
    project_accountId,
    project_createDate,
    project_createdBy,
    project_datasetName,
    project_lastModifiedBy,
    project_lastModifiedDate,
    project_openDate,
    project_openedBy,
    project_resourceArn,
    project_roleArn,
    project_sample,
    project_tags,
    project_name,
    project_recipeName,

    -- * Recipe
    Recipe (..),
    newRecipe,
    recipe_createDate,
    recipe_createdBy,
    recipe_description,
    recipe_lastModifiedBy,
    recipe_lastModifiedDate,
    recipe_projectName,
    recipe_publishedBy,
    recipe_publishedDate,
    recipe_recipeVersion,
    recipe_resourceArn,
    recipe_steps,
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
    recipeVersionErrorDetail_errorCode,
    recipeVersionErrorDetail_errorMessage,
    recipeVersionErrorDetail_recipeVersion,

    -- * Rule
    Rule (..),
    newRule,
    rule_columnSelectors,
    rule_disabled,
    rule_substitutionMap,
    rule_threshold,
    rule_name,
    rule_checkExpression,

    -- * RulesetItem
    RulesetItem (..),
    newRulesetItem,
    rulesetItem_accountId,
    rulesetItem_createDate,
    rulesetItem_createdBy,
    rulesetItem_description,
    rulesetItem_lastModifiedBy,
    rulesetItem_lastModifiedDate,
    rulesetItem_resourceArn,
    rulesetItem_ruleCount,
    rulesetItem_tags,
    rulesetItem_name,
    rulesetItem_targetArn,

    -- * S3Location
    S3Location (..),
    newS3Location,
    s3Location_bucketOwner,
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
    schedule_accountId,
    schedule_createDate,
    schedule_createdBy,
    schedule_cronExpression,
    schedule_jobNames,
    schedule_lastModifiedBy,
    schedule_lastModifiedDate,
    schedule_resourceArn,
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
    viewFrame_columnRange,
    viewFrame_hiddenColumns,
    viewFrame_rowRange,
    viewFrame_startRowIndex,
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

-- | Access to the specified resource was denied.
_AccessDeniedException :: Core.AsError a => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | Updating or deleting a resource can cause an inconsistent state.
_ConflictException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | An internal service failure occurred.
_InternalServerException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | One or more resources can\'t be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | A service quota is exceeded.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The input parameters for this request failed validation.
_ValidationException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

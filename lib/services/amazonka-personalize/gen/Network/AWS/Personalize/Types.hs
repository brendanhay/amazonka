{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Personalize.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ResourceAlreadyExistsException,
    _InvalidNextTokenException,
    _InvalidInputException,
    _ResourceNotFoundException,
    _LimitExceededException,
    _ResourceInUseException,

    -- * IngestionMode
    IngestionMode (..),

    -- * ObjectiveSensitivity
    ObjectiveSensitivity (..),

    -- * RecipeProvider
    RecipeProvider (..),

    -- * TrainingMode
    TrainingMode (..),

    -- * Algorithm
    Algorithm (..),
    newAlgorithm,
    algorithm_defaultHyperParameters,
    algorithm_algorithmArn,
    algorithm_trainingInputMode,
    algorithm_defaultHyperParameterRanges,
    algorithm_algorithmImage,
    algorithm_lastUpdatedDateTime,
    algorithm_name,
    algorithm_creationDateTime,
    algorithm_defaultResourceConfig,
    algorithm_roleArn,

    -- * AlgorithmImage
    AlgorithmImage (..),
    newAlgorithmImage,
    algorithmImage_name,
    algorithmImage_dockerURI,

    -- * AutoMLConfig
    AutoMLConfig (..),
    newAutoMLConfig,
    autoMLConfig_recipeList,
    autoMLConfig_metricName,

    -- * AutoMLResult
    AutoMLResult (..),
    newAutoMLResult,
    autoMLResult_bestRecipeArn,

    -- * BatchInferenceJob
    BatchInferenceJob (..),
    newBatchInferenceJob,
    batchInferenceJob_failureReason,
    batchInferenceJob_status,
    batchInferenceJob_jobOutput,
    batchInferenceJob_jobName,
    batchInferenceJob_lastUpdatedDateTime,
    batchInferenceJob_numResults,
    batchInferenceJob_batchInferenceJobConfig,
    batchInferenceJob_batchInferenceJobArn,
    batchInferenceJob_filterArn,
    batchInferenceJob_creationDateTime,
    batchInferenceJob_solutionVersionArn,
    batchInferenceJob_roleArn,
    batchInferenceJob_jobInput,

    -- * BatchInferenceJobConfig
    BatchInferenceJobConfig (..),
    newBatchInferenceJobConfig,
    batchInferenceJobConfig_itemExplorationConfig,

    -- * BatchInferenceJobInput
    BatchInferenceJobInput (..),
    newBatchInferenceJobInput,
    batchInferenceJobInput_s3DataSource,

    -- * BatchInferenceJobOutput
    BatchInferenceJobOutput (..),
    newBatchInferenceJobOutput,
    batchInferenceJobOutput_s3DataDestination,

    -- * BatchInferenceJobSummary
    BatchInferenceJobSummary (..),
    newBatchInferenceJobSummary,
    batchInferenceJobSummary_failureReason,
    batchInferenceJobSummary_status,
    batchInferenceJobSummary_jobName,
    batchInferenceJobSummary_lastUpdatedDateTime,
    batchInferenceJobSummary_batchInferenceJobArn,
    batchInferenceJobSummary_creationDateTime,
    batchInferenceJobSummary_solutionVersionArn,

    -- * Campaign
    Campaign (..),
    newCampaign,
    campaign_failureReason,
    campaign_status,
    campaign_lastUpdatedDateTime,
    campaign_campaignConfig,
    campaign_latestCampaignUpdate,
    campaign_name,
    campaign_minProvisionedTPS,
    campaign_creationDateTime,
    campaign_campaignArn,
    campaign_solutionVersionArn,

    -- * CampaignConfig
    CampaignConfig (..),
    newCampaignConfig,
    campaignConfig_itemExplorationConfig,

    -- * CampaignSummary
    CampaignSummary (..),
    newCampaignSummary,
    campaignSummary_failureReason,
    campaignSummary_status,
    campaignSummary_lastUpdatedDateTime,
    campaignSummary_name,
    campaignSummary_creationDateTime,
    campaignSummary_campaignArn,

    -- * CampaignUpdateSummary
    CampaignUpdateSummary (..),
    newCampaignUpdateSummary,
    campaignUpdateSummary_failureReason,
    campaignUpdateSummary_status,
    campaignUpdateSummary_lastUpdatedDateTime,
    campaignUpdateSummary_campaignConfig,
    campaignUpdateSummary_minProvisionedTPS,
    campaignUpdateSummary_creationDateTime,
    campaignUpdateSummary_solutionVersionArn,

    -- * CategoricalHyperParameterRange
    CategoricalHyperParameterRange (..),
    newCategoricalHyperParameterRange,
    categoricalHyperParameterRange_values,
    categoricalHyperParameterRange_name,

    -- * ContinuousHyperParameterRange
    ContinuousHyperParameterRange (..),
    newContinuousHyperParameterRange,
    continuousHyperParameterRange_maxValue,
    continuousHyperParameterRange_name,
    continuousHyperParameterRange_minValue,

    -- * DataSource
    DataSource (..),
    newDataSource,
    dataSource_dataLocation,

    -- * Dataset
    Dataset (..),
    newDataset,
    dataset_status,
    dataset_datasetArn,
    dataset_lastUpdatedDateTime,
    dataset_schemaArn,
    dataset_name,
    dataset_datasetType,
    dataset_creationDateTime,
    dataset_datasetGroupArn,

    -- * DatasetExportJob
    DatasetExportJob (..),
    newDatasetExportJob,
    datasetExportJob_failureReason,
    datasetExportJob_status,
    datasetExportJob_datasetExportJobArn,
    datasetExportJob_datasetArn,
    datasetExportJob_jobOutput,
    datasetExportJob_jobName,
    datasetExportJob_lastUpdatedDateTime,
    datasetExportJob_ingestionMode,
    datasetExportJob_creationDateTime,
    datasetExportJob_roleArn,

    -- * DatasetExportJobOutput
    DatasetExportJobOutput (..),
    newDatasetExportJobOutput,
    datasetExportJobOutput_s3DataDestination,

    -- * DatasetExportJobSummary
    DatasetExportJobSummary (..),
    newDatasetExportJobSummary,
    datasetExportJobSummary_failureReason,
    datasetExportJobSummary_status,
    datasetExportJobSummary_datasetExportJobArn,
    datasetExportJobSummary_jobName,
    datasetExportJobSummary_lastUpdatedDateTime,
    datasetExportJobSummary_creationDateTime,

    -- * DatasetGroup
    DatasetGroup (..),
    newDatasetGroup,
    datasetGroup_failureReason,
    datasetGroup_status,
    datasetGroup_kmsKeyArn,
    datasetGroup_lastUpdatedDateTime,
    datasetGroup_name,
    datasetGroup_creationDateTime,
    datasetGroup_datasetGroupArn,
    datasetGroup_roleArn,

    -- * DatasetGroupSummary
    DatasetGroupSummary (..),
    newDatasetGroupSummary,
    datasetGroupSummary_failureReason,
    datasetGroupSummary_status,
    datasetGroupSummary_lastUpdatedDateTime,
    datasetGroupSummary_name,
    datasetGroupSummary_creationDateTime,
    datasetGroupSummary_datasetGroupArn,

    -- * DatasetImportJob
    DatasetImportJob (..),
    newDatasetImportJob,
    datasetImportJob_failureReason,
    datasetImportJob_status,
    datasetImportJob_datasetArn,
    datasetImportJob_jobName,
    datasetImportJob_lastUpdatedDateTime,
    datasetImportJob_datasetImportJobArn,
    datasetImportJob_dataSource,
    datasetImportJob_creationDateTime,
    datasetImportJob_roleArn,

    -- * DatasetImportJobSummary
    DatasetImportJobSummary (..),
    newDatasetImportJobSummary,
    datasetImportJobSummary_failureReason,
    datasetImportJobSummary_status,
    datasetImportJobSummary_jobName,
    datasetImportJobSummary_lastUpdatedDateTime,
    datasetImportJobSummary_datasetImportJobArn,
    datasetImportJobSummary_creationDateTime,

    -- * DatasetSchema
    DatasetSchema (..),
    newDatasetSchema,
    datasetSchema_lastUpdatedDateTime,
    datasetSchema_schema,
    datasetSchema_schemaArn,
    datasetSchema_name,
    datasetSchema_creationDateTime,

    -- * DatasetSchemaSummary
    DatasetSchemaSummary (..),
    newDatasetSchemaSummary,
    datasetSchemaSummary_lastUpdatedDateTime,
    datasetSchemaSummary_schemaArn,
    datasetSchemaSummary_name,
    datasetSchemaSummary_creationDateTime,

    -- * DatasetSummary
    DatasetSummary (..),
    newDatasetSummary,
    datasetSummary_status,
    datasetSummary_datasetArn,
    datasetSummary_lastUpdatedDateTime,
    datasetSummary_name,
    datasetSummary_datasetType,
    datasetSummary_creationDateTime,

    -- * DefaultCategoricalHyperParameterRange
    DefaultCategoricalHyperParameterRange (..),
    newDefaultCategoricalHyperParameterRange,
    defaultCategoricalHyperParameterRange_isTunable,
    defaultCategoricalHyperParameterRange_values,
    defaultCategoricalHyperParameterRange_name,

    -- * DefaultContinuousHyperParameterRange
    DefaultContinuousHyperParameterRange (..),
    newDefaultContinuousHyperParameterRange,
    defaultContinuousHyperParameterRange_maxValue,
    defaultContinuousHyperParameterRange_isTunable,
    defaultContinuousHyperParameterRange_name,
    defaultContinuousHyperParameterRange_minValue,

    -- * DefaultHyperParameterRanges
    DefaultHyperParameterRanges (..),
    newDefaultHyperParameterRanges,
    defaultHyperParameterRanges_integerHyperParameterRanges,
    defaultHyperParameterRanges_categoricalHyperParameterRanges,
    defaultHyperParameterRanges_continuousHyperParameterRanges,

    -- * DefaultIntegerHyperParameterRange
    DefaultIntegerHyperParameterRange (..),
    newDefaultIntegerHyperParameterRange,
    defaultIntegerHyperParameterRange_maxValue,
    defaultIntegerHyperParameterRange_isTunable,
    defaultIntegerHyperParameterRange_name,
    defaultIntegerHyperParameterRange_minValue,

    -- * EventTracker
    EventTracker (..),
    newEventTracker,
    eventTracker_status,
    eventTracker_trackingId,
    eventTracker_lastUpdatedDateTime,
    eventTracker_accountId,
    eventTracker_name,
    eventTracker_creationDateTime,
    eventTracker_datasetGroupArn,
    eventTracker_eventTrackerArn,

    -- * EventTrackerSummary
    EventTrackerSummary (..),
    newEventTrackerSummary,
    eventTrackerSummary_status,
    eventTrackerSummary_lastUpdatedDateTime,
    eventTrackerSummary_name,
    eventTrackerSummary_creationDateTime,
    eventTrackerSummary_eventTrackerArn,

    -- * FeatureTransformation
    FeatureTransformation (..),
    newFeatureTransformation,
    featureTransformation_status,
    featureTransformation_featureTransformationArn,
    featureTransformation_lastUpdatedDateTime,
    featureTransformation_name,
    featureTransformation_creationDateTime,
    featureTransformation_defaultParameters,

    -- * Filter
    Filter (..),
    newFilter,
    filter_failureReason,
    filter_status,
    filter_filterExpression,
    filter_lastUpdatedDateTime,
    filter_name,
    filter_filterArn,
    filter_creationDateTime,
    filter_datasetGroupArn,

    -- * FilterSummary
    FilterSummary (..),
    newFilterSummary,
    filterSummary_failureReason,
    filterSummary_status,
    filterSummary_lastUpdatedDateTime,
    filterSummary_name,
    filterSummary_filterArn,
    filterSummary_creationDateTime,
    filterSummary_datasetGroupArn,

    -- * HPOConfig
    HPOConfig (..),
    newHPOConfig,
    hPOConfig_algorithmHyperParameterRanges,
    hPOConfig_hpoResourceConfig,
    hPOConfig_hpoObjective,

    -- * HPOObjective
    HPOObjective (..),
    newHPOObjective,
    hPOObjective_metricName,
    hPOObjective_type,
    hPOObjective_metricRegex,

    -- * HPOResourceConfig
    HPOResourceConfig (..),
    newHPOResourceConfig,
    hPOResourceConfig_maxNumberOfTrainingJobs,
    hPOResourceConfig_maxParallelTrainingJobs,

    -- * HyperParameterRanges
    HyperParameterRanges (..),
    newHyperParameterRanges,
    hyperParameterRanges_integerHyperParameterRanges,
    hyperParameterRanges_categoricalHyperParameterRanges,
    hyperParameterRanges_continuousHyperParameterRanges,

    -- * IntegerHyperParameterRange
    IntegerHyperParameterRange (..),
    newIntegerHyperParameterRange,
    integerHyperParameterRange_maxValue,
    integerHyperParameterRange_name,
    integerHyperParameterRange_minValue,

    -- * OptimizationObjective
    OptimizationObjective (..),
    newOptimizationObjective,
    optimizationObjective_itemAttribute,
    optimizationObjective_objectiveSensitivity,

    -- * Recipe
    Recipe (..),
    newRecipe,
    recipe_status,
    recipe_algorithmArn,
    recipe_recipeArn,
    recipe_featureTransformationArn,
    recipe_lastUpdatedDateTime,
    recipe_name,
    recipe_creationDateTime,
    recipe_recipeType,
    recipe_description,

    -- * RecipeSummary
    RecipeSummary (..),
    newRecipeSummary,
    recipeSummary_status,
    recipeSummary_recipeArn,
    recipeSummary_lastUpdatedDateTime,
    recipeSummary_name,
    recipeSummary_creationDateTime,

    -- * S3DataConfig
    S3DataConfig (..),
    newS3DataConfig,
    s3DataConfig_kmsKeyArn,
    s3DataConfig_path,

    -- * Solution
    Solution (..),
    newSolution,
    solution_solutionArn,
    solution_status,
    solution_performAutoML,
    solution_recipeArn,
    solution_lastUpdatedDateTime,
    solution_eventType,
    solution_name,
    solution_autoMLResult,
    solution_creationDateTime,
    solution_datasetGroupArn,
    solution_latestSolutionVersion,
    solution_solutionConfig,
    solution_performHPO,

    -- * SolutionConfig
    SolutionConfig (..),
    newSolutionConfig,
    solutionConfig_featureTransformationParameters,
    solutionConfig_hpoConfig,
    solutionConfig_eventValueThreshold,
    solutionConfig_autoMLConfig,
    solutionConfig_algorithmHyperParameters,
    solutionConfig_optimizationObjective,

    -- * SolutionSummary
    SolutionSummary (..),
    newSolutionSummary,
    solutionSummary_solutionArn,
    solutionSummary_status,
    solutionSummary_lastUpdatedDateTime,
    solutionSummary_name,
    solutionSummary_creationDateTime,

    -- * SolutionVersion
    SolutionVersion (..),
    newSolutionVersion,
    solutionVersion_failureReason,
    solutionVersion_solutionArn,
    solutionVersion_status,
    solutionVersion_performAutoML,
    solutionVersion_tunedHPOParams,
    solutionVersion_recipeArn,
    solutionVersion_lastUpdatedDateTime,
    solutionVersion_eventType,
    solutionVersion_creationDateTime,
    solutionVersion_datasetGroupArn,
    solutionVersion_trainingMode,
    solutionVersion_trainingHours,
    solutionVersion_solutionConfig,
    solutionVersion_performHPO,
    solutionVersion_solutionVersionArn,

    -- * SolutionVersionSummary
    SolutionVersionSummary (..),
    newSolutionVersionSummary,
    solutionVersionSummary_failureReason,
    solutionVersionSummary_status,
    solutionVersionSummary_lastUpdatedDateTime,
    solutionVersionSummary_creationDateTime,
    solutionVersionSummary_solutionVersionArn,

    -- * TunedHPOParams
    TunedHPOParams (..),
    newTunedHPOParams,
    tunedHPOParams_algorithmHyperParameters,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Personalize.Types.Algorithm
import Amazonka.Personalize.Types.AlgorithmImage
import Amazonka.Personalize.Types.AutoMLConfig
import Amazonka.Personalize.Types.AutoMLResult
import Amazonka.Personalize.Types.BatchInferenceJob
import Amazonka.Personalize.Types.BatchInferenceJobConfig
import Amazonka.Personalize.Types.BatchInferenceJobInput
import Amazonka.Personalize.Types.BatchInferenceJobOutput
import Amazonka.Personalize.Types.BatchInferenceJobSummary
import Amazonka.Personalize.Types.Campaign
import Amazonka.Personalize.Types.CampaignConfig
import Amazonka.Personalize.Types.CampaignSummary
import Amazonka.Personalize.Types.CampaignUpdateSummary
import Amazonka.Personalize.Types.CategoricalHyperParameterRange
import Amazonka.Personalize.Types.ContinuousHyperParameterRange
import Amazonka.Personalize.Types.DataSource
import Amazonka.Personalize.Types.Dataset
import Amazonka.Personalize.Types.DatasetExportJob
import Amazonka.Personalize.Types.DatasetExportJobOutput
import Amazonka.Personalize.Types.DatasetExportJobSummary
import Amazonka.Personalize.Types.DatasetGroup
import Amazonka.Personalize.Types.DatasetGroupSummary
import Amazonka.Personalize.Types.DatasetImportJob
import Amazonka.Personalize.Types.DatasetImportJobSummary
import Amazonka.Personalize.Types.DatasetSchema
import Amazonka.Personalize.Types.DatasetSchemaSummary
import Amazonka.Personalize.Types.DatasetSummary
import Amazonka.Personalize.Types.DefaultCategoricalHyperParameterRange
import Amazonka.Personalize.Types.DefaultContinuousHyperParameterRange
import Amazonka.Personalize.Types.DefaultHyperParameterRanges
import Amazonka.Personalize.Types.DefaultIntegerHyperParameterRange
import Amazonka.Personalize.Types.EventTracker
import Amazonka.Personalize.Types.EventTrackerSummary
import Amazonka.Personalize.Types.FeatureTransformation
import Amazonka.Personalize.Types.Filter
import Amazonka.Personalize.Types.FilterSummary
import Amazonka.Personalize.Types.HPOConfig
import Amazonka.Personalize.Types.HPOObjective
import Amazonka.Personalize.Types.HPOResourceConfig
import Amazonka.Personalize.Types.HyperParameterRanges
import Amazonka.Personalize.Types.IngestionMode
import Amazonka.Personalize.Types.IntegerHyperParameterRange
import Amazonka.Personalize.Types.ObjectiveSensitivity
import Amazonka.Personalize.Types.OptimizationObjective
import Amazonka.Personalize.Types.Recipe
import Amazonka.Personalize.Types.RecipeProvider
import Amazonka.Personalize.Types.RecipeSummary
import Amazonka.Personalize.Types.S3DataConfig
import Amazonka.Personalize.Types.Solution
import Amazonka.Personalize.Types.SolutionConfig
import Amazonka.Personalize.Types.SolutionSummary
import Amazonka.Personalize.Types.SolutionVersion
import Amazonka.Personalize.Types.SolutionVersionSummary
import Amazonka.Personalize.Types.TrainingMode
import Amazonka.Personalize.Types.TunedHPOParams
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-05-22@ of the Amazon Personalize SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "Personalize",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "personalize",
      Core._serviceSigningName = "personalize",
      Core._serviceVersion = "2018-05-22",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "Personalize",
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

-- | The specified resource already exists.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"

-- | The token is not valid.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"

-- | Provide a valid value for the field or parameter.
_InvalidInputException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInputException =
  Core._MatchServiceError
    defaultService
    "InvalidInputException"

-- | Could not find the specified resource.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The limit on the number of requests per second has been exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The specified resource is in use.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"

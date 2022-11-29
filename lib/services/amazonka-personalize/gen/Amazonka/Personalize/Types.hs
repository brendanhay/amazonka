{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Personalize.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ResourceAlreadyExistsException,
    _InvalidInputException,
    _TooManyTagsException,
    _TooManyTagKeysException,
    _ResourceNotFoundException,
    _ResourceInUseException,
    _LimitExceededException,
    _InvalidNextTokenException,

    -- * Domain
    Domain (..),

    -- * ImportMode
    ImportMode (..),

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
    algorithm_algorithmImage,
    algorithm_name,
    algorithm_roleArn,
    algorithm_defaultHyperParameterRanges,
    algorithm_creationDateTime,
    algorithm_algorithmArn,
    algorithm_trainingInputMode,
    algorithm_defaultResourceConfig,
    algorithm_defaultHyperParameters,
    algorithm_lastUpdatedDateTime,

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
    batchInferenceJob_roleArn,
    batchInferenceJob_filterArn,
    batchInferenceJob_jobOutput,
    batchInferenceJob_creationDateTime,
    batchInferenceJob_jobName,
    batchInferenceJob_numResults,
    batchInferenceJob_status,
    batchInferenceJob_batchInferenceJobConfig,
    batchInferenceJob_batchInferenceJobArn,
    batchInferenceJob_solutionVersionArn,
    batchInferenceJob_jobInput,
    batchInferenceJob_lastUpdatedDateTime,
    batchInferenceJob_failureReason,

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
    batchInferenceJobSummary_creationDateTime,
    batchInferenceJobSummary_jobName,
    batchInferenceJobSummary_status,
    batchInferenceJobSummary_batchInferenceJobArn,
    batchInferenceJobSummary_solutionVersionArn,
    batchInferenceJobSummary_lastUpdatedDateTime,
    batchInferenceJobSummary_failureReason,

    -- * BatchSegmentJob
    BatchSegmentJob (..),
    newBatchSegmentJob,
    batchSegmentJob_roleArn,
    batchSegmentJob_filterArn,
    batchSegmentJob_jobOutput,
    batchSegmentJob_creationDateTime,
    batchSegmentJob_jobName,
    batchSegmentJob_numResults,
    batchSegmentJob_status,
    batchSegmentJob_solutionVersionArn,
    batchSegmentJob_jobInput,
    batchSegmentJob_lastUpdatedDateTime,
    batchSegmentJob_failureReason,
    batchSegmentJob_batchSegmentJobArn,

    -- * BatchSegmentJobInput
    BatchSegmentJobInput (..),
    newBatchSegmentJobInput,
    batchSegmentJobInput_s3DataSource,

    -- * BatchSegmentJobOutput
    BatchSegmentJobOutput (..),
    newBatchSegmentJobOutput,
    batchSegmentJobOutput_s3DataDestination,

    -- * BatchSegmentJobSummary
    BatchSegmentJobSummary (..),
    newBatchSegmentJobSummary,
    batchSegmentJobSummary_creationDateTime,
    batchSegmentJobSummary_jobName,
    batchSegmentJobSummary_status,
    batchSegmentJobSummary_solutionVersionArn,
    batchSegmentJobSummary_lastUpdatedDateTime,
    batchSegmentJobSummary_failureReason,
    batchSegmentJobSummary_batchSegmentJobArn,

    -- * Campaign
    Campaign (..),
    newCampaign,
    campaign_name,
    campaign_creationDateTime,
    campaign_latestCampaignUpdate,
    campaign_campaignConfig,
    campaign_status,
    campaign_minProvisionedTPS,
    campaign_solutionVersionArn,
    campaign_campaignArn,
    campaign_lastUpdatedDateTime,
    campaign_failureReason,

    -- * CampaignConfig
    CampaignConfig (..),
    newCampaignConfig,
    campaignConfig_itemExplorationConfig,

    -- * CampaignSummary
    CampaignSummary (..),
    newCampaignSummary,
    campaignSummary_name,
    campaignSummary_creationDateTime,
    campaignSummary_status,
    campaignSummary_campaignArn,
    campaignSummary_lastUpdatedDateTime,
    campaignSummary_failureReason,

    -- * CampaignUpdateSummary
    CampaignUpdateSummary (..),
    newCampaignUpdateSummary,
    campaignUpdateSummary_creationDateTime,
    campaignUpdateSummary_campaignConfig,
    campaignUpdateSummary_status,
    campaignUpdateSummary_minProvisionedTPS,
    campaignUpdateSummary_solutionVersionArn,
    campaignUpdateSummary_lastUpdatedDateTime,
    campaignUpdateSummary_failureReason,

    -- * CategoricalHyperParameterRange
    CategoricalHyperParameterRange (..),
    newCategoricalHyperParameterRange,
    categoricalHyperParameterRange_name,
    categoricalHyperParameterRange_values,

    -- * ContinuousHyperParameterRange
    ContinuousHyperParameterRange (..),
    newContinuousHyperParameterRange,
    continuousHyperParameterRange_name,
    continuousHyperParameterRange_minValue,
    continuousHyperParameterRange_maxValue,

    -- * DataSource
    DataSource (..),
    newDataSource,
    dataSource_dataLocation,

    -- * Dataset
    Dataset (..),
    newDataset,
    dataset_name,
    dataset_creationDateTime,
    dataset_datasetType,
    dataset_status,
    dataset_datasetArn,
    dataset_schemaArn,
    dataset_datasetGroupArn,
    dataset_lastUpdatedDateTime,

    -- * DatasetExportJob
    DatasetExportJob (..),
    newDatasetExportJob,
    datasetExportJob_roleArn,
    datasetExportJob_jobOutput,
    datasetExportJob_creationDateTime,
    datasetExportJob_jobName,
    datasetExportJob_status,
    datasetExportJob_datasetArn,
    datasetExportJob_ingestionMode,
    datasetExportJob_datasetExportJobArn,
    datasetExportJob_lastUpdatedDateTime,
    datasetExportJob_failureReason,

    -- * DatasetExportJobOutput
    DatasetExportJobOutput (..),
    newDatasetExportJobOutput,
    datasetExportJobOutput_s3DataDestination,

    -- * DatasetExportJobSummary
    DatasetExportJobSummary (..),
    newDatasetExportJobSummary,
    datasetExportJobSummary_creationDateTime,
    datasetExportJobSummary_jobName,
    datasetExportJobSummary_status,
    datasetExportJobSummary_datasetExportJobArn,
    datasetExportJobSummary_lastUpdatedDateTime,
    datasetExportJobSummary_failureReason,

    -- * DatasetGroup
    DatasetGroup (..),
    newDatasetGroup,
    datasetGroup_name,
    datasetGroup_roleArn,
    datasetGroup_creationDateTime,
    datasetGroup_domain,
    datasetGroup_status,
    datasetGroup_kmsKeyArn,
    datasetGroup_datasetGroupArn,
    datasetGroup_lastUpdatedDateTime,
    datasetGroup_failureReason,

    -- * DatasetGroupSummary
    DatasetGroupSummary (..),
    newDatasetGroupSummary,
    datasetGroupSummary_name,
    datasetGroupSummary_creationDateTime,
    datasetGroupSummary_domain,
    datasetGroupSummary_status,
    datasetGroupSummary_datasetGroupArn,
    datasetGroupSummary_lastUpdatedDateTime,
    datasetGroupSummary_failureReason,

    -- * DatasetImportJob
    DatasetImportJob (..),
    newDatasetImportJob,
    datasetImportJob_roleArn,
    datasetImportJob_creationDateTime,
    datasetImportJob_jobName,
    datasetImportJob_status,
    datasetImportJob_publishAttributionMetricsToS3,
    datasetImportJob_datasetArn,
    datasetImportJob_datasetImportJobArn,
    datasetImportJob_dataSource,
    datasetImportJob_importMode,
    datasetImportJob_lastUpdatedDateTime,
    datasetImportJob_failureReason,

    -- * DatasetImportJobSummary
    DatasetImportJobSummary (..),
    newDatasetImportJobSummary,
    datasetImportJobSummary_creationDateTime,
    datasetImportJobSummary_jobName,
    datasetImportJobSummary_status,
    datasetImportJobSummary_datasetImportJobArn,
    datasetImportJobSummary_importMode,
    datasetImportJobSummary_lastUpdatedDateTime,
    datasetImportJobSummary_failureReason,

    -- * DatasetSchema
    DatasetSchema (..),
    newDatasetSchema,
    datasetSchema_name,
    datasetSchema_creationDateTime,
    datasetSchema_domain,
    datasetSchema_schemaArn,
    datasetSchema_schema,
    datasetSchema_lastUpdatedDateTime,

    -- * DatasetSchemaSummary
    DatasetSchemaSummary (..),
    newDatasetSchemaSummary,
    datasetSchemaSummary_name,
    datasetSchemaSummary_creationDateTime,
    datasetSchemaSummary_domain,
    datasetSchemaSummary_schemaArn,
    datasetSchemaSummary_lastUpdatedDateTime,

    -- * DatasetSummary
    DatasetSummary (..),
    newDatasetSummary,
    datasetSummary_name,
    datasetSummary_creationDateTime,
    datasetSummary_datasetType,
    datasetSummary_status,
    datasetSummary_datasetArn,
    datasetSummary_lastUpdatedDateTime,

    -- * DefaultCategoricalHyperParameterRange
    DefaultCategoricalHyperParameterRange (..),
    newDefaultCategoricalHyperParameterRange,
    defaultCategoricalHyperParameterRange_name,
    defaultCategoricalHyperParameterRange_isTunable,
    defaultCategoricalHyperParameterRange_values,

    -- * DefaultContinuousHyperParameterRange
    DefaultContinuousHyperParameterRange (..),
    newDefaultContinuousHyperParameterRange,
    defaultContinuousHyperParameterRange_name,
    defaultContinuousHyperParameterRange_minValue,
    defaultContinuousHyperParameterRange_isTunable,
    defaultContinuousHyperParameterRange_maxValue,

    -- * DefaultHyperParameterRanges
    DefaultHyperParameterRanges (..),
    newDefaultHyperParameterRanges,
    defaultHyperParameterRanges_integerHyperParameterRanges,
    defaultHyperParameterRanges_categoricalHyperParameterRanges,
    defaultHyperParameterRanges_continuousHyperParameterRanges,

    -- * DefaultIntegerHyperParameterRange
    DefaultIntegerHyperParameterRange (..),
    newDefaultIntegerHyperParameterRange,
    defaultIntegerHyperParameterRange_name,
    defaultIntegerHyperParameterRange_minValue,
    defaultIntegerHyperParameterRange_isTunable,
    defaultIntegerHyperParameterRange_maxValue,

    -- * EventTracker
    EventTracker (..),
    newEventTracker,
    eventTracker_name,
    eventTracker_creationDateTime,
    eventTracker_trackingId,
    eventTracker_eventTrackerArn,
    eventTracker_status,
    eventTracker_accountId,
    eventTracker_datasetGroupArn,
    eventTracker_lastUpdatedDateTime,

    -- * EventTrackerSummary
    EventTrackerSummary (..),
    newEventTrackerSummary,
    eventTrackerSummary_name,
    eventTrackerSummary_creationDateTime,
    eventTrackerSummary_eventTrackerArn,
    eventTrackerSummary_status,
    eventTrackerSummary_lastUpdatedDateTime,

    -- * FeatureTransformation
    FeatureTransformation (..),
    newFeatureTransformation,
    featureTransformation_name,
    featureTransformation_creationDateTime,
    featureTransformation_featureTransformationArn,
    featureTransformation_status,
    featureTransformation_defaultParameters,
    featureTransformation_lastUpdatedDateTime,

    -- * Filter
    Filter (..),
    newFilter,
    filter_name,
    filter_filterArn,
    filter_creationDateTime,
    filter_filterExpression,
    filter_status,
    filter_datasetGroupArn,
    filter_lastUpdatedDateTime,
    filter_failureReason,

    -- * FilterSummary
    FilterSummary (..),
    newFilterSummary,
    filterSummary_name,
    filterSummary_filterArn,
    filterSummary_creationDateTime,
    filterSummary_status,
    filterSummary_datasetGroupArn,
    filterSummary_lastUpdatedDateTime,
    filterSummary_failureReason,

    -- * HPOConfig
    HPOConfig (..),
    newHPOConfig,
    hPOConfig_algorithmHyperParameterRanges,
    hPOConfig_hpoObjective,
    hPOConfig_hpoResourceConfig,

    -- * HPOObjective
    HPOObjective (..),
    newHPOObjective,
    hPOObjective_type,
    hPOObjective_metricName,
    hPOObjective_metricRegex,

    -- * HPOResourceConfig
    HPOResourceConfig (..),
    newHPOResourceConfig,
    hPOResourceConfig_maxParallelTrainingJobs,
    hPOResourceConfig_maxNumberOfTrainingJobs,

    -- * HyperParameterRanges
    HyperParameterRanges (..),
    newHyperParameterRanges,
    hyperParameterRanges_integerHyperParameterRanges,
    hyperParameterRanges_categoricalHyperParameterRanges,
    hyperParameterRanges_continuousHyperParameterRanges,

    -- * IntegerHyperParameterRange
    IntegerHyperParameterRange (..),
    newIntegerHyperParameterRange,
    integerHyperParameterRange_name,
    integerHyperParameterRange_minValue,
    integerHyperParameterRange_maxValue,

    -- * MetricAttribute
    MetricAttribute (..),
    newMetricAttribute,
    metricAttribute_eventType,
    metricAttribute_metricName,
    metricAttribute_expression,

    -- * MetricAttribution
    MetricAttribution (..),
    newMetricAttribution,
    metricAttribution_name,
    metricAttribution_creationDateTime,
    metricAttribution_status,
    metricAttribution_metricsOutputConfig,
    metricAttribution_metricAttributionArn,
    metricAttribution_datasetGroupArn,
    metricAttribution_lastUpdatedDateTime,
    metricAttribution_failureReason,

    -- * MetricAttributionOutput
    MetricAttributionOutput (..),
    newMetricAttributionOutput,
    metricAttributionOutput_s3DataDestination,
    metricAttributionOutput_roleArn,

    -- * MetricAttributionSummary
    MetricAttributionSummary (..),
    newMetricAttributionSummary,
    metricAttributionSummary_name,
    metricAttributionSummary_creationDateTime,
    metricAttributionSummary_status,
    metricAttributionSummary_metricAttributionArn,
    metricAttributionSummary_lastUpdatedDateTime,
    metricAttributionSummary_failureReason,

    -- * OptimizationObjective
    OptimizationObjective (..),
    newOptimizationObjective,
    optimizationObjective_objectiveSensitivity,
    optimizationObjective_itemAttribute,

    -- * Recipe
    Recipe (..),
    newRecipe,
    recipe_name,
    recipe_creationDateTime,
    recipe_featureTransformationArn,
    recipe_status,
    recipe_description,
    recipe_algorithmArn,
    recipe_recipeType,
    recipe_recipeArn,
    recipe_lastUpdatedDateTime,

    -- * RecipeSummary
    RecipeSummary (..),
    newRecipeSummary,
    recipeSummary_name,
    recipeSummary_creationDateTime,
    recipeSummary_domain,
    recipeSummary_status,
    recipeSummary_recipeArn,
    recipeSummary_lastUpdatedDateTime,

    -- * Recommender
    Recommender (..),
    newRecommender,
    recommender_name,
    recommender_creationDateTime,
    recommender_recommenderArn,
    recommender_recommenderConfig,
    recommender_latestRecommenderUpdate,
    recommender_status,
    recommender_modelMetrics,
    recommender_datasetGroupArn,
    recommender_recipeArn,
    recommender_lastUpdatedDateTime,
    recommender_failureReason,

    -- * RecommenderConfig
    RecommenderConfig (..),
    newRecommenderConfig,
    recommenderConfig_minRecommendationRequestsPerSecond,
    recommenderConfig_itemExplorationConfig,

    -- * RecommenderSummary
    RecommenderSummary (..),
    newRecommenderSummary,
    recommenderSummary_name,
    recommenderSummary_creationDateTime,
    recommenderSummary_recommenderArn,
    recommenderSummary_recommenderConfig,
    recommenderSummary_status,
    recommenderSummary_datasetGroupArn,
    recommenderSummary_recipeArn,
    recommenderSummary_lastUpdatedDateTime,

    -- * RecommenderUpdateSummary
    RecommenderUpdateSummary (..),
    newRecommenderUpdateSummary,
    recommenderUpdateSummary_creationDateTime,
    recommenderUpdateSummary_recommenderConfig,
    recommenderUpdateSummary_status,
    recommenderUpdateSummary_lastUpdatedDateTime,
    recommenderUpdateSummary_failureReason,

    -- * S3DataConfig
    S3DataConfig (..),
    newS3DataConfig,
    s3DataConfig_kmsKeyArn,
    s3DataConfig_path,

    -- * Solution
    Solution (..),
    newSolution,
    solution_solutionArn,
    solution_eventType,
    solution_latestSolutionVersion,
    solution_name,
    solution_performAutoML,
    solution_performHPO,
    solution_creationDateTime,
    solution_solutionConfig,
    solution_autoMLResult,
    solution_status,
    solution_datasetGroupArn,
    solution_recipeArn,
    solution_lastUpdatedDateTime,

    -- * SolutionConfig
    SolutionConfig (..),
    newSolutionConfig,
    solutionConfig_featureTransformationParameters,
    solutionConfig_optimizationObjective,
    solutionConfig_algorithmHyperParameters,
    solutionConfig_autoMLConfig,
    solutionConfig_eventValueThreshold,
    solutionConfig_hpoConfig,

    -- * SolutionSummary
    SolutionSummary (..),
    newSolutionSummary,
    solutionSummary_solutionArn,
    solutionSummary_name,
    solutionSummary_creationDateTime,
    solutionSummary_status,
    solutionSummary_recipeArn,
    solutionSummary_lastUpdatedDateTime,

    -- * SolutionVersion
    SolutionVersion (..),
    newSolutionVersion,
    solutionVersion_solutionArn,
    solutionVersion_eventType,
    solutionVersion_name,
    solutionVersion_tunedHPOParams,
    solutionVersion_performAutoML,
    solutionVersion_performHPO,
    solutionVersion_creationDateTime,
    solutionVersion_solutionConfig,
    solutionVersion_status,
    solutionVersion_trainingHours,
    solutionVersion_trainingMode,
    solutionVersion_datasetGroupArn,
    solutionVersion_solutionVersionArn,
    solutionVersion_recipeArn,
    solutionVersion_lastUpdatedDateTime,
    solutionVersion_failureReason,

    -- * SolutionVersionSummary
    SolutionVersionSummary (..),
    newSolutionVersionSummary,
    solutionVersionSummary_creationDateTime,
    solutionVersionSummary_status,
    solutionVersionSummary_solutionVersionArn,
    solutionVersionSummary_lastUpdatedDateTime,
    solutionVersionSummary_failureReason,

    -- * Tag
    Tag (..),
    newTag,
    tag_tagKey,
    tag_tagValue,

    -- * TunedHPOParams
    TunedHPOParams (..),
    newTunedHPOParams,
    tunedHPOParams_algorithmHyperParameters,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Personalize.Types.Algorithm
import Amazonka.Personalize.Types.AlgorithmImage
import Amazonka.Personalize.Types.AutoMLConfig
import Amazonka.Personalize.Types.AutoMLResult
import Amazonka.Personalize.Types.BatchInferenceJob
import Amazonka.Personalize.Types.BatchInferenceJobConfig
import Amazonka.Personalize.Types.BatchInferenceJobInput
import Amazonka.Personalize.Types.BatchInferenceJobOutput
import Amazonka.Personalize.Types.BatchInferenceJobSummary
import Amazonka.Personalize.Types.BatchSegmentJob
import Amazonka.Personalize.Types.BatchSegmentJobInput
import Amazonka.Personalize.Types.BatchSegmentJobOutput
import Amazonka.Personalize.Types.BatchSegmentJobSummary
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
import Amazonka.Personalize.Types.Domain
import Amazonka.Personalize.Types.EventTracker
import Amazonka.Personalize.Types.EventTrackerSummary
import Amazonka.Personalize.Types.FeatureTransformation
import Amazonka.Personalize.Types.Filter
import Amazonka.Personalize.Types.FilterSummary
import Amazonka.Personalize.Types.HPOConfig
import Amazonka.Personalize.Types.HPOObjective
import Amazonka.Personalize.Types.HPOResourceConfig
import Amazonka.Personalize.Types.HyperParameterRanges
import Amazonka.Personalize.Types.ImportMode
import Amazonka.Personalize.Types.IngestionMode
import Amazonka.Personalize.Types.IntegerHyperParameterRange
import Amazonka.Personalize.Types.MetricAttribute
import Amazonka.Personalize.Types.MetricAttribution
import Amazonka.Personalize.Types.MetricAttributionOutput
import Amazonka.Personalize.Types.MetricAttributionSummary
import Amazonka.Personalize.Types.ObjectiveSensitivity
import Amazonka.Personalize.Types.OptimizationObjective
import Amazonka.Personalize.Types.Recipe
import Amazonka.Personalize.Types.RecipeProvider
import Amazonka.Personalize.Types.RecipeSummary
import Amazonka.Personalize.Types.Recommender
import Amazonka.Personalize.Types.RecommenderConfig
import Amazonka.Personalize.Types.RecommenderSummary
import Amazonka.Personalize.Types.RecommenderUpdateSummary
import Amazonka.Personalize.Types.S3DataConfig
import Amazonka.Personalize.Types.Solution
import Amazonka.Personalize.Types.SolutionConfig
import Amazonka.Personalize.Types.SolutionSummary
import Amazonka.Personalize.Types.SolutionVersion
import Amazonka.Personalize.Types.SolutionVersionSummary
import Amazonka.Personalize.Types.Tag
import Amazonka.Personalize.Types.TrainingMode
import Amazonka.Personalize.Types.TunedHPOParams
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-05-22@ of the Amazon Personalize SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Personalize",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "personalize",
      Core.signingName = "personalize",
      Core.version = "2018-05-22",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Personalize",
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

-- | The specified resource already exists.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"

-- | Provide a valid value for the field or parameter.
_InvalidInputException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInputException =
  Core._MatchServiceError
    defaultService
    "InvalidInputException"

-- | You have exceeded the maximum number of tags you can apply to this
-- resource.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"

-- | The request contains more tag keys than can be associated with a
-- resource (50 tag keys per resource).
_TooManyTagKeysException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagKeysException =
  Core._MatchServiceError
    defaultService
    "TooManyTagKeysException"

-- | Could not find the specified resource.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The specified resource is in use.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"

-- | The limit on the number of requests per second has been exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The token is not valid.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"

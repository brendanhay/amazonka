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
    _InvalidInputException,
    _InvalidNextTokenException,
    _LimitExceededException,
    _ResourceAlreadyExistsException,
    _ResourceInUseException,
    _ResourceNotFoundException,
    _TooManyTagKeysException,
    _TooManyTagsException,

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
    algorithm_algorithmArn,
    algorithm_algorithmImage,
    algorithm_creationDateTime,
    algorithm_defaultHyperParameterRanges,
    algorithm_defaultHyperParameters,
    algorithm_defaultResourceConfig,
    algorithm_lastUpdatedDateTime,
    algorithm_name,
    algorithm_roleArn,
    algorithm_trainingInputMode,

    -- * AlgorithmImage
    AlgorithmImage (..),
    newAlgorithmImage,
    algorithmImage_name,
    algorithmImage_dockerURI,

    -- * AutoMLConfig
    AutoMLConfig (..),
    newAutoMLConfig,
    autoMLConfig_metricName,
    autoMLConfig_recipeList,

    -- * AutoMLResult
    AutoMLResult (..),
    newAutoMLResult,
    autoMLResult_bestRecipeArn,

    -- * BatchInferenceJob
    BatchInferenceJob (..),
    newBatchInferenceJob,
    batchInferenceJob_batchInferenceJobArn,
    batchInferenceJob_batchInferenceJobConfig,
    batchInferenceJob_creationDateTime,
    batchInferenceJob_failureReason,
    batchInferenceJob_filterArn,
    batchInferenceJob_jobInput,
    batchInferenceJob_jobName,
    batchInferenceJob_jobOutput,
    batchInferenceJob_lastUpdatedDateTime,
    batchInferenceJob_numResults,
    batchInferenceJob_roleArn,
    batchInferenceJob_solutionVersionArn,
    batchInferenceJob_status,

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
    batchInferenceJobSummary_batchInferenceJobArn,
    batchInferenceJobSummary_creationDateTime,
    batchInferenceJobSummary_failureReason,
    batchInferenceJobSummary_jobName,
    batchInferenceJobSummary_lastUpdatedDateTime,
    batchInferenceJobSummary_solutionVersionArn,
    batchInferenceJobSummary_status,

    -- * BatchSegmentJob
    BatchSegmentJob (..),
    newBatchSegmentJob,
    batchSegmentJob_batchSegmentJobArn,
    batchSegmentJob_creationDateTime,
    batchSegmentJob_failureReason,
    batchSegmentJob_filterArn,
    batchSegmentJob_jobInput,
    batchSegmentJob_jobName,
    batchSegmentJob_jobOutput,
    batchSegmentJob_lastUpdatedDateTime,
    batchSegmentJob_numResults,
    batchSegmentJob_roleArn,
    batchSegmentJob_solutionVersionArn,
    batchSegmentJob_status,

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
    batchSegmentJobSummary_batchSegmentJobArn,
    batchSegmentJobSummary_creationDateTime,
    batchSegmentJobSummary_failureReason,
    batchSegmentJobSummary_jobName,
    batchSegmentJobSummary_lastUpdatedDateTime,
    batchSegmentJobSummary_solutionVersionArn,
    batchSegmentJobSummary_status,

    -- * Campaign
    Campaign (..),
    newCampaign,
    campaign_campaignArn,
    campaign_campaignConfig,
    campaign_creationDateTime,
    campaign_failureReason,
    campaign_lastUpdatedDateTime,
    campaign_latestCampaignUpdate,
    campaign_minProvisionedTPS,
    campaign_name,
    campaign_solutionVersionArn,
    campaign_status,

    -- * CampaignConfig
    CampaignConfig (..),
    newCampaignConfig,
    campaignConfig_itemExplorationConfig,

    -- * CampaignSummary
    CampaignSummary (..),
    newCampaignSummary,
    campaignSummary_campaignArn,
    campaignSummary_creationDateTime,
    campaignSummary_failureReason,
    campaignSummary_lastUpdatedDateTime,
    campaignSummary_name,
    campaignSummary_status,

    -- * CampaignUpdateSummary
    CampaignUpdateSummary (..),
    newCampaignUpdateSummary,
    campaignUpdateSummary_campaignConfig,
    campaignUpdateSummary_creationDateTime,
    campaignUpdateSummary_failureReason,
    campaignUpdateSummary_lastUpdatedDateTime,
    campaignUpdateSummary_minProvisionedTPS,
    campaignUpdateSummary_solutionVersionArn,
    campaignUpdateSummary_status,

    -- * CategoricalHyperParameterRange
    CategoricalHyperParameterRange (..),
    newCategoricalHyperParameterRange,
    categoricalHyperParameterRange_name,
    categoricalHyperParameterRange_values,

    -- * ContinuousHyperParameterRange
    ContinuousHyperParameterRange (..),
    newContinuousHyperParameterRange,
    continuousHyperParameterRange_maxValue,
    continuousHyperParameterRange_minValue,
    continuousHyperParameterRange_name,

    -- * DataSource
    DataSource (..),
    newDataSource,
    dataSource_dataLocation,

    -- * Dataset
    Dataset (..),
    newDataset,
    dataset_creationDateTime,
    dataset_datasetArn,
    dataset_datasetGroupArn,
    dataset_datasetType,
    dataset_lastUpdatedDateTime,
    dataset_name,
    dataset_schemaArn,
    dataset_status,

    -- * DatasetExportJob
    DatasetExportJob (..),
    newDatasetExportJob,
    datasetExportJob_creationDateTime,
    datasetExportJob_datasetArn,
    datasetExportJob_datasetExportJobArn,
    datasetExportJob_failureReason,
    datasetExportJob_ingestionMode,
    datasetExportJob_jobName,
    datasetExportJob_jobOutput,
    datasetExportJob_lastUpdatedDateTime,
    datasetExportJob_roleArn,
    datasetExportJob_status,

    -- * DatasetExportJobOutput
    DatasetExportJobOutput (..),
    newDatasetExportJobOutput,
    datasetExportJobOutput_s3DataDestination,

    -- * DatasetExportJobSummary
    DatasetExportJobSummary (..),
    newDatasetExportJobSummary,
    datasetExportJobSummary_creationDateTime,
    datasetExportJobSummary_datasetExportJobArn,
    datasetExportJobSummary_failureReason,
    datasetExportJobSummary_jobName,
    datasetExportJobSummary_lastUpdatedDateTime,
    datasetExportJobSummary_status,

    -- * DatasetGroup
    DatasetGroup (..),
    newDatasetGroup,
    datasetGroup_creationDateTime,
    datasetGroup_datasetGroupArn,
    datasetGroup_domain,
    datasetGroup_failureReason,
    datasetGroup_kmsKeyArn,
    datasetGroup_lastUpdatedDateTime,
    datasetGroup_name,
    datasetGroup_roleArn,
    datasetGroup_status,

    -- * DatasetGroupSummary
    DatasetGroupSummary (..),
    newDatasetGroupSummary,
    datasetGroupSummary_creationDateTime,
    datasetGroupSummary_datasetGroupArn,
    datasetGroupSummary_domain,
    datasetGroupSummary_failureReason,
    datasetGroupSummary_lastUpdatedDateTime,
    datasetGroupSummary_name,
    datasetGroupSummary_status,

    -- * DatasetImportJob
    DatasetImportJob (..),
    newDatasetImportJob,
    datasetImportJob_creationDateTime,
    datasetImportJob_dataSource,
    datasetImportJob_datasetArn,
    datasetImportJob_datasetImportJobArn,
    datasetImportJob_failureReason,
    datasetImportJob_importMode,
    datasetImportJob_jobName,
    datasetImportJob_lastUpdatedDateTime,
    datasetImportJob_publishAttributionMetricsToS3,
    datasetImportJob_roleArn,
    datasetImportJob_status,

    -- * DatasetImportJobSummary
    DatasetImportJobSummary (..),
    newDatasetImportJobSummary,
    datasetImportJobSummary_creationDateTime,
    datasetImportJobSummary_datasetImportJobArn,
    datasetImportJobSummary_failureReason,
    datasetImportJobSummary_importMode,
    datasetImportJobSummary_jobName,
    datasetImportJobSummary_lastUpdatedDateTime,
    datasetImportJobSummary_status,

    -- * DatasetSchema
    DatasetSchema (..),
    newDatasetSchema,
    datasetSchema_creationDateTime,
    datasetSchema_domain,
    datasetSchema_lastUpdatedDateTime,
    datasetSchema_name,
    datasetSchema_schema,
    datasetSchema_schemaArn,

    -- * DatasetSchemaSummary
    DatasetSchemaSummary (..),
    newDatasetSchemaSummary,
    datasetSchemaSummary_creationDateTime,
    datasetSchemaSummary_domain,
    datasetSchemaSummary_lastUpdatedDateTime,
    datasetSchemaSummary_name,
    datasetSchemaSummary_schemaArn,

    -- * DatasetSummary
    DatasetSummary (..),
    newDatasetSummary,
    datasetSummary_creationDateTime,
    datasetSummary_datasetArn,
    datasetSummary_datasetType,
    datasetSummary_lastUpdatedDateTime,
    datasetSummary_name,
    datasetSummary_status,

    -- * DefaultCategoricalHyperParameterRange
    DefaultCategoricalHyperParameterRange (..),
    newDefaultCategoricalHyperParameterRange,
    defaultCategoricalHyperParameterRange_isTunable,
    defaultCategoricalHyperParameterRange_name,
    defaultCategoricalHyperParameterRange_values,

    -- * DefaultContinuousHyperParameterRange
    DefaultContinuousHyperParameterRange (..),
    newDefaultContinuousHyperParameterRange,
    defaultContinuousHyperParameterRange_isTunable,
    defaultContinuousHyperParameterRange_maxValue,
    defaultContinuousHyperParameterRange_minValue,
    defaultContinuousHyperParameterRange_name,

    -- * DefaultHyperParameterRanges
    DefaultHyperParameterRanges (..),
    newDefaultHyperParameterRanges,
    defaultHyperParameterRanges_categoricalHyperParameterRanges,
    defaultHyperParameterRanges_continuousHyperParameterRanges,
    defaultHyperParameterRanges_integerHyperParameterRanges,

    -- * DefaultIntegerHyperParameterRange
    DefaultIntegerHyperParameterRange (..),
    newDefaultIntegerHyperParameterRange,
    defaultIntegerHyperParameterRange_isTunable,
    defaultIntegerHyperParameterRange_maxValue,
    defaultIntegerHyperParameterRange_minValue,
    defaultIntegerHyperParameterRange_name,

    -- * EventTracker
    EventTracker (..),
    newEventTracker,
    eventTracker_accountId,
    eventTracker_creationDateTime,
    eventTracker_datasetGroupArn,
    eventTracker_eventTrackerArn,
    eventTracker_lastUpdatedDateTime,
    eventTracker_name,
    eventTracker_status,
    eventTracker_trackingId,

    -- * EventTrackerSummary
    EventTrackerSummary (..),
    newEventTrackerSummary,
    eventTrackerSummary_creationDateTime,
    eventTrackerSummary_eventTrackerArn,
    eventTrackerSummary_lastUpdatedDateTime,
    eventTrackerSummary_name,
    eventTrackerSummary_status,

    -- * FeatureTransformation
    FeatureTransformation (..),
    newFeatureTransformation,
    featureTransformation_creationDateTime,
    featureTransformation_defaultParameters,
    featureTransformation_featureTransformationArn,
    featureTransformation_lastUpdatedDateTime,
    featureTransformation_name,
    featureTransformation_status,

    -- * Filter
    Filter (..),
    newFilter,
    filter_creationDateTime,
    filter_datasetGroupArn,
    filter_failureReason,
    filter_filterArn,
    filter_filterExpression,
    filter_lastUpdatedDateTime,
    filter_name,
    filter_status,

    -- * FilterSummary
    FilterSummary (..),
    newFilterSummary,
    filterSummary_creationDateTime,
    filterSummary_datasetGroupArn,
    filterSummary_failureReason,
    filterSummary_filterArn,
    filterSummary_lastUpdatedDateTime,
    filterSummary_name,
    filterSummary_status,

    -- * HPOConfig
    HPOConfig (..),
    newHPOConfig,
    hPOConfig_algorithmHyperParameterRanges,
    hPOConfig_hpoObjective,
    hPOConfig_hpoResourceConfig,

    -- * HPOObjective
    HPOObjective (..),
    newHPOObjective,
    hPOObjective_metricName,
    hPOObjective_metricRegex,
    hPOObjective_type,

    -- * HPOResourceConfig
    HPOResourceConfig (..),
    newHPOResourceConfig,
    hPOResourceConfig_maxNumberOfTrainingJobs,
    hPOResourceConfig_maxParallelTrainingJobs,

    -- * HyperParameterRanges
    HyperParameterRanges (..),
    newHyperParameterRanges,
    hyperParameterRanges_categoricalHyperParameterRanges,
    hyperParameterRanges_continuousHyperParameterRanges,
    hyperParameterRanges_integerHyperParameterRanges,

    -- * IntegerHyperParameterRange
    IntegerHyperParameterRange (..),
    newIntegerHyperParameterRange,
    integerHyperParameterRange_maxValue,
    integerHyperParameterRange_minValue,
    integerHyperParameterRange_name,

    -- * MetricAttribute
    MetricAttribute (..),
    newMetricAttribute,
    metricAttribute_eventType,
    metricAttribute_metricName,
    metricAttribute_expression,

    -- * MetricAttribution
    MetricAttribution (..),
    newMetricAttribution,
    metricAttribution_creationDateTime,
    metricAttribution_datasetGroupArn,
    metricAttribution_failureReason,
    metricAttribution_lastUpdatedDateTime,
    metricAttribution_metricAttributionArn,
    metricAttribution_metricsOutputConfig,
    metricAttribution_name,
    metricAttribution_status,

    -- * MetricAttributionOutput
    MetricAttributionOutput (..),
    newMetricAttributionOutput,
    metricAttributionOutput_s3DataDestination,
    metricAttributionOutput_roleArn,

    -- * MetricAttributionSummary
    MetricAttributionSummary (..),
    newMetricAttributionSummary,
    metricAttributionSummary_creationDateTime,
    metricAttributionSummary_failureReason,
    metricAttributionSummary_lastUpdatedDateTime,
    metricAttributionSummary_metricAttributionArn,
    metricAttributionSummary_name,
    metricAttributionSummary_status,

    -- * OptimizationObjective
    OptimizationObjective (..),
    newOptimizationObjective,
    optimizationObjective_itemAttribute,
    optimizationObjective_objectiveSensitivity,

    -- * Recipe
    Recipe (..),
    newRecipe,
    recipe_algorithmArn,
    recipe_creationDateTime,
    recipe_description,
    recipe_featureTransformationArn,
    recipe_lastUpdatedDateTime,
    recipe_name,
    recipe_recipeArn,
    recipe_recipeType,
    recipe_status,

    -- * RecipeSummary
    RecipeSummary (..),
    newRecipeSummary,
    recipeSummary_creationDateTime,
    recipeSummary_domain,
    recipeSummary_lastUpdatedDateTime,
    recipeSummary_name,
    recipeSummary_recipeArn,
    recipeSummary_status,

    -- * Recommender
    Recommender (..),
    newRecommender,
    recommender_creationDateTime,
    recommender_datasetGroupArn,
    recommender_failureReason,
    recommender_lastUpdatedDateTime,
    recommender_latestRecommenderUpdate,
    recommender_modelMetrics,
    recommender_name,
    recommender_recipeArn,
    recommender_recommenderArn,
    recommender_recommenderConfig,
    recommender_status,

    -- * RecommenderConfig
    RecommenderConfig (..),
    newRecommenderConfig,
    recommenderConfig_itemExplorationConfig,
    recommenderConfig_minRecommendationRequestsPerSecond,

    -- * RecommenderSummary
    RecommenderSummary (..),
    newRecommenderSummary,
    recommenderSummary_creationDateTime,
    recommenderSummary_datasetGroupArn,
    recommenderSummary_lastUpdatedDateTime,
    recommenderSummary_name,
    recommenderSummary_recipeArn,
    recommenderSummary_recommenderArn,
    recommenderSummary_recommenderConfig,
    recommenderSummary_status,

    -- * RecommenderUpdateSummary
    RecommenderUpdateSummary (..),
    newRecommenderUpdateSummary,
    recommenderUpdateSummary_creationDateTime,
    recommenderUpdateSummary_failureReason,
    recommenderUpdateSummary_lastUpdatedDateTime,
    recommenderUpdateSummary_recommenderConfig,
    recommenderUpdateSummary_status,

    -- * S3DataConfig
    S3DataConfig (..),
    newS3DataConfig,
    s3DataConfig_kmsKeyArn,
    s3DataConfig_path,

    -- * Solution
    Solution (..),
    newSolution,
    solution_autoMLResult,
    solution_creationDateTime,
    solution_datasetGroupArn,
    solution_eventType,
    solution_lastUpdatedDateTime,
    solution_latestSolutionVersion,
    solution_name,
    solution_performAutoML,
    solution_performHPO,
    solution_recipeArn,
    solution_solutionArn,
    solution_solutionConfig,
    solution_status,

    -- * SolutionConfig
    SolutionConfig (..),
    newSolutionConfig,
    solutionConfig_algorithmHyperParameters,
    solutionConfig_autoMLConfig,
    solutionConfig_eventValueThreshold,
    solutionConfig_featureTransformationParameters,
    solutionConfig_hpoConfig,
    solutionConfig_optimizationObjective,

    -- * SolutionSummary
    SolutionSummary (..),
    newSolutionSummary,
    solutionSummary_creationDateTime,
    solutionSummary_lastUpdatedDateTime,
    solutionSummary_name,
    solutionSummary_recipeArn,
    solutionSummary_solutionArn,
    solutionSummary_status,

    -- * SolutionVersion
    SolutionVersion (..),
    newSolutionVersion,
    solutionVersion_creationDateTime,
    solutionVersion_datasetGroupArn,
    solutionVersion_eventType,
    solutionVersion_failureReason,
    solutionVersion_lastUpdatedDateTime,
    solutionVersion_name,
    solutionVersion_performAutoML,
    solutionVersion_performHPO,
    solutionVersion_recipeArn,
    solutionVersion_solutionArn,
    solutionVersion_solutionConfig,
    solutionVersion_solutionVersionArn,
    solutionVersion_status,
    solutionVersion_trainingHours,
    solutionVersion_trainingMode,
    solutionVersion_tunedHPOParams,

    -- * SolutionVersionSummary
    SolutionVersionSummary (..),
    newSolutionVersionSummary,
    solutionVersionSummary_creationDateTime,
    solutionVersionSummary_failureReason,
    solutionVersionSummary_lastUpdatedDateTime,
    solutionVersionSummary_solutionVersionArn,
    solutionVersionSummary_status,

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

-- | Provide a valid value for the field or parameter.
_InvalidInputException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInputException =
  Core._MatchServiceError
    defaultService
    "InvalidInputException"

-- | The token is not valid.
_InvalidNextTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNextTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidNextTokenException"

-- | The limit on the number of requests per second has been exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The specified resource already exists.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"

-- | The specified resource is in use.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"

-- | Could not find the specified resource.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The request contains more tag keys than can be associated with a
-- resource (50 tag keys per resource).
_TooManyTagKeysException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagKeysException =
  Core._MatchServiceError
    defaultService
    "TooManyTagKeysException"

-- | You have exceeded the maximum number of tags you can apply to this
-- resource.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"

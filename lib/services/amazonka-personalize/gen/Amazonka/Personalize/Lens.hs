{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Personalize.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Lens
  ( -- * Operations

    -- ** CreateBatchInferenceJob
    createBatchInferenceJob_filterArn,
    createBatchInferenceJob_numResults,
    createBatchInferenceJob_batchInferenceJobConfig,
    createBatchInferenceJob_jobName,
    createBatchInferenceJob_solutionVersionArn,
    createBatchInferenceJob_jobInput,
    createBatchInferenceJob_jobOutput,
    createBatchInferenceJob_roleArn,
    createBatchInferenceJobResponse_batchInferenceJobArn,
    createBatchInferenceJobResponse_httpStatus,

    -- ** CreateCampaign
    createCampaign_campaignConfig,
    createCampaign_minProvisionedTPS,
    createCampaign_name,
    createCampaign_solutionVersionArn,
    createCampaignResponse_campaignArn,
    createCampaignResponse_httpStatus,

    -- ** CreateDataset
    createDataset_name,
    createDataset_schemaArn,
    createDataset_datasetGroupArn,
    createDataset_datasetType,
    createDatasetResponse_datasetArn,
    createDatasetResponse_httpStatus,

    -- ** CreateDatasetExportJob
    createDatasetExportJob_ingestionMode,
    createDatasetExportJob_jobName,
    createDatasetExportJob_datasetArn,
    createDatasetExportJob_roleArn,
    createDatasetExportJob_jobOutput,
    createDatasetExportJobResponse_datasetExportJobArn,
    createDatasetExportJobResponse_httpStatus,

    -- ** CreateDatasetGroup
    createDatasetGroup_roleArn,
    createDatasetGroup_kmsKeyArn,
    createDatasetGroup_name,
    createDatasetGroupResponse_datasetGroupArn,
    createDatasetGroupResponse_httpStatus,

    -- ** CreateDatasetImportJob
    createDatasetImportJob_jobName,
    createDatasetImportJob_datasetArn,
    createDatasetImportJob_dataSource,
    createDatasetImportJob_roleArn,
    createDatasetImportJobResponse_datasetImportJobArn,
    createDatasetImportJobResponse_httpStatus,

    -- ** CreateEventTracker
    createEventTracker_name,
    createEventTracker_datasetGroupArn,
    createEventTrackerResponse_trackingId,
    createEventTrackerResponse_eventTrackerArn,
    createEventTrackerResponse_httpStatus,

    -- ** CreateFilter
    createFilter_name,
    createFilter_datasetGroupArn,
    createFilter_filterExpression,
    createFilterResponse_filterArn,
    createFilterResponse_httpStatus,

    -- ** CreateSchema
    createSchema_name,
    createSchema_schema,
    createSchemaResponse_schemaArn,
    createSchemaResponse_httpStatus,

    -- ** CreateSolution
    createSolution_eventType,
    createSolution_performAutoML,
    createSolution_performHPO,
    createSolution_solutionConfig,
    createSolution_recipeArn,
    createSolution_name,
    createSolution_datasetGroupArn,
    createSolutionResponse_solutionArn,
    createSolutionResponse_httpStatus,

    -- ** CreateSolutionVersion
    createSolutionVersion_trainingMode,
    createSolutionVersion_solutionArn,
    createSolutionVersionResponse_solutionVersionArn,
    createSolutionVersionResponse_httpStatus,

    -- ** DeleteCampaign
    deleteCampaign_campaignArn,

    -- ** DeleteDataset
    deleteDataset_datasetArn,

    -- ** DeleteDatasetGroup
    deleteDatasetGroup_datasetGroupArn,

    -- ** DeleteEventTracker
    deleteEventTracker_eventTrackerArn,

    -- ** DeleteFilter
    deleteFilter_filterArn,

    -- ** DeleteSchema
    deleteSchema_schemaArn,

    -- ** DeleteSolution
    deleteSolution_solutionArn,

    -- ** DescribeAlgorithm
    describeAlgorithm_algorithmArn,
    describeAlgorithmResponse_algorithm,
    describeAlgorithmResponse_httpStatus,

    -- ** DescribeBatchInferenceJob
    describeBatchInferenceJob_batchInferenceJobArn,
    describeBatchInferenceJobResponse_batchInferenceJob,
    describeBatchInferenceJobResponse_httpStatus,

    -- ** DescribeCampaign
    describeCampaign_campaignArn,
    describeCampaignResponse_campaign,
    describeCampaignResponse_httpStatus,

    -- ** DescribeDataset
    describeDataset_datasetArn,
    describeDatasetResponse_dataset,
    describeDatasetResponse_httpStatus,

    -- ** DescribeDatasetExportJob
    describeDatasetExportJob_datasetExportJobArn,
    describeDatasetExportJobResponse_datasetExportJob,
    describeDatasetExportJobResponse_httpStatus,

    -- ** DescribeDatasetGroup
    describeDatasetGroup_datasetGroupArn,
    describeDatasetGroupResponse_datasetGroup,
    describeDatasetGroupResponse_httpStatus,

    -- ** DescribeDatasetImportJob
    describeDatasetImportJob_datasetImportJobArn,
    describeDatasetImportJobResponse_datasetImportJob,
    describeDatasetImportJobResponse_httpStatus,

    -- ** DescribeEventTracker
    describeEventTracker_eventTrackerArn,
    describeEventTrackerResponse_eventTracker,
    describeEventTrackerResponse_httpStatus,

    -- ** DescribeFeatureTransformation
    describeFeatureTransformation_featureTransformationArn,
    describeFeatureTransformationResponse_featureTransformation,
    describeFeatureTransformationResponse_httpStatus,

    -- ** DescribeFilter
    describeFilter_filterArn,
    describeFilterResponse_filter,
    describeFilterResponse_httpStatus,

    -- ** DescribeRecipe
    describeRecipe_recipeArn,
    describeRecipeResponse_recipe,
    describeRecipeResponse_httpStatus,

    -- ** DescribeSchema
    describeSchema_schemaArn,
    describeSchemaResponse_schema,
    describeSchemaResponse_httpStatus,

    -- ** DescribeSolution
    describeSolution_solutionArn,
    describeSolutionResponse_solution,
    describeSolutionResponse_httpStatus,

    -- ** DescribeSolutionVersion
    describeSolutionVersion_solutionVersionArn,
    describeSolutionVersionResponse_solutionVersion,
    describeSolutionVersionResponse_httpStatus,

    -- ** GetSolutionMetrics
    getSolutionMetrics_solutionVersionArn,
    getSolutionMetricsResponse_metrics,
    getSolutionMetricsResponse_solutionVersionArn,
    getSolutionMetricsResponse_httpStatus,

    -- ** ListBatchInferenceJobs
    listBatchInferenceJobs_nextToken,
    listBatchInferenceJobs_maxResults,
    listBatchInferenceJobs_solutionVersionArn,
    listBatchInferenceJobsResponse_nextToken,
    listBatchInferenceJobsResponse_batchInferenceJobs,
    listBatchInferenceJobsResponse_httpStatus,

    -- ** ListCampaigns
    listCampaigns_solutionArn,
    listCampaigns_nextToken,
    listCampaigns_maxResults,
    listCampaignsResponse_nextToken,
    listCampaignsResponse_campaigns,
    listCampaignsResponse_httpStatus,

    -- ** ListDatasetExportJobs
    listDatasetExportJobs_nextToken,
    listDatasetExportJobs_datasetArn,
    listDatasetExportJobs_maxResults,
    listDatasetExportJobsResponse_nextToken,
    listDatasetExportJobsResponse_datasetExportJobs,
    listDatasetExportJobsResponse_httpStatus,

    -- ** ListDatasetGroups
    listDatasetGroups_nextToken,
    listDatasetGroups_maxResults,
    listDatasetGroupsResponse_nextToken,
    listDatasetGroupsResponse_datasetGroups,
    listDatasetGroupsResponse_httpStatus,

    -- ** ListDatasetImportJobs
    listDatasetImportJobs_nextToken,
    listDatasetImportJobs_datasetArn,
    listDatasetImportJobs_maxResults,
    listDatasetImportJobsResponse_nextToken,
    listDatasetImportJobsResponse_datasetImportJobs,
    listDatasetImportJobsResponse_httpStatus,

    -- ** ListDatasets
    listDatasets_nextToken,
    listDatasets_maxResults,
    listDatasets_datasetGroupArn,
    listDatasetsResponse_nextToken,
    listDatasetsResponse_datasets,
    listDatasetsResponse_httpStatus,

    -- ** ListEventTrackers
    listEventTrackers_nextToken,
    listEventTrackers_maxResults,
    listEventTrackers_datasetGroupArn,
    listEventTrackersResponse_nextToken,
    listEventTrackersResponse_eventTrackers,
    listEventTrackersResponse_httpStatus,

    -- ** ListFilters
    listFilters_nextToken,
    listFilters_maxResults,
    listFilters_datasetGroupArn,
    listFiltersResponse_nextToken,
    listFiltersResponse_filters,
    listFiltersResponse_httpStatus,

    -- ** ListRecipes
    listRecipes_nextToken,
    listRecipes_recipeProvider,
    listRecipes_maxResults,
    listRecipesResponse_recipes,
    listRecipesResponse_nextToken,
    listRecipesResponse_httpStatus,

    -- ** ListSchemas
    listSchemas_nextToken,
    listSchemas_maxResults,
    listSchemasResponse_nextToken,
    listSchemasResponse_schemas,
    listSchemasResponse_httpStatus,

    -- ** ListSolutionVersions
    listSolutionVersions_solutionArn,
    listSolutionVersions_nextToken,
    listSolutionVersions_maxResults,
    listSolutionVersionsResponse_nextToken,
    listSolutionVersionsResponse_solutionVersions,
    listSolutionVersionsResponse_httpStatus,

    -- ** ListSolutions
    listSolutions_nextToken,
    listSolutions_maxResults,
    listSolutions_datasetGroupArn,
    listSolutionsResponse_nextToken,
    listSolutionsResponse_solutions,
    listSolutionsResponse_httpStatus,

    -- ** StopSolutionVersionCreation
    stopSolutionVersionCreation_solutionVersionArn,

    -- ** UpdateCampaign
    updateCampaign_campaignConfig,
    updateCampaign_minProvisionedTPS,
    updateCampaign_solutionVersionArn,
    updateCampaign_campaignArn,
    updateCampaignResponse_campaignArn,
    updateCampaignResponse_httpStatus,

    -- * Types

    -- ** Algorithm
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

    -- ** AlgorithmImage
    algorithmImage_name,
    algorithmImage_dockerURI,

    -- ** AutoMLConfig
    autoMLConfig_recipeList,
    autoMLConfig_metricName,

    -- ** AutoMLResult
    autoMLResult_bestRecipeArn,

    -- ** BatchInferenceJob
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

    -- ** BatchInferenceJobConfig
    batchInferenceJobConfig_itemExplorationConfig,

    -- ** BatchInferenceJobInput
    batchInferenceJobInput_s3DataSource,

    -- ** BatchInferenceJobOutput
    batchInferenceJobOutput_s3DataDestination,

    -- ** BatchInferenceJobSummary
    batchInferenceJobSummary_creationDateTime,
    batchInferenceJobSummary_jobName,
    batchInferenceJobSummary_status,
    batchInferenceJobSummary_batchInferenceJobArn,
    batchInferenceJobSummary_solutionVersionArn,
    batchInferenceJobSummary_lastUpdatedDateTime,
    batchInferenceJobSummary_failureReason,

    -- ** Campaign
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

    -- ** CampaignConfig
    campaignConfig_itemExplorationConfig,

    -- ** CampaignSummary
    campaignSummary_name,
    campaignSummary_creationDateTime,
    campaignSummary_status,
    campaignSummary_campaignArn,
    campaignSummary_lastUpdatedDateTime,
    campaignSummary_failureReason,

    -- ** CampaignUpdateSummary
    campaignUpdateSummary_creationDateTime,
    campaignUpdateSummary_campaignConfig,
    campaignUpdateSummary_status,
    campaignUpdateSummary_minProvisionedTPS,
    campaignUpdateSummary_solutionVersionArn,
    campaignUpdateSummary_lastUpdatedDateTime,
    campaignUpdateSummary_failureReason,

    -- ** CategoricalHyperParameterRange
    categoricalHyperParameterRange_name,
    categoricalHyperParameterRange_values,

    -- ** ContinuousHyperParameterRange
    continuousHyperParameterRange_name,
    continuousHyperParameterRange_minValue,
    continuousHyperParameterRange_maxValue,

    -- ** DataSource
    dataSource_dataLocation,

    -- ** Dataset
    dataset_name,
    dataset_creationDateTime,
    dataset_datasetType,
    dataset_status,
    dataset_datasetArn,
    dataset_schemaArn,
    dataset_datasetGroupArn,
    dataset_lastUpdatedDateTime,

    -- ** DatasetExportJob
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

    -- ** DatasetExportJobOutput
    datasetExportJobOutput_s3DataDestination,

    -- ** DatasetExportJobSummary
    datasetExportJobSummary_creationDateTime,
    datasetExportJobSummary_jobName,
    datasetExportJobSummary_status,
    datasetExportJobSummary_datasetExportJobArn,
    datasetExportJobSummary_lastUpdatedDateTime,
    datasetExportJobSummary_failureReason,

    -- ** DatasetGroup
    datasetGroup_name,
    datasetGroup_roleArn,
    datasetGroup_creationDateTime,
    datasetGroup_status,
    datasetGroup_kmsKeyArn,
    datasetGroup_datasetGroupArn,
    datasetGroup_lastUpdatedDateTime,
    datasetGroup_failureReason,

    -- ** DatasetGroupSummary
    datasetGroupSummary_name,
    datasetGroupSummary_creationDateTime,
    datasetGroupSummary_status,
    datasetGroupSummary_datasetGroupArn,
    datasetGroupSummary_lastUpdatedDateTime,
    datasetGroupSummary_failureReason,

    -- ** DatasetImportJob
    datasetImportJob_roleArn,
    datasetImportJob_creationDateTime,
    datasetImportJob_jobName,
    datasetImportJob_status,
    datasetImportJob_datasetArn,
    datasetImportJob_datasetImportJobArn,
    datasetImportJob_dataSource,
    datasetImportJob_lastUpdatedDateTime,
    datasetImportJob_failureReason,

    -- ** DatasetImportJobSummary
    datasetImportJobSummary_creationDateTime,
    datasetImportJobSummary_jobName,
    datasetImportJobSummary_status,
    datasetImportJobSummary_datasetImportJobArn,
    datasetImportJobSummary_lastUpdatedDateTime,
    datasetImportJobSummary_failureReason,

    -- ** DatasetSchema
    datasetSchema_name,
    datasetSchema_creationDateTime,
    datasetSchema_schemaArn,
    datasetSchema_schema,
    datasetSchema_lastUpdatedDateTime,

    -- ** DatasetSchemaSummary
    datasetSchemaSummary_name,
    datasetSchemaSummary_creationDateTime,
    datasetSchemaSummary_schemaArn,
    datasetSchemaSummary_lastUpdatedDateTime,

    -- ** DatasetSummary
    datasetSummary_name,
    datasetSummary_creationDateTime,
    datasetSummary_datasetType,
    datasetSummary_status,
    datasetSummary_datasetArn,
    datasetSummary_lastUpdatedDateTime,

    -- ** DefaultCategoricalHyperParameterRange
    defaultCategoricalHyperParameterRange_name,
    defaultCategoricalHyperParameterRange_isTunable,
    defaultCategoricalHyperParameterRange_values,

    -- ** DefaultContinuousHyperParameterRange
    defaultContinuousHyperParameterRange_name,
    defaultContinuousHyperParameterRange_minValue,
    defaultContinuousHyperParameterRange_isTunable,
    defaultContinuousHyperParameterRange_maxValue,

    -- ** DefaultHyperParameterRanges
    defaultHyperParameterRanges_integerHyperParameterRanges,
    defaultHyperParameterRanges_categoricalHyperParameterRanges,
    defaultHyperParameterRanges_continuousHyperParameterRanges,

    -- ** DefaultIntegerHyperParameterRange
    defaultIntegerHyperParameterRange_name,
    defaultIntegerHyperParameterRange_minValue,
    defaultIntegerHyperParameterRange_isTunable,
    defaultIntegerHyperParameterRange_maxValue,

    -- ** EventTracker
    eventTracker_name,
    eventTracker_creationDateTime,
    eventTracker_trackingId,
    eventTracker_eventTrackerArn,
    eventTracker_status,
    eventTracker_accountId,
    eventTracker_datasetGroupArn,
    eventTracker_lastUpdatedDateTime,

    -- ** EventTrackerSummary
    eventTrackerSummary_name,
    eventTrackerSummary_creationDateTime,
    eventTrackerSummary_eventTrackerArn,
    eventTrackerSummary_status,
    eventTrackerSummary_lastUpdatedDateTime,

    -- ** FeatureTransformation
    featureTransformation_name,
    featureTransformation_creationDateTime,
    featureTransformation_featureTransformationArn,
    featureTransformation_status,
    featureTransformation_defaultParameters,
    featureTransformation_lastUpdatedDateTime,

    -- ** Filter
    filter_name,
    filter_filterArn,
    filter_creationDateTime,
    filter_filterExpression,
    filter_status,
    filter_datasetGroupArn,
    filter_lastUpdatedDateTime,
    filter_failureReason,

    -- ** FilterSummary
    filterSummary_name,
    filterSummary_filterArn,
    filterSummary_creationDateTime,
    filterSummary_status,
    filterSummary_datasetGroupArn,
    filterSummary_lastUpdatedDateTime,
    filterSummary_failureReason,

    -- ** HPOConfig
    hPOConfig_algorithmHyperParameterRanges,
    hPOConfig_hpoObjective,
    hPOConfig_hpoResourceConfig,

    -- ** HPOObjective
    hPOObjective_type,
    hPOObjective_metricName,
    hPOObjective_metricRegex,

    -- ** HPOResourceConfig
    hPOResourceConfig_maxParallelTrainingJobs,
    hPOResourceConfig_maxNumberOfTrainingJobs,

    -- ** HyperParameterRanges
    hyperParameterRanges_integerHyperParameterRanges,
    hyperParameterRanges_categoricalHyperParameterRanges,
    hyperParameterRanges_continuousHyperParameterRanges,

    -- ** IntegerHyperParameterRange
    integerHyperParameterRange_name,
    integerHyperParameterRange_minValue,
    integerHyperParameterRange_maxValue,

    -- ** OptimizationObjective
    optimizationObjective_objectiveSensitivity,
    optimizationObjective_itemAttribute,

    -- ** Recipe
    recipe_name,
    recipe_creationDateTime,
    recipe_featureTransformationArn,
    recipe_status,
    recipe_description,
    recipe_algorithmArn,
    recipe_recipeType,
    recipe_recipeArn,
    recipe_lastUpdatedDateTime,

    -- ** RecipeSummary
    recipeSummary_name,
    recipeSummary_creationDateTime,
    recipeSummary_status,
    recipeSummary_recipeArn,
    recipeSummary_lastUpdatedDateTime,

    -- ** S3DataConfig
    s3DataConfig_kmsKeyArn,
    s3DataConfig_path,

    -- ** Solution
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

    -- ** SolutionConfig
    solutionConfig_featureTransformationParameters,
    solutionConfig_optimizationObjective,
    solutionConfig_algorithmHyperParameters,
    solutionConfig_autoMLConfig,
    solutionConfig_eventValueThreshold,
    solutionConfig_hpoConfig,

    -- ** SolutionSummary
    solutionSummary_solutionArn,
    solutionSummary_name,
    solutionSummary_creationDateTime,
    solutionSummary_status,
    solutionSummary_lastUpdatedDateTime,

    -- ** SolutionVersion
    solutionVersion_solutionArn,
    solutionVersion_eventType,
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

    -- ** SolutionVersionSummary
    solutionVersionSummary_creationDateTime,
    solutionVersionSummary_status,
    solutionVersionSummary_solutionVersionArn,
    solutionVersionSummary_lastUpdatedDateTime,
    solutionVersionSummary_failureReason,

    -- ** TunedHPOParams
    tunedHPOParams_algorithmHyperParameters,
  )
where

import Amazonka.Personalize.CreateBatchInferenceJob
import Amazonka.Personalize.CreateCampaign
import Amazonka.Personalize.CreateDataset
import Amazonka.Personalize.CreateDatasetExportJob
import Amazonka.Personalize.CreateDatasetGroup
import Amazonka.Personalize.CreateDatasetImportJob
import Amazonka.Personalize.CreateEventTracker
import Amazonka.Personalize.CreateFilter
import Amazonka.Personalize.CreateSchema
import Amazonka.Personalize.CreateSolution
import Amazonka.Personalize.CreateSolutionVersion
import Amazonka.Personalize.DeleteCampaign
import Amazonka.Personalize.DeleteDataset
import Amazonka.Personalize.DeleteDatasetGroup
import Amazonka.Personalize.DeleteEventTracker
import Amazonka.Personalize.DeleteFilter
import Amazonka.Personalize.DeleteSchema
import Amazonka.Personalize.DeleteSolution
import Amazonka.Personalize.DescribeAlgorithm
import Amazonka.Personalize.DescribeBatchInferenceJob
import Amazonka.Personalize.DescribeCampaign
import Amazonka.Personalize.DescribeDataset
import Amazonka.Personalize.DescribeDatasetExportJob
import Amazonka.Personalize.DescribeDatasetGroup
import Amazonka.Personalize.DescribeDatasetImportJob
import Amazonka.Personalize.DescribeEventTracker
import Amazonka.Personalize.DescribeFeatureTransformation
import Amazonka.Personalize.DescribeFilter
import Amazonka.Personalize.DescribeRecipe
import Amazonka.Personalize.DescribeSchema
import Amazonka.Personalize.DescribeSolution
import Amazonka.Personalize.DescribeSolutionVersion
import Amazonka.Personalize.GetSolutionMetrics
import Amazonka.Personalize.ListBatchInferenceJobs
import Amazonka.Personalize.ListCampaigns
import Amazonka.Personalize.ListDatasetExportJobs
import Amazonka.Personalize.ListDatasetGroups
import Amazonka.Personalize.ListDatasetImportJobs
import Amazonka.Personalize.ListDatasets
import Amazonka.Personalize.ListEventTrackers
import Amazonka.Personalize.ListFilters
import Amazonka.Personalize.ListRecipes
import Amazonka.Personalize.ListSchemas
import Amazonka.Personalize.ListSolutionVersions
import Amazonka.Personalize.ListSolutions
import Amazonka.Personalize.StopSolutionVersionCreation
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
import Amazonka.Personalize.Types.IntegerHyperParameterRange
import Amazonka.Personalize.Types.OptimizationObjective
import Amazonka.Personalize.Types.Recipe
import Amazonka.Personalize.Types.RecipeSummary
import Amazonka.Personalize.Types.S3DataConfig
import Amazonka.Personalize.Types.Solution
import Amazonka.Personalize.Types.SolutionConfig
import Amazonka.Personalize.Types.SolutionSummary
import Amazonka.Personalize.Types.SolutionVersion
import Amazonka.Personalize.Types.SolutionVersionSummary
import Amazonka.Personalize.Types.TunedHPOParams
import Amazonka.Personalize.UpdateCampaign

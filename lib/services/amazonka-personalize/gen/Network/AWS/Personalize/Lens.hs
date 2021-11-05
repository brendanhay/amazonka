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

    -- ** ListDatasetGroups
    listDatasetGroups_nextToken,
    listDatasetGroups_maxResults,
    listDatasetGroupsResponse_nextToken,
    listDatasetGroupsResponse_datasetGroups,
    listDatasetGroupsResponse_httpStatus,

    -- ** CreateBatchInferenceJob
    createBatchInferenceJob_numResults,
    createBatchInferenceJob_batchInferenceJobConfig,
    createBatchInferenceJob_filterArn,
    createBatchInferenceJob_jobName,
    createBatchInferenceJob_solutionVersionArn,
    createBatchInferenceJob_jobInput,
    createBatchInferenceJob_jobOutput,
    createBatchInferenceJob_roleArn,
    createBatchInferenceJobResponse_batchInferenceJobArn,
    createBatchInferenceJobResponse_httpStatus,

    -- ** CreateFilter
    createFilter_name,
    createFilter_datasetGroupArn,
    createFilter_filterExpression,
    createFilterResponse_filterArn,
    createFilterResponse_httpStatus,

    -- ** CreateDatasetImportJob
    createDatasetImportJob_jobName,
    createDatasetImportJob_datasetArn,
    createDatasetImportJob_dataSource,
    createDatasetImportJob_roleArn,
    createDatasetImportJobResponse_datasetImportJobArn,
    createDatasetImportJobResponse_httpStatus,

    -- ** DescribeSolution
    describeSolution_solutionArn,
    describeSolutionResponse_solution,
    describeSolutionResponse_httpStatus,

    -- ** DescribeDatasetExportJob
    describeDatasetExportJob_datasetExportJobArn,
    describeDatasetExportJobResponse_datasetExportJob,
    describeDatasetExportJobResponse_httpStatus,

    -- ** DeleteCampaign
    deleteCampaign_campaignArn,

    -- ** UpdateCampaign
    updateCampaign_campaignConfig,
    updateCampaign_minProvisionedTPS,
    updateCampaign_solutionVersionArn,
    updateCampaign_campaignArn,
    updateCampaignResponse_campaignArn,
    updateCampaignResponse_httpStatus,

    -- ** ListCampaigns
    listCampaigns_solutionArn,
    listCampaigns_nextToken,
    listCampaigns_maxResults,
    listCampaignsResponse_campaigns,
    listCampaignsResponse_nextToken,
    listCampaignsResponse_httpStatus,

    -- ** DescribeDataset
    describeDataset_datasetArn,
    describeDatasetResponse_dataset,
    describeDatasetResponse_httpStatus,

    -- ** CreateSolutionVersion
    createSolutionVersion_trainingMode,
    createSolutionVersion_solutionArn,
    createSolutionVersionResponse_solutionVersionArn,
    createSolutionVersionResponse_httpStatus,

    -- ** StopSolutionVersionCreation
    stopSolutionVersionCreation_solutionVersionArn,

    -- ** CreateCampaign
    createCampaign_campaignConfig,
    createCampaign_minProvisionedTPS,
    createCampaign_name,
    createCampaign_solutionVersionArn,
    createCampaignResponse_campaignArn,
    createCampaignResponse_httpStatus,

    -- ** DescribeFilter
    describeFilter_filterArn,
    describeFilterResponse_filter,
    describeFilterResponse_httpStatus,

    -- ** ListEventTrackers
    listEventTrackers_nextToken,
    listEventTrackers_datasetGroupArn,
    listEventTrackers_maxResults,
    listEventTrackersResponse_eventTrackers,
    listEventTrackersResponse_nextToken,
    listEventTrackersResponse_httpStatus,

    -- ** CreateDatasetExportJob
    createDatasetExportJob_ingestionMode,
    createDatasetExportJob_jobName,
    createDatasetExportJob_datasetArn,
    createDatasetExportJob_roleArn,
    createDatasetExportJob_jobOutput,
    createDatasetExportJobResponse_datasetExportJobArn,
    createDatasetExportJobResponse_httpStatus,

    -- ** CreateSolution
    createSolution_performAutoML,
    createSolution_recipeArn,
    createSolution_eventType,
    createSolution_solutionConfig,
    createSolution_performHPO,
    createSolution_name,
    createSolution_datasetGroupArn,
    createSolutionResponse_solutionArn,
    createSolutionResponse_httpStatus,

    -- ** DeleteEventTracker
    deleteEventTracker_eventTrackerArn,

    -- ** DescribeDatasetImportJob
    describeDatasetImportJob_datasetImportJobArn,
    describeDatasetImportJobResponse_datasetImportJob,
    describeDatasetImportJobResponse_httpStatus,

    -- ** ListSchemas
    listSchemas_nextToken,
    listSchemas_maxResults,
    listSchemasResponse_schemas,
    listSchemasResponse_nextToken,
    listSchemasResponse_httpStatus,

    -- ** CreateEventTracker
    createEventTracker_name,
    createEventTracker_datasetGroupArn,
    createEventTrackerResponse_trackingId,
    createEventTrackerResponse_eventTrackerArn,
    createEventTrackerResponse_httpStatus,

    -- ** DeleteSolution
    deleteSolution_solutionArn,

    -- ** DescribeCampaign
    describeCampaign_campaignArn,
    describeCampaignResponse_campaign,
    describeCampaignResponse_httpStatus,

    -- ** DeleteDataset
    deleteDataset_datasetArn,

    -- ** CreateDataset
    createDataset_name,
    createDataset_schemaArn,
    createDataset_datasetGroupArn,
    createDataset_datasetType,
    createDatasetResponse_datasetArn,
    createDatasetResponse_httpStatus,

    -- ** DescribeSolutionVersion
    describeSolutionVersion_solutionVersionArn,
    describeSolutionVersionResponse_solutionVersion,
    describeSolutionVersionResponse_httpStatus,

    -- ** DescribeEventTracker
    describeEventTracker_eventTrackerArn,
    describeEventTrackerResponse_eventTracker,
    describeEventTrackerResponse_httpStatus,

    -- ** ListDatasetImportJobs
    listDatasetImportJobs_datasetArn,
    listDatasetImportJobs_nextToken,
    listDatasetImportJobs_maxResults,
    listDatasetImportJobsResponse_datasetImportJobs,
    listDatasetImportJobsResponse_nextToken,
    listDatasetImportJobsResponse_httpStatus,

    -- ** DeleteFilter
    deleteFilter_filterArn,

    -- ** ListBatchInferenceJobs
    listBatchInferenceJobs_nextToken,
    listBatchInferenceJobs_maxResults,
    listBatchInferenceJobs_solutionVersionArn,
    listBatchInferenceJobsResponse_batchInferenceJobs,
    listBatchInferenceJobsResponse_nextToken,
    listBatchInferenceJobsResponse_httpStatus,

    -- ** ListFilters
    listFilters_nextToken,
    listFilters_datasetGroupArn,
    listFilters_maxResults,
    listFiltersResponse_filters,
    listFiltersResponse_nextToken,
    listFiltersResponse_httpStatus,

    -- ** DeleteDatasetGroup
    deleteDatasetGroup_datasetGroupArn,

    -- ** DescribeSchema
    describeSchema_schemaArn,
    describeSchemaResponse_schema,
    describeSchemaResponse_httpStatus,

    -- ** DescribeAlgorithm
    describeAlgorithm_algorithmArn,
    describeAlgorithmResponse_algorithm,
    describeAlgorithmResponse_httpStatus,

    -- ** ListSolutionVersions
    listSolutionVersions_solutionArn,
    listSolutionVersions_nextToken,
    listSolutionVersions_maxResults,
    listSolutionVersionsResponse_nextToken,
    listSolutionVersionsResponse_solutionVersions,
    listSolutionVersionsResponse_httpStatus,

    -- ** DescribeBatchInferenceJob
    describeBatchInferenceJob_batchInferenceJobArn,
    describeBatchInferenceJobResponse_batchInferenceJob,
    describeBatchInferenceJobResponse_httpStatus,

    -- ** CreateSchema
    createSchema_name,
    createSchema_schema,
    createSchemaResponse_schemaArn,
    createSchemaResponse_httpStatus,

    -- ** DescribeRecipe
    describeRecipe_recipeArn,
    describeRecipeResponse_recipe,
    describeRecipeResponse_httpStatus,

    -- ** ListSolutions
    listSolutions_nextToken,
    listSolutions_datasetGroupArn,
    listSolutions_maxResults,
    listSolutionsResponse_nextToken,
    listSolutionsResponse_solutions,
    listSolutionsResponse_httpStatus,

    -- ** ListDatasetExportJobs
    listDatasetExportJobs_datasetArn,
    listDatasetExportJobs_nextToken,
    listDatasetExportJobs_maxResults,
    listDatasetExportJobsResponse_nextToken,
    listDatasetExportJobsResponse_datasetExportJobs,
    listDatasetExportJobsResponse_httpStatus,

    -- ** DescribeDatasetGroup
    describeDatasetGroup_datasetGroupArn,
    describeDatasetGroupResponse_datasetGroup,
    describeDatasetGroupResponse_httpStatus,

    -- ** DescribeFeatureTransformation
    describeFeatureTransformation_featureTransformationArn,
    describeFeatureTransformationResponse_featureTransformation,
    describeFeatureTransformationResponse_httpStatus,

    -- ** GetSolutionMetrics
    getSolutionMetrics_solutionVersionArn,
    getSolutionMetricsResponse_metrics,
    getSolutionMetricsResponse_solutionVersionArn,
    getSolutionMetricsResponse_httpStatus,

    -- ** DeleteSchema
    deleteSchema_schemaArn,

    -- ** ListDatasets
    listDatasets_nextToken,
    listDatasets_datasetGroupArn,
    listDatasets_maxResults,
    listDatasetsResponse_nextToken,
    listDatasetsResponse_datasets,
    listDatasetsResponse_httpStatus,

    -- ** CreateDatasetGroup
    createDatasetGroup_kmsKeyArn,
    createDatasetGroup_roleArn,
    createDatasetGroup_name,
    createDatasetGroupResponse_datasetGroupArn,
    createDatasetGroupResponse_httpStatus,

    -- ** ListRecipes
    listRecipes_nextToken,
    listRecipes_maxResults,
    listRecipes_recipeProvider,
    listRecipesResponse_nextToken,
    listRecipesResponse_recipes,
    listRecipesResponse_httpStatus,

    -- * Types

    -- ** Algorithm
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

    -- ** AlgorithmImage
    algorithmImage_name,
    algorithmImage_dockerURI,

    -- ** AutoMLConfig
    autoMLConfig_recipeList,
    autoMLConfig_metricName,

    -- ** AutoMLResult
    autoMLResult_bestRecipeArn,

    -- ** BatchInferenceJob
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

    -- ** BatchInferenceJobConfig
    batchInferenceJobConfig_itemExplorationConfig,

    -- ** BatchInferenceJobInput
    batchInferenceJobInput_s3DataSource,

    -- ** BatchInferenceJobOutput
    batchInferenceJobOutput_s3DataDestination,

    -- ** BatchInferenceJobSummary
    batchInferenceJobSummary_failureReason,
    batchInferenceJobSummary_status,
    batchInferenceJobSummary_jobName,
    batchInferenceJobSummary_lastUpdatedDateTime,
    batchInferenceJobSummary_batchInferenceJobArn,
    batchInferenceJobSummary_creationDateTime,
    batchInferenceJobSummary_solutionVersionArn,

    -- ** Campaign
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

    -- ** CampaignConfig
    campaignConfig_itemExplorationConfig,

    -- ** CampaignSummary
    campaignSummary_failureReason,
    campaignSummary_status,
    campaignSummary_lastUpdatedDateTime,
    campaignSummary_name,
    campaignSummary_creationDateTime,
    campaignSummary_campaignArn,

    -- ** CampaignUpdateSummary
    campaignUpdateSummary_failureReason,
    campaignUpdateSummary_status,
    campaignUpdateSummary_lastUpdatedDateTime,
    campaignUpdateSummary_campaignConfig,
    campaignUpdateSummary_minProvisionedTPS,
    campaignUpdateSummary_creationDateTime,
    campaignUpdateSummary_solutionVersionArn,

    -- ** CategoricalHyperParameterRange
    categoricalHyperParameterRange_values,
    categoricalHyperParameterRange_name,

    -- ** ContinuousHyperParameterRange
    continuousHyperParameterRange_maxValue,
    continuousHyperParameterRange_name,
    continuousHyperParameterRange_minValue,

    -- ** DataSource
    dataSource_dataLocation,

    -- ** Dataset
    dataset_status,
    dataset_datasetArn,
    dataset_lastUpdatedDateTime,
    dataset_schemaArn,
    dataset_name,
    dataset_datasetType,
    dataset_creationDateTime,
    dataset_datasetGroupArn,

    -- ** DatasetExportJob
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

    -- ** DatasetExportJobOutput
    datasetExportJobOutput_s3DataDestination,

    -- ** DatasetExportJobSummary
    datasetExportJobSummary_failureReason,
    datasetExportJobSummary_status,
    datasetExportJobSummary_datasetExportJobArn,
    datasetExportJobSummary_jobName,
    datasetExportJobSummary_lastUpdatedDateTime,
    datasetExportJobSummary_creationDateTime,

    -- ** DatasetGroup
    datasetGroup_failureReason,
    datasetGroup_status,
    datasetGroup_kmsKeyArn,
    datasetGroup_lastUpdatedDateTime,
    datasetGroup_name,
    datasetGroup_creationDateTime,
    datasetGroup_datasetGroupArn,
    datasetGroup_roleArn,

    -- ** DatasetGroupSummary
    datasetGroupSummary_failureReason,
    datasetGroupSummary_status,
    datasetGroupSummary_lastUpdatedDateTime,
    datasetGroupSummary_name,
    datasetGroupSummary_creationDateTime,
    datasetGroupSummary_datasetGroupArn,

    -- ** DatasetImportJob
    datasetImportJob_failureReason,
    datasetImportJob_status,
    datasetImportJob_datasetArn,
    datasetImportJob_jobName,
    datasetImportJob_lastUpdatedDateTime,
    datasetImportJob_datasetImportJobArn,
    datasetImportJob_dataSource,
    datasetImportJob_creationDateTime,
    datasetImportJob_roleArn,

    -- ** DatasetImportJobSummary
    datasetImportJobSummary_failureReason,
    datasetImportJobSummary_status,
    datasetImportJobSummary_jobName,
    datasetImportJobSummary_lastUpdatedDateTime,
    datasetImportJobSummary_datasetImportJobArn,
    datasetImportJobSummary_creationDateTime,

    -- ** DatasetSchema
    datasetSchema_lastUpdatedDateTime,
    datasetSchema_schema,
    datasetSchema_schemaArn,
    datasetSchema_name,
    datasetSchema_creationDateTime,

    -- ** DatasetSchemaSummary
    datasetSchemaSummary_lastUpdatedDateTime,
    datasetSchemaSummary_schemaArn,
    datasetSchemaSummary_name,
    datasetSchemaSummary_creationDateTime,

    -- ** DatasetSummary
    datasetSummary_status,
    datasetSummary_datasetArn,
    datasetSummary_lastUpdatedDateTime,
    datasetSummary_name,
    datasetSummary_datasetType,
    datasetSummary_creationDateTime,

    -- ** DefaultCategoricalHyperParameterRange
    defaultCategoricalHyperParameterRange_isTunable,
    defaultCategoricalHyperParameterRange_values,
    defaultCategoricalHyperParameterRange_name,

    -- ** DefaultContinuousHyperParameterRange
    defaultContinuousHyperParameterRange_maxValue,
    defaultContinuousHyperParameterRange_isTunable,
    defaultContinuousHyperParameterRange_name,
    defaultContinuousHyperParameterRange_minValue,

    -- ** DefaultHyperParameterRanges
    defaultHyperParameterRanges_integerHyperParameterRanges,
    defaultHyperParameterRanges_categoricalHyperParameterRanges,
    defaultHyperParameterRanges_continuousHyperParameterRanges,

    -- ** DefaultIntegerHyperParameterRange
    defaultIntegerHyperParameterRange_maxValue,
    defaultIntegerHyperParameterRange_isTunable,
    defaultIntegerHyperParameterRange_name,
    defaultIntegerHyperParameterRange_minValue,

    -- ** EventTracker
    eventTracker_status,
    eventTracker_trackingId,
    eventTracker_lastUpdatedDateTime,
    eventTracker_accountId,
    eventTracker_name,
    eventTracker_creationDateTime,
    eventTracker_datasetGroupArn,
    eventTracker_eventTrackerArn,

    -- ** EventTrackerSummary
    eventTrackerSummary_status,
    eventTrackerSummary_lastUpdatedDateTime,
    eventTrackerSummary_name,
    eventTrackerSummary_creationDateTime,
    eventTrackerSummary_eventTrackerArn,

    -- ** FeatureTransformation
    featureTransformation_status,
    featureTransformation_featureTransformationArn,
    featureTransformation_lastUpdatedDateTime,
    featureTransformation_name,
    featureTransformation_creationDateTime,
    featureTransformation_defaultParameters,

    -- ** Filter
    filter_failureReason,
    filter_status,
    filter_filterExpression,
    filter_lastUpdatedDateTime,
    filter_name,
    filter_filterArn,
    filter_creationDateTime,
    filter_datasetGroupArn,

    -- ** FilterSummary
    filterSummary_failureReason,
    filterSummary_status,
    filterSummary_lastUpdatedDateTime,
    filterSummary_name,
    filterSummary_filterArn,
    filterSummary_creationDateTime,
    filterSummary_datasetGroupArn,

    -- ** HPOConfig
    hPOConfig_algorithmHyperParameterRanges,
    hPOConfig_hpoResourceConfig,
    hPOConfig_hpoObjective,

    -- ** HPOObjective
    hPOObjective_metricName,
    hPOObjective_type,
    hPOObjective_metricRegex,

    -- ** HPOResourceConfig
    hPOResourceConfig_maxNumberOfTrainingJobs,
    hPOResourceConfig_maxParallelTrainingJobs,

    -- ** HyperParameterRanges
    hyperParameterRanges_integerHyperParameterRanges,
    hyperParameterRanges_categoricalHyperParameterRanges,
    hyperParameterRanges_continuousHyperParameterRanges,

    -- ** IntegerHyperParameterRange
    integerHyperParameterRange_maxValue,
    integerHyperParameterRange_name,
    integerHyperParameterRange_minValue,

    -- ** OptimizationObjective
    optimizationObjective_itemAttribute,
    optimizationObjective_objectiveSensitivity,

    -- ** Recipe
    recipe_status,
    recipe_algorithmArn,
    recipe_recipeArn,
    recipe_featureTransformationArn,
    recipe_lastUpdatedDateTime,
    recipe_name,
    recipe_creationDateTime,
    recipe_recipeType,
    recipe_description,

    -- ** RecipeSummary
    recipeSummary_status,
    recipeSummary_recipeArn,
    recipeSummary_lastUpdatedDateTime,
    recipeSummary_name,
    recipeSummary_creationDateTime,

    -- ** S3DataConfig
    s3DataConfig_kmsKeyArn,
    s3DataConfig_path,

    -- ** Solution
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

    -- ** SolutionConfig
    solutionConfig_featureTransformationParameters,
    solutionConfig_hpoConfig,
    solutionConfig_eventValueThreshold,
    solutionConfig_autoMLConfig,
    solutionConfig_algorithmHyperParameters,
    solutionConfig_optimizationObjective,

    -- ** SolutionSummary
    solutionSummary_solutionArn,
    solutionSummary_status,
    solutionSummary_lastUpdatedDateTime,
    solutionSummary_name,
    solutionSummary_creationDateTime,

    -- ** SolutionVersion
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

    -- ** SolutionVersionSummary
    solutionVersionSummary_failureReason,
    solutionVersionSummary_status,
    solutionVersionSummary_lastUpdatedDateTime,
    solutionVersionSummary_creationDateTime,
    solutionVersionSummary_solutionVersionArn,

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

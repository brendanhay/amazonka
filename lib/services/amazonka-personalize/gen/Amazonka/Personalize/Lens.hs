{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Personalize.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Lens
  ( -- * Operations

    -- ** CreateBatchInferenceJob
    createBatchInferenceJob_tags,
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

    -- ** CreateBatchSegmentJob
    createBatchSegmentJob_tags,
    createBatchSegmentJob_filterArn,
    createBatchSegmentJob_numResults,
    createBatchSegmentJob_jobName,
    createBatchSegmentJob_solutionVersionArn,
    createBatchSegmentJob_jobInput,
    createBatchSegmentJob_jobOutput,
    createBatchSegmentJob_roleArn,
    createBatchSegmentJobResponse_batchSegmentJobArn,
    createBatchSegmentJobResponse_httpStatus,

    -- ** CreateCampaign
    createCampaign_tags,
    createCampaign_campaignConfig,
    createCampaign_minProvisionedTPS,
    createCampaign_name,
    createCampaign_solutionVersionArn,
    createCampaignResponse_campaignArn,
    createCampaignResponse_httpStatus,

    -- ** CreateDataset
    createDataset_tags,
    createDataset_name,
    createDataset_schemaArn,
    createDataset_datasetGroupArn,
    createDataset_datasetType,
    createDatasetResponse_datasetArn,
    createDatasetResponse_httpStatus,

    -- ** CreateDatasetExportJob
    createDatasetExportJob_tags,
    createDatasetExportJob_ingestionMode,
    createDatasetExportJob_jobName,
    createDatasetExportJob_datasetArn,
    createDatasetExportJob_roleArn,
    createDatasetExportJob_jobOutput,
    createDatasetExportJobResponse_datasetExportJobArn,
    createDatasetExportJobResponse_httpStatus,

    -- ** CreateDatasetGroup
    createDatasetGroup_tags,
    createDatasetGroup_roleArn,
    createDatasetGroup_domain,
    createDatasetGroup_kmsKeyArn,
    createDatasetGroup_name,
    createDatasetGroupResponse_domain,
    createDatasetGroupResponse_datasetGroupArn,
    createDatasetGroupResponse_httpStatus,

    -- ** CreateDatasetImportJob
    createDatasetImportJob_tags,
    createDatasetImportJob_publishAttributionMetricsToS3,
    createDatasetImportJob_importMode,
    createDatasetImportJob_jobName,
    createDatasetImportJob_datasetArn,
    createDatasetImportJob_dataSource,
    createDatasetImportJob_roleArn,
    createDatasetImportJobResponse_datasetImportJobArn,
    createDatasetImportJobResponse_httpStatus,

    -- ** CreateEventTracker
    createEventTracker_tags,
    createEventTracker_name,
    createEventTracker_datasetGroupArn,
    createEventTrackerResponse_trackingId,
    createEventTrackerResponse_eventTrackerArn,
    createEventTrackerResponse_httpStatus,

    -- ** CreateFilter
    createFilter_tags,
    createFilter_name,
    createFilter_datasetGroupArn,
    createFilter_filterExpression,
    createFilterResponse_filterArn,
    createFilterResponse_httpStatus,

    -- ** CreateMetricAttribution
    createMetricAttribution_name,
    createMetricAttribution_datasetGroupArn,
    createMetricAttribution_metrics,
    createMetricAttribution_metricsOutputConfig,
    createMetricAttributionResponse_metricAttributionArn,
    createMetricAttributionResponse_httpStatus,

    -- ** CreateRecommender
    createRecommender_tags,
    createRecommender_recommenderConfig,
    createRecommender_name,
    createRecommender_datasetGroupArn,
    createRecommender_recipeArn,
    createRecommenderResponse_recommenderArn,
    createRecommenderResponse_httpStatus,

    -- ** CreateSchema
    createSchema_domain,
    createSchema_name,
    createSchema_schema,
    createSchemaResponse_schemaArn,
    createSchemaResponse_httpStatus,

    -- ** CreateSolution
    createSolution_tags,
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
    createSolutionVersion_tags,
    createSolutionVersion_name,
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

    -- ** DeleteMetricAttribution
    deleteMetricAttribution_metricAttributionArn,

    -- ** DeleteRecommender
    deleteRecommender_recommenderArn,

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

    -- ** DescribeBatchSegmentJob
    describeBatchSegmentJob_batchSegmentJobArn,
    describeBatchSegmentJobResponse_batchSegmentJob,
    describeBatchSegmentJobResponse_httpStatus,

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

    -- ** DescribeMetricAttribution
    describeMetricAttribution_metricAttributionArn,
    describeMetricAttributionResponse_metricAttribution,
    describeMetricAttributionResponse_httpStatus,

    -- ** DescribeRecipe
    describeRecipe_recipeArn,
    describeRecipeResponse_recipe,
    describeRecipeResponse_httpStatus,

    -- ** DescribeRecommender
    describeRecommender_recommenderArn,
    describeRecommenderResponse_recommender,
    describeRecommenderResponse_httpStatus,

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

    -- ** ListBatchSegmentJobs
    listBatchSegmentJobs_nextToken,
    listBatchSegmentJobs_maxResults,
    listBatchSegmentJobs_solutionVersionArn,
    listBatchSegmentJobsResponse_nextToken,
    listBatchSegmentJobsResponse_batchSegmentJobs,
    listBatchSegmentJobsResponse_httpStatus,

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

    -- ** ListMetricAttributionMetrics
    listMetricAttributionMetrics_nextToken,
    listMetricAttributionMetrics_maxResults,
    listMetricAttributionMetrics_metricAttributionArn,
    listMetricAttributionMetricsResponse_nextToken,
    listMetricAttributionMetricsResponse_metrics,
    listMetricAttributionMetricsResponse_httpStatus,

    -- ** ListMetricAttributions
    listMetricAttributions_nextToken,
    listMetricAttributions_maxResults,
    listMetricAttributions_datasetGroupArn,
    listMetricAttributionsResponse_nextToken,
    listMetricAttributionsResponse_metricAttributions,
    listMetricAttributionsResponse_httpStatus,

    -- ** ListRecipes
    listRecipes_nextToken,
    listRecipes_recipeProvider,
    listRecipes_domain,
    listRecipes_maxResults,
    listRecipesResponse_recipes,
    listRecipesResponse_nextToken,
    listRecipesResponse_httpStatus,

    -- ** ListRecommenders
    listRecommenders_nextToken,
    listRecommenders_maxResults,
    listRecommenders_datasetGroupArn,
    listRecommendersResponse_nextToken,
    listRecommendersResponse_recommenders,
    listRecommendersResponse_httpStatus,

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

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** StartRecommender
    startRecommender_recommenderArn,
    startRecommenderResponse_recommenderArn,
    startRecommenderResponse_httpStatus,

    -- ** StopRecommender
    stopRecommender_recommenderArn,
    stopRecommenderResponse_recommenderArn,
    stopRecommenderResponse_httpStatus,

    -- ** StopSolutionVersionCreation
    stopSolutionVersionCreation_solutionVersionArn,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateCampaign
    updateCampaign_campaignConfig,
    updateCampaign_minProvisionedTPS,
    updateCampaign_solutionVersionArn,
    updateCampaign_campaignArn,
    updateCampaignResponse_campaignArn,
    updateCampaignResponse_httpStatus,

    -- ** UpdateMetricAttribution
    updateMetricAttribution_addMetrics,
    updateMetricAttribution_metricsOutputConfig,
    updateMetricAttribution_metricAttributionArn,
    updateMetricAttribution_removeMetrics,
    updateMetricAttributionResponse_metricAttributionArn,
    updateMetricAttributionResponse_httpStatus,

    -- ** UpdateRecommender
    updateRecommender_recommenderArn,
    updateRecommender_recommenderConfig,
    updateRecommenderResponse_recommenderArn,
    updateRecommenderResponse_httpStatus,

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

    -- ** BatchSegmentJob
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

    -- ** BatchSegmentJobInput
    batchSegmentJobInput_s3DataSource,

    -- ** BatchSegmentJobOutput
    batchSegmentJobOutput_s3DataDestination,

    -- ** BatchSegmentJobSummary
    batchSegmentJobSummary_creationDateTime,
    batchSegmentJobSummary_jobName,
    batchSegmentJobSummary_status,
    batchSegmentJobSummary_solutionVersionArn,
    batchSegmentJobSummary_lastUpdatedDateTime,
    batchSegmentJobSummary_failureReason,
    batchSegmentJobSummary_batchSegmentJobArn,

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
    datasetGroup_domain,
    datasetGroup_status,
    datasetGroup_kmsKeyArn,
    datasetGroup_datasetGroupArn,
    datasetGroup_lastUpdatedDateTime,
    datasetGroup_failureReason,

    -- ** DatasetGroupSummary
    datasetGroupSummary_name,
    datasetGroupSummary_creationDateTime,
    datasetGroupSummary_domain,
    datasetGroupSummary_status,
    datasetGroupSummary_datasetGroupArn,
    datasetGroupSummary_lastUpdatedDateTime,
    datasetGroupSummary_failureReason,

    -- ** DatasetImportJob
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

    -- ** DatasetImportJobSummary
    datasetImportJobSummary_creationDateTime,
    datasetImportJobSummary_jobName,
    datasetImportJobSummary_status,
    datasetImportJobSummary_datasetImportJobArn,
    datasetImportJobSummary_importMode,
    datasetImportJobSummary_lastUpdatedDateTime,
    datasetImportJobSummary_failureReason,

    -- ** DatasetSchema
    datasetSchema_name,
    datasetSchema_creationDateTime,
    datasetSchema_domain,
    datasetSchema_schemaArn,
    datasetSchema_schema,
    datasetSchema_lastUpdatedDateTime,

    -- ** DatasetSchemaSummary
    datasetSchemaSummary_name,
    datasetSchemaSummary_creationDateTime,
    datasetSchemaSummary_domain,
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

    -- ** MetricAttribute
    metricAttribute_eventType,
    metricAttribute_metricName,
    metricAttribute_expression,

    -- ** MetricAttribution
    metricAttribution_name,
    metricAttribution_creationDateTime,
    metricAttribution_status,
    metricAttribution_metricsOutputConfig,
    metricAttribution_metricAttributionArn,
    metricAttribution_datasetGroupArn,
    metricAttribution_lastUpdatedDateTime,
    metricAttribution_failureReason,

    -- ** MetricAttributionOutput
    metricAttributionOutput_s3DataDestination,
    metricAttributionOutput_roleArn,

    -- ** MetricAttributionSummary
    metricAttributionSummary_name,
    metricAttributionSummary_creationDateTime,
    metricAttributionSummary_status,
    metricAttributionSummary_metricAttributionArn,
    metricAttributionSummary_lastUpdatedDateTime,
    metricAttributionSummary_failureReason,

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
    recipeSummary_domain,
    recipeSummary_status,
    recipeSummary_recipeArn,
    recipeSummary_lastUpdatedDateTime,

    -- ** Recommender
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

    -- ** RecommenderConfig
    recommenderConfig_minRecommendationRequestsPerSecond,
    recommenderConfig_itemExplorationConfig,

    -- ** RecommenderSummary
    recommenderSummary_name,
    recommenderSummary_creationDateTime,
    recommenderSummary_recommenderArn,
    recommenderSummary_recommenderConfig,
    recommenderSummary_status,
    recommenderSummary_datasetGroupArn,
    recommenderSummary_recipeArn,
    recommenderSummary_lastUpdatedDateTime,

    -- ** RecommenderUpdateSummary
    recommenderUpdateSummary_creationDateTime,
    recommenderUpdateSummary_recommenderConfig,
    recommenderUpdateSummary_status,
    recommenderUpdateSummary_lastUpdatedDateTime,
    recommenderUpdateSummary_failureReason,

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
    solutionSummary_recipeArn,
    solutionSummary_lastUpdatedDateTime,

    -- ** SolutionVersion
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

    -- ** SolutionVersionSummary
    solutionVersionSummary_creationDateTime,
    solutionVersionSummary_status,
    solutionVersionSummary_solutionVersionArn,
    solutionVersionSummary_lastUpdatedDateTime,
    solutionVersionSummary_failureReason,

    -- ** Tag
    tag_tagKey,
    tag_tagValue,

    -- ** TunedHPOParams
    tunedHPOParams_algorithmHyperParameters,
  )
where

import Amazonka.Personalize.CreateBatchInferenceJob
import Amazonka.Personalize.CreateBatchSegmentJob
import Amazonka.Personalize.CreateCampaign
import Amazonka.Personalize.CreateDataset
import Amazonka.Personalize.CreateDatasetExportJob
import Amazonka.Personalize.CreateDatasetGroup
import Amazonka.Personalize.CreateDatasetImportJob
import Amazonka.Personalize.CreateEventTracker
import Amazonka.Personalize.CreateFilter
import Amazonka.Personalize.CreateMetricAttribution
import Amazonka.Personalize.CreateRecommender
import Amazonka.Personalize.CreateSchema
import Amazonka.Personalize.CreateSolution
import Amazonka.Personalize.CreateSolutionVersion
import Amazonka.Personalize.DeleteCampaign
import Amazonka.Personalize.DeleteDataset
import Amazonka.Personalize.DeleteDatasetGroup
import Amazonka.Personalize.DeleteEventTracker
import Amazonka.Personalize.DeleteFilter
import Amazonka.Personalize.DeleteMetricAttribution
import Amazonka.Personalize.DeleteRecommender
import Amazonka.Personalize.DeleteSchema
import Amazonka.Personalize.DeleteSolution
import Amazonka.Personalize.DescribeAlgorithm
import Amazonka.Personalize.DescribeBatchInferenceJob
import Amazonka.Personalize.DescribeBatchSegmentJob
import Amazonka.Personalize.DescribeCampaign
import Amazonka.Personalize.DescribeDataset
import Amazonka.Personalize.DescribeDatasetExportJob
import Amazonka.Personalize.DescribeDatasetGroup
import Amazonka.Personalize.DescribeDatasetImportJob
import Amazonka.Personalize.DescribeEventTracker
import Amazonka.Personalize.DescribeFeatureTransformation
import Amazonka.Personalize.DescribeFilter
import Amazonka.Personalize.DescribeMetricAttribution
import Amazonka.Personalize.DescribeRecipe
import Amazonka.Personalize.DescribeRecommender
import Amazonka.Personalize.DescribeSchema
import Amazonka.Personalize.DescribeSolution
import Amazonka.Personalize.DescribeSolutionVersion
import Amazonka.Personalize.GetSolutionMetrics
import Amazonka.Personalize.ListBatchInferenceJobs
import Amazonka.Personalize.ListBatchSegmentJobs
import Amazonka.Personalize.ListCampaigns
import Amazonka.Personalize.ListDatasetExportJobs
import Amazonka.Personalize.ListDatasetGroups
import Amazonka.Personalize.ListDatasetImportJobs
import Amazonka.Personalize.ListDatasets
import Amazonka.Personalize.ListEventTrackers
import Amazonka.Personalize.ListFilters
import Amazonka.Personalize.ListMetricAttributionMetrics
import Amazonka.Personalize.ListMetricAttributions
import Amazonka.Personalize.ListRecipes
import Amazonka.Personalize.ListRecommenders
import Amazonka.Personalize.ListSchemas
import Amazonka.Personalize.ListSolutionVersions
import Amazonka.Personalize.ListSolutions
import Amazonka.Personalize.ListTagsForResource
import Amazonka.Personalize.StartRecommender
import Amazonka.Personalize.StopRecommender
import Amazonka.Personalize.StopSolutionVersionCreation
import Amazonka.Personalize.TagResource
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
import Amazonka.Personalize.Types.MetricAttribute
import Amazonka.Personalize.Types.MetricAttribution
import Amazonka.Personalize.Types.MetricAttributionOutput
import Amazonka.Personalize.Types.MetricAttributionSummary
import Amazonka.Personalize.Types.OptimizationObjective
import Amazonka.Personalize.Types.Recipe
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
import Amazonka.Personalize.Types.TunedHPOParams
import Amazonka.Personalize.UntagResource
import Amazonka.Personalize.UpdateCampaign
import Amazonka.Personalize.UpdateMetricAttribution
import Amazonka.Personalize.UpdateRecommender

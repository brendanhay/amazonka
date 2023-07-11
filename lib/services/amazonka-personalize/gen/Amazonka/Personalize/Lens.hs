{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Personalize.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Lens
  ( -- * Operations

    -- ** CreateBatchInferenceJob
    createBatchInferenceJob_batchInferenceJobConfig,
    createBatchInferenceJob_filterArn,
    createBatchInferenceJob_numResults,
    createBatchInferenceJob_tags,
    createBatchInferenceJob_jobName,
    createBatchInferenceJob_solutionVersionArn,
    createBatchInferenceJob_jobInput,
    createBatchInferenceJob_jobOutput,
    createBatchInferenceJob_roleArn,
    createBatchInferenceJobResponse_batchInferenceJobArn,
    createBatchInferenceJobResponse_httpStatus,

    -- ** CreateBatchSegmentJob
    createBatchSegmentJob_filterArn,
    createBatchSegmentJob_numResults,
    createBatchSegmentJob_tags,
    createBatchSegmentJob_jobName,
    createBatchSegmentJob_solutionVersionArn,
    createBatchSegmentJob_jobInput,
    createBatchSegmentJob_jobOutput,
    createBatchSegmentJob_roleArn,
    createBatchSegmentJobResponse_batchSegmentJobArn,
    createBatchSegmentJobResponse_httpStatus,

    -- ** CreateCampaign
    createCampaign_campaignConfig,
    createCampaign_minProvisionedTPS,
    createCampaign_tags,
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
    createDatasetExportJob_ingestionMode,
    createDatasetExportJob_tags,
    createDatasetExportJob_jobName,
    createDatasetExportJob_datasetArn,
    createDatasetExportJob_roleArn,
    createDatasetExportJob_jobOutput,
    createDatasetExportJobResponse_datasetExportJobArn,
    createDatasetExportJobResponse_httpStatus,

    -- ** CreateDatasetGroup
    createDatasetGroup_domain,
    createDatasetGroup_kmsKeyArn,
    createDatasetGroup_roleArn,
    createDatasetGroup_tags,
    createDatasetGroup_name,
    createDatasetGroupResponse_datasetGroupArn,
    createDatasetGroupResponse_domain,
    createDatasetGroupResponse_httpStatus,

    -- ** CreateDatasetImportJob
    createDatasetImportJob_importMode,
    createDatasetImportJob_publishAttributionMetricsToS3,
    createDatasetImportJob_tags,
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
    createEventTrackerResponse_eventTrackerArn,
    createEventTrackerResponse_trackingId,
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
    createRecommender_recommenderConfig,
    createRecommender_tags,
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
    createSolution_eventType,
    createSolution_performAutoML,
    createSolution_performHPO,
    createSolution_recipeArn,
    createSolution_solutionConfig,
    createSolution_tags,
    createSolution_name,
    createSolution_datasetGroupArn,
    createSolutionResponse_solutionArn,
    createSolutionResponse_httpStatus,

    -- ** CreateSolutionVersion
    createSolutionVersion_name,
    createSolutionVersion_tags,
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
    listBatchInferenceJobs_maxResults,
    listBatchInferenceJobs_nextToken,
    listBatchInferenceJobs_solutionVersionArn,
    listBatchInferenceJobsResponse_batchInferenceJobs,
    listBatchInferenceJobsResponse_nextToken,
    listBatchInferenceJobsResponse_httpStatus,

    -- ** ListBatchSegmentJobs
    listBatchSegmentJobs_maxResults,
    listBatchSegmentJobs_nextToken,
    listBatchSegmentJobs_solutionVersionArn,
    listBatchSegmentJobsResponse_batchSegmentJobs,
    listBatchSegmentJobsResponse_nextToken,
    listBatchSegmentJobsResponse_httpStatus,

    -- ** ListCampaigns
    listCampaigns_maxResults,
    listCampaigns_nextToken,
    listCampaigns_solutionArn,
    listCampaignsResponse_campaigns,
    listCampaignsResponse_nextToken,
    listCampaignsResponse_httpStatus,

    -- ** ListDatasetExportJobs
    listDatasetExportJobs_datasetArn,
    listDatasetExportJobs_maxResults,
    listDatasetExportJobs_nextToken,
    listDatasetExportJobsResponse_datasetExportJobs,
    listDatasetExportJobsResponse_nextToken,
    listDatasetExportJobsResponse_httpStatus,

    -- ** ListDatasetGroups
    listDatasetGroups_maxResults,
    listDatasetGroups_nextToken,
    listDatasetGroupsResponse_datasetGroups,
    listDatasetGroupsResponse_nextToken,
    listDatasetGroupsResponse_httpStatus,

    -- ** ListDatasetImportJobs
    listDatasetImportJobs_datasetArn,
    listDatasetImportJobs_maxResults,
    listDatasetImportJobs_nextToken,
    listDatasetImportJobsResponse_datasetImportJobs,
    listDatasetImportJobsResponse_nextToken,
    listDatasetImportJobsResponse_httpStatus,

    -- ** ListDatasets
    listDatasets_datasetGroupArn,
    listDatasets_maxResults,
    listDatasets_nextToken,
    listDatasetsResponse_datasets,
    listDatasetsResponse_nextToken,
    listDatasetsResponse_httpStatus,

    -- ** ListEventTrackers
    listEventTrackers_datasetGroupArn,
    listEventTrackers_maxResults,
    listEventTrackers_nextToken,
    listEventTrackersResponse_eventTrackers,
    listEventTrackersResponse_nextToken,
    listEventTrackersResponse_httpStatus,

    -- ** ListFilters
    listFilters_datasetGroupArn,
    listFilters_maxResults,
    listFilters_nextToken,
    listFiltersResponse_filters,
    listFiltersResponse_nextToken,
    listFiltersResponse_httpStatus,

    -- ** ListMetricAttributionMetrics
    listMetricAttributionMetrics_maxResults,
    listMetricAttributionMetrics_metricAttributionArn,
    listMetricAttributionMetrics_nextToken,
    listMetricAttributionMetricsResponse_metrics,
    listMetricAttributionMetricsResponse_nextToken,
    listMetricAttributionMetricsResponse_httpStatus,

    -- ** ListMetricAttributions
    listMetricAttributions_datasetGroupArn,
    listMetricAttributions_maxResults,
    listMetricAttributions_nextToken,
    listMetricAttributionsResponse_metricAttributions,
    listMetricAttributionsResponse_nextToken,
    listMetricAttributionsResponse_httpStatus,

    -- ** ListRecipes
    listRecipes_domain,
    listRecipes_maxResults,
    listRecipes_nextToken,
    listRecipes_recipeProvider,
    listRecipesResponse_nextToken,
    listRecipesResponse_recipes,
    listRecipesResponse_httpStatus,

    -- ** ListRecommenders
    listRecommenders_datasetGroupArn,
    listRecommenders_maxResults,
    listRecommenders_nextToken,
    listRecommendersResponse_nextToken,
    listRecommendersResponse_recommenders,
    listRecommendersResponse_httpStatus,

    -- ** ListSchemas
    listSchemas_maxResults,
    listSchemas_nextToken,
    listSchemasResponse_nextToken,
    listSchemasResponse_schemas,
    listSchemasResponse_httpStatus,

    -- ** ListSolutionVersions
    listSolutionVersions_maxResults,
    listSolutionVersions_nextToken,
    listSolutionVersions_solutionArn,
    listSolutionVersionsResponse_nextToken,
    listSolutionVersionsResponse_solutionVersions,
    listSolutionVersionsResponse_httpStatus,

    -- ** ListSolutions
    listSolutions_datasetGroupArn,
    listSolutions_maxResults,
    listSolutions_nextToken,
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
    updateMetricAttribution_metricAttributionArn,
    updateMetricAttribution_metricsOutputConfig,
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

    -- ** AlgorithmImage
    algorithmImage_name,
    algorithmImage_dockerURI,

    -- ** AutoMLConfig
    autoMLConfig_metricName,
    autoMLConfig_recipeList,

    -- ** AutoMLResult
    autoMLResult_bestRecipeArn,

    -- ** BatchInferenceJob
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

    -- ** BatchInferenceJobConfig
    batchInferenceJobConfig_itemExplorationConfig,

    -- ** BatchInferenceJobInput
    batchInferenceJobInput_s3DataSource,

    -- ** BatchInferenceJobOutput
    batchInferenceJobOutput_s3DataDestination,

    -- ** BatchInferenceJobSummary
    batchInferenceJobSummary_batchInferenceJobArn,
    batchInferenceJobSummary_creationDateTime,
    batchInferenceJobSummary_failureReason,
    batchInferenceJobSummary_jobName,
    batchInferenceJobSummary_lastUpdatedDateTime,
    batchInferenceJobSummary_solutionVersionArn,
    batchInferenceJobSummary_status,

    -- ** BatchSegmentJob
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

    -- ** BatchSegmentJobInput
    batchSegmentJobInput_s3DataSource,

    -- ** BatchSegmentJobOutput
    batchSegmentJobOutput_s3DataDestination,

    -- ** BatchSegmentJobSummary
    batchSegmentJobSummary_batchSegmentJobArn,
    batchSegmentJobSummary_creationDateTime,
    batchSegmentJobSummary_failureReason,
    batchSegmentJobSummary_jobName,
    batchSegmentJobSummary_lastUpdatedDateTime,
    batchSegmentJobSummary_solutionVersionArn,
    batchSegmentJobSummary_status,

    -- ** Campaign
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

    -- ** CampaignConfig
    campaignConfig_itemExplorationConfig,

    -- ** CampaignSummary
    campaignSummary_campaignArn,
    campaignSummary_creationDateTime,
    campaignSummary_failureReason,
    campaignSummary_lastUpdatedDateTime,
    campaignSummary_name,
    campaignSummary_status,

    -- ** CampaignUpdateSummary
    campaignUpdateSummary_campaignConfig,
    campaignUpdateSummary_creationDateTime,
    campaignUpdateSummary_failureReason,
    campaignUpdateSummary_lastUpdatedDateTime,
    campaignUpdateSummary_minProvisionedTPS,
    campaignUpdateSummary_solutionVersionArn,
    campaignUpdateSummary_status,

    -- ** CategoricalHyperParameterRange
    categoricalHyperParameterRange_name,
    categoricalHyperParameterRange_values,

    -- ** ContinuousHyperParameterRange
    continuousHyperParameterRange_maxValue,
    continuousHyperParameterRange_minValue,
    continuousHyperParameterRange_name,

    -- ** DataSource
    dataSource_dataLocation,

    -- ** Dataset
    dataset_creationDateTime,
    dataset_datasetArn,
    dataset_datasetGroupArn,
    dataset_datasetType,
    dataset_lastUpdatedDateTime,
    dataset_name,
    dataset_schemaArn,
    dataset_status,

    -- ** DatasetExportJob
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

    -- ** DatasetExportJobOutput
    datasetExportJobOutput_s3DataDestination,

    -- ** DatasetExportJobSummary
    datasetExportJobSummary_creationDateTime,
    datasetExportJobSummary_datasetExportJobArn,
    datasetExportJobSummary_failureReason,
    datasetExportJobSummary_jobName,
    datasetExportJobSummary_lastUpdatedDateTime,
    datasetExportJobSummary_status,

    -- ** DatasetGroup
    datasetGroup_creationDateTime,
    datasetGroup_datasetGroupArn,
    datasetGroup_domain,
    datasetGroup_failureReason,
    datasetGroup_kmsKeyArn,
    datasetGroup_lastUpdatedDateTime,
    datasetGroup_name,
    datasetGroup_roleArn,
    datasetGroup_status,

    -- ** DatasetGroupSummary
    datasetGroupSummary_creationDateTime,
    datasetGroupSummary_datasetGroupArn,
    datasetGroupSummary_domain,
    datasetGroupSummary_failureReason,
    datasetGroupSummary_lastUpdatedDateTime,
    datasetGroupSummary_name,
    datasetGroupSummary_status,

    -- ** DatasetImportJob
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

    -- ** DatasetImportJobSummary
    datasetImportJobSummary_creationDateTime,
    datasetImportJobSummary_datasetImportJobArn,
    datasetImportJobSummary_failureReason,
    datasetImportJobSummary_importMode,
    datasetImportJobSummary_jobName,
    datasetImportJobSummary_lastUpdatedDateTime,
    datasetImportJobSummary_status,

    -- ** DatasetSchema
    datasetSchema_creationDateTime,
    datasetSchema_domain,
    datasetSchema_lastUpdatedDateTime,
    datasetSchema_name,
    datasetSchema_schema,
    datasetSchema_schemaArn,

    -- ** DatasetSchemaSummary
    datasetSchemaSummary_creationDateTime,
    datasetSchemaSummary_domain,
    datasetSchemaSummary_lastUpdatedDateTime,
    datasetSchemaSummary_name,
    datasetSchemaSummary_schemaArn,

    -- ** DatasetSummary
    datasetSummary_creationDateTime,
    datasetSummary_datasetArn,
    datasetSummary_datasetType,
    datasetSummary_lastUpdatedDateTime,
    datasetSummary_name,
    datasetSummary_status,

    -- ** DefaultCategoricalHyperParameterRange
    defaultCategoricalHyperParameterRange_isTunable,
    defaultCategoricalHyperParameterRange_name,
    defaultCategoricalHyperParameterRange_values,

    -- ** DefaultContinuousHyperParameterRange
    defaultContinuousHyperParameterRange_isTunable,
    defaultContinuousHyperParameterRange_maxValue,
    defaultContinuousHyperParameterRange_minValue,
    defaultContinuousHyperParameterRange_name,

    -- ** DefaultHyperParameterRanges
    defaultHyperParameterRanges_categoricalHyperParameterRanges,
    defaultHyperParameterRanges_continuousHyperParameterRanges,
    defaultHyperParameterRanges_integerHyperParameterRanges,

    -- ** DefaultIntegerHyperParameterRange
    defaultIntegerHyperParameterRange_isTunable,
    defaultIntegerHyperParameterRange_maxValue,
    defaultIntegerHyperParameterRange_minValue,
    defaultIntegerHyperParameterRange_name,

    -- ** EventTracker
    eventTracker_accountId,
    eventTracker_creationDateTime,
    eventTracker_datasetGroupArn,
    eventTracker_eventTrackerArn,
    eventTracker_lastUpdatedDateTime,
    eventTracker_name,
    eventTracker_status,
    eventTracker_trackingId,

    -- ** EventTrackerSummary
    eventTrackerSummary_creationDateTime,
    eventTrackerSummary_eventTrackerArn,
    eventTrackerSummary_lastUpdatedDateTime,
    eventTrackerSummary_name,
    eventTrackerSummary_status,

    -- ** FeatureTransformation
    featureTransformation_creationDateTime,
    featureTransformation_defaultParameters,
    featureTransformation_featureTransformationArn,
    featureTransformation_lastUpdatedDateTime,
    featureTransformation_name,
    featureTransformation_status,

    -- ** Filter
    filter_creationDateTime,
    filter_datasetGroupArn,
    filter_failureReason,
    filter_filterArn,
    filter_filterExpression,
    filter_lastUpdatedDateTime,
    filter_name,
    filter_status,

    -- ** FilterSummary
    filterSummary_creationDateTime,
    filterSummary_datasetGroupArn,
    filterSummary_failureReason,
    filterSummary_filterArn,
    filterSummary_lastUpdatedDateTime,
    filterSummary_name,
    filterSummary_status,

    -- ** HPOConfig
    hPOConfig_algorithmHyperParameterRanges,
    hPOConfig_hpoObjective,
    hPOConfig_hpoResourceConfig,

    -- ** HPOObjective
    hPOObjective_metricName,
    hPOObjective_metricRegex,
    hPOObjective_type,

    -- ** HPOResourceConfig
    hPOResourceConfig_maxNumberOfTrainingJobs,
    hPOResourceConfig_maxParallelTrainingJobs,

    -- ** HyperParameterRanges
    hyperParameterRanges_categoricalHyperParameterRanges,
    hyperParameterRanges_continuousHyperParameterRanges,
    hyperParameterRanges_integerHyperParameterRanges,

    -- ** IntegerHyperParameterRange
    integerHyperParameterRange_maxValue,
    integerHyperParameterRange_minValue,
    integerHyperParameterRange_name,

    -- ** MetricAttribute
    metricAttribute_eventType,
    metricAttribute_metricName,
    metricAttribute_expression,

    -- ** MetricAttribution
    metricAttribution_creationDateTime,
    metricAttribution_datasetGroupArn,
    metricAttribution_failureReason,
    metricAttribution_lastUpdatedDateTime,
    metricAttribution_metricAttributionArn,
    metricAttribution_metricsOutputConfig,
    metricAttribution_name,
    metricAttribution_status,

    -- ** MetricAttributionOutput
    metricAttributionOutput_s3DataDestination,
    metricAttributionOutput_roleArn,

    -- ** MetricAttributionSummary
    metricAttributionSummary_creationDateTime,
    metricAttributionSummary_failureReason,
    metricAttributionSummary_lastUpdatedDateTime,
    metricAttributionSummary_metricAttributionArn,
    metricAttributionSummary_name,
    metricAttributionSummary_status,

    -- ** OptimizationObjective
    optimizationObjective_itemAttribute,
    optimizationObjective_objectiveSensitivity,

    -- ** Recipe
    recipe_algorithmArn,
    recipe_creationDateTime,
    recipe_description,
    recipe_featureTransformationArn,
    recipe_lastUpdatedDateTime,
    recipe_name,
    recipe_recipeArn,
    recipe_recipeType,
    recipe_status,

    -- ** RecipeSummary
    recipeSummary_creationDateTime,
    recipeSummary_domain,
    recipeSummary_lastUpdatedDateTime,
    recipeSummary_name,
    recipeSummary_recipeArn,
    recipeSummary_status,

    -- ** Recommender
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

    -- ** RecommenderConfig
    recommenderConfig_itemExplorationConfig,
    recommenderConfig_minRecommendationRequestsPerSecond,

    -- ** RecommenderSummary
    recommenderSummary_creationDateTime,
    recommenderSummary_datasetGroupArn,
    recommenderSummary_lastUpdatedDateTime,
    recommenderSummary_name,
    recommenderSummary_recipeArn,
    recommenderSummary_recommenderArn,
    recommenderSummary_recommenderConfig,
    recommenderSummary_status,

    -- ** RecommenderUpdateSummary
    recommenderUpdateSummary_creationDateTime,
    recommenderUpdateSummary_failureReason,
    recommenderUpdateSummary_lastUpdatedDateTime,
    recommenderUpdateSummary_recommenderConfig,
    recommenderUpdateSummary_status,

    -- ** S3DataConfig
    s3DataConfig_kmsKeyArn,
    s3DataConfig_path,

    -- ** Solution
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

    -- ** SolutionConfig
    solutionConfig_algorithmHyperParameters,
    solutionConfig_autoMLConfig,
    solutionConfig_eventValueThreshold,
    solutionConfig_featureTransformationParameters,
    solutionConfig_hpoConfig,
    solutionConfig_optimizationObjective,

    -- ** SolutionSummary
    solutionSummary_creationDateTime,
    solutionSummary_lastUpdatedDateTime,
    solutionSummary_name,
    solutionSummary_recipeArn,
    solutionSummary_solutionArn,
    solutionSummary_status,

    -- ** SolutionVersion
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

    -- ** SolutionVersionSummary
    solutionVersionSummary_creationDateTime,
    solutionVersionSummary_failureReason,
    solutionVersionSummary_lastUpdatedDateTime,
    solutionVersionSummary_solutionVersionArn,
    solutionVersionSummary_status,

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

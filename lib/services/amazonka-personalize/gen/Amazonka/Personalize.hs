{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Personalize
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-05-22@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Personalize is a machine learning service that makes it easy to
-- add individualized recommendations to customers.
module Amazonka.Personalize
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

    -- ** InvalidInputException
    _InvalidInputException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** TooManyTagKeysException
    _TooManyTagKeysException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateBatchInferenceJob
    CreateBatchInferenceJob (CreateBatchInferenceJob'),
    newCreateBatchInferenceJob,
    CreateBatchInferenceJobResponse (CreateBatchInferenceJobResponse'),
    newCreateBatchInferenceJobResponse,

    -- ** CreateBatchSegmentJob
    CreateBatchSegmentJob (CreateBatchSegmentJob'),
    newCreateBatchSegmentJob,
    CreateBatchSegmentJobResponse (CreateBatchSegmentJobResponse'),
    newCreateBatchSegmentJobResponse,

    -- ** CreateCampaign
    CreateCampaign (CreateCampaign'),
    newCreateCampaign,
    CreateCampaignResponse (CreateCampaignResponse'),
    newCreateCampaignResponse,

    -- ** CreateDataset
    CreateDataset (CreateDataset'),
    newCreateDataset,
    CreateDatasetResponse (CreateDatasetResponse'),
    newCreateDatasetResponse,

    -- ** CreateDatasetExportJob
    CreateDatasetExportJob (CreateDatasetExportJob'),
    newCreateDatasetExportJob,
    CreateDatasetExportJobResponse (CreateDatasetExportJobResponse'),
    newCreateDatasetExportJobResponse,

    -- ** CreateDatasetGroup
    CreateDatasetGroup (CreateDatasetGroup'),
    newCreateDatasetGroup,
    CreateDatasetGroupResponse (CreateDatasetGroupResponse'),
    newCreateDatasetGroupResponse,

    -- ** CreateDatasetImportJob
    CreateDatasetImportJob (CreateDatasetImportJob'),
    newCreateDatasetImportJob,
    CreateDatasetImportJobResponse (CreateDatasetImportJobResponse'),
    newCreateDatasetImportJobResponse,

    -- ** CreateEventTracker
    CreateEventTracker (CreateEventTracker'),
    newCreateEventTracker,
    CreateEventTrackerResponse (CreateEventTrackerResponse'),
    newCreateEventTrackerResponse,

    -- ** CreateFilter
    CreateFilter (CreateFilter'),
    newCreateFilter,
    CreateFilterResponse (CreateFilterResponse'),
    newCreateFilterResponse,

    -- ** CreateMetricAttribution
    CreateMetricAttribution (CreateMetricAttribution'),
    newCreateMetricAttribution,
    CreateMetricAttributionResponse (CreateMetricAttributionResponse'),
    newCreateMetricAttributionResponse,

    -- ** CreateRecommender
    CreateRecommender (CreateRecommender'),
    newCreateRecommender,
    CreateRecommenderResponse (CreateRecommenderResponse'),
    newCreateRecommenderResponse,

    -- ** CreateSchema
    CreateSchema (CreateSchema'),
    newCreateSchema,
    CreateSchemaResponse (CreateSchemaResponse'),
    newCreateSchemaResponse,

    -- ** CreateSolution
    CreateSolution (CreateSolution'),
    newCreateSolution,
    CreateSolutionResponse (CreateSolutionResponse'),
    newCreateSolutionResponse,

    -- ** CreateSolutionVersion
    CreateSolutionVersion (CreateSolutionVersion'),
    newCreateSolutionVersion,
    CreateSolutionVersionResponse (CreateSolutionVersionResponse'),
    newCreateSolutionVersionResponse,

    -- ** DeleteCampaign
    DeleteCampaign (DeleteCampaign'),
    newDeleteCampaign,
    DeleteCampaignResponse (DeleteCampaignResponse'),
    newDeleteCampaignResponse,

    -- ** DeleteDataset
    DeleteDataset (DeleteDataset'),
    newDeleteDataset,
    DeleteDatasetResponse (DeleteDatasetResponse'),
    newDeleteDatasetResponse,

    -- ** DeleteDatasetGroup
    DeleteDatasetGroup (DeleteDatasetGroup'),
    newDeleteDatasetGroup,
    DeleteDatasetGroupResponse (DeleteDatasetGroupResponse'),
    newDeleteDatasetGroupResponse,

    -- ** DeleteEventTracker
    DeleteEventTracker (DeleteEventTracker'),
    newDeleteEventTracker,
    DeleteEventTrackerResponse (DeleteEventTrackerResponse'),
    newDeleteEventTrackerResponse,

    -- ** DeleteFilter
    DeleteFilter (DeleteFilter'),
    newDeleteFilter,
    DeleteFilterResponse (DeleteFilterResponse'),
    newDeleteFilterResponse,

    -- ** DeleteMetricAttribution
    DeleteMetricAttribution (DeleteMetricAttribution'),
    newDeleteMetricAttribution,
    DeleteMetricAttributionResponse (DeleteMetricAttributionResponse'),
    newDeleteMetricAttributionResponse,

    -- ** DeleteRecommender
    DeleteRecommender (DeleteRecommender'),
    newDeleteRecommender,
    DeleteRecommenderResponse (DeleteRecommenderResponse'),
    newDeleteRecommenderResponse,

    -- ** DeleteSchema
    DeleteSchema (DeleteSchema'),
    newDeleteSchema,
    DeleteSchemaResponse (DeleteSchemaResponse'),
    newDeleteSchemaResponse,

    -- ** DeleteSolution
    DeleteSolution (DeleteSolution'),
    newDeleteSolution,
    DeleteSolutionResponse (DeleteSolutionResponse'),
    newDeleteSolutionResponse,

    -- ** DescribeAlgorithm
    DescribeAlgorithm (DescribeAlgorithm'),
    newDescribeAlgorithm,
    DescribeAlgorithmResponse (DescribeAlgorithmResponse'),
    newDescribeAlgorithmResponse,

    -- ** DescribeBatchInferenceJob
    DescribeBatchInferenceJob (DescribeBatchInferenceJob'),
    newDescribeBatchInferenceJob,
    DescribeBatchInferenceJobResponse (DescribeBatchInferenceJobResponse'),
    newDescribeBatchInferenceJobResponse,

    -- ** DescribeBatchSegmentJob
    DescribeBatchSegmentJob (DescribeBatchSegmentJob'),
    newDescribeBatchSegmentJob,
    DescribeBatchSegmentJobResponse (DescribeBatchSegmentJobResponse'),
    newDescribeBatchSegmentJobResponse,

    -- ** DescribeCampaign
    DescribeCampaign (DescribeCampaign'),
    newDescribeCampaign,
    DescribeCampaignResponse (DescribeCampaignResponse'),
    newDescribeCampaignResponse,

    -- ** DescribeDataset
    DescribeDataset (DescribeDataset'),
    newDescribeDataset,
    DescribeDatasetResponse (DescribeDatasetResponse'),
    newDescribeDatasetResponse,

    -- ** DescribeDatasetExportJob
    DescribeDatasetExportJob (DescribeDatasetExportJob'),
    newDescribeDatasetExportJob,
    DescribeDatasetExportJobResponse (DescribeDatasetExportJobResponse'),
    newDescribeDatasetExportJobResponse,

    -- ** DescribeDatasetGroup
    DescribeDatasetGroup (DescribeDatasetGroup'),
    newDescribeDatasetGroup,
    DescribeDatasetGroupResponse (DescribeDatasetGroupResponse'),
    newDescribeDatasetGroupResponse,

    -- ** DescribeDatasetImportJob
    DescribeDatasetImportJob (DescribeDatasetImportJob'),
    newDescribeDatasetImportJob,
    DescribeDatasetImportJobResponse (DescribeDatasetImportJobResponse'),
    newDescribeDatasetImportJobResponse,

    -- ** DescribeEventTracker
    DescribeEventTracker (DescribeEventTracker'),
    newDescribeEventTracker,
    DescribeEventTrackerResponse (DescribeEventTrackerResponse'),
    newDescribeEventTrackerResponse,

    -- ** DescribeFeatureTransformation
    DescribeFeatureTransformation (DescribeFeatureTransformation'),
    newDescribeFeatureTransformation,
    DescribeFeatureTransformationResponse (DescribeFeatureTransformationResponse'),
    newDescribeFeatureTransformationResponse,

    -- ** DescribeFilter
    DescribeFilter (DescribeFilter'),
    newDescribeFilter,
    DescribeFilterResponse (DescribeFilterResponse'),
    newDescribeFilterResponse,

    -- ** DescribeMetricAttribution
    DescribeMetricAttribution (DescribeMetricAttribution'),
    newDescribeMetricAttribution,
    DescribeMetricAttributionResponse (DescribeMetricAttributionResponse'),
    newDescribeMetricAttributionResponse,

    -- ** DescribeRecipe
    DescribeRecipe (DescribeRecipe'),
    newDescribeRecipe,
    DescribeRecipeResponse (DescribeRecipeResponse'),
    newDescribeRecipeResponse,

    -- ** DescribeRecommender
    DescribeRecommender (DescribeRecommender'),
    newDescribeRecommender,
    DescribeRecommenderResponse (DescribeRecommenderResponse'),
    newDescribeRecommenderResponse,

    -- ** DescribeSchema
    DescribeSchema (DescribeSchema'),
    newDescribeSchema,
    DescribeSchemaResponse (DescribeSchemaResponse'),
    newDescribeSchemaResponse,

    -- ** DescribeSolution
    DescribeSolution (DescribeSolution'),
    newDescribeSolution,
    DescribeSolutionResponse (DescribeSolutionResponse'),
    newDescribeSolutionResponse,

    -- ** DescribeSolutionVersion
    DescribeSolutionVersion (DescribeSolutionVersion'),
    newDescribeSolutionVersion,
    DescribeSolutionVersionResponse (DescribeSolutionVersionResponse'),
    newDescribeSolutionVersionResponse,

    -- ** GetSolutionMetrics
    GetSolutionMetrics (GetSolutionMetrics'),
    newGetSolutionMetrics,
    GetSolutionMetricsResponse (GetSolutionMetricsResponse'),
    newGetSolutionMetricsResponse,

    -- ** ListBatchInferenceJobs (Paginated)
    ListBatchInferenceJobs (ListBatchInferenceJobs'),
    newListBatchInferenceJobs,
    ListBatchInferenceJobsResponse (ListBatchInferenceJobsResponse'),
    newListBatchInferenceJobsResponse,

    -- ** ListBatchSegmentJobs (Paginated)
    ListBatchSegmentJobs (ListBatchSegmentJobs'),
    newListBatchSegmentJobs,
    ListBatchSegmentJobsResponse (ListBatchSegmentJobsResponse'),
    newListBatchSegmentJobsResponse,

    -- ** ListCampaigns (Paginated)
    ListCampaigns (ListCampaigns'),
    newListCampaigns,
    ListCampaignsResponse (ListCampaignsResponse'),
    newListCampaignsResponse,

    -- ** ListDatasetExportJobs (Paginated)
    ListDatasetExportJobs (ListDatasetExportJobs'),
    newListDatasetExportJobs,
    ListDatasetExportJobsResponse (ListDatasetExportJobsResponse'),
    newListDatasetExportJobsResponse,

    -- ** ListDatasetGroups (Paginated)
    ListDatasetGroups (ListDatasetGroups'),
    newListDatasetGroups,
    ListDatasetGroupsResponse (ListDatasetGroupsResponse'),
    newListDatasetGroupsResponse,

    -- ** ListDatasetImportJobs (Paginated)
    ListDatasetImportJobs (ListDatasetImportJobs'),
    newListDatasetImportJobs,
    ListDatasetImportJobsResponse (ListDatasetImportJobsResponse'),
    newListDatasetImportJobsResponse,

    -- ** ListDatasets (Paginated)
    ListDatasets (ListDatasets'),
    newListDatasets,
    ListDatasetsResponse (ListDatasetsResponse'),
    newListDatasetsResponse,

    -- ** ListEventTrackers (Paginated)
    ListEventTrackers (ListEventTrackers'),
    newListEventTrackers,
    ListEventTrackersResponse (ListEventTrackersResponse'),
    newListEventTrackersResponse,

    -- ** ListFilters (Paginated)
    ListFilters (ListFilters'),
    newListFilters,
    ListFiltersResponse (ListFiltersResponse'),
    newListFiltersResponse,

    -- ** ListMetricAttributionMetrics (Paginated)
    ListMetricAttributionMetrics (ListMetricAttributionMetrics'),
    newListMetricAttributionMetrics,
    ListMetricAttributionMetricsResponse (ListMetricAttributionMetricsResponse'),
    newListMetricAttributionMetricsResponse,

    -- ** ListMetricAttributions (Paginated)
    ListMetricAttributions (ListMetricAttributions'),
    newListMetricAttributions,
    ListMetricAttributionsResponse (ListMetricAttributionsResponse'),
    newListMetricAttributionsResponse,

    -- ** ListRecipes (Paginated)
    ListRecipes (ListRecipes'),
    newListRecipes,
    ListRecipesResponse (ListRecipesResponse'),
    newListRecipesResponse,

    -- ** ListRecommenders (Paginated)
    ListRecommenders (ListRecommenders'),
    newListRecommenders,
    ListRecommendersResponse (ListRecommendersResponse'),
    newListRecommendersResponse,

    -- ** ListSchemas (Paginated)
    ListSchemas (ListSchemas'),
    newListSchemas,
    ListSchemasResponse (ListSchemasResponse'),
    newListSchemasResponse,

    -- ** ListSolutionVersions (Paginated)
    ListSolutionVersions (ListSolutionVersions'),
    newListSolutionVersions,
    ListSolutionVersionsResponse (ListSolutionVersionsResponse'),
    newListSolutionVersionsResponse,

    -- ** ListSolutions (Paginated)
    ListSolutions (ListSolutions'),
    newListSolutions,
    ListSolutionsResponse (ListSolutionsResponse'),
    newListSolutionsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** StartRecommender
    StartRecommender (StartRecommender'),
    newStartRecommender,
    StartRecommenderResponse (StartRecommenderResponse'),
    newStartRecommenderResponse,

    -- ** StopRecommender
    StopRecommender (StopRecommender'),
    newStopRecommender,
    StopRecommenderResponse (StopRecommenderResponse'),
    newStopRecommenderResponse,

    -- ** StopSolutionVersionCreation
    StopSolutionVersionCreation (StopSolutionVersionCreation'),
    newStopSolutionVersionCreation,
    StopSolutionVersionCreationResponse (StopSolutionVersionCreationResponse'),
    newStopSolutionVersionCreationResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateCampaign
    UpdateCampaign (UpdateCampaign'),
    newUpdateCampaign,
    UpdateCampaignResponse (UpdateCampaignResponse'),
    newUpdateCampaignResponse,

    -- ** UpdateMetricAttribution
    UpdateMetricAttribution (UpdateMetricAttribution'),
    newUpdateMetricAttribution,
    UpdateMetricAttributionResponse (UpdateMetricAttributionResponse'),
    newUpdateMetricAttributionResponse,

    -- ** UpdateRecommender
    UpdateRecommender (UpdateRecommender'),
    newUpdateRecommender,
    UpdateRecommenderResponse (UpdateRecommenderResponse'),
    newUpdateRecommenderResponse,

    -- * Types

    -- ** Domain
    Domain (..),

    -- ** ImportMode
    ImportMode (..),

    -- ** IngestionMode
    IngestionMode (..),

    -- ** ObjectiveSensitivity
    ObjectiveSensitivity (..),

    -- ** RecipeProvider
    RecipeProvider (..),

    -- ** TrainingMode
    TrainingMode (..),

    -- ** Algorithm
    Algorithm (Algorithm'),
    newAlgorithm,

    -- ** AlgorithmImage
    AlgorithmImage (AlgorithmImage'),
    newAlgorithmImage,

    -- ** AutoMLConfig
    AutoMLConfig (AutoMLConfig'),
    newAutoMLConfig,

    -- ** AutoMLResult
    AutoMLResult (AutoMLResult'),
    newAutoMLResult,

    -- ** BatchInferenceJob
    BatchInferenceJob (BatchInferenceJob'),
    newBatchInferenceJob,

    -- ** BatchInferenceJobConfig
    BatchInferenceJobConfig (BatchInferenceJobConfig'),
    newBatchInferenceJobConfig,

    -- ** BatchInferenceJobInput
    BatchInferenceJobInput (BatchInferenceJobInput'),
    newBatchInferenceJobInput,

    -- ** BatchInferenceJobOutput
    BatchInferenceJobOutput (BatchInferenceJobOutput'),
    newBatchInferenceJobOutput,

    -- ** BatchInferenceJobSummary
    BatchInferenceJobSummary (BatchInferenceJobSummary'),
    newBatchInferenceJobSummary,

    -- ** BatchSegmentJob
    BatchSegmentJob (BatchSegmentJob'),
    newBatchSegmentJob,

    -- ** BatchSegmentJobInput
    BatchSegmentJobInput (BatchSegmentJobInput'),
    newBatchSegmentJobInput,

    -- ** BatchSegmentJobOutput
    BatchSegmentJobOutput (BatchSegmentJobOutput'),
    newBatchSegmentJobOutput,

    -- ** BatchSegmentJobSummary
    BatchSegmentJobSummary (BatchSegmentJobSummary'),
    newBatchSegmentJobSummary,

    -- ** Campaign
    Campaign (Campaign'),
    newCampaign,

    -- ** CampaignConfig
    CampaignConfig (CampaignConfig'),
    newCampaignConfig,

    -- ** CampaignSummary
    CampaignSummary (CampaignSummary'),
    newCampaignSummary,

    -- ** CampaignUpdateSummary
    CampaignUpdateSummary (CampaignUpdateSummary'),
    newCampaignUpdateSummary,

    -- ** CategoricalHyperParameterRange
    CategoricalHyperParameterRange (CategoricalHyperParameterRange'),
    newCategoricalHyperParameterRange,

    -- ** ContinuousHyperParameterRange
    ContinuousHyperParameterRange (ContinuousHyperParameterRange'),
    newContinuousHyperParameterRange,

    -- ** DataSource
    DataSource (DataSource'),
    newDataSource,

    -- ** Dataset
    Dataset (Dataset'),
    newDataset,

    -- ** DatasetExportJob
    DatasetExportJob (DatasetExportJob'),
    newDatasetExportJob,

    -- ** DatasetExportJobOutput
    DatasetExportJobOutput (DatasetExportJobOutput'),
    newDatasetExportJobOutput,

    -- ** DatasetExportJobSummary
    DatasetExportJobSummary (DatasetExportJobSummary'),
    newDatasetExportJobSummary,

    -- ** DatasetGroup
    DatasetGroup (DatasetGroup'),
    newDatasetGroup,

    -- ** DatasetGroupSummary
    DatasetGroupSummary (DatasetGroupSummary'),
    newDatasetGroupSummary,

    -- ** DatasetImportJob
    DatasetImportJob (DatasetImportJob'),
    newDatasetImportJob,

    -- ** DatasetImportJobSummary
    DatasetImportJobSummary (DatasetImportJobSummary'),
    newDatasetImportJobSummary,

    -- ** DatasetSchema
    DatasetSchema (DatasetSchema'),
    newDatasetSchema,

    -- ** DatasetSchemaSummary
    DatasetSchemaSummary (DatasetSchemaSummary'),
    newDatasetSchemaSummary,

    -- ** DatasetSummary
    DatasetSummary (DatasetSummary'),
    newDatasetSummary,

    -- ** DefaultCategoricalHyperParameterRange
    DefaultCategoricalHyperParameterRange (DefaultCategoricalHyperParameterRange'),
    newDefaultCategoricalHyperParameterRange,

    -- ** DefaultContinuousHyperParameterRange
    DefaultContinuousHyperParameterRange (DefaultContinuousHyperParameterRange'),
    newDefaultContinuousHyperParameterRange,

    -- ** DefaultHyperParameterRanges
    DefaultHyperParameterRanges (DefaultHyperParameterRanges'),
    newDefaultHyperParameterRanges,

    -- ** DefaultIntegerHyperParameterRange
    DefaultIntegerHyperParameterRange (DefaultIntegerHyperParameterRange'),
    newDefaultIntegerHyperParameterRange,

    -- ** EventTracker
    EventTracker (EventTracker'),
    newEventTracker,

    -- ** EventTrackerSummary
    EventTrackerSummary (EventTrackerSummary'),
    newEventTrackerSummary,

    -- ** FeatureTransformation
    FeatureTransformation (FeatureTransformation'),
    newFeatureTransformation,

    -- ** Filter
    Filter (Filter'),
    newFilter,

    -- ** FilterSummary
    FilterSummary (FilterSummary'),
    newFilterSummary,

    -- ** HPOConfig
    HPOConfig (HPOConfig'),
    newHPOConfig,

    -- ** HPOObjective
    HPOObjective (HPOObjective'),
    newHPOObjective,

    -- ** HPOResourceConfig
    HPOResourceConfig (HPOResourceConfig'),
    newHPOResourceConfig,

    -- ** HyperParameterRanges
    HyperParameterRanges (HyperParameterRanges'),
    newHyperParameterRanges,

    -- ** IntegerHyperParameterRange
    IntegerHyperParameterRange (IntegerHyperParameterRange'),
    newIntegerHyperParameterRange,

    -- ** MetricAttribute
    MetricAttribute (MetricAttribute'),
    newMetricAttribute,

    -- ** MetricAttribution
    MetricAttribution (MetricAttribution'),
    newMetricAttribution,

    -- ** MetricAttributionOutput
    MetricAttributionOutput (MetricAttributionOutput'),
    newMetricAttributionOutput,

    -- ** MetricAttributionSummary
    MetricAttributionSummary (MetricAttributionSummary'),
    newMetricAttributionSummary,

    -- ** OptimizationObjective
    OptimizationObjective (OptimizationObjective'),
    newOptimizationObjective,

    -- ** Recipe
    Recipe (Recipe'),
    newRecipe,

    -- ** RecipeSummary
    RecipeSummary (RecipeSummary'),
    newRecipeSummary,

    -- ** Recommender
    Recommender (Recommender'),
    newRecommender,

    -- ** RecommenderConfig
    RecommenderConfig (RecommenderConfig'),
    newRecommenderConfig,

    -- ** RecommenderSummary
    RecommenderSummary (RecommenderSummary'),
    newRecommenderSummary,

    -- ** RecommenderUpdateSummary
    RecommenderUpdateSummary (RecommenderUpdateSummary'),
    newRecommenderUpdateSummary,

    -- ** S3DataConfig
    S3DataConfig (S3DataConfig'),
    newS3DataConfig,

    -- ** Solution
    Solution (Solution'),
    newSolution,

    -- ** SolutionConfig
    SolutionConfig (SolutionConfig'),
    newSolutionConfig,

    -- ** SolutionSummary
    SolutionSummary (SolutionSummary'),
    newSolutionSummary,

    -- ** SolutionVersion
    SolutionVersion (SolutionVersion'),
    newSolutionVersion,

    -- ** SolutionVersionSummary
    SolutionVersionSummary (SolutionVersionSummary'),
    newSolutionVersionSummary,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TunedHPOParams
    TunedHPOParams (TunedHPOParams'),
    newTunedHPOParams,
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
import Amazonka.Personalize.Lens
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
import Amazonka.Personalize.Types
import Amazonka.Personalize.UntagResource
import Amazonka.Personalize.UpdateCampaign
import Amazonka.Personalize.UpdateMetricAttribution
import Amazonka.Personalize.UpdateRecommender
import Amazonka.Personalize.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Personalize'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.

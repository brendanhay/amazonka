{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.DataBrew
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-07-25@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Glue DataBrew is a visual, cloud-scale data-preparation service.
-- DataBrew simplifies data preparation tasks, targeting data issues that
-- are hard to spot and time-consuming to fix. DataBrew empowers users of
-- all technical levels to visualize the data and perform one-click data
-- transformations, with no coding required.
module Amazonka.DataBrew
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** BatchDeleteRecipeVersion
    BatchDeleteRecipeVersion (BatchDeleteRecipeVersion'),
    newBatchDeleteRecipeVersion,
    BatchDeleteRecipeVersionResponse (BatchDeleteRecipeVersionResponse'),
    newBatchDeleteRecipeVersionResponse,

    -- ** CreateDataset
    CreateDataset (CreateDataset'),
    newCreateDataset,
    CreateDatasetResponse (CreateDatasetResponse'),
    newCreateDatasetResponse,

    -- ** CreateProfileJob
    CreateProfileJob (CreateProfileJob'),
    newCreateProfileJob,
    CreateProfileJobResponse (CreateProfileJobResponse'),
    newCreateProfileJobResponse,

    -- ** CreateProject
    CreateProject (CreateProject'),
    newCreateProject,
    CreateProjectResponse (CreateProjectResponse'),
    newCreateProjectResponse,

    -- ** CreateRecipe
    CreateRecipe (CreateRecipe'),
    newCreateRecipe,
    CreateRecipeResponse (CreateRecipeResponse'),
    newCreateRecipeResponse,

    -- ** CreateRecipeJob
    CreateRecipeJob (CreateRecipeJob'),
    newCreateRecipeJob,
    CreateRecipeJobResponse (CreateRecipeJobResponse'),
    newCreateRecipeJobResponse,

    -- ** CreateRuleset
    CreateRuleset (CreateRuleset'),
    newCreateRuleset,
    CreateRulesetResponse (CreateRulesetResponse'),
    newCreateRulesetResponse,

    -- ** CreateSchedule
    CreateSchedule (CreateSchedule'),
    newCreateSchedule,
    CreateScheduleResponse (CreateScheduleResponse'),
    newCreateScheduleResponse,

    -- ** DeleteDataset
    DeleteDataset (DeleteDataset'),
    newDeleteDataset,
    DeleteDatasetResponse (DeleteDatasetResponse'),
    newDeleteDatasetResponse,

    -- ** DeleteJob
    DeleteJob (DeleteJob'),
    newDeleteJob,
    DeleteJobResponse (DeleteJobResponse'),
    newDeleteJobResponse,

    -- ** DeleteProject
    DeleteProject (DeleteProject'),
    newDeleteProject,
    DeleteProjectResponse (DeleteProjectResponse'),
    newDeleteProjectResponse,

    -- ** DeleteRecipeVersion
    DeleteRecipeVersion (DeleteRecipeVersion'),
    newDeleteRecipeVersion,
    DeleteRecipeVersionResponse (DeleteRecipeVersionResponse'),
    newDeleteRecipeVersionResponse,

    -- ** DeleteRuleset
    DeleteRuleset (DeleteRuleset'),
    newDeleteRuleset,
    DeleteRulesetResponse (DeleteRulesetResponse'),
    newDeleteRulesetResponse,

    -- ** DeleteSchedule
    DeleteSchedule (DeleteSchedule'),
    newDeleteSchedule,
    DeleteScheduleResponse (DeleteScheduleResponse'),
    newDeleteScheduleResponse,

    -- ** DescribeDataset
    DescribeDataset (DescribeDataset'),
    newDescribeDataset,
    DescribeDatasetResponse (DescribeDatasetResponse'),
    newDescribeDatasetResponse,

    -- ** DescribeJob
    DescribeJob (DescribeJob'),
    newDescribeJob,
    DescribeJobResponse (DescribeJobResponse'),
    newDescribeJobResponse,

    -- ** DescribeJobRun
    DescribeJobRun (DescribeJobRun'),
    newDescribeJobRun,
    DescribeJobRunResponse (DescribeJobRunResponse'),
    newDescribeJobRunResponse,

    -- ** DescribeProject
    DescribeProject (DescribeProject'),
    newDescribeProject,
    DescribeProjectResponse (DescribeProjectResponse'),
    newDescribeProjectResponse,

    -- ** DescribeRecipe
    DescribeRecipe (DescribeRecipe'),
    newDescribeRecipe,
    DescribeRecipeResponse (DescribeRecipeResponse'),
    newDescribeRecipeResponse,

    -- ** DescribeRuleset
    DescribeRuleset (DescribeRuleset'),
    newDescribeRuleset,
    DescribeRulesetResponse (DescribeRulesetResponse'),
    newDescribeRulesetResponse,

    -- ** DescribeSchedule
    DescribeSchedule (DescribeSchedule'),
    newDescribeSchedule,
    DescribeScheduleResponse (DescribeScheduleResponse'),
    newDescribeScheduleResponse,

    -- ** ListDatasets (Paginated)
    ListDatasets (ListDatasets'),
    newListDatasets,
    ListDatasetsResponse (ListDatasetsResponse'),
    newListDatasetsResponse,

    -- ** ListJobRuns (Paginated)
    ListJobRuns (ListJobRuns'),
    newListJobRuns,
    ListJobRunsResponse (ListJobRunsResponse'),
    newListJobRunsResponse,

    -- ** ListJobs (Paginated)
    ListJobs (ListJobs'),
    newListJobs,
    ListJobsResponse (ListJobsResponse'),
    newListJobsResponse,

    -- ** ListProjects (Paginated)
    ListProjects (ListProjects'),
    newListProjects,
    ListProjectsResponse (ListProjectsResponse'),
    newListProjectsResponse,

    -- ** ListRecipeVersions (Paginated)
    ListRecipeVersions (ListRecipeVersions'),
    newListRecipeVersions,
    ListRecipeVersionsResponse (ListRecipeVersionsResponse'),
    newListRecipeVersionsResponse,

    -- ** ListRecipes (Paginated)
    ListRecipes (ListRecipes'),
    newListRecipes,
    ListRecipesResponse (ListRecipesResponse'),
    newListRecipesResponse,

    -- ** ListRulesets (Paginated)
    ListRulesets (ListRulesets'),
    newListRulesets,
    ListRulesetsResponse (ListRulesetsResponse'),
    newListRulesetsResponse,

    -- ** ListSchedules (Paginated)
    ListSchedules (ListSchedules'),
    newListSchedules,
    ListSchedulesResponse (ListSchedulesResponse'),
    newListSchedulesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** PublishRecipe
    PublishRecipe (PublishRecipe'),
    newPublishRecipe,
    PublishRecipeResponse (PublishRecipeResponse'),
    newPublishRecipeResponse,

    -- ** SendProjectSessionAction
    SendProjectSessionAction (SendProjectSessionAction'),
    newSendProjectSessionAction,
    SendProjectSessionActionResponse (SendProjectSessionActionResponse'),
    newSendProjectSessionActionResponse,

    -- ** StartJobRun
    StartJobRun (StartJobRun'),
    newStartJobRun,
    StartJobRunResponse (StartJobRunResponse'),
    newStartJobRunResponse,

    -- ** StartProjectSession
    StartProjectSession (StartProjectSession'),
    newStartProjectSession,
    StartProjectSessionResponse (StartProjectSessionResponse'),
    newStartProjectSessionResponse,

    -- ** StopJobRun
    StopJobRun (StopJobRun'),
    newStopJobRun,
    StopJobRunResponse (StopJobRunResponse'),
    newStopJobRunResponse,

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

    -- ** UpdateDataset
    UpdateDataset (UpdateDataset'),
    newUpdateDataset,
    UpdateDatasetResponse (UpdateDatasetResponse'),
    newUpdateDatasetResponse,

    -- ** UpdateProfileJob
    UpdateProfileJob (UpdateProfileJob'),
    newUpdateProfileJob,
    UpdateProfileJobResponse (UpdateProfileJobResponse'),
    newUpdateProfileJobResponse,

    -- ** UpdateProject
    UpdateProject (UpdateProject'),
    newUpdateProject,
    UpdateProjectResponse (UpdateProjectResponse'),
    newUpdateProjectResponse,

    -- ** UpdateRecipe
    UpdateRecipe (UpdateRecipe'),
    newUpdateRecipe,
    UpdateRecipeResponse (UpdateRecipeResponse'),
    newUpdateRecipeResponse,

    -- ** UpdateRecipeJob
    UpdateRecipeJob (UpdateRecipeJob'),
    newUpdateRecipeJob,
    UpdateRecipeJobResponse (UpdateRecipeJobResponse'),
    newUpdateRecipeJobResponse,

    -- ** UpdateRuleset
    UpdateRuleset (UpdateRuleset'),
    newUpdateRuleset,
    UpdateRulesetResponse (UpdateRulesetResponse'),
    newUpdateRulesetResponse,

    -- ** UpdateSchedule
    UpdateSchedule (UpdateSchedule'),
    newUpdateSchedule,
    UpdateScheduleResponse (UpdateScheduleResponse'),
    newUpdateScheduleResponse,

    -- * Types

    -- ** AnalyticsMode
    AnalyticsMode (..),

    -- ** CompressionFormat
    CompressionFormat (..),

    -- ** DatabaseOutputMode
    DatabaseOutputMode (..),

    -- ** EncryptionMode
    EncryptionMode (..),

    -- ** InputFormat
    InputFormat (..),

    -- ** JobRunState
    JobRunState (..),

    -- ** JobType
    JobType (..),

    -- ** LogSubscription
    LogSubscription (..),

    -- ** Order
    Order (..),

    -- ** OrderedBy
    OrderedBy (..),

    -- ** OutputFormat
    OutputFormat (..),

    -- ** ParameterType
    ParameterType (..),

    -- ** SampleMode
    SampleMode (..),

    -- ** SampleType
    SampleType (..),

    -- ** SessionStatus
    SessionStatus (..),

    -- ** Source
    Source (..),

    -- ** ThresholdType
    ThresholdType (..),

    -- ** ThresholdUnit
    ThresholdUnit (..),

    -- ** ValidationMode
    ValidationMode (..),

    -- ** AllowedStatistics
    AllowedStatistics (AllowedStatistics'),
    newAllowedStatistics,

    -- ** ColumnSelector
    ColumnSelector (ColumnSelector'),
    newColumnSelector,

    -- ** ColumnStatisticsConfiguration
    ColumnStatisticsConfiguration (ColumnStatisticsConfiguration'),
    newColumnStatisticsConfiguration,

    -- ** ConditionExpression
    ConditionExpression (ConditionExpression'),
    newConditionExpression,

    -- ** CsvOptions
    CsvOptions (CsvOptions'),
    newCsvOptions,

    -- ** CsvOutputOptions
    CsvOutputOptions (CsvOutputOptions'),
    newCsvOutputOptions,

    -- ** DataCatalogInputDefinition
    DataCatalogInputDefinition (DataCatalogInputDefinition'),
    newDataCatalogInputDefinition,

    -- ** DataCatalogOutput
    DataCatalogOutput (DataCatalogOutput'),
    newDataCatalogOutput,

    -- ** DatabaseInputDefinition
    DatabaseInputDefinition (DatabaseInputDefinition'),
    newDatabaseInputDefinition,

    -- ** DatabaseOutput
    DatabaseOutput (DatabaseOutput'),
    newDatabaseOutput,

    -- ** DatabaseTableOutputOptions
    DatabaseTableOutputOptions (DatabaseTableOutputOptions'),
    newDatabaseTableOutputOptions,

    -- ** Dataset
    Dataset (Dataset'),
    newDataset,

    -- ** DatasetParameter
    DatasetParameter (DatasetParameter'),
    newDatasetParameter,

    -- ** DatetimeOptions
    DatetimeOptions (DatetimeOptions'),
    newDatetimeOptions,

    -- ** EntityDetectorConfiguration
    EntityDetectorConfiguration (EntityDetectorConfiguration'),
    newEntityDetectorConfiguration,

    -- ** ExcelOptions
    ExcelOptions (ExcelOptions'),
    newExcelOptions,

    -- ** FilesLimit
    FilesLimit (FilesLimit'),
    newFilesLimit,

    -- ** FilterExpression
    FilterExpression (FilterExpression'),
    newFilterExpression,

    -- ** FormatOptions
    FormatOptions (FormatOptions'),
    newFormatOptions,

    -- ** Input
    Input (Input'),
    newInput,

    -- ** Job
    Job (Job'),
    newJob,

    -- ** JobRun
    JobRun (JobRun'),
    newJobRun,

    -- ** JobSample
    JobSample (JobSample'),
    newJobSample,

    -- ** JsonOptions
    JsonOptions (JsonOptions'),
    newJsonOptions,

    -- ** Metadata
    Metadata (Metadata'),
    newMetadata,

    -- ** Output
    Output (Output'),
    newOutput,

    -- ** OutputFormatOptions
    OutputFormatOptions (OutputFormatOptions'),
    newOutputFormatOptions,

    -- ** PathOptions
    PathOptions (PathOptions'),
    newPathOptions,

    -- ** ProfileConfiguration
    ProfileConfiguration (ProfileConfiguration'),
    newProfileConfiguration,

    -- ** Project
    Project (Project'),
    newProject,

    -- ** Recipe
    Recipe (Recipe'),
    newRecipe,

    -- ** RecipeAction
    RecipeAction (RecipeAction'),
    newRecipeAction,

    -- ** RecipeReference
    RecipeReference (RecipeReference'),
    newRecipeReference,

    -- ** RecipeStep
    RecipeStep (RecipeStep'),
    newRecipeStep,

    -- ** RecipeVersionErrorDetail
    RecipeVersionErrorDetail (RecipeVersionErrorDetail'),
    newRecipeVersionErrorDetail,

    -- ** Rule
    Rule (Rule'),
    newRule,

    -- ** RulesetItem
    RulesetItem (RulesetItem'),
    newRulesetItem,

    -- ** S3Location
    S3Location (S3Location'),
    newS3Location,

    -- ** S3TableOutputOptions
    S3TableOutputOptions (S3TableOutputOptions'),
    newS3TableOutputOptions,

    -- ** Sample
    Sample (Sample'),
    newSample,

    -- ** Schedule
    Schedule (Schedule'),
    newSchedule,

    -- ** StatisticOverride
    StatisticOverride (StatisticOverride'),
    newStatisticOverride,

    -- ** StatisticsConfiguration
    StatisticsConfiguration (StatisticsConfiguration'),
    newStatisticsConfiguration,

    -- ** Threshold
    Threshold (Threshold'),
    newThreshold,

    -- ** ValidationConfiguration
    ValidationConfiguration (ValidationConfiguration'),
    newValidationConfiguration,

    -- ** ViewFrame
    ViewFrame (ViewFrame'),
    newViewFrame,
  )
where

import Amazonka.DataBrew.BatchDeleteRecipeVersion
import Amazonka.DataBrew.CreateDataset
import Amazonka.DataBrew.CreateProfileJob
import Amazonka.DataBrew.CreateProject
import Amazonka.DataBrew.CreateRecipe
import Amazonka.DataBrew.CreateRecipeJob
import Amazonka.DataBrew.CreateRuleset
import Amazonka.DataBrew.CreateSchedule
import Amazonka.DataBrew.DeleteDataset
import Amazonka.DataBrew.DeleteJob
import Amazonka.DataBrew.DeleteProject
import Amazonka.DataBrew.DeleteRecipeVersion
import Amazonka.DataBrew.DeleteRuleset
import Amazonka.DataBrew.DeleteSchedule
import Amazonka.DataBrew.DescribeDataset
import Amazonka.DataBrew.DescribeJob
import Amazonka.DataBrew.DescribeJobRun
import Amazonka.DataBrew.DescribeProject
import Amazonka.DataBrew.DescribeRecipe
import Amazonka.DataBrew.DescribeRuleset
import Amazonka.DataBrew.DescribeSchedule
import Amazonka.DataBrew.Lens
import Amazonka.DataBrew.ListDatasets
import Amazonka.DataBrew.ListJobRuns
import Amazonka.DataBrew.ListJobs
import Amazonka.DataBrew.ListProjects
import Amazonka.DataBrew.ListRecipeVersions
import Amazonka.DataBrew.ListRecipes
import Amazonka.DataBrew.ListRulesets
import Amazonka.DataBrew.ListSchedules
import Amazonka.DataBrew.ListTagsForResource
import Amazonka.DataBrew.PublishRecipe
import Amazonka.DataBrew.SendProjectSessionAction
import Amazonka.DataBrew.StartJobRun
import Amazonka.DataBrew.StartProjectSession
import Amazonka.DataBrew.StopJobRun
import Amazonka.DataBrew.TagResource
import Amazonka.DataBrew.Types
import Amazonka.DataBrew.UntagResource
import Amazonka.DataBrew.UpdateDataset
import Amazonka.DataBrew.UpdateProfileJob
import Amazonka.DataBrew.UpdateProject
import Amazonka.DataBrew.UpdateRecipe
import Amazonka.DataBrew.UpdateRecipeJob
import Amazonka.DataBrew.UpdateRuleset
import Amazonka.DataBrew.UpdateSchedule
import Amazonka.DataBrew.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'DataBrew'.

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

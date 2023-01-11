{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.CodeBuild
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2016-10-06@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- CodeBuild
--
-- CodeBuild is a fully managed build service in the cloud. CodeBuild
-- compiles your source code, runs unit tests, and produces artifacts that
-- are ready to deploy. CodeBuild eliminates the need to provision, manage,
-- and scale your own build servers. It provides prepackaged build
-- environments for the most popular programming languages and build tools,
-- such as Apache Maven, Gradle, and more. You can also fully customize
-- build environments in CodeBuild to use your own build tools. CodeBuild
-- scales automatically to meet peak build requests. You pay only for the
-- build time you consume. For more information about CodeBuild, see the
-- /<https://docs.aws.amazon.com/codebuild/latest/userguide/welcome.html CodeBuild User Guide>./
module Amazonka.CodeBuild
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccountLimitExceededException
    _AccountLimitExceededException,

    -- ** InvalidInputException
    _InvalidInputException,

    -- ** OAuthProviderException
    _OAuthProviderException,

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** BatchDeleteBuilds
    BatchDeleteBuilds (BatchDeleteBuilds'),
    newBatchDeleteBuilds,
    BatchDeleteBuildsResponse (BatchDeleteBuildsResponse'),
    newBatchDeleteBuildsResponse,

    -- ** BatchGetBuildBatches
    BatchGetBuildBatches (BatchGetBuildBatches'),
    newBatchGetBuildBatches,
    BatchGetBuildBatchesResponse (BatchGetBuildBatchesResponse'),
    newBatchGetBuildBatchesResponse,

    -- ** BatchGetBuilds
    BatchGetBuilds (BatchGetBuilds'),
    newBatchGetBuilds,
    BatchGetBuildsResponse (BatchGetBuildsResponse'),
    newBatchGetBuildsResponse,

    -- ** BatchGetProjects
    BatchGetProjects (BatchGetProjects'),
    newBatchGetProjects,
    BatchGetProjectsResponse (BatchGetProjectsResponse'),
    newBatchGetProjectsResponse,

    -- ** BatchGetReportGroups
    BatchGetReportGroups (BatchGetReportGroups'),
    newBatchGetReportGroups,
    BatchGetReportGroupsResponse (BatchGetReportGroupsResponse'),
    newBatchGetReportGroupsResponse,

    -- ** BatchGetReports
    BatchGetReports (BatchGetReports'),
    newBatchGetReports,
    BatchGetReportsResponse (BatchGetReportsResponse'),
    newBatchGetReportsResponse,

    -- ** CreateProject
    CreateProject (CreateProject'),
    newCreateProject,
    CreateProjectResponse (CreateProjectResponse'),
    newCreateProjectResponse,

    -- ** CreateReportGroup
    CreateReportGroup (CreateReportGroup'),
    newCreateReportGroup,
    CreateReportGroupResponse (CreateReportGroupResponse'),
    newCreateReportGroupResponse,

    -- ** CreateWebhook
    CreateWebhook (CreateWebhook'),
    newCreateWebhook,
    CreateWebhookResponse (CreateWebhookResponse'),
    newCreateWebhookResponse,

    -- ** DeleteBuildBatch
    DeleteBuildBatch (DeleteBuildBatch'),
    newDeleteBuildBatch,
    DeleteBuildBatchResponse (DeleteBuildBatchResponse'),
    newDeleteBuildBatchResponse,

    -- ** DeleteProject
    DeleteProject (DeleteProject'),
    newDeleteProject,
    DeleteProjectResponse (DeleteProjectResponse'),
    newDeleteProjectResponse,

    -- ** DeleteReport
    DeleteReport (DeleteReport'),
    newDeleteReport,
    DeleteReportResponse (DeleteReportResponse'),
    newDeleteReportResponse,

    -- ** DeleteReportGroup
    DeleteReportGroup (DeleteReportGroup'),
    newDeleteReportGroup,
    DeleteReportGroupResponse (DeleteReportGroupResponse'),
    newDeleteReportGroupResponse,

    -- ** DeleteResourcePolicy
    DeleteResourcePolicy (DeleteResourcePolicy'),
    newDeleteResourcePolicy,
    DeleteResourcePolicyResponse (DeleteResourcePolicyResponse'),
    newDeleteResourcePolicyResponse,

    -- ** DeleteSourceCredentials
    DeleteSourceCredentials (DeleteSourceCredentials'),
    newDeleteSourceCredentials,
    DeleteSourceCredentialsResponse (DeleteSourceCredentialsResponse'),
    newDeleteSourceCredentialsResponse,

    -- ** DeleteWebhook
    DeleteWebhook (DeleteWebhook'),
    newDeleteWebhook,
    DeleteWebhookResponse (DeleteWebhookResponse'),
    newDeleteWebhookResponse,

    -- ** DescribeCodeCoverages (Paginated)
    DescribeCodeCoverages (DescribeCodeCoverages'),
    newDescribeCodeCoverages,
    DescribeCodeCoveragesResponse (DescribeCodeCoveragesResponse'),
    newDescribeCodeCoveragesResponse,

    -- ** DescribeTestCases (Paginated)
    DescribeTestCases (DescribeTestCases'),
    newDescribeTestCases,
    DescribeTestCasesResponse (DescribeTestCasesResponse'),
    newDescribeTestCasesResponse,

    -- ** GetReportGroupTrend
    GetReportGroupTrend (GetReportGroupTrend'),
    newGetReportGroupTrend,
    GetReportGroupTrendResponse (GetReportGroupTrendResponse'),
    newGetReportGroupTrendResponse,

    -- ** GetResourcePolicy
    GetResourcePolicy (GetResourcePolicy'),
    newGetResourcePolicy,
    GetResourcePolicyResponse (GetResourcePolicyResponse'),
    newGetResourcePolicyResponse,

    -- ** ImportSourceCredentials
    ImportSourceCredentials (ImportSourceCredentials'),
    newImportSourceCredentials,
    ImportSourceCredentialsResponse (ImportSourceCredentialsResponse'),
    newImportSourceCredentialsResponse,

    -- ** InvalidateProjectCache
    InvalidateProjectCache (InvalidateProjectCache'),
    newInvalidateProjectCache,
    InvalidateProjectCacheResponse (InvalidateProjectCacheResponse'),
    newInvalidateProjectCacheResponse,

    -- ** ListBuildBatches (Paginated)
    ListBuildBatches (ListBuildBatches'),
    newListBuildBatches,
    ListBuildBatchesResponse (ListBuildBatchesResponse'),
    newListBuildBatchesResponse,

    -- ** ListBuildBatchesForProject (Paginated)
    ListBuildBatchesForProject (ListBuildBatchesForProject'),
    newListBuildBatchesForProject,
    ListBuildBatchesForProjectResponse (ListBuildBatchesForProjectResponse'),
    newListBuildBatchesForProjectResponse,

    -- ** ListBuilds (Paginated)
    ListBuilds (ListBuilds'),
    newListBuilds,
    ListBuildsResponse (ListBuildsResponse'),
    newListBuildsResponse,

    -- ** ListBuildsForProject (Paginated)
    ListBuildsForProject (ListBuildsForProject'),
    newListBuildsForProject,
    ListBuildsForProjectResponse (ListBuildsForProjectResponse'),
    newListBuildsForProjectResponse,

    -- ** ListCuratedEnvironmentImages
    ListCuratedEnvironmentImages (ListCuratedEnvironmentImages'),
    newListCuratedEnvironmentImages,
    ListCuratedEnvironmentImagesResponse (ListCuratedEnvironmentImagesResponse'),
    newListCuratedEnvironmentImagesResponse,

    -- ** ListProjects (Paginated)
    ListProjects (ListProjects'),
    newListProjects,
    ListProjectsResponse (ListProjectsResponse'),
    newListProjectsResponse,

    -- ** ListReportGroups (Paginated)
    ListReportGroups (ListReportGroups'),
    newListReportGroups,
    ListReportGroupsResponse (ListReportGroupsResponse'),
    newListReportGroupsResponse,

    -- ** ListReports (Paginated)
    ListReports (ListReports'),
    newListReports,
    ListReportsResponse (ListReportsResponse'),
    newListReportsResponse,

    -- ** ListReportsForReportGroup (Paginated)
    ListReportsForReportGroup (ListReportsForReportGroup'),
    newListReportsForReportGroup,
    ListReportsForReportGroupResponse (ListReportsForReportGroupResponse'),
    newListReportsForReportGroupResponse,

    -- ** ListSharedProjects (Paginated)
    ListSharedProjects (ListSharedProjects'),
    newListSharedProjects,
    ListSharedProjectsResponse (ListSharedProjectsResponse'),
    newListSharedProjectsResponse,

    -- ** ListSharedReportGroups (Paginated)
    ListSharedReportGroups (ListSharedReportGroups'),
    newListSharedReportGroups,
    ListSharedReportGroupsResponse (ListSharedReportGroupsResponse'),
    newListSharedReportGroupsResponse,

    -- ** ListSourceCredentials
    ListSourceCredentials (ListSourceCredentials'),
    newListSourceCredentials,
    ListSourceCredentialsResponse (ListSourceCredentialsResponse'),
    newListSourceCredentialsResponse,

    -- ** PutResourcePolicy
    PutResourcePolicy (PutResourcePolicy'),
    newPutResourcePolicy,
    PutResourcePolicyResponse (PutResourcePolicyResponse'),
    newPutResourcePolicyResponse,

    -- ** RetryBuild
    RetryBuild (RetryBuild'),
    newRetryBuild,
    RetryBuildResponse (RetryBuildResponse'),
    newRetryBuildResponse,

    -- ** RetryBuildBatch
    RetryBuildBatch (RetryBuildBatch'),
    newRetryBuildBatch,
    RetryBuildBatchResponse (RetryBuildBatchResponse'),
    newRetryBuildBatchResponse,

    -- ** StartBuild
    StartBuild (StartBuild'),
    newStartBuild,
    StartBuildResponse (StartBuildResponse'),
    newStartBuildResponse,

    -- ** StartBuildBatch
    StartBuildBatch (StartBuildBatch'),
    newStartBuildBatch,
    StartBuildBatchResponse (StartBuildBatchResponse'),
    newStartBuildBatchResponse,

    -- ** StopBuild
    StopBuild (StopBuild'),
    newStopBuild,
    StopBuildResponse (StopBuildResponse'),
    newStopBuildResponse,

    -- ** StopBuildBatch
    StopBuildBatch (StopBuildBatch'),
    newStopBuildBatch,
    StopBuildBatchResponse (StopBuildBatchResponse'),
    newStopBuildBatchResponse,

    -- ** UpdateProject
    UpdateProject (UpdateProject'),
    newUpdateProject,
    UpdateProjectResponse (UpdateProjectResponse'),
    newUpdateProjectResponse,

    -- ** UpdateProjectVisibility
    UpdateProjectVisibility (UpdateProjectVisibility'),
    newUpdateProjectVisibility,
    UpdateProjectVisibilityResponse (UpdateProjectVisibilityResponse'),
    newUpdateProjectVisibilityResponse,

    -- ** UpdateReportGroup
    UpdateReportGroup (UpdateReportGroup'),
    newUpdateReportGroup,
    UpdateReportGroupResponse (UpdateReportGroupResponse'),
    newUpdateReportGroupResponse,

    -- ** UpdateWebhook
    UpdateWebhook (UpdateWebhook'),
    newUpdateWebhook,
    UpdateWebhookResponse (UpdateWebhookResponse'),
    newUpdateWebhookResponse,

    -- * Types

    -- ** ArtifactNamespace
    ArtifactNamespace (..),

    -- ** ArtifactPackaging
    ArtifactPackaging (..),

    -- ** ArtifactsType
    ArtifactsType (..),

    -- ** AuthType
    AuthType (..),

    -- ** BatchReportModeType
    BatchReportModeType (..),

    -- ** BucketOwnerAccess
    BucketOwnerAccess (..),

    -- ** BuildBatchPhaseType
    BuildBatchPhaseType (..),

    -- ** BuildPhaseType
    BuildPhaseType (..),

    -- ** CacheMode
    CacheMode (..),

    -- ** CacheType
    CacheType (..),

    -- ** ComputeType
    ComputeType (..),

    -- ** CredentialProviderType
    CredentialProviderType (..),

    -- ** EnvironmentType
    EnvironmentType (..),

    -- ** EnvironmentVariableType
    EnvironmentVariableType (..),

    -- ** FileSystemType
    FileSystemType (..),

    -- ** ImagePullCredentialsType
    ImagePullCredentialsType (..),

    -- ** LanguageType
    LanguageType (..),

    -- ** LogsConfigStatusType
    LogsConfigStatusType (..),

    -- ** PlatformType
    PlatformType (..),

    -- ** ProjectSortByType
    ProjectSortByType (..),

    -- ** ProjectVisibilityType
    ProjectVisibilityType (..),

    -- ** ReportCodeCoverageSortByType
    ReportCodeCoverageSortByType (..),

    -- ** ReportExportConfigType
    ReportExportConfigType (..),

    -- ** ReportGroupSortByType
    ReportGroupSortByType (..),

    -- ** ReportGroupStatusType
    ReportGroupStatusType (..),

    -- ** ReportGroupTrendFieldType
    ReportGroupTrendFieldType (..),

    -- ** ReportPackagingType
    ReportPackagingType (..),

    -- ** ReportStatusType
    ReportStatusType (..),

    -- ** ReportType
    ReportType (..),

    -- ** RetryBuildBatchType
    RetryBuildBatchType (..),

    -- ** ServerType
    ServerType (..),

    -- ** SharedResourceSortByType
    SharedResourceSortByType (..),

    -- ** SortOrderType
    SortOrderType (..),

    -- ** SourceAuthType
    SourceAuthType (..),

    -- ** SourceType
    SourceType (..),

    -- ** StatusType
    StatusType (..),

    -- ** WebhookBuildType
    WebhookBuildType (..),

    -- ** WebhookFilterType
    WebhookFilterType (..),

    -- ** BatchRestrictions
    BatchRestrictions (BatchRestrictions'),
    newBatchRestrictions,

    -- ** Build
    Build (Build'),
    newBuild,

    -- ** BuildArtifacts
    BuildArtifacts (BuildArtifacts'),
    newBuildArtifacts,

    -- ** BuildBatch
    BuildBatch (BuildBatch'),
    newBuildBatch,

    -- ** BuildBatchFilter
    BuildBatchFilter (BuildBatchFilter'),
    newBuildBatchFilter,

    -- ** BuildBatchPhase
    BuildBatchPhase (BuildBatchPhase'),
    newBuildBatchPhase,

    -- ** BuildGroup
    BuildGroup (BuildGroup'),
    newBuildGroup,

    -- ** BuildNotDeleted
    BuildNotDeleted (BuildNotDeleted'),
    newBuildNotDeleted,

    -- ** BuildPhase
    BuildPhase (BuildPhase'),
    newBuildPhase,

    -- ** BuildStatusConfig
    BuildStatusConfig (BuildStatusConfig'),
    newBuildStatusConfig,

    -- ** BuildSummary
    BuildSummary (BuildSummary'),
    newBuildSummary,

    -- ** CloudWatchLogsConfig
    CloudWatchLogsConfig (CloudWatchLogsConfig'),
    newCloudWatchLogsConfig,

    -- ** CodeCoverage
    CodeCoverage (CodeCoverage'),
    newCodeCoverage,

    -- ** CodeCoverageReportSummary
    CodeCoverageReportSummary (CodeCoverageReportSummary'),
    newCodeCoverageReportSummary,

    -- ** DebugSession
    DebugSession (DebugSession'),
    newDebugSession,

    -- ** EnvironmentImage
    EnvironmentImage (EnvironmentImage'),
    newEnvironmentImage,

    -- ** EnvironmentLanguage
    EnvironmentLanguage (EnvironmentLanguage'),
    newEnvironmentLanguage,

    -- ** EnvironmentPlatform
    EnvironmentPlatform (EnvironmentPlatform'),
    newEnvironmentPlatform,

    -- ** EnvironmentVariable
    EnvironmentVariable (EnvironmentVariable'),
    newEnvironmentVariable,

    -- ** ExportedEnvironmentVariable
    ExportedEnvironmentVariable (ExportedEnvironmentVariable'),
    newExportedEnvironmentVariable,

    -- ** GitSubmodulesConfig
    GitSubmodulesConfig (GitSubmodulesConfig'),
    newGitSubmodulesConfig,

    -- ** LogsConfig
    LogsConfig (LogsConfig'),
    newLogsConfig,

    -- ** LogsLocation
    LogsLocation (LogsLocation'),
    newLogsLocation,

    -- ** NetworkInterface
    NetworkInterface (NetworkInterface'),
    newNetworkInterface,

    -- ** PhaseContext
    PhaseContext (PhaseContext'),
    newPhaseContext,

    -- ** Project
    Project (Project'),
    newProject,

    -- ** ProjectArtifacts
    ProjectArtifacts (ProjectArtifacts'),
    newProjectArtifacts,

    -- ** ProjectBadge
    ProjectBadge (ProjectBadge'),
    newProjectBadge,

    -- ** ProjectBuildBatchConfig
    ProjectBuildBatchConfig (ProjectBuildBatchConfig'),
    newProjectBuildBatchConfig,

    -- ** ProjectCache
    ProjectCache (ProjectCache'),
    newProjectCache,

    -- ** ProjectEnvironment
    ProjectEnvironment (ProjectEnvironment'),
    newProjectEnvironment,

    -- ** ProjectFileSystemLocation
    ProjectFileSystemLocation (ProjectFileSystemLocation'),
    newProjectFileSystemLocation,

    -- ** ProjectSource
    ProjectSource (ProjectSource'),
    newProjectSource,

    -- ** ProjectSourceVersion
    ProjectSourceVersion (ProjectSourceVersion'),
    newProjectSourceVersion,

    -- ** RegistryCredential
    RegistryCredential (RegistryCredential'),
    newRegistryCredential,

    -- ** Report
    Report (Report'),
    newReport,

    -- ** ReportExportConfig
    ReportExportConfig (ReportExportConfig'),
    newReportExportConfig,

    -- ** ReportFilter
    ReportFilter (ReportFilter'),
    newReportFilter,

    -- ** ReportGroup
    ReportGroup (ReportGroup'),
    newReportGroup,

    -- ** ReportGroupTrendStats
    ReportGroupTrendStats (ReportGroupTrendStats'),
    newReportGroupTrendStats,

    -- ** ReportWithRawData
    ReportWithRawData (ReportWithRawData'),
    newReportWithRawData,

    -- ** ResolvedArtifact
    ResolvedArtifact (ResolvedArtifact'),
    newResolvedArtifact,

    -- ** S3LogsConfig
    S3LogsConfig (S3LogsConfig'),
    newS3LogsConfig,

    -- ** S3ReportExportConfig
    S3ReportExportConfig (S3ReportExportConfig'),
    newS3ReportExportConfig,

    -- ** SourceAuth
    SourceAuth (SourceAuth'),
    newSourceAuth,

    -- ** SourceCredentialsInfo
    SourceCredentialsInfo (SourceCredentialsInfo'),
    newSourceCredentialsInfo,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TestCase
    TestCase (TestCase'),
    newTestCase,

    -- ** TestCaseFilter
    TestCaseFilter (TestCaseFilter'),
    newTestCaseFilter,

    -- ** TestReportSummary
    TestReportSummary (TestReportSummary'),
    newTestReportSummary,

    -- ** VpcConfig
    VpcConfig (VpcConfig'),
    newVpcConfig,

    -- ** Webhook
    Webhook (Webhook'),
    newWebhook,

    -- ** WebhookFilter
    WebhookFilter (WebhookFilter'),
    newWebhookFilter,
  )
where

import Amazonka.CodeBuild.BatchDeleteBuilds
import Amazonka.CodeBuild.BatchGetBuildBatches
import Amazonka.CodeBuild.BatchGetBuilds
import Amazonka.CodeBuild.BatchGetProjects
import Amazonka.CodeBuild.BatchGetReportGroups
import Amazonka.CodeBuild.BatchGetReports
import Amazonka.CodeBuild.CreateProject
import Amazonka.CodeBuild.CreateReportGroup
import Amazonka.CodeBuild.CreateWebhook
import Amazonka.CodeBuild.DeleteBuildBatch
import Amazonka.CodeBuild.DeleteProject
import Amazonka.CodeBuild.DeleteReport
import Amazonka.CodeBuild.DeleteReportGroup
import Amazonka.CodeBuild.DeleteResourcePolicy
import Amazonka.CodeBuild.DeleteSourceCredentials
import Amazonka.CodeBuild.DeleteWebhook
import Amazonka.CodeBuild.DescribeCodeCoverages
import Amazonka.CodeBuild.DescribeTestCases
import Amazonka.CodeBuild.GetReportGroupTrend
import Amazonka.CodeBuild.GetResourcePolicy
import Amazonka.CodeBuild.ImportSourceCredentials
import Amazonka.CodeBuild.InvalidateProjectCache
import Amazonka.CodeBuild.Lens
import Amazonka.CodeBuild.ListBuildBatches
import Amazonka.CodeBuild.ListBuildBatchesForProject
import Amazonka.CodeBuild.ListBuilds
import Amazonka.CodeBuild.ListBuildsForProject
import Amazonka.CodeBuild.ListCuratedEnvironmentImages
import Amazonka.CodeBuild.ListProjects
import Amazonka.CodeBuild.ListReportGroups
import Amazonka.CodeBuild.ListReports
import Amazonka.CodeBuild.ListReportsForReportGroup
import Amazonka.CodeBuild.ListSharedProjects
import Amazonka.CodeBuild.ListSharedReportGroups
import Amazonka.CodeBuild.ListSourceCredentials
import Amazonka.CodeBuild.PutResourcePolicy
import Amazonka.CodeBuild.RetryBuild
import Amazonka.CodeBuild.RetryBuildBatch
import Amazonka.CodeBuild.StartBuild
import Amazonka.CodeBuild.StartBuildBatch
import Amazonka.CodeBuild.StopBuild
import Amazonka.CodeBuild.StopBuildBatch
import Amazonka.CodeBuild.Types
import Amazonka.CodeBuild.UpdateProject
import Amazonka.CodeBuild.UpdateProjectVisibility
import Amazonka.CodeBuild.UpdateReportGroup
import Amazonka.CodeBuild.UpdateWebhook
import Amazonka.CodeBuild.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'CodeBuild'.

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

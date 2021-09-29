{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.CodeBuild
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.CodeBuild
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** OAuthProviderException
    _OAuthProviderException,

    -- ** ResourceAlreadyExistsException
    _ResourceAlreadyExistsException,

    -- ** AccountLimitExceededException
    _AccountLimitExceededException,

    -- ** InvalidInputException
    _InvalidInputException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListBuilds (Paginated)
    ListBuilds (ListBuilds'),
    newListBuilds,
    ListBuildsResponse (ListBuildsResponse'),
    newListBuildsResponse,

    -- ** DeleteReport
    DeleteReport (DeleteReport'),
    newDeleteReport,
    DeleteReportResponse (DeleteReportResponse'),
    newDeleteReportResponse,

    -- ** BatchGetReports
    BatchGetReports (BatchGetReports'),
    newBatchGetReports,
    BatchGetReportsResponse (BatchGetReportsResponse'),
    newBatchGetReportsResponse,

    -- ** GetResourcePolicy
    GetResourcePolicy (GetResourcePolicy'),
    newGetResourcePolicy,
    GetResourcePolicyResponse (GetResourcePolicyResponse'),
    newGetResourcePolicyResponse,

    -- ** ListProjects (Paginated)
    ListProjects (ListProjects'),
    newListProjects,
    ListProjectsResponse (ListProjectsResponse'),
    newListProjectsResponse,

    -- ** DescribeTestCases (Paginated)
    DescribeTestCases (DescribeTestCases'),
    newDescribeTestCases,
    DescribeTestCasesResponse (DescribeTestCasesResponse'),
    newDescribeTestCasesResponse,

    -- ** CreateProject
    CreateProject (CreateProject'),
    newCreateProject,
    CreateProjectResponse (CreateProjectResponse'),
    newCreateProjectResponse,

    -- ** ListBuildsForProject (Paginated)
    ListBuildsForProject (ListBuildsForProject'),
    newListBuildsForProject,
    ListBuildsForProjectResponse (ListBuildsForProjectResponse'),
    newListBuildsForProjectResponse,

    -- ** ListBuildBatches (Paginated)
    ListBuildBatches (ListBuildBatches'),
    newListBuildBatches,
    ListBuildBatchesResponse (ListBuildBatchesResponse'),
    newListBuildBatchesResponse,

    -- ** PutResourcePolicy
    PutResourcePolicy (PutResourcePolicy'),
    newPutResourcePolicy,
    PutResourcePolicyResponse (PutResourcePolicyResponse'),
    newPutResourcePolicyResponse,

    -- ** DeleteBuildBatch
    DeleteBuildBatch (DeleteBuildBatch'),
    newDeleteBuildBatch,
    DeleteBuildBatchResponse (DeleteBuildBatchResponse'),
    newDeleteBuildBatchResponse,

    -- ** UpdateReportGroup
    UpdateReportGroup (UpdateReportGroup'),
    newUpdateReportGroup,
    UpdateReportGroupResponse (UpdateReportGroupResponse'),
    newUpdateReportGroupResponse,

    -- ** ListBuildBatchesForProject (Paginated)
    ListBuildBatchesForProject (ListBuildBatchesForProject'),
    newListBuildBatchesForProject,
    ListBuildBatchesForProjectResponse (ListBuildBatchesForProjectResponse'),
    newListBuildBatchesForProjectResponse,

    -- ** BatchGetReportGroups
    BatchGetReportGroups (BatchGetReportGroups'),
    newBatchGetReportGroups,
    BatchGetReportGroupsResponse (BatchGetReportGroupsResponse'),
    newBatchGetReportGroupsResponse,

    -- ** BatchDeleteBuilds
    BatchDeleteBuilds (BatchDeleteBuilds'),
    newBatchDeleteBuilds,
    BatchDeleteBuildsResponse (BatchDeleteBuildsResponse'),
    newBatchDeleteBuildsResponse,

    -- ** DeleteReportGroup
    DeleteReportGroup (DeleteReportGroup'),
    newDeleteReportGroup,
    DeleteReportGroupResponse (DeleteReportGroupResponse'),
    newDeleteReportGroupResponse,

    -- ** CreateReportGroup
    CreateReportGroup (CreateReportGroup'),
    newCreateReportGroup,
    CreateReportGroupResponse (CreateReportGroupResponse'),
    newCreateReportGroupResponse,

    -- ** DescribeCodeCoverages (Paginated)
    DescribeCodeCoverages (DescribeCodeCoverages'),
    newDescribeCodeCoverages,
    DescribeCodeCoveragesResponse (DescribeCodeCoveragesResponse'),
    newDescribeCodeCoveragesResponse,

    -- ** StartBuildBatch
    StartBuildBatch (StartBuildBatch'),
    newStartBuildBatch,
    StartBuildBatchResponse (StartBuildBatchResponse'),
    newStartBuildBatchResponse,

    -- ** DeleteWebhook
    DeleteWebhook (DeleteWebhook'),
    newDeleteWebhook,
    DeleteWebhookResponse (DeleteWebhookResponse'),
    newDeleteWebhookResponse,

    -- ** UpdateProjectVisibility
    UpdateProjectVisibility (UpdateProjectVisibility'),
    newUpdateProjectVisibility,
    UpdateProjectVisibilityResponse (UpdateProjectVisibilityResponse'),
    newUpdateProjectVisibilityResponse,

    -- ** RetryBuildBatch
    RetryBuildBatch (RetryBuildBatch'),
    newRetryBuildBatch,
    RetryBuildBatchResponse (RetryBuildBatchResponse'),
    newRetryBuildBatchResponse,

    -- ** StopBuildBatch
    StopBuildBatch (StopBuildBatch'),
    newStopBuildBatch,
    StopBuildBatchResponse (StopBuildBatchResponse'),
    newStopBuildBatchResponse,

    -- ** UpdateWebhook
    UpdateWebhook (UpdateWebhook'),
    newUpdateWebhook,
    UpdateWebhookResponse (UpdateWebhookResponse'),
    newUpdateWebhookResponse,

    -- ** BatchGetBuilds
    BatchGetBuilds (BatchGetBuilds'),
    newBatchGetBuilds,
    BatchGetBuildsResponse (BatchGetBuildsResponse'),
    newBatchGetBuildsResponse,

    -- ** ListReports (Paginated)
    ListReports (ListReports'),
    newListReports,
    ListReportsResponse (ListReportsResponse'),
    newListReportsResponse,

    -- ** CreateWebhook
    CreateWebhook (CreateWebhook'),
    newCreateWebhook,
    CreateWebhookResponse (CreateWebhookResponse'),
    newCreateWebhookResponse,

    -- ** ListSourceCredentials
    ListSourceCredentials (ListSourceCredentials'),
    newListSourceCredentials,
    ListSourceCredentialsResponse (ListSourceCredentialsResponse'),
    newListSourceCredentialsResponse,

    -- ** UpdateProject
    UpdateProject (UpdateProject'),
    newUpdateProject,
    UpdateProjectResponse (UpdateProjectResponse'),
    newUpdateProjectResponse,

    -- ** DeleteProject
    DeleteProject (DeleteProject'),
    newDeleteProject,
    DeleteProjectResponse (DeleteProjectResponse'),
    newDeleteProjectResponse,

    -- ** DeleteSourceCredentials
    DeleteSourceCredentials (DeleteSourceCredentials'),
    newDeleteSourceCredentials,
    DeleteSourceCredentialsResponse (DeleteSourceCredentialsResponse'),
    newDeleteSourceCredentialsResponse,

    -- ** BatchGetProjects
    BatchGetProjects (BatchGetProjects'),
    newBatchGetProjects,
    BatchGetProjectsResponse (BatchGetProjectsResponse'),
    newBatchGetProjectsResponse,

    -- ** ListSharedReportGroups (Paginated)
    ListSharedReportGroups (ListSharedReportGroups'),
    newListSharedReportGroups,
    ListSharedReportGroupsResponse (ListSharedReportGroupsResponse'),
    newListSharedReportGroupsResponse,

    -- ** RetryBuild
    RetryBuild (RetryBuild'),
    newRetryBuild,
    RetryBuildResponse (RetryBuildResponse'),
    newRetryBuildResponse,

    -- ** StopBuild
    StopBuild (StopBuild'),
    newStopBuild,
    StopBuildResponse (StopBuildResponse'),
    newStopBuildResponse,

    -- ** BatchGetBuildBatches
    BatchGetBuildBatches (BatchGetBuildBatches'),
    newBatchGetBuildBatches,
    BatchGetBuildBatchesResponse (BatchGetBuildBatchesResponse'),
    newBatchGetBuildBatchesResponse,

    -- ** StartBuild
    StartBuild (StartBuild'),
    newStartBuild,
    StartBuildResponse (StartBuildResponse'),
    newStartBuildResponse,

    -- ** GetReportGroupTrend
    GetReportGroupTrend (GetReportGroupTrend'),
    newGetReportGroupTrend,
    GetReportGroupTrendResponse (GetReportGroupTrendResponse'),
    newGetReportGroupTrendResponse,

    -- ** DeleteResourcePolicy
    DeleteResourcePolicy (DeleteResourcePolicy'),
    newDeleteResourcePolicy,
    DeleteResourcePolicyResponse (DeleteResourcePolicyResponse'),
    newDeleteResourcePolicyResponse,

    -- ** ListCuratedEnvironmentImages
    ListCuratedEnvironmentImages (ListCuratedEnvironmentImages'),
    newListCuratedEnvironmentImages,
    ListCuratedEnvironmentImagesResponse (ListCuratedEnvironmentImagesResponse'),
    newListCuratedEnvironmentImagesResponse,

    -- ** ListReportGroups (Paginated)
    ListReportGroups (ListReportGroups'),
    newListReportGroups,
    ListReportGroupsResponse (ListReportGroupsResponse'),
    newListReportGroupsResponse,

    -- ** InvalidateProjectCache
    InvalidateProjectCache (InvalidateProjectCache'),
    newInvalidateProjectCache,
    InvalidateProjectCacheResponse (InvalidateProjectCacheResponse'),
    newInvalidateProjectCacheResponse,

    -- ** ImportSourceCredentials
    ImportSourceCredentials (ImportSourceCredentials'),
    newImportSourceCredentials,
    ImportSourceCredentialsResponse (ImportSourceCredentialsResponse'),
    newImportSourceCredentialsResponse,

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

    -- * Types

    -- ** ArtifactNamespace
    ArtifactNamespace (..),

    -- ** ArtifactPackaging
    ArtifactPackaging (..),

    -- ** ArtifactsType
    ArtifactsType (..),

    -- ** AuthType
    AuthType (..),

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

import Network.AWS.CodeBuild.BatchDeleteBuilds
import Network.AWS.CodeBuild.BatchGetBuildBatches
import Network.AWS.CodeBuild.BatchGetBuilds
import Network.AWS.CodeBuild.BatchGetProjects
import Network.AWS.CodeBuild.BatchGetReportGroups
import Network.AWS.CodeBuild.BatchGetReports
import Network.AWS.CodeBuild.CreateProject
import Network.AWS.CodeBuild.CreateReportGroup
import Network.AWS.CodeBuild.CreateWebhook
import Network.AWS.CodeBuild.DeleteBuildBatch
import Network.AWS.CodeBuild.DeleteProject
import Network.AWS.CodeBuild.DeleteReport
import Network.AWS.CodeBuild.DeleteReportGroup
import Network.AWS.CodeBuild.DeleteResourcePolicy
import Network.AWS.CodeBuild.DeleteSourceCredentials
import Network.AWS.CodeBuild.DeleteWebhook
import Network.AWS.CodeBuild.DescribeCodeCoverages
import Network.AWS.CodeBuild.DescribeTestCases
import Network.AWS.CodeBuild.GetReportGroupTrend
import Network.AWS.CodeBuild.GetResourcePolicy
import Network.AWS.CodeBuild.ImportSourceCredentials
import Network.AWS.CodeBuild.InvalidateProjectCache
import Network.AWS.CodeBuild.Lens
import Network.AWS.CodeBuild.ListBuildBatches
import Network.AWS.CodeBuild.ListBuildBatchesForProject
import Network.AWS.CodeBuild.ListBuilds
import Network.AWS.CodeBuild.ListBuildsForProject
import Network.AWS.CodeBuild.ListCuratedEnvironmentImages
import Network.AWS.CodeBuild.ListProjects
import Network.AWS.CodeBuild.ListReportGroups
import Network.AWS.CodeBuild.ListReports
import Network.AWS.CodeBuild.ListReportsForReportGroup
import Network.AWS.CodeBuild.ListSharedProjects
import Network.AWS.CodeBuild.ListSharedReportGroups
import Network.AWS.CodeBuild.ListSourceCredentials
import Network.AWS.CodeBuild.PutResourcePolicy
import Network.AWS.CodeBuild.RetryBuild
import Network.AWS.CodeBuild.RetryBuildBatch
import Network.AWS.CodeBuild.StartBuild
import Network.AWS.CodeBuild.StartBuildBatch
import Network.AWS.CodeBuild.StopBuild
import Network.AWS.CodeBuild.StopBuildBatch
import Network.AWS.CodeBuild.Types
import Network.AWS.CodeBuild.UpdateProject
import Network.AWS.CodeBuild.UpdateProjectVisibility
import Network.AWS.CodeBuild.UpdateReportGroup
import Network.AWS.CodeBuild.UpdateWebhook
import Network.AWS.CodeBuild.Waiters

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

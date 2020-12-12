{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS CodeBuild__
--
-- AWS CodeBuild is a fully managed build service in the cloud. AWS CodeBuild compiles your source code, runs unit tests, and produces artifacts that are ready to deploy. AWS CodeBuild eliminates the need to provision, manage, and scale your own build servers. It provides prepackaged build environments for the most popular programming languages and build tools, such as Apache Maven, Gradle, and more. You can also fully customize build environments in AWS CodeBuild to use your own build tools. AWS CodeBuild scales automatically to meet peak build requests. You pay only for the build time you consume. For more information about AWS CodeBuild, see the /<https:\/\/docs.aws.amazon.com\/codebuild\/latest\/userguide\/welcome.html AWS CodeBuild User Guide> ./
-- AWS CodeBuild supports these operations:
--
--     * @BatchDeleteBuilds@ : Deletes one or more builds.
--
--
--     * @BatchGetBuilds@ : Gets information about one or more builds.
--
--
--     * @BatchGetProjects@ : Gets information about one or more build projects. A /build project/ defines how AWS CodeBuild runs a build. This includes information such as where to get the source code to build, the build environment to use, the build commands to run, and where to store the build output. A /build environment/ is a representation of operating system, programming language runtime, and tools that AWS CodeBuild uses to run a build. You can add tags to build projects to help manage your resources and costs.
--
--
--     * @BatchGetReportGroups@ : Returns an array of report groups.
--
--
--     * @BatchGetReports@ : Returns an array of reports.
--
--
--     * @CreateProject@ : Creates a build project.
--
--
--     * @CreateReportGroup@ : Creates a report group. A report group contains a collection of reports.
--
--
--     * @CreateWebhook@ : For an existing AWS CodeBuild build project that has its source code stored in a GitHub or Bitbucket repository, enables AWS CodeBuild to start rebuilding the source code every time a code change is pushed to the repository.
--
--
--     * @DeleteProject@ : Deletes a build project.
--
--
--     * @DeleteReport@ : Deletes a report.
--
--
--     * @DeleteReportGroup@ : Deletes a report group.
--
--
--     * @DeleteResourcePolicy@ : Deletes a resource policy that is identified by its resource ARN.
--
--
--     * @DeleteSourceCredentials@ : Deletes a set of GitHub, GitHub Enterprise, or Bitbucket source credentials.
--
--
--     * @DeleteWebhook@ : For an existing AWS CodeBuild build project that has its source code stored in a GitHub or Bitbucket repository, stops AWS CodeBuild from rebuilding the source code every time a code change is pushed to the repository.
--
--
--     * @DescribeTestCases@ : Returns a list of details about test cases for a report.
--
--
--     * @GetResourcePolicy@ : Gets a resource policy that is identified by its resource ARN.
--
--
--     * @ImportSourceCredentials@ : Imports the source repository credentials for an AWS CodeBuild project that has its source code stored in a GitHub, GitHub Enterprise, or Bitbucket repository.
--
--
--     * @InvalidateProjectCache@ : Resets the cache for a project.
--
--
--     * @ListBuilds@ : Gets a list of build IDs, with each build ID representing a single build.
--
--
--     * @ListBuildsForProject@ : Gets a list of build IDs for the specified build project, with each build ID representing a single build.
--
--
--     * @ListCuratedEnvironmentImages@ : Gets information about Docker images that are managed by AWS CodeBuild.
--
--
--     * @ListProjects@ : Gets a list of build project names, with each build project name representing a single build project.
--
--
--     * @ListReportGroups@ : Gets a list ARNs for the report groups in the current AWS account.
--
--
--     * @ListReports@ : Gets a list ARNs for the reports in the current AWS account.
--
--
--     * @ListReportsForReportGroup@ : Returns a list of ARNs for the reports that belong to a @ReportGroup@ .
--
--
--     * @ListSharedProjects@ : Gets a list of ARNs associated with projects shared with the current AWS account or user.
--
--
--     * @ListSharedReportGroups@ : Gets a list of ARNs associated with report groups shared with the current AWS account or user
--
--
--     * @ListSourceCredentials@ : Returns a list of @SourceCredentialsInfo@ objects. Each @SourceCredentialsInfo@ object includes the authentication type, token ARN, and type of source provider for one set of credentials.
--
--
--     * @PutResourcePolicy@ : Stores a resource policy for the ARN of a @Project@ or @ReportGroup@ object.
--
--
--     * @StartBuild@ : Starts running a build.
--
--
--     * @StopBuild@ : Attempts to stop running a build.
--
--
--     * @UpdateProject@ : Changes the settings of an existing build project.
--
--
--     * @UpdateReportGroup@ : Changes a report group.
--
--
--     * @UpdateWebhook@ : Changes the settings of an existing webhook.
module Network.AWS.CodeBuild
  ( -- * Service configuration
    codeBuildService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListProjects (Paginated)
    module Network.AWS.CodeBuild.ListProjects,

    -- ** DeleteProject
    module Network.AWS.CodeBuild.DeleteProject,

    -- ** UpdateProject
    module Network.AWS.CodeBuild.UpdateProject,

    -- ** DeleteSourceCredentials
    module Network.AWS.CodeBuild.DeleteSourceCredentials,

    -- ** ListBuilds (Paginated)
    module Network.AWS.CodeBuild.ListBuilds,

    -- ** ListSourceCredentials
    module Network.AWS.CodeBuild.ListSourceCredentials,

    -- ** ListReports (Paginated)
    module Network.AWS.CodeBuild.ListReports,

    -- ** DeleteReport
    module Network.AWS.CodeBuild.DeleteReport,

    -- ** CreateWebhook
    module Network.AWS.CodeBuild.CreateWebhook,

    -- ** StopBuildBatch
    module Network.AWS.CodeBuild.StopBuildBatch,

    -- ** ListSharedProjects (Paginated)
    module Network.AWS.CodeBuild.ListSharedProjects,

    -- ** CreateReportGroup
    module Network.AWS.CodeBuild.CreateReportGroup,

    -- ** DescribeCodeCoverages (Paginated)
    module Network.AWS.CodeBuild.DescribeCodeCoverages,

    -- ** ImportSourceCredentials
    module Network.AWS.CodeBuild.ImportSourceCredentials,

    -- ** ListBuildBatchesForProject (Paginated)
    module Network.AWS.CodeBuild.ListBuildBatchesForProject,

    -- ** BatchGetReportGroups
    module Network.AWS.CodeBuild.BatchGetReportGroups,

    -- ** DeleteBuildBatch
    module Network.AWS.CodeBuild.DeleteBuildBatch,

    -- ** StartBuild
    module Network.AWS.CodeBuild.StartBuild,

    -- ** BatchGetBuildBatches
    module Network.AWS.CodeBuild.BatchGetBuildBatches,

    -- ** RetryBuild
    module Network.AWS.CodeBuild.RetryBuild,

    -- ** ListBuildsForProject (Paginated)
    module Network.AWS.CodeBuild.ListBuildsForProject,

    -- ** DescribeTestCases (Paginated)
    module Network.AWS.CodeBuild.DescribeTestCases,

    -- ** GetResourcePolicy
    module Network.AWS.CodeBuild.GetResourcePolicy,

    -- ** BatchGetProjects
    module Network.AWS.CodeBuild.BatchGetProjects,

    -- ** BatchGetBuilds
    module Network.AWS.CodeBuild.BatchGetBuilds,

    -- ** BatchGetReports
    module Network.AWS.CodeBuild.BatchGetReports,

    -- ** UpdateWebhook
    module Network.AWS.CodeBuild.UpdateWebhook,

    -- ** DeleteWebhook
    module Network.AWS.CodeBuild.DeleteWebhook,

    -- ** StartBuildBatch
    module Network.AWS.CodeBuild.StartBuildBatch,

    -- ** RetryBuildBatch
    module Network.AWS.CodeBuild.RetryBuildBatch,

    -- ** ListReportsForReportGroup (Paginated)
    module Network.AWS.CodeBuild.ListReportsForReportGroup,

    -- ** InvalidateProjectCache
    module Network.AWS.CodeBuild.InvalidateProjectCache,

    -- ** UpdateReportGroup
    module Network.AWS.CodeBuild.UpdateReportGroup,

    -- ** DeleteReportGroup
    module Network.AWS.CodeBuild.DeleteReportGroup,

    -- ** BatchDeleteBuilds
    module Network.AWS.CodeBuild.BatchDeleteBuilds,

    -- ** ListReportGroups (Paginated)
    module Network.AWS.CodeBuild.ListReportGroups,

    -- ** PutResourcePolicy
    module Network.AWS.CodeBuild.PutResourcePolicy,

    -- ** DeleteResourcePolicy
    module Network.AWS.CodeBuild.DeleteResourcePolicy,

    -- ** ListCuratedEnvironmentImages
    module Network.AWS.CodeBuild.ListCuratedEnvironmentImages,

    -- ** GetReportGroupTrend
    module Network.AWS.CodeBuild.GetReportGroupTrend,

    -- ** StopBuild
    module Network.AWS.CodeBuild.StopBuild,

    -- ** ListBuildBatches (Paginated)
    module Network.AWS.CodeBuild.ListBuildBatches,

    -- ** CreateProject
    module Network.AWS.CodeBuild.CreateProject,

    -- ** ListSharedReportGroups (Paginated)
    module Network.AWS.CodeBuild.ListSharedReportGroups,

    -- * Types

    -- ** ArtifactNamespace
    ArtifactNamespace (..),

    -- ** ArtifactPackaging
    ArtifactPackaging (..),

    -- ** ArtifactsType
    ArtifactsType (..),

    -- ** AuthType
    AuthType (..),

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
    BatchRestrictions (..),
    mkBatchRestrictions,
    brMaximumBuildsAllowed,
    brComputeTypesAllowed,

    -- ** Build
    Build (..),
    mkBuild,
    bPhases,
    bBuildComplete,
    bSecondaryArtifacts,
    bArn,
    bExportedEnvironmentVariables,
    bBuildNumber,
    bStartTime,
    bArtifacts,
    bEnvironment,
    bInitiator,
    bNetworkInterface,
    bSecondarySourceVersions,
    bCurrentPhase,
    bQueuedTimeoutInMinutes,
    bCache,
    bSecondarySources,
    bDebugSession,
    bSourceVersion,
    bBuildBatchARN,
    bLogs,
    bResolvedSourceVersion,
    bVpcConfig,
    bEndTime,
    bProjectName,
    bBuildStatus,
    bSource,
    bId,
    bFileSystemLocations,
    bReportARNs,
    bEncryptionKey,
    bServiceRole,
    bTimeoutInMinutes,

    -- ** BuildArtifacts
    BuildArtifacts (..),
    mkBuildArtifacts,
    baLocation,
    baMd5sum,
    baEncryptionDisabled,
    baOverrideArtifactName,
    baArtifactIdentifier,
    baSha256sum,

    -- ** BuildBatch
    BuildBatch (..),
    mkBuildBatch,
    bbPhases,
    bbSecondaryArtifacts,
    bbBuildTimeoutInMinutes,
    bbArn,
    bbStartTime,
    bbArtifacts,
    bbEnvironment,
    bbInitiator,
    bbSecondarySourceVersions,
    bbBuildBatchStatus,
    bbCurrentPhase,
    bbBuildBatchNumber,
    bbQueuedTimeoutInMinutes,
    bbCache,
    bbSecondarySources,
    bbSourceVersion,
    bbResolvedSourceVersion,
    bbVpcConfig,
    bbEndTime,
    bbProjectName,
    bbBuildGroups,
    bbSource,
    bbId,
    bbFileSystemLocations,
    bbBuildBatchConfig,
    bbEncryptionKey,
    bbLogConfig,
    bbServiceRole,
    bbComplete,

    -- ** BuildBatchFilter
    BuildBatchFilter (..),
    mkBuildBatchFilter,
    bbfStatus,

    -- ** BuildBatchPhase
    BuildBatchPhase (..),
    mkBuildBatchPhase,
    bbpContexts,
    bbpStartTime,
    bbpPhaseStatus,
    bbpPhaseType,
    bbpEndTime,
    bbpDurationInSeconds,

    -- ** BuildGroup
    BuildGroup (..),
    mkBuildGroup,
    bgIdentifier,
    bgDependsOn,
    bgIgnoreFailure,
    bgCurrentBuildSummary,
    bgPriorBuildSummaryList,

    -- ** BuildNotDeleted
    BuildNotDeleted (..),
    mkBuildNotDeleted,
    bndId,
    bndStatusCode,

    -- ** BuildPhase
    BuildPhase (..),
    mkBuildPhase,
    bpContexts,
    bpStartTime,
    bpPhaseStatus,
    bpPhaseType,
    bpEndTime,
    bpDurationInSeconds,

    -- ** BuildStatusConfig
    BuildStatusConfig (..),
    mkBuildStatusConfig,
    bscContext,
    bscTargetURL,

    -- ** BuildSummary
    BuildSummary (..),
    mkBuildSummary,
    bsSecondaryArtifacts,
    bsPrimaryArtifact,
    bsArn,
    bsBuildStatus,
    bsRequestedOn,

    -- ** CloudWatchLogsConfig
    CloudWatchLogsConfig (..),
    mkCloudWatchLogsConfig,
    cwlcGroupName,
    cwlcStreamName,
    cwlcStatus,

    -- ** CodeCoverage
    CodeCoverage (..),
    mkCodeCoverage,
    ccExpired,
    ccBranchesMissed,
    ccLinesMissed,
    ccFilePath,
    ccBranchesCovered,
    ccLinesCovered,
    ccBranchCoveragePercentage,
    ccId,
    ccLineCoveragePercentage,
    ccReportARN,

    -- ** CodeCoverageReportSummary
    CodeCoverageReportSummary (..),
    mkCodeCoverageReportSummary,
    ccrsBranchesMissed,
    ccrsLinesMissed,
    ccrsBranchesCovered,
    ccrsLinesCovered,
    ccrsBranchCoveragePercentage,
    ccrsLineCoveragePercentage,

    -- ** DebugSession
    DebugSession (..),
    mkDebugSession,
    dsSessionEnabled,
    dsSessionTarget,

    -- ** EnvironmentImage
    EnvironmentImage (..),
    mkEnvironmentImage,
    eiVersions,
    eiName,
    eiDescription,

    -- ** EnvironmentLanguage
    EnvironmentLanguage (..),
    mkEnvironmentLanguage,
    elImages,
    elLanguage,

    -- ** EnvironmentPlatform
    EnvironmentPlatform (..),
    mkEnvironmentPlatform,
    epPlatform,
    epLanguages,

    -- ** EnvironmentVariable
    EnvironmentVariable (..),
    mkEnvironmentVariable,
    evType,
    evName,
    evValue,

    -- ** ExportedEnvironmentVariable
    ExportedEnvironmentVariable (..),
    mkExportedEnvironmentVariable,
    eevValue,
    eevName,

    -- ** GitSubmodulesConfig
    GitSubmodulesConfig (..),
    mkGitSubmodulesConfig,
    gscFetchSubmodules,

    -- ** LogsConfig
    LogsConfig (..),
    mkLogsConfig,
    lcS3Logs,
    lcCloudWatchLogs,

    -- ** LogsLocation
    LogsLocation (..),
    mkLogsLocation,
    llDeepLink,
    llS3Logs,
    llCloudWatchLogs,
    llS3DeepLink,
    llS3LogsARN,
    llCloudWatchLogsARN,
    llGroupName,
    llStreamName,

    -- ** NetworkInterface
    NetworkInterface (..),
    mkNetworkInterface,
    niSubnetId,
    niNetworkInterfaceId,

    -- ** PhaseContext
    PhaseContext (..),
    mkPhaseContext,
    pcMessage,
    pcStatusCode,

    -- ** Project
    Project (..),
    mkProject,
    pSecondaryArtifacts,
    pArn,
    pArtifacts,
    pEnvironment,
    pCreated,
    pSecondarySourceVersions,
    pQueuedTimeoutInMinutes,
    pCache,
    pSecondarySources,
    pSourceVersion,
    pName,
    pVpcConfig,
    pSource,
    pBadge,
    pLogsConfig,
    pFileSystemLocations,
    pBuildBatchConfig,
    pEncryptionKey,
    pLastModified,
    pWebhook,
    pDescription,
    pServiceRole,
    pTags,
    pTimeoutInMinutes,

    -- ** ProjectArtifacts
    ProjectArtifacts (..),
    mkProjectArtifacts,
    paPackaging,
    paPath,
    paLocation,
    paName,
    paEncryptionDisabled,
    paOverrideArtifactName,
    paArtifactIdentifier,
    paNamespaceType,
    paType,

    -- ** ProjectBadge
    ProjectBadge (..),
    mkProjectBadge,
    pbBadgeEnabled,
    pbBadgeRequestURL,

    -- ** ProjectBuildBatchConfig
    ProjectBuildBatchConfig (..),
    mkProjectBuildBatchConfig,
    pbbcCombineArtifacts,
    pbbcTimeoutInMins,
    pbbcRestrictions,
    pbbcServiceRole,

    -- ** ProjectCache
    ProjectCache (..),
    mkProjectCache,
    pcLocation,
    pcModes,
    pcType,

    -- ** ProjectEnvironment
    ProjectEnvironment (..),
    mkProjectEnvironment,
    peImagePullCredentialsType,
    pePrivilegedMode,
    peRegistryCredential,
    peCertificate,
    peEnvironmentVariables,
    peType,
    peImage,
    peComputeType,

    -- ** ProjectFileSystemLocation
    ProjectFileSystemLocation (..),
    mkProjectFileSystemLocation,
    pfslLocation,
    pfslIdentifier,
    pfslMountOptions,
    pfslType,
    pfslMountPoint,

    -- ** ProjectSource
    ProjectSource (..),
    mkProjectSource,
    psReportBuildStatus,
    psInsecureSSL,
    psLocation,
    psAuth,
    psBuildspec,
    psSourceIdentifier,
    psGitCloneDepth,
    psGitSubmodulesConfig,
    psBuildStatusConfig,
    psType,

    -- ** ProjectSourceVersion
    ProjectSourceVersion (..),
    mkProjectSourceVersion,
    psvSourceIdentifier,
    psvSourceVersion,

    -- ** RegistryCredential
    RegistryCredential (..),
    mkRegistryCredential,
    rcCredential,
    rcCredentialProvider,

    -- ** Report
    Report (..),
    mkReport,
    rReportGroupARN,
    rStatus,
    rExpired,
    rExecutionId,
    rTruncated,
    rArn,
    rCreated,
    rName,
    rCodeCoverageSummary,
    rTestSummary,
    rType,
    rExportConfig,

    -- ** ReportExportConfig
    ReportExportConfig (..),
    mkReportExportConfig,
    recExportConfigType,
    recS3Destination,

    -- ** ReportFilter
    ReportFilter (..),
    mkReportFilter,
    rfStatus,

    -- ** ReportGroup
    ReportGroup (..),
    mkReportGroup,
    rgStatus,
    rgArn,
    rgCreated,
    rgName,
    rgType,
    rgLastModified,
    rgExportConfig,
    rgTags,

    -- ** ReportGroupTrendStats
    ReportGroupTrendStats (..),
    mkReportGroupTrendStats,
    rgtsMax,
    rgtsAverage,
    rgtsMin,

    -- ** ReportWithRawData
    ReportWithRawData (..),
    mkReportWithRawData,
    rwrdData,
    rwrdReportARN,

    -- ** ResolvedArtifact
    ResolvedArtifact (..),
    mkResolvedArtifact,
    raLocation,
    raIdentifier,
    raType,

    -- ** S3LogsConfig
    S3LogsConfig (..),
    mkS3LogsConfig,
    slcLocation,
    slcEncryptionDisabled,
    slcStatus,

    -- ** S3ReportExportConfig
    S3ReportExportConfig (..),
    mkS3ReportExportConfig,
    srecPackaging,
    srecPath,
    srecBucket,
    srecEncryptionDisabled,
    srecEncryptionKey,

    -- ** SourceAuth
    SourceAuth (..),
    mkSourceAuth,
    saResource,
    saType,

    -- ** SourceCredentialsInfo
    SourceCredentialsInfo (..),
    mkSourceCredentialsInfo,
    sciArn,
    sciServerType,
    sciAuthType,

    -- ** Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- ** TestCase
    TestCase (..),
    mkTestCase,
    tcDurationInNanoSeconds,
    tcStatus,
    tcExpired,
    tcPrefix,
    tcName,
    tcTestRawDataPath,
    tcMessage,
    tcReportARN,

    -- ** TestCaseFilter
    TestCaseFilter (..),
    mkTestCaseFilter,
    tcfStatus,
    tcfKeyword,

    -- ** TestReportSummary
    TestReportSummary (..),
    mkTestReportSummary,
    trsTotal,
    trsStatusCounts,
    trsDurationInNanoSeconds,

    -- ** VPCConfig
    VPCConfig (..),
    mkVPCConfig,
    vcSecurityGroupIds,
    vcVpcId,
    vcSubnets,

    -- ** Webhook
    Webhook (..),
    mkWebhook,
    wBranchFilter,
    wLastModifiedSecret,
    wUrl,
    wSecret,
    wFilterGroups,
    wPayloadURL,
    wBuildType,

    -- ** WebhookFilter
    WebhookFilter (..),
    mkWebhookFilter,
    wfExcludeMatchedPattern,
    wfType,
    wfPattern,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.DateTime,
    Lude.Timestamp,
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
import Network.AWS.CodeBuild.UpdateReportGroup
import Network.AWS.CodeBuild.UpdateWebhook
import Network.AWS.CodeBuild.Waiters
import qualified Network.AWS.Prelude as Lude

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

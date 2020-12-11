-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types
  ( -- * Service configuration
    codeBuildService,

    -- * Errors

    -- * ArtifactNamespace
    ArtifactNamespace (..),

    -- * ArtifactPackaging
    ArtifactPackaging (..),

    -- * ArtifactsType
    ArtifactsType (..),

    -- * AuthType
    AuthType (..),

    -- * BuildBatchPhaseType
    BuildBatchPhaseType (..),

    -- * BuildPhaseType
    BuildPhaseType (..),

    -- * CacheMode
    CacheMode (..),

    -- * CacheType
    CacheType (..),

    -- * ComputeType
    ComputeType (..),

    -- * CredentialProviderType
    CredentialProviderType (..),

    -- * EnvironmentType
    EnvironmentType (..),

    -- * EnvironmentVariableType
    EnvironmentVariableType (..),

    -- * FileSystemType
    FileSystemType (..),

    -- * ImagePullCredentialsType
    ImagePullCredentialsType (..),

    -- * LanguageType
    LanguageType (..),

    -- * LogsConfigStatusType
    LogsConfigStatusType (..),

    -- * PlatformType
    PlatformType (..),

    -- * ProjectSortByType
    ProjectSortByType (..),

    -- * ReportCodeCoverageSortByType
    ReportCodeCoverageSortByType (..),

    -- * ReportExportConfigType
    ReportExportConfigType (..),

    -- * ReportGroupSortByType
    ReportGroupSortByType (..),

    -- * ReportGroupStatusType
    ReportGroupStatusType (..),

    -- * ReportGroupTrendFieldType
    ReportGroupTrendFieldType (..),

    -- * ReportPackagingType
    ReportPackagingType (..),

    -- * ReportStatusType
    ReportStatusType (..),

    -- * ReportType
    ReportType (..),

    -- * RetryBuildBatchType
    RetryBuildBatchType (..),

    -- * ServerType
    ServerType (..),

    -- * SharedResourceSortByType
    SharedResourceSortByType (..),

    -- * SortOrderType
    SortOrderType (..),

    -- * SourceAuthType
    SourceAuthType (..),

    -- * SourceType
    SourceType (..),

    -- * StatusType
    StatusType (..),

    -- * WebhookBuildType
    WebhookBuildType (..),

    -- * WebhookFilterType
    WebhookFilterType (..),

    -- * BatchRestrictions
    BatchRestrictions (..),
    mkBatchRestrictions,
    brMaximumBuildsAllowed,
    brComputeTypesAllowed,

    -- * Build
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

    -- * BuildArtifacts
    BuildArtifacts (..),
    mkBuildArtifacts,
    baLocation,
    baMd5sum,
    baEncryptionDisabled,
    baOverrideArtifactName,
    baArtifactIdentifier,
    baSha256sum,

    -- * BuildBatch
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

    -- * BuildBatchFilter
    BuildBatchFilter (..),
    mkBuildBatchFilter,
    bbfStatus,

    -- * BuildBatchPhase
    BuildBatchPhase (..),
    mkBuildBatchPhase,
    bbpContexts,
    bbpStartTime,
    bbpPhaseStatus,
    bbpPhaseType,
    bbpEndTime,
    bbpDurationInSeconds,

    -- * BuildGroup
    BuildGroup (..),
    mkBuildGroup,
    bgIdentifier,
    bgDependsOn,
    bgIgnoreFailure,
    bgCurrentBuildSummary,
    bgPriorBuildSummaryList,

    -- * BuildNotDeleted
    BuildNotDeleted (..),
    mkBuildNotDeleted,
    bndId,
    bndStatusCode,

    -- * BuildPhase
    BuildPhase (..),
    mkBuildPhase,
    bpContexts,
    bpStartTime,
    bpPhaseStatus,
    bpPhaseType,
    bpEndTime,
    bpDurationInSeconds,

    -- * BuildStatusConfig
    BuildStatusConfig (..),
    mkBuildStatusConfig,
    bscContext,
    bscTargetURL,

    -- * BuildSummary
    BuildSummary (..),
    mkBuildSummary,
    bsSecondaryArtifacts,
    bsPrimaryArtifact,
    bsArn,
    bsBuildStatus,
    bsRequestedOn,

    -- * CloudWatchLogsConfig
    CloudWatchLogsConfig (..),
    mkCloudWatchLogsConfig,
    cwlcGroupName,
    cwlcStreamName,
    cwlcStatus,

    -- * CodeCoverage
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

    -- * CodeCoverageReportSummary
    CodeCoverageReportSummary (..),
    mkCodeCoverageReportSummary,
    ccrsBranchesMissed,
    ccrsLinesMissed,
    ccrsBranchesCovered,
    ccrsLinesCovered,
    ccrsBranchCoveragePercentage,
    ccrsLineCoveragePercentage,

    -- * DebugSession
    DebugSession (..),
    mkDebugSession,
    dsSessionEnabled,
    dsSessionTarget,

    -- * EnvironmentImage
    EnvironmentImage (..),
    mkEnvironmentImage,
    eiVersions,
    eiName,
    eiDescription,

    -- * EnvironmentLanguage
    EnvironmentLanguage (..),
    mkEnvironmentLanguage,
    elImages,
    elLanguage,

    -- * EnvironmentPlatform
    EnvironmentPlatform (..),
    mkEnvironmentPlatform,
    epPlatform,
    epLanguages,

    -- * EnvironmentVariable
    EnvironmentVariable (..),
    mkEnvironmentVariable,
    evType,
    evName,
    evValue,

    -- * ExportedEnvironmentVariable
    ExportedEnvironmentVariable (..),
    mkExportedEnvironmentVariable,
    eevValue,
    eevName,

    -- * GitSubmodulesConfig
    GitSubmodulesConfig (..),
    mkGitSubmodulesConfig,
    gscFetchSubmodules,

    -- * LogsConfig
    LogsConfig (..),
    mkLogsConfig,
    lcS3Logs,
    lcCloudWatchLogs,

    -- * LogsLocation
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

    -- * NetworkInterface
    NetworkInterface (..),
    mkNetworkInterface,
    niSubnetId,
    niNetworkInterfaceId,

    -- * PhaseContext
    PhaseContext (..),
    mkPhaseContext,
    pcMessage,
    pcStatusCode,

    -- * Project
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

    -- * ProjectArtifacts
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

    -- * ProjectBadge
    ProjectBadge (..),
    mkProjectBadge,
    pbBadgeEnabled,
    pbBadgeRequestURL,

    -- * ProjectBuildBatchConfig
    ProjectBuildBatchConfig (..),
    mkProjectBuildBatchConfig,
    pbbcCombineArtifacts,
    pbbcTimeoutInMins,
    pbbcRestrictions,
    pbbcServiceRole,

    -- * ProjectCache
    ProjectCache (..),
    mkProjectCache,
    pcLocation,
    pcModes,
    pcType,

    -- * ProjectEnvironment
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

    -- * ProjectFileSystemLocation
    ProjectFileSystemLocation (..),
    mkProjectFileSystemLocation,
    pfslLocation,
    pfslIdentifier,
    pfslMountOptions,
    pfslType,
    pfslMountPoint,

    -- * ProjectSource
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

    -- * ProjectSourceVersion
    ProjectSourceVersion (..),
    mkProjectSourceVersion,
    psvSourceIdentifier,
    psvSourceVersion,

    -- * RegistryCredential
    RegistryCredential (..),
    mkRegistryCredential,
    rcCredential,
    rcCredentialProvider,

    -- * Report
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

    -- * ReportExportConfig
    ReportExportConfig (..),
    mkReportExportConfig,
    recExportConfigType,
    recS3Destination,

    -- * ReportFilter
    ReportFilter (..),
    mkReportFilter,
    rfStatus,

    -- * ReportGroup
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

    -- * ReportGroupTrendStats
    ReportGroupTrendStats (..),
    mkReportGroupTrendStats,
    rgtsMax,
    rgtsAverage,
    rgtsMin,

    -- * ReportWithRawData
    ReportWithRawData (..),
    mkReportWithRawData,
    rwrdData,
    rwrdReportARN,

    -- * ResolvedArtifact
    ResolvedArtifact (..),
    mkResolvedArtifact,
    raLocation,
    raIdentifier,
    raType,

    -- * S3LogsConfig
    S3LogsConfig (..),
    mkS3LogsConfig,
    slcLocation,
    slcEncryptionDisabled,
    slcStatus,

    -- * S3ReportExportConfig
    S3ReportExportConfig (..),
    mkS3ReportExportConfig,
    srecPackaging,
    srecPath,
    srecBucket,
    srecEncryptionDisabled,
    srecEncryptionKey,

    -- * SourceAuth
    SourceAuth (..),
    mkSourceAuth,
    saResource,
    saType,

    -- * SourceCredentialsInfo
    SourceCredentialsInfo (..),
    mkSourceCredentialsInfo,
    sciArn,
    sciServerType,
    sciAuthType,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- * TestCase
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

    -- * TestCaseFilter
    TestCaseFilter (..),
    mkTestCaseFilter,
    tcfStatus,
    tcfKeyword,

    -- * TestReportSummary
    TestReportSummary (..),
    mkTestReportSummary,
    trsTotal,
    trsStatusCounts,
    trsDurationInNanoSeconds,

    -- * VPCConfig
    VPCConfig (..),
    mkVPCConfig,
    vcSecurityGroupIds,
    vcVpcId,
    vcSubnets,

    -- * Webhook
    Webhook (..),
    mkWebhook,
    wBranchFilter,
    wLastModifiedSecret,
    wUrl,
    wSecret,
    wFilterGroups,
    wPayloadURL,
    wBuildType,

    -- * WebhookFilter
    WebhookFilter (..),
    mkWebhookFilter,
    wfExcludeMatchedPattern,
    wfType,
    wfPattern,
  )
where

import Network.AWS.CodeBuild.Types.ArtifactNamespace
import Network.AWS.CodeBuild.Types.ArtifactPackaging
import Network.AWS.CodeBuild.Types.ArtifactsType
import Network.AWS.CodeBuild.Types.AuthType
import Network.AWS.CodeBuild.Types.BatchRestrictions
import Network.AWS.CodeBuild.Types.Build
import Network.AWS.CodeBuild.Types.BuildArtifacts
import Network.AWS.CodeBuild.Types.BuildBatch
import Network.AWS.CodeBuild.Types.BuildBatchFilter
import Network.AWS.CodeBuild.Types.BuildBatchPhase
import Network.AWS.CodeBuild.Types.BuildBatchPhaseType
import Network.AWS.CodeBuild.Types.BuildGroup
import Network.AWS.CodeBuild.Types.BuildNotDeleted
import Network.AWS.CodeBuild.Types.BuildPhase
import Network.AWS.CodeBuild.Types.BuildPhaseType
import Network.AWS.CodeBuild.Types.BuildStatusConfig
import Network.AWS.CodeBuild.Types.BuildSummary
import Network.AWS.CodeBuild.Types.CacheMode
import Network.AWS.CodeBuild.Types.CacheType
import Network.AWS.CodeBuild.Types.CloudWatchLogsConfig
import Network.AWS.CodeBuild.Types.CodeCoverage
import Network.AWS.CodeBuild.Types.CodeCoverageReportSummary
import Network.AWS.CodeBuild.Types.ComputeType
import Network.AWS.CodeBuild.Types.CredentialProviderType
import Network.AWS.CodeBuild.Types.DebugSession
import Network.AWS.CodeBuild.Types.EnvironmentImage
import Network.AWS.CodeBuild.Types.EnvironmentLanguage
import Network.AWS.CodeBuild.Types.EnvironmentPlatform
import Network.AWS.CodeBuild.Types.EnvironmentType
import Network.AWS.CodeBuild.Types.EnvironmentVariable
import Network.AWS.CodeBuild.Types.EnvironmentVariableType
import Network.AWS.CodeBuild.Types.ExportedEnvironmentVariable
import Network.AWS.CodeBuild.Types.FileSystemType
import Network.AWS.CodeBuild.Types.GitSubmodulesConfig
import Network.AWS.CodeBuild.Types.ImagePullCredentialsType
import Network.AWS.CodeBuild.Types.LanguageType
import Network.AWS.CodeBuild.Types.LogsConfig
import Network.AWS.CodeBuild.Types.LogsConfigStatusType
import Network.AWS.CodeBuild.Types.LogsLocation
import Network.AWS.CodeBuild.Types.NetworkInterface
import Network.AWS.CodeBuild.Types.PhaseContext
import Network.AWS.CodeBuild.Types.PlatformType
import Network.AWS.CodeBuild.Types.Project
import Network.AWS.CodeBuild.Types.ProjectArtifacts
import Network.AWS.CodeBuild.Types.ProjectBadge
import Network.AWS.CodeBuild.Types.ProjectBuildBatchConfig
import Network.AWS.CodeBuild.Types.ProjectCache
import Network.AWS.CodeBuild.Types.ProjectEnvironment
import Network.AWS.CodeBuild.Types.ProjectFileSystemLocation
import Network.AWS.CodeBuild.Types.ProjectSortByType
import Network.AWS.CodeBuild.Types.ProjectSource
import Network.AWS.CodeBuild.Types.ProjectSourceVersion
import Network.AWS.CodeBuild.Types.RegistryCredential
import Network.AWS.CodeBuild.Types.Report
import Network.AWS.CodeBuild.Types.ReportCodeCoverageSortByType
import Network.AWS.CodeBuild.Types.ReportExportConfig
import Network.AWS.CodeBuild.Types.ReportExportConfigType
import Network.AWS.CodeBuild.Types.ReportFilter
import Network.AWS.CodeBuild.Types.ReportGroup
import Network.AWS.CodeBuild.Types.ReportGroupSortByType
import Network.AWS.CodeBuild.Types.ReportGroupStatusType
import Network.AWS.CodeBuild.Types.ReportGroupTrendFieldType
import Network.AWS.CodeBuild.Types.ReportGroupTrendStats
import Network.AWS.CodeBuild.Types.ReportPackagingType
import Network.AWS.CodeBuild.Types.ReportStatusType
import Network.AWS.CodeBuild.Types.ReportType
import Network.AWS.CodeBuild.Types.ReportWithRawData
import Network.AWS.CodeBuild.Types.ResolvedArtifact
import Network.AWS.CodeBuild.Types.RetryBuildBatchType
import Network.AWS.CodeBuild.Types.S3LogsConfig
import Network.AWS.CodeBuild.Types.S3ReportExportConfig
import Network.AWS.CodeBuild.Types.ServerType
import Network.AWS.CodeBuild.Types.SharedResourceSortByType
import Network.AWS.CodeBuild.Types.SortOrderType
import Network.AWS.CodeBuild.Types.SourceAuth
import Network.AWS.CodeBuild.Types.SourceAuthType
import Network.AWS.CodeBuild.Types.SourceCredentialsInfo
import Network.AWS.CodeBuild.Types.SourceType
import Network.AWS.CodeBuild.Types.StatusType
import Network.AWS.CodeBuild.Types.Tag
import Network.AWS.CodeBuild.Types.TestCase
import Network.AWS.CodeBuild.Types.TestCaseFilter
import Network.AWS.CodeBuild.Types.TestReportSummary
import Network.AWS.CodeBuild.Types.VPCConfig
import Network.AWS.CodeBuild.Types.Webhook
import Network.AWS.CodeBuild.Types.WebhookBuildType
import Network.AWS.CodeBuild.Types.WebhookFilter
import Network.AWS.CodeBuild.Types.WebhookFilterType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-10-06@ of the Amazon CodeBuild SDK configuration.
codeBuildService :: Lude.Service
codeBuildService =
  Lude.Service
    { Lude._svcAbbrev = "CodeBuild",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "codebuild",
      Lude._svcVersion = "2016-10-06",
      Lude._svcEndpoint = Lude.defaultEndpoint codeBuildService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "CodeBuild",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing

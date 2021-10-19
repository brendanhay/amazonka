{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ResourceAlreadyExistsException,
    _OAuthProviderException,
    _AccountLimitExceededException,
    _InvalidInputException,
    _ResourceNotFoundException,

    -- * ArtifactNamespace
    ArtifactNamespace (..),

    -- * ArtifactPackaging
    ArtifactPackaging (..),

    -- * ArtifactsType
    ArtifactsType (..),

    -- * AuthType
    AuthType (..),

    -- * BatchReportModeType
    BatchReportModeType (..),

    -- * BucketOwnerAccess
    BucketOwnerAccess (..),

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

    -- * ProjectVisibilityType
    ProjectVisibilityType (..),

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
    newBatchRestrictions,
    batchRestrictions_maximumBuildsAllowed,
    batchRestrictions_computeTypesAllowed,

    -- * Build
    Build (..),
    newBuild,
    build_phases,
    build_buildComplete,
    build_secondaryArtifacts,
    build_arn,
    build_exportedEnvironmentVariables,
    build_buildNumber,
    build_startTime,
    build_artifacts,
    build_environment,
    build_initiator,
    build_networkInterface,
    build_secondarySourceVersions,
    build_currentPhase,
    build_queuedTimeoutInMinutes,
    build_cache,
    build_secondarySources,
    build_debugSession,
    build_sourceVersion,
    build_buildBatchArn,
    build_logs,
    build_resolvedSourceVersion,
    build_vpcConfig,
    build_endTime,
    build_projectName,
    build_buildStatus,
    build_source,
    build_id,
    build_fileSystemLocations,
    build_reportArns,
    build_encryptionKey,
    build_serviceRole,
    build_timeoutInMinutes,

    -- * BuildArtifacts
    BuildArtifacts (..),
    newBuildArtifacts,
    buildArtifacts_location,
    buildArtifacts_md5sum,
    buildArtifacts_encryptionDisabled,
    buildArtifacts_overrideArtifactName,
    buildArtifacts_artifactIdentifier,
    buildArtifacts_sha256sum,
    buildArtifacts_bucketOwnerAccess,

    -- * BuildBatch
    BuildBatch (..),
    newBuildBatch,
    buildBatch_phases,
    buildBatch_secondaryArtifacts,
    buildBatch_buildTimeoutInMinutes,
    buildBatch_debugSessionEnabled,
    buildBatch_arn,
    buildBatch_startTime,
    buildBatch_artifacts,
    buildBatch_environment,
    buildBatch_initiator,
    buildBatch_secondarySourceVersions,
    buildBatch_buildBatchStatus,
    buildBatch_currentPhase,
    buildBatch_buildBatchNumber,
    buildBatch_queuedTimeoutInMinutes,
    buildBatch_cache,
    buildBatch_secondarySources,
    buildBatch_sourceVersion,
    buildBatch_resolvedSourceVersion,
    buildBatch_vpcConfig,
    buildBatch_endTime,
    buildBatch_projectName,
    buildBatch_buildGroups,
    buildBatch_source,
    buildBatch_id,
    buildBatch_fileSystemLocations,
    buildBatch_buildBatchConfig,
    buildBatch_encryptionKey,
    buildBatch_logConfig,
    buildBatch_serviceRole,
    buildBatch_complete,

    -- * BuildBatchFilter
    BuildBatchFilter (..),
    newBuildBatchFilter,
    buildBatchFilter_status,

    -- * BuildBatchPhase
    BuildBatchPhase (..),
    newBuildBatchPhase,
    buildBatchPhase_contexts,
    buildBatchPhase_startTime,
    buildBatchPhase_phaseStatus,
    buildBatchPhase_phaseType,
    buildBatchPhase_endTime,
    buildBatchPhase_durationInSeconds,

    -- * BuildGroup
    BuildGroup (..),
    newBuildGroup,
    buildGroup_identifier,
    buildGroup_dependsOn,
    buildGroup_ignoreFailure,
    buildGroup_currentBuildSummary,
    buildGroup_priorBuildSummaryList,

    -- * BuildNotDeleted
    BuildNotDeleted (..),
    newBuildNotDeleted,
    buildNotDeleted_id,
    buildNotDeleted_statusCode,

    -- * BuildPhase
    BuildPhase (..),
    newBuildPhase,
    buildPhase_contexts,
    buildPhase_startTime,
    buildPhase_phaseStatus,
    buildPhase_phaseType,
    buildPhase_endTime,
    buildPhase_durationInSeconds,

    -- * BuildStatusConfig
    BuildStatusConfig (..),
    newBuildStatusConfig,
    buildStatusConfig_context,
    buildStatusConfig_targetUrl,

    -- * BuildSummary
    BuildSummary (..),
    newBuildSummary,
    buildSummary_secondaryArtifacts,
    buildSummary_primaryArtifact,
    buildSummary_arn,
    buildSummary_buildStatus,
    buildSummary_requestedOn,

    -- * CloudWatchLogsConfig
    CloudWatchLogsConfig (..),
    newCloudWatchLogsConfig,
    cloudWatchLogsConfig_groupName,
    cloudWatchLogsConfig_streamName,
    cloudWatchLogsConfig_status,

    -- * CodeCoverage
    CodeCoverage (..),
    newCodeCoverage,
    codeCoverage_expired,
    codeCoverage_branchesMissed,
    codeCoverage_linesMissed,
    codeCoverage_filePath,
    codeCoverage_branchesCovered,
    codeCoverage_linesCovered,
    codeCoverage_branchCoveragePercentage,
    codeCoverage_id,
    codeCoverage_lineCoveragePercentage,
    codeCoverage_reportARN,

    -- * CodeCoverageReportSummary
    CodeCoverageReportSummary (..),
    newCodeCoverageReportSummary,
    codeCoverageReportSummary_branchesMissed,
    codeCoverageReportSummary_linesMissed,
    codeCoverageReportSummary_branchesCovered,
    codeCoverageReportSummary_linesCovered,
    codeCoverageReportSummary_branchCoveragePercentage,
    codeCoverageReportSummary_lineCoveragePercentage,

    -- * DebugSession
    DebugSession (..),
    newDebugSession,
    debugSession_sessionEnabled,
    debugSession_sessionTarget,

    -- * EnvironmentImage
    EnvironmentImage (..),
    newEnvironmentImage,
    environmentImage_versions,
    environmentImage_name,
    environmentImage_description,

    -- * EnvironmentLanguage
    EnvironmentLanguage (..),
    newEnvironmentLanguage,
    environmentLanguage_images,
    environmentLanguage_language,

    -- * EnvironmentPlatform
    EnvironmentPlatform (..),
    newEnvironmentPlatform,
    environmentPlatform_platform,
    environmentPlatform_languages,

    -- * EnvironmentVariable
    EnvironmentVariable (..),
    newEnvironmentVariable,
    environmentVariable_type,
    environmentVariable_name,
    environmentVariable_value,

    -- * ExportedEnvironmentVariable
    ExportedEnvironmentVariable (..),
    newExportedEnvironmentVariable,
    exportedEnvironmentVariable_value,
    exportedEnvironmentVariable_name,

    -- * GitSubmodulesConfig
    GitSubmodulesConfig (..),
    newGitSubmodulesConfig,
    gitSubmodulesConfig_fetchSubmodules,

    -- * LogsConfig
    LogsConfig (..),
    newLogsConfig,
    logsConfig_s3Logs,
    logsConfig_cloudWatchLogs,

    -- * LogsLocation
    LogsLocation (..),
    newLogsLocation,
    logsLocation_deepLink,
    logsLocation_s3Logs,
    logsLocation_cloudWatchLogs,
    logsLocation_s3DeepLink,
    logsLocation_s3LogsArn,
    logsLocation_cloudWatchLogsArn,
    logsLocation_groupName,
    logsLocation_streamName,

    -- * NetworkInterface
    NetworkInterface (..),
    newNetworkInterface,
    networkInterface_subnetId,
    networkInterface_networkInterfaceId,

    -- * PhaseContext
    PhaseContext (..),
    newPhaseContext,
    phaseContext_message,
    phaseContext_statusCode,

    -- * Project
    Project (..),
    newProject,
    project_secondaryArtifacts,
    project_resourceAccessRole,
    project_arn,
    project_artifacts,
    project_environment,
    project_created,
    project_concurrentBuildLimit,
    project_secondarySourceVersions,
    project_queuedTimeoutInMinutes,
    project_cache,
    project_secondarySources,
    project_sourceVersion,
    project_name,
    project_vpcConfig,
    project_publicProjectAlias,
    project_source,
    project_badge,
    project_logsConfig,
    project_fileSystemLocations,
    project_buildBatchConfig,
    project_encryptionKey,
    project_lastModified,
    project_projectVisibility,
    project_webhook,
    project_description,
    project_serviceRole,
    project_tags,
    project_timeoutInMinutes,

    -- * ProjectArtifacts
    ProjectArtifacts (..),
    newProjectArtifacts,
    projectArtifacts_packaging,
    projectArtifacts_path,
    projectArtifacts_location,
    projectArtifacts_name,
    projectArtifacts_encryptionDisabled,
    projectArtifacts_overrideArtifactName,
    projectArtifacts_artifactIdentifier,
    projectArtifacts_bucketOwnerAccess,
    projectArtifacts_namespaceType,
    projectArtifacts_type,

    -- * ProjectBadge
    ProjectBadge (..),
    newProjectBadge,
    projectBadge_badgeEnabled,
    projectBadge_badgeRequestUrl,

    -- * ProjectBuildBatchConfig
    ProjectBuildBatchConfig (..),
    newProjectBuildBatchConfig,
    projectBuildBatchConfig_combineArtifacts,
    projectBuildBatchConfig_timeoutInMins,
    projectBuildBatchConfig_restrictions,
    projectBuildBatchConfig_batchReportMode,
    projectBuildBatchConfig_serviceRole,

    -- * ProjectCache
    ProjectCache (..),
    newProjectCache,
    projectCache_location,
    projectCache_modes,
    projectCache_type,

    -- * ProjectEnvironment
    ProjectEnvironment (..),
    newProjectEnvironment,
    projectEnvironment_imagePullCredentialsType,
    projectEnvironment_privilegedMode,
    projectEnvironment_registryCredential,
    projectEnvironment_certificate,
    projectEnvironment_environmentVariables,
    projectEnvironment_type,
    projectEnvironment_image,
    projectEnvironment_computeType,

    -- * ProjectFileSystemLocation
    ProjectFileSystemLocation (..),
    newProjectFileSystemLocation,
    projectFileSystemLocation_location,
    projectFileSystemLocation_identifier,
    projectFileSystemLocation_mountOptions,
    projectFileSystemLocation_type,
    projectFileSystemLocation_mountPoint,

    -- * ProjectSource
    ProjectSource (..),
    newProjectSource,
    projectSource_reportBuildStatus,
    projectSource_insecureSsl,
    projectSource_location,
    projectSource_auth,
    projectSource_buildspec,
    projectSource_sourceIdentifier,
    projectSource_gitCloneDepth,
    projectSource_gitSubmodulesConfig,
    projectSource_buildStatusConfig,
    projectSource_type,

    -- * ProjectSourceVersion
    ProjectSourceVersion (..),
    newProjectSourceVersion,
    projectSourceVersion_sourceIdentifier,
    projectSourceVersion_sourceVersion,

    -- * RegistryCredential
    RegistryCredential (..),
    newRegistryCredential,
    registryCredential_credential,
    registryCredential_credentialProvider,

    -- * Report
    Report (..),
    newReport,
    report_reportGroupArn,
    report_status,
    report_expired,
    report_executionId,
    report_truncated,
    report_arn,
    report_created,
    report_name,
    report_codeCoverageSummary,
    report_testSummary,
    report_type,
    report_exportConfig,

    -- * ReportExportConfig
    ReportExportConfig (..),
    newReportExportConfig,
    reportExportConfig_exportConfigType,
    reportExportConfig_s3Destination,

    -- * ReportFilter
    ReportFilter (..),
    newReportFilter,
    reportFilter_status,

    -- * ReportGroup
    ReportGroup (..),
    newReportGroup,
    reportGroup_status,
    reportGroup_arn,
    reportGroup_created,
    reportGroup_name,
    reportGroup_type,
    reportGroup_lastModified,
    reportGroup_exportConfig,
    reportGroup_tags,

    -- * ReportGroupTrendStats
    ReportGroupTrendStats (..),
    newReportGroupTrendStats,
    reportGroupTrendStats_max,
    reportGroupTrendStats_average,
    reportGroupTrendStats_min,

    -- * ReportWithRawData
    ReportWithRawData (..),
    newReportWithRawData,
    reportWithRawData_data,
    reportWithRawData_reportArn,

    -- * ResolvedArtifact
    ResolvedArtifact (..),
    newResolvedArtifact,
    resolvedArtifact_location,
    resolvedArtifact_identifier,
    resolvedArtifact_type,

    -- * S3LogsConfig
    S3LogsConfig (..),
    newS3LogsConfig,
    s3LogsConfig_location,
    s3LogsConfig_encryptionDisabled,
    s3LogsConfig_bucketOwnerAccess,
    s3LogsConfig_status,

    -- * S3ReportExportConfig
    S3ReportExportConfig (..),
    newS3ReportExportConfig,
    s3ReportExportConfig_packaging,
    s3ReportExportConfig_path,
    s3ReportExportConfig_bucket,
    s3ReportExportConfig_bucketOwner,
    s3ReportExportConfig_encryptionDisabled,
    s3ReportExportConfig_encryptionKey,

    -- * SourceAuth
    SourceAuth (..),
    newSourceAuth,
    sourceAuth_resource,
    sourceAuth_type,

    -- * SourceCredentialsInfo
    SourceCredentialsInfo (..),
    newSourceCredentialsInfo,
    sourceCredentialsInfo_arn,
    sourceCredentialsInfo_serverType,
    sourceCredentialsInfo_authType,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,

    -- * TestCase
    TestCase (..),
    newTestCase,
    testCase_durationInNanoSeconds,
    testCase_status,
    testCase_expired,
    testCase_prefix,
    testCase_name,
    testCase_testRawDataPath,
    testCase_message,
    testCase_reportArn,

    -- * TestCaseFilter
    TestCaseFilter (..),
    newTestCaseFilter,
    testCaseFilter_status,
    testCaseFilter_keyword,

    -- * TestReportSummary
    TestReportSummary (..),
    newTestReportSummary,
    testReportSummary_total,
    testReportSummary_statusCounts,
    testReportSummary_durationInNanoSeconds,

    -- * VpcConfig
    VpcConfig (..),
    newVpcConfig,
    vpcConfig_securityGroupIds,
    vpcConfig_vpcId,
    vpcConfig_subnets,

    -- * Webhook
    Webhook (..),
    newWebhook,
    webhook_branchFilter,
    webhook_lastModifiedSecret,
    webhook_url,
    webhook_secret,
    webhook_filterGroups,
    webhook_payloadUrl,
    webhook_buildType,

    -- * WebhookFilter
    WebhookFilter (..),
    newWebhookFilter,
    webhookFilter_excludeMatchedPattern,
    webhookFilter_type,
    webhookFilter_pattern,
  )
where

import Network.AWS.CodeBuild.Types.ArtifactNamespace
import Network.AWS.CodeBuild.Types.ArtifactPackaging
import Network.AWS.CodeBuild.Types.ArtifactsType
import Network.AWS.CodeBuild.Types.AuthType
import Network.AWS.CodeBuild.Types.BatchReportModeType
import Network.AWS.CodeBuild.Types.BatchRestrictions
import Network.AWS.CodeBuild.Types.BucketOwnerAccess
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
import Network.AWS.CodeBuild.Types.ProjectVisibilityType
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
import Network.AWS.CodeBuild.Types.VpcConfig
import Network.AWS.CodeBuild.Types.Webhook
import Network.AWS.CodeBuild.Types.WebhookBuildType
import Network.AWS.CodeBuild.Types.WebhookFilter
import Network.AWS.CodeBuild.Types.WebhookFilterType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-10-06@ of the Amazon CodeBuild SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "CodeBuild",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "codebuild",
      Core._serviceSigningName = "codebuild",
      Core._serviceVersion = "2016-10-06",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "CodeBuild",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The specified Amazon Web Services resource cannot be created, because an
-- Amazon Web Services resource with the same settings already exists.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"

-- | There was a problem with the underlying OAuth provider.
_OAuthProviderException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OAuthProviderException =
  Core._MatchServiceError
    defaultService
    "OAuthProviderException"

-- | An Amazon Web Services service limit was exceeded for the calling Amazon
-- Web Services account.
_AccountLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccountLimitExceededException =
  Core._MatchServiceError
    defaultService
    "AccountLimitExceededException"

-- | The input value that was provided is not valid.
_InvalidInputException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInputException =
  Core._MatchServiceError
    defaultService
    "InvalidInputException"

-- | The specified Amazon Web Services resource cannot be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

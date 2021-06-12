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
    _OAuthProviderException,
    _ResourceAlreadyExistsException,
    _InvalidInputException,
    _AccountLimitExceededException,
    _ResourceNotFoundException,

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
    newBatchRestrictions,
    batchRestrictions_computeTypesAllowed,
    batchRestrictions_maximumBuildsAllowed,

    -- * Build
    Build (..),
    newBuild,
    build_vpcConfig,
    build_buildBatchArn,
    build_resolvedSourceVersion,
    build_secondaryArtifacts,
    build_sourceVersion,
    build_phases,
    build_cache,
    build_serviceRole,
    build_secondarySourceVersions,
    build_networkInterface,
    build_encryptionKey,
    build_artifacts,
    build_buildNumber,
    build_startTime,
    build_id,
    build_environment,
    build_source,
    build_arn,
    build_projectName,
    build_endTime,
    build_buildStatus,
    build_logs,
    build_buildComplete,
    build_debugSession,
    build_queuedTimeoutInMinutes,
    build_secondarySources,
    build_timeoutInMinutes,
    build_currentPhase,
    build_initiator,
    build_reportArns,
    build_fileSystemLocations,
    build_exportedEnvironmentVariables,

    -- * BuildArtifacts
    BuildArtifacts (..),
    newBuildArtifacts,
    buildArtifacts_sha256sum,
    buildArtifacts_overrideArtifactName,
    buildArtifacts_artifactIdentifier,
    buildArtifacts_md5sum,
    buildArtifacts_encryptionDisabled,
    buildArtifacts_location,

    -- * BuildBatch
    BuildBatch (..),
    newBuildBatch,
    buildBatch_vpcConfig,
    buildBatch_resolvedSourceVersion,
    buildBatch_secondaryArtifacts,
    buildBatch_sourceVersion,
    buildBatch_phases,
    buildBatch_cache,
    buildBatch_serviceRole,
    buildBatch_buildBatchNumber,
    buildBatch_secondarySourceVersions,
    buildBatch_encryptionKey,
    buildBatch_artifacts,
    buildBatch_startTime,
    buildBatch_id,
    buildBatch_environment,
    buildBatch_source,
    buildBatch_arn,
    buildBatch_projectName,
    buildBatch_endTime,
    buildBatch_buildGroups,
    buildBatch_buildTimeoutInMinutes,
    buildBatch_queuedTimeoutInMinutes,
    buildBatch_secondarySources,
    buildBatch_complete,
    buildBatch_logConfig,
    buildBatch_currentPhase,
    buildBatch_buildBatchStatus,
    buildBatch_initiator,
    buildBatch_buildBatchConfig,
    buildBatch_fileSystemLocations,
    buildBatch_debugSessionEnabled,

    -- * BuildBatchFilter
    BuildBatchFilter (..),
    newBuildBatchFilter,
    buildBatchFilter_status,

    -- * BuildBatchPhase
    BuildBatchPhase (..),
    newBuildBatchPhase,
    buildBatchPhase_phaseType,
    buildBatchPhase_contexts,
    buildBatchPhase_startTime,
    buildBatchPhase_endTime,
    buildBatchPhase_durationInSeconds,
    buildBatchPhase_phaseStatus,

    -- * BuildGroup
    BuildGroup (..),
    newBuildGroup,
    buildGroup_dependsOn,
    buildGroup_currentBuildSummary,
    buildGroup_identifier,
    buildGroup_ignoreFailure,
    buildGroup_priorBuildSummaryList,

    -- * BuildNotDeleted
    BuildNotDeleted (..),
    newBuildNotDeleted,
    buildNotDeleted_id,
    buildNotDeleted_statusCode,

    -- * BuildPhase
    BuildPhase (..),
    newBuildPhase,
    buildPhase_phaseType,
    buildPhase_contexts,
    buildPhase_startTime,
    buildPhase_endTime,
    buildPhase_durationInSeconds,
    buildPhase_phaseStatus,

    -- * BuildStatusConfig
    BuildStatusConfig (..),
    newBuildStatusConfig,
    buildStatusConfig_context,
    buildStatusConfig_targetUrl,

    -- * BuildSummary
    BuildSummary (..),
    newBuildSummary,
    buildSummary_secondaryArtifacts,
    buildSummary_requestedOn,
    buildSummary_arn,
    buildSummary_buildStatus,
    buildSummary_primaryArtifact,

    -- * CloudWatchLogsConfig
    CloudWatchLogsConfig (..),
    newCloudWatchLogsConfig,
    cloudWatchLogsConfig_groupName,
    cloudWatchLogsConfig_streamName,
    cloudWatchLogsConfig_status,

    -- * CodeCoverage
    CodeCoverage (..),
    newCodeCoverage,
    codeCoverage_branchesMissed,
    codeCoverage_linesCovered,
    codeCoverage_branchesCovered,
    codeCoverage_filePath,
    codeCoverage_reportARN,
    codeCoverage_id,
    codeCoverage_expired,
    codeCoverage_lineCoveragePercentage,
    codeCoverage_linesMissed,
    codeCoverage_branchCoveragePercentage,

    -- * CodeCoverageReportSummary
    CodeCoverageReportSummary (..),
    newCodeCoverageReportSummary,
    codeCoverageReportSummary_branchesMissed,
    codeCoverageReportSummary_linesCovered,
    codeCoverageReportSummary_branchesCovered,
    codeCoverageReportSummary_lineCoveragePercentage,
    codeCoverageReportSummary_linesMissed,
    codeCoverageReportSummary_branchCoveragePercentage,

    -- * DebugSession
    DebugSession (..),
    newDebugSession,
    debugSession_sessionTarget,
    debugSession_sessionEnabled,

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
    exportedEnvironmentVariable_name,
    exportedEnvironmentVariable_value,

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
    logsLocation_s3Logs,
    logsLocation_cloudWatchLogs,
    logsLocation_deepLink,
    logsLocation_groupName,
    logsLocation_cloudWatchLogsArn,
    logsLocation_s3LogsArn,
    logsLocation_s3DeepLink,
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
    project_vpcConfig,
    project_secondaryArtifacts,
    project_sourceVersion,
    project_cache,
    project_serviceRole,
    project_secondarySourceVersions,
    project_webhook,
    project_encryptionKey,
    project_concurrentBuildLimit,
    project_artifacts,
    project_environment,
    project_source,
    project_arn,
    project_logsConfig,
    project_name,
    project_queuedTimeoutInMinutes,
    project_secondarySources,
    project_tags,
    project_timeoutInMinutes,
    project_description,
    project_lastModified,
    project_created,
    project_buildBatchConfig,
    project_badge,
    project_fileSystemLocations,

    -- * ProjectArtifacts
    ProjectArtifacts (..),
    newProjectArtifacts,
    projectArtifacts_namespaceType,
    projectArtifacts_overrideArtifactName,
    projectArtifacts_artifactIdentifier,
    projectArtifacts_name,
    projectArtifacts_packaging,
    projectArtifacts_encryptionDisabled,
    projectArtifacts_location,
    projectArtifacts_path,
    projectArtifacts_type,

    -- * ProjectBadge
    ProjectBadge (..),
    newProjectBadge,
    projectBadge_badgeRequestUrl,
    projectBadge_badgeEnabled,

    -- * ProjectBuildBatchConfig
    ProjectBuildBatchConfig (..),
    newProjectBuildBatchConfig,
    projectBuildBatchConfig_combineArtifacts,
    projectBuildBatchConfig_serviceRole,
    projectBuildBatchConfig_timeoutInMins,
    projectBuildBatchConfig_restrictions,

    -- * ProjectCache
    ProjectCache (..),
    newProjectCache,
    projectCache_modes,
    projectCache_location,
    projectCache_type,

    -- * ProjectEnvironment
    ProjectEnvironment (..),
    newProjectEnvironment,
    projectEnvironment_privilegedMode,
    projectEnvironment_imagePullCredentialsType,
    projectEnvironment_registryCredential,
    projectEnvironment_environmentVariables,
    projectEnvironment_certificate,
    projectEnvironment_type,
    projectEnvironment_image,
    projectEnvironment_computeType,

    -- * ProjectFileSystemLocation
    ProjectFileSystemLocation (..),
    newProjectFileSystemLocation,
    projectFileSystemLocation_identifier,
    projectFileSystemLocation_mountOptions,
    projectFileSystemLocation_mountPoint,
    projectFileSystemLocation_type,
    projectFileSystemLocation_location,

    -- * ProjectSource
    ProjectSource (..),
    newProjectSource,
    projectSource_gitCloneDepth,
    projectSource_buildStatusConfig,
    projectSource_auth,
    projectSource_reportBuildStatus,
    projectSource_insecureSsl,
    projectSource_sourceIdentifier,
    projectSource_buildspec,
    projectSource_location,
    projectSource_gitSubmodulesConfig,
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
    report_codeCoverageSummary,
    report_reportGroupArn,
    report_status,
    report_exportConfig,
    report_arn,
    report_testSummary,
    report_name,
    report_expired,
    report_executionId,
    report_created,
    report_type,
    report_truncated,

    -- * ReportExportConfig
    ReportExportConfig (..),
    newReportExportConfig,
    reportExportConfig_s3Destination,
    reportExportConfig_exportConfigType,

    -- * ReportFilter
    ReportFilter (..),
    newReportFilter,
    reportFilter_status,

    -- * ReportGroup
    ReportGroup (..),
    newReportGroup,
    reportGroup_status,
    reportGroup_exportConfig,
    reportGroup_arn,
    reportGroup_name,
    reportGroup_tags,
    reportGroup_lastModified,
    reportGroup_created,
    reportGroup_type,

    -- * ReportGroupTrendStats
    ReportGroupTrendStats (..),
    newReportGroupTrendStats,
    reportGroupTrendStats_min,
    reportGroupTrendStats_max,
    reportGroupTrendStats_average,

    -- * ReportWithRawData
    ReportWithRawData (..),
    newReportWithRawData,
    reportWithRawData_reportArn,
    reportWithRawData_data,

    -- * ResolvedArtifact
    ResolvedArtifact (..),
    newResolvedArtifact,
    resolvedArtifact_identifier,
    resolvedArtifact_type,
    resolvedArtifact_location,

    -- * S3LogsConfig
    S3LogsConfig (..),
    newS3LogsConfig,
    s3LogsConfig_encryptionDisabled,
    s3LogsConfig_location,
    s3LogsConfig_status,

    -- * S3ReportExportConfig
    S3ReportExportConfig (..),
    newS3ReportExportConfig,
    s3ReportExportConfig_bucketOwner,
    s3ReportExportConfig_encryptionKey,
    s3ReportExportConfig_packaging,
    s3ReportExportConfig_encryptionDisabled,
    s3ReportExportConfig_bucket,
    s3ReportExportConfig_path,

    -- * SourceAuth
    SourceAuth (..),
    newSourceAuth,
    sourceAuth_resource,
    sourceAuth_type,

    -- * SourceCredentialsInfo
    SourceCredentialsInfo (..),
    newSourceCredentialsInfo,
    sourceCredentialsInfo_arn,
    sourceCredentialsInfo_authType,
    sourceCredentialsInfo_serverType,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TestCase
    TestCase (..),
    newTestCase,
    testCase_testRawDataPath,
    testCase_status,
    testCase_message,
    testCase_reportArn,
    testCase_prefix,
    testCase_name,
    testCase_expired,
    testCase_durationInNanoSeconds,

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
    webhook_payloadUrl,
    webhook_filterGroups,
    webhook_secret,
    webhook_buildType,
    webhook_url,
    webhook_lastModifiedSecret,

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
import Network.AWS.CodeBuild.Types.VpcConfig
import Network.AWS.CodeBuild.Types.Webhook
import Network.AWS.CodeBuild.Types.WebhookBuildType
import Network.AWS.CodeBuild.Types.WebhookFilter
import Network.AWS.CodeBuild.Types.WebhookFilterType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
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
      Core._serviceTimeout = Core.Just 70,
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
      | Lens.has (Core.hasStatus 504) e =
        Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Core.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Core.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttling_exception"
      | Lens.has
          (Core.hasCode "Throttling" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling"
      | Core.otherwise = Core.Nothing

-- | There was a problem with the underlying OAuth provider.
_OAuthProviderException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OAuthProviderException =
  Core._MatchServiceError
    defaultService
    "OAuthProviderException"

-- | The specified AWS resource cannot be created, because an AWS resource
-- with the same settings already exists.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"

-- | The input value that was provided is not valid.
_InvalidInputException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidInputException =
  Core._MatchServiceError
    defaultService
    "InvalidInputException"

-- | An AWS service limit was exceeded for the calling AWS account.
_AccountLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AccountLimitExceededException =
  Core._MatchServiceError
    defaultService
    "AccountLimitExceededException"

-- | The specified AWS resource cannot be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

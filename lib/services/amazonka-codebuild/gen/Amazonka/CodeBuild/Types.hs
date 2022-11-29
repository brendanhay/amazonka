{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodeBuild.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ResourceAlreadyExistsException,
    _InvalidInputException,
    _AccountLimitExceededException,
    _ResourceNotFoundException,
    _OAuthProviderException,

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
    batchRestrictions_computeTypesAllowed,
    batchRestrictions_maximumBuildsAllowed,

    -- * Build
    Build (..),
    newBuild,
    build_environment,
    build_resolvedSourceVersion,
    build_secondarySources,
    build_fileSystemLocations,
    build_exportedEnvironmentVariables,
    build_timeoutInMinutes,
    build_queuedTimeoutInMinutes,
    build_vpcConfig,
    build_secondaryArtifacts,
    build_buildStatus,
    build_debugSession,
    build_sourceVersion,
    build_arn,
    build_buildBatchArn,
    build_phases,
    build_currentPhase,
    build_reportArns,
    build_endTime,
    build_cache,
    build_id,
    build_serviceRole,
    build_logs,
    build_secondarySourceVersions,
    build_source,
    build_buildComplete,
    build_buildNumber,
    build_projectName,
    build_encryptionKey,
    build_networkInterface,
    build_initiator,
    build_startTime,
    build_artifacts,

    -- * BuildArtifacts
    BuildArtifacts (..),
    newBuildArtifacts,
    buildArtifacts_encryptionDisabled,
    buildArtifacts_md5sum,
    buildArtifacts_artifactIdentifier,
    buildArtifacts_location,
    buildArtifacts_overrideArtifactName,
    buildArtifacts_bucketOwnerAccess,
    buildArtifacts_sha256sum,

    -- * BuildBatch
    BuildBatch (..),
    newBuildBatch,
    buildBatch_environment,
    buildBatch_resolvedSourceVersion,
    buildBatch_secondarySources,
    buildBatch_fileSystemLocations,
    buildBatch_queuedTimeoutInMinutes,
    buildBatch_vpcConfig,
    buildBatch_secondaryArtifacts,
    buildBatch_debugSessionEnabled,
    buildBatch_sourceVersion,
    buildBatch_arn,
    buildBatch_complete,
    buildBatch_phases,
    buildBatch_currentPhase,
    buildBatch_endTime,
    buildBatch_cache,
    buildBatch_id,
    buildBatch_serviceRole,
    buildBatch_buildBatchStatus,
    buildBatch_secondarySourceVersions,
    buildBatch_source,
    buildBatch_buildBatchConfig,
    buildBatch_projectName,
    buildBatch_encryptionKey,
    buildBatch_buildGroups,
    buildBatch_buildTimeoutInMinutes,
    buildBatch_initiator,
    buildBatch_logConfig,
    buildBatch_buildBatchNumber,
    buildBatch_startTime,
    buildBatch_artifacts,

    -- * BuildBatchFilter
    BuildBatchFilter (..),
    newBuildBatchFilter,
    buildBatchFilter_status,

    -- * BuildBatchPhase
    BuildBatchPhase (..),
    newBuildBatchPhase,
    buildBatchPhase_contexts,
    buildBatchPhase_phaseStatus,
    buildBatchPhase_endTime,
    buildBatchPhase_phaseType,
    buildBatchPhase_durationInSeconds,
    buildBatchPhase_startTime,

    -- * BuildGroup
    BuildGroup (..),
    newBuildGroup,
    buildGroup_dependsOn,
    buildGroup_priorBuildSummaryList,
    buildGroup_ignoreFailure,
    buildGroup_identifier,
    buildGroup_currentBuildSummary,

    -- * BuildNotDeleted
    BuildNotDeleted (..),
    newBuildNotDeleted,
    buildNotDeleted_id,
    buildNotDeleted_statusCode,

    -- * BuildPhase
    BuildPhase (..),
    newBuildPhase,
    buildPhase_contexts,
    buildPhase_phaseStatus,
    buildPhase_endTime,
    buildPhase_phaseType,
    buildPhase_durationInSeconds,
    buildPhase_startTime,

    -- * BuildStatusConfig
    BuildStatusConfig (..),
    newBuildStatusConfig,
    buildStatusConfig_targetUrl,
    buildStatusConfig_context,

    -- * BuildSummary
    BuildSummary (..),
    newBuildSummary,
    buildSummary_secondaryArtifacts,
    buildSummary_buildStatus,
    buildSummary_arn,
    buildSummary_primaryArtifact,
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
    codeCoverage_linesCovered,
    codeCoverage_lineCoveragePercentage,
    codeCoverage_filePath,
    codeCoverage_reportARN,
    codeCoverage_linesMissed,
    codeCoverage_branchesMissed,
    codeCoverage_expired,
    codeCoverage_id,
    codeCoverage_branchesCovered,
    codeCoverage_branchCoveragePercentage,

    -- * CodeCoverageReportSummary
    CodeCoverageReportSummary (..),
    newCodeCoverageReportSummary,
    codeCoverageReportSummary_linesCovered,
    codeCoverageReportSummary_lineCoveragePercentage,
    codeCoverageReportSummary_linesMissed,
    codeCoverageReportSummary_branchesMissed,
    codeCoverageReportSummary_branchesCovered,
    codeCoverageReportSummary_branchCoveragePercentage,

    -- * DebugSession
    DebugSession (..),
    newDebugSession,
    debugSession_sessionEnabled,
    debugSession_sessionTarget,

    -- * EnvironmentImage
    EnvironmentImage (..),
    newEnvironmentImage,
    environmentImage_name,
    environmentImage_description,
    environmentImage_versions,

    -- * EnvironmentLanguage
    EnvironmentLanguage (..),
    newEnvironmentLanguage,
    environmentLanguage_language,
    environmentLanguage_images,

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
    logsConfig_cloudWatchLogs,
    logsConfig_s3Logs,

    -- * LogsLocation
    LogsLocation (..),
    newLogsLocation,
    logsLocation_cloudWatchLogs,
    logsLocation_s3Logs,
    logsLocation_groupName,
    logsLocation_s3LogsArn,
    logsLocation_deepLink,
    logsLocation_cloudWatchLogsArn,
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
    project_tags,
    project_webhook,
    project_name,
    project_environment,
    project_secondarySources,
    project_fileSystemLocations,
    project_timeoutInMinutes,
    project_queuedTimeoutInMinutes,
    project_vpcConfig,
    project_secondaryArtifacts,
    project_created,
    project_sourceVersion,
    project_arn,
    project_concurrentBuildLimit,
    project_projectVisibility,
    project_description,
    project_cache,
    project_serviceRole,
    project_badge,
    project_secondarySourceVersions,
    project_source,
    project_logsConfig,
    project_lastModified,
    project_publicProjectAlias,
    project_buildBatchConfig,
    project_encryptionKey,
    project_artifacts,
    project_resourceAccessRole,

    -- * ProjectArtifacts
    ProjectArtifacts (..),
    newProjectArtifacts,
    projectArtifacts_encryptionDisabled,
    projectArtifacts_name,
    projectArtifacts_path,
    projectArtifacts_artifactIdentifier,
    projectArtifacts_packaging,
    projectArtifacts_location,
    projectArtifacts_overrideArtifactName,
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
    projectBuildBatchConfig_timeoutInMins,
    projectBuildBatchConfig_restrictions,
    projectBuildBatchConfig_batchReportMode,
    projectBuildBatchConfig_combineArtifacts,
    projectBuildBatchConfig_serviceRole,

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
    projectEnvironment_certificate,
    projectEnvironment_environmentVariables,
    projectEnvironment_registryCredential,
    projectEnvironment_type,
    projectEnvironment_image,
    projectEnvironment_computeType,

    -- * ProjectFileSystemLocation
    ProjectFileSystemLocation (..),
    newProjectFileSystemLocation,
    projectFileSystemLocation_type,
    projectFileSystemLocation_mountPoint,
    projectFileSystemLocation_mountOptions,
    projectFileSystemLocation_location,
    projectFileSystemLocation_identifier,

    -- * ProjectSource
    ProjectSource (..),
    newProjectSource,
    projectSource_insecureSsl,
    projectSource_reportBuildStatus,
    projectSource_gitSubmodulesConfig,
    projectSource_location,
    projectSource_sourceIdentifier,
    projectSource_buildStatusConfig,
    projectSource_gitCloneDepth,
    projectSource_auth,
    projectSource_buildspec,
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
    report_name,
    report_type,
    report_reportGroupArn,
    report_created,
    report_truncated,
    report_arn,
    report_expired,
    report_status,
    report_codeCoverageSummary,
    report_executionId,
    report_exportConfig,
    report_testSummary,

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
    reportGroup_tags,
    reportGroup_name,
    reportGroup_type,
    reportGroup_created,
    reportGroup_arn,
    reportGroup_status,
    reportGroup_exportConfig,
    reportGroup_lastModified,

    -- * ReportGroupTrendStats
    ReportGroupTrendStats (..),
    newReportGroupTrendStats,
    reportGroupTrendStats_max,
    reportGroupTrendStats_average,
    reportGroupTrendStats_min,

    -- * ReportWithRawData
    ReportWithRawData (..),
    newReportWithRawData,
    reportWithRawData_reportArn,
    reportWithRawData_data,

    -- * ResolvedArtifact
    ResolvedArtifact (..),
    newResolvedArtifact,
    resolvedArtifact_type,
    resolvedArtifact_location,
    resolvedArtifact_identifier,

    -- * S3LogsConfig
    S3LogsConfig (..),
    newS3LogsConfig,
    s3LogsConfig_encryptionDisabled,
    s3LogsConfig_location,
    s3LogsConfig_bucketOwnerAccess,
    s3LogsConfig_status,

    -- * S3ReportExportConfig
    S3ReportExportConfig (..),
    newS3ReportExportConfig,
    s3ReportExportConfig_encryptionDisabled,
    s3ReportExportConfig_bucket,
    s3ReportExportConfig_bucketOwner,
    s3ReportExportConfig_path,
    s3ReportExportConfig_packaging,
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
    tag_key,
    tag_value,

    -- * TestCase
    TestCase (..),
    newTestCase,
    testCase_message,
    testCase_name,
    testCase_reportArn,
    testCase_expired,
    testCase_status,
    testCase_durationInNanoSeconds,
    testCase_prefix,
    testCase_testRawDataPath,

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
    vpcConfig_subnets,
    vpcConfig_vpcId,

    -- * Webhook
    Webhook (..),
    newWebhook,
    webhook_lastModifiedSecret,
    webhook_url,
    webhook_payloadUrl,
    webhook_secret,
    webhook_branchFilter,
    webhook_buildType,
    webhook_filterGroups,

    -- * WebhookFilter
    WebhookFilter (..),
    newWebhookFilter,
    webhookFilter_excludeMatchedPattern,
    webhookFilter_type,
    webhookFilter_pattern,
  )
where

import Amazonka.CodeBuild.Types.ArtifactNamespace
import Amazonka.CodeBuild.Types.ArtifactPackaging
import Amazonka.CodeBuild.Types.ArtifactsType
import Amazonka.CodeBuild.Types.AuthType
import Amazonka.CodeBuild.Types.BatchReportModeType
import Amazonka.CodeBuild.Types.BatchRestrictions
import Amazonka.CodeBuild.Types.BucketOwnerAccess
import Amazonka.CodeBuild.Types.Build
import Amazonka.CodeBuild.Types.BuildArtifacts
import Amazonka.CodeBuild.Types.BuildBatch
import Amazonka.CodeBuild.Types.BuildBatchFilter
import Amazonka.CodeBuild.Types.BuildBatchPhase
import Amazonka.CodeBuild.Types.BuildBatchPhaseType
import Amazonka.CodeBuild.Types.BuildGroup
import Amazonka.CodeBuild.Types.BuildNotDeleted
import Amazonka.CodeBuild.Types.BuildPhase
import Amazonka.CodeBuild.Types.BuildPhaseType
import Amazonka.CodeBuild.Types.BuildStatusConfig
import Amazonka.CodeBuild.Types.BuildSummary
import Amazonka.CodeBuild.Types.CacheMode
import Amazonka.CodeBuild.Types.CacheType
import Amazonka.CodeBuild.Types.CloudWatchLogsConfig
import Amazonka.CodeBuild.Types.CodeCoverage
import Amazonka.CodeBuild.Types.CodeCoverageReportSummary
import Amazonka.CodeBuild.Types.ComputeType
import Amazonka.CodeBuild.Types.CredentialProviderType
import Amazonka.CodeBuild.Types.DebugSession
import Amazonka.CodeBuild.Types.EnvironmentImage
import Amazonka.CodeBuild.Types.EnvironmentLanguage
import Amazonka.CodeBuild.Types.EnvironmentPlatform
import Amazonka.CodeBuild.Types.EnvironmentType
import Amazonka.CodeBuild.Types.EnvironmentVariable
import Amazonka.CodeBuild.Types.EnvironmentVariableType
import Amazonka.CodeBuild.Types.ExportedEnvironmentVariable
import Amazonka.CodeBuild.Types.FileSystemType
import Amazonka.CodeBuild.Types.GitSubmodulesConfig
import Amazonka.CodeBuild.Types.ImagePullCredentialsType
import Amazonka.CodeBuild.Types.LanguageType
import Amazonka.CodeBuild.Types.LogsConfig
import Amazonka.CodeBuild.Types.LogsConfigStatusType
import Amazonka.CodeBuild.Types.LogsLocation
import Amazonka.CodeBuild.Types.NetworkInterface
import Amazonka.CodeBuild.Types.PhaseContext
import Amazonka.CodeBuild.Types.PlatformType
import Amazonka.CodeBuild.Types.Project
import Amazonka.CodeBuild.Types.ProjectArtifacts
import Amazonka.CodeBuild.Types.ProjectBadge
import Amazonka.CodeBuild.Types.ProjectBuildBatchConfig
import Amazonka.CodeBuild.Types.ProjectCache
import Amazonka.CodeBuild.Types.ProjectEnvironment
import Amazonka.CodeBuild.Types.ProjectFileSystemLocation
import Amazonka.CodeBuild.Types.ProjectSortByType
import Amazonka.CodeBuild.Types.ProjectSource
import Amazonka.CodeBuild.Types.ProjectSourceVersion
import Amazonka.CodeBuild.Types.ProjectVisibilityType
import Amazonka.CodeBuild.Types.RegistryCredential
import Amazonka.CodeBuild.Types.Report
import Amazonka.CodeBuild.Types.ReportCodeCoverageSortByType
import Amazonka.CodeBuild.Types.ReportExportConfig
import Amazonka.CodeBuild.Types.ReportExportConfigType
import Amazonka.CodeBuild.Types.ReportFilter
import Amazonka.CodeBuild.Types.ReportGroup
import Amazonka.CodeBuild.Types.ReportGroupSortByType
import Amazonka.CodeBuild.Types.ReportGroupStatusType
import Amazonka.CodeBuild.Types.ReportGroupTrendFieldType
import Amazonka.CodeBuild.Types.ReportGroupTrendStats
import Amazonka.CodeBuild.Types.ReportPackagingType
import Amazonka.CodeBuild.Types.ReportStatusType
import Amazonka.CodeBuild.Types.ReportType
import Amazonka.CodeBuild.Types.ReportWithRawData
import Amazonka.CodeBuild.Types.ResolvedArtifact
import Amazonka.CodeBuild.Types.RetryBuildBatchType
import Amazonka.CodeBuild.Types.S3LogsConfig
import Amazonka.CodeBuild.Types.S3ReportExportConfig
import Amazonka.CodeBuild.Types.ServerType
import Amazonka.CodeBuild.Types.SharedResourceSortByType
import Amazonka.CodeBuild.Types.SortOrderType
import Amazonka.CodeBuild.Types.SourceAuth
import Amazonka.CodeBuild.Types.SourceAuthType
import Amazonka.CodeBuild.Types.SourceCredentialsInfo
import Amazonka.CodeBuild.Types.SourceType
import Amazonka.CodeBuild.Types.StatusType
import Amazonka.CodeBuild.Types.Tag
import Amazonka.CodeBuild.Types.TestCase
import Amazonka.CodeBuild.Types.TestCaseFilter
import Amazonka.CodeBuild.Types.TestReportSummary
import Amazonka.CodeBuild.Types.VpcConfig
import Amazonka.CodeBuild.Types.Webhook
import Amazonka.CodeBuild.Types.WebhookBuildType
import Amazonka.CodeBuild.Types.WebhookFilter
import Amazonka.CodeBuild.Types.WebhookFilterType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2016-10-06@ of the Amazon CodeBuild SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "CodeBuild",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "codebuild",
      Core.signingName = "codebuild",
      Core.version = "2016-10-06",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "CodeBuild",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The specified Amazon Web Services resource cannot be created, because an
-- Amazon Web Services resource with the same settings already exists.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"

-- | The input value that was provided is not valid.
_InvalidInputException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInputException =
  Core._MatchServiceError
    defaultService
    "InvalidInputException"

-- | An Amazon Web Services service limit was exceeded for the calling Amazon
-- Web Services account.
_AccountLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccountLimitExceededException =
  Core._MatchServiceError
    defaultService
    "AccountLimitExceededException"

-- | The specified Amazon Web Services resource cannot be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | There was a problem with the underlying OAuth provider.
_OAuthProviderException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OAuthProviderException =
  Core._MatchServiceError
    defaultService
    "OAuthProviderException"

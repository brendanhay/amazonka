{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodeBuild.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccountLimitExceededException,
    _InvalidInputException,
    _OAuthProviderException,
    _ResourceAlreadyExistsException,
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
    batchRestrictions_computeTypesAllowed,
    batchRestrictions_maximumBuildsAllowed,

    -- * Build
    Build (..),
    newBuild,
    build_arn,
    build_artifacts,
    build_buildBatchArn,
    build_buildComplete,
    build_buildNumber,
    build_buildStatus,
    build_cache,
    build_currentPhase,
    build_debugSession,
    build_encryptionKey,
    build_endTime,
    build_environment,
    build_exportedEnvironmentVariables,
    build_fileSystemLocations,
    build_id,
    build_initiator,
    build_logs,
    build_networkInterface,
    build_phases,
    build_projectName,
    build_queuedTimeoutInMinutes,
    build_reportArns,
    build_resolvedSourceVersion,
    build_secondaryArtifacts,
    build_secondarySourceVersions,
    build_secondarySources,
    build_serviceRole,
    build_source,
    build_sourceVersion,
    build_startTime,
    build_timeoutInMinutes,
    build_vpcConfig,

    -- * BuildArtifacts
    BuildArtifacts (..),
    newBuildArtifacts,
    buildArtifacts_artifactIdentifier,
    buildArtifacts_bucketOwnerAccess,
    buildArtifacts_encryptionDisabled,
    buildArtifacts_location,
    buildArtifacts_md5sum,
    buildArtifacts_overrideArtifactName,
    buildArtifacts_sha256sum,

    -- * BuildBatch
    BuildBatch (..),
    newBuildBatch,
    buildBatch_arn,
    buildBatch_artifacts,
    buildBatch_buildBatchConfig,
    buildBatch_buildBatchNumber,
    buildBatch_buildBatchStatus,
    buildBatch_buildGroups,
    buildBatch_buildTimeoutInMinutes,
    buildBatch_cache,
    buildBatch_complete,
    buildBatch_currentPhase,
    buildBatch_debugSessionEnabled,
    buildBatch_encryptionKey,
    buildBatch_endTime,
    buildBatch_environment,
    buildBatch_fileSystemLocations,
    buildBatch_id,
    buildBatch_initiator,
    buildBatch_logConfig,
    buildBatch_phases,
    buildBatch_projectName,
    buildBatch_queuedTimeoutInMinutes,
    buildBatch_resolvedSourceVersion,
    buildBatch_secondaryArtifacts,
    buildBatch_secondarySourceVersions,
    buildBatch_secondarySources,
    buildBatch_serviceRole,
    buildBatch_source,
    buildBatch_sourceVersion,
    buildBatch_startTime,
    buildBatch_vpcConfig,

    -- * BuildBatchFilter
    BuildBatchFilter (..),
    newBuildBatchFilter,
    buildBatchFilter_status,

    -- * BuildBatchPhase
    BuildBatchPhase (..),
    newBuildBatchPhase,
    buildBatchPhase_contexts,
    buildBatchPhase_durationInSeconds,
    buildBatchPhase_endTime,
    buildBatchPhase_phaseStatus,
    buildBatchPhase_phaseType,
    buildBatchPhase_startTime,

    -- * BuildGroup
    BuildGroup (..),
    newBuildGroup,
    buildGroup_currentBuildSummary,
    buildGroup_dependsOn,
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
    buildPhase_contexts,
    buildPhase_durationInSeconds,
    buildPhase_endTime,
    buildPhase_phaseStatus,
    buildPhase_phaseType,
    buildPhase_startTime,

    -- * BuildStatusConfig
    BuildStatusConfig (..),
    newBuildStatusConfig,
    buildStatusConfig_context,
    buildStatusConfig_targetUrl,

    -- * BuildSummary
    BuildSummary (..),
    newBuildSummary,
    buildSummary_arn,
    buildSummary_buildStatus,
    buildSummary_primaryArtifact,
    buildSummary_requestedOn,
    buildSummary_secondaryArtifacts,

    -- * CloudWatchLogsConfig
    CloudWatchLogsConfig (..),
    newCloudWatchLogsConfig,
    cloudWatchLogsConfig_groupName,
    cloudWatchLogsConfig_streamName,
    cloudWatchLogsConfig_status,

    -- * CodeCoverage
    CodeCoverage (..),
    newCodeCoverage,
    codeCoverage_branchCoveragePercentage,
    codeCoverage_branchesCovered,
    codeCoverage_branchesMissed,
    codeCoverage_expired,
    codeCoverage_filePath,
    codeCoverage_id,
    codeCoverage_lineCoveragePercentage,
    codeCoverage_linesCovered,
    codeCoverage_linesMissed,
    codeCoverage_reportARN,

    -- * CodeCoverageReportSummary
    CodeCoverageReportSummary (..),
    newCodeCoverageReportSummary,
    codeCoverageReportSummary_branchCoveragePercentage,
    codeCoverageReportSummary_branchesCovered,
    codeCoverageReportSummary_branchesMissed,
    codeCoverageReportSummary_lineCoveragePercentage,
    codeCoverageReportSummary_linesCovered,
    codeCoverageReportSummary_linesMissed,

    -- * DebugSession
    DebugSession (..),
    newDebugSession,
    debugSession_sessionEnabled,
    debugSession_sessionTarget,

    -- * EnvironmentImage
    EnvironmentImage (..),
    newEnvironmentImage,
    environmentImage_description,
    environmentImage_name,
    environmentImage_versions,

    -- * EnvironmentLanguage
    EnvironmentLanguage (..),
    newEnvironmentLanguage,
    environmentLanguage_images,
    environmentLanguage_language,

    -- * EnvironmentPlatform
    EnvironmentPlatform (..),
    newEnvironmentPlatform,
    environmentPlatform_languages,
    environmentPlatform_platform,

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
    logsLocation_cloudWatchLogsArn,
    logsLocation_deepLink,
    logsLocation_groupName,
    logsLocation_s3DeepLink,
    logsLocation_s3Logs,
    logsLocation_s3LogsArn,
    logsLocation_streamName,

    -- * NetworkInterface
    NetworkInterface (..),
    newNetworkInterface,
    networkInterface_networkInterfaceId,
    networkInterface_subnetId,

    -- * PhaseContext
    PhaseContext (..),
    newPhaseContext,
    phaseContext_message,
    phaseContext_statusCode,

    -- * Project
    Project (..),
    newProject,
    project_arn,
    project_artifacts,
    project_badge,
    project_buildBatchConfig,
    project_cache,
    project_concurrentBuildLimit,
    project_created,
    project_description,
    project_encryptionKey,
    project_environment,
    project_fileSystemLocations,
    project_lastModified,
    project_logsConfig,
    project_name,
    project_projectVisibility,
    project_publicProjectAlias,
    project_queuedTimeoutInMinutes,
    project_resourceAccessRole,
    project_secondaryArtifacts,
    project_secondarySourceVersions,
    project_secondarySources,
    project_serviceRole,
    project_source,
    project_sourceVersion,
    project_tags,
    project_timeoutInMinutes,
    project_vpcConfig,
    project_webhook,

    -- * ProjectArtifacts
    ProjectArtifacts (..),
    newProjectArtifacts,
    projectArtifacts_artifactIdentifier,
    projectArtifacts_bucketOwnerAccess,
    projectArtifacts_encryptionDisabled,
    projectArtifacts_location,
    projectArtifacts_name,
    projectArtifacts_namespaceType,
    projectArtifacts_overrideArtifactName,
    projectArtifacts_packaging,
    projectArtifacts_path,
    projectArtifacts_type,

    -- * ProjectBadge
    ProjectBadge (..),
    newProjectBadge,
    projectBadge_badgeEnabled,
    projectBadge_badgeRequestUrl,

    -- * ProjectBuildBatchConfig
    ProjectBuildBatchConfig (..),
    newProjectBuildBatchConfig,
    projectBuildBatchConfig_batchReportMode,
    projectBuildBatchConfig_combineArtifacts,
    projectBuildBatchConfig_restrictions,
    projectBuildBatchConfig_serviceRole,
    projectBuildBatchConfig_timeoutInMins,

    -- * ProjectCache
    ProjectCache (..),
    newProjectCache,
    projectCache_location,
    projectCache_modes,
    projectCache_type,

    -- * ProjectEnvironment
    ProjectEnvironment (..),
    newProjectEnvironment,
    projectEnvironment_certificate,
    projectEnvironment_environmentVariables,
    projectEnvironment_imagePullCredentialsType,
    projectEnvironment_privilegedMode,
    projectEnvironment_registryCredential,
    projectEnvironment_type,
    projectEnvironment_image,
    projectEnvironment_computeType,

    -- * ProjectFileSystemLocation
    ProjectFileSystemLocation (..),
    newProjectFileSystemLocation,
    projectFileSystemLocation_identifier,
    projectFileSystemLocation_location,
    projectFileSystemLocation_mountOptions,
    projectFileSystemLocation_mountPoint,
    projectFileSystemLocation_type,

    -- * ProjectSource
    ProjectSource (..),
    newProjectSource,
    projectSource_auth,
    projectSource_buildStatusConfig,
    projectSource_buildspec,
    projectSource_gitCloneDepth,
    projectSource_gitSubmodulesConfig,
    projectSource_insecureSsl,
    projectSource_location,
    projectSource_reportBuildStatus,
    projectSource_sourceIdentifier,
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
    report_arn,
    report_codeCoverageSummary,
    report_created,
    report_executionId,
    report_expired,
    report_exportConfig,
    report_name,
    report_reportGroupArn,
    report_status,
    report_testSummary,
    report_truncated,
    report_type,

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
    reportGroup_arn,
    reportGroup_created,
    reportGroup_exportConfig,
    reportGroup_lastModified,
    reportGroup_name,
    reportGroup_status,
    reportGroup_tags,
    reportGroup_type,

    -- * ReportGroupTrendStats
    ReportGroupTrendStats (..),
    newReportGroupTrendStats,
    reportGroupTrendStats_average,
    reportGroupTrendStats_max,
    reportGroupTrendStats_min,

    -- * ReportWithRawData
    ReportWithRawData (..),
    newReportWithRawData,
    reportWithRawData_data,
    reportWithRawData_reportArn,

    -- * ResolvedArtifact
    ResolvedArtifact (..),
    newResolvedArtifact,
    resolvedArtifact_identifier,
    resolvedArtifact_location,
    resolvedArtifact_type,

    -- * S3LogsConfig
    S3LogsConfig (..),
    newS3LogsConfig,
    s3LogsConfig_bucketOwnerAccess,
    s3LogsConfig_encryptionDisabled,
    s3LogsConfig_location,
    s3LogsConfig_status,

    -- * S3ReportExportConfig
    S3ReportExportConfig (..),
    newS3ReportExportConfig,
    s3ReportExportConfig_bucket,
    s3ReportExportConfig_bucketOwner,
    s3ReportExportConfig_encryptionDisabled,
    s3ReportExportConfig_encryptionKey,
    s3ReportExportConfig_packaging,
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
    testCase_durationInNanoSeconds,
    testCase_expired,
    testCase_message,
    testCase_name,
    testCase_prefix,
    testCase_reportArn,
    testCase_status,
    testCase_testRawDataPath,

    -- * TestCaseFilter
    TestCaseFilter (..),
    newTestCaseFilter,
    testCaseFilter_keyword,
    testCaseFilter_status,

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
    webhook_branchFilter,
    webhook_buildType,
    webhook_filterGroups,
    webhook_lastModifiedSecret,
    webhook_payloadUrl,
    webhook_secret,
    webhook_url,

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
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | An Amazon Web Services service limit was exceeded for the calling Amazon
-- Web Services account.
_AccountLimitExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccountLimitExceededException =
  Core._MatchServiceError
    defaultService
    "AccountLimitExceededException"

-- | The input value that was provided is not valid.
_InvalidInputException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidInputException =
  Core._MatchServiceError
    defaultService
    "InvalidInputException"

-- | There was a problem with the underlying OAuth provider.
_OAuthProviderException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_OAuthProviderException =
  Core._MatchServiceError
    defaultService
    "OAuthProviderException"

-- | The specified Amazon Web Services resource cannot be created, because an
-- Amazon Web Services resource with the same settings already exists.
_ResourceAlreadyExistsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"

-- | The specified Amazon Web Services resource cannot be found.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

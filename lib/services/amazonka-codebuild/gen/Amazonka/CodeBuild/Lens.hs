{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodeBuild.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Lens
  ( -- * Operations

    -- ** BatchDeleteBuilds
    batchDeleteBuilds_ids,
    batchDeleteBuildsResponse_buildsDeleted,
    batchDeleteBuildsResponse_buildsNotDeleted,
    batchDeleteBuildsResponse_httpStatus,

    -- ** BatchGetBuildBatches
    batchGetBuildBatches_ids,
    batchGetBuildBatchesResponse_buildBatches,
    batchGetBuildBatchesResponse_buildBatchesNotFound,
    batchGetBuildBatchesResponse_httpStatus,

    -- ** BatchGetBuilds
    batchGetBuilds_ids,
    batchGetBuildsResponse_builds,
    batchGetBuildsResponse_buildsNotFound,
    batchGetBuildsResponse_httpStatus,

    -- ** BatchGetProjects
    batchGetProjects_names,
    batchGetProjectsResponse_projects,
    batchGetProjectsResponse_projectsNotFound,
    batchGetProjectsResponse_httpStatus,

    -- ** BatchGetReportGroups
    batchGetReportGroups_reportGroupArns,
    batchGetReportGroupsResponse_reportGroups,
    batchGetReportGroupsResponse_reportGroupsNotFound,
    batchGetReportGroupsResponse_httpStatus,

    -- ** BatchGetReports
    batchGetReports_reportArns,
    batchGetReportsResponse_reports,
    batchGetReportsResponse_reportsNotFound,
    batchGetReportsResponse_httpStatus,

    -- ** CreateProject
    createProject_badgeEnabled,
    createProject_buildBatchConfig,
    createProject_cache,
    createProject_concurrentBuildLimit,
    createProject_description,
    createProject_encryptionKey,
    createProject_fileSystemLocations,
    createProject_logsConfig,
    createProject_queuedTimeoutInMinutes,
    createProject_secondaryArtifacts,
    createProject_secondarySourceVersions,
    createProject_secondarySources,
    createProject_sourceVersion,
    createProject_tags,
    createProject_timeoutInMinutes,
    createProject_vpcConfig,
    createProject_name,
    createProject_source,
    createProject_artifacts,
    createProject_environment,
    createProject_serviceRole,
    createProjectResponse_project,
    createProjectResponse_httpStatus,

    -- ** CreateReportGroup
    createReportGroup_tags,
    createReportGroup_name,
    createReportGroup_type,
    createReportGroup_exportConfig,
    createReportGroupResponse_reportGroup,
    createReportGroupResponse_httpStatus,

    -- ** CreateWebhook
    createWebhook_branchFilter,
    createWebhook_buildType,
    createWebhook_filterGroups,
    createWebhook_projectName,
    createWebhookResponse_webhook,
    createWebhookResponse_httpStatus,

    -- ** DeleteBuildBatch
    deleteBuildBatch_id,
    deleteBuildBatchResponse_buildsDeleted,
    deleteBuildBatchResponse_buildsNotDeleted,
    deleteBuildBatchResponse_statusCode,
    deleteBuildBatchResponse_httpStatus,

    -- ** DeleteProject
    deleteProject_name,
    deleteProjectResponse_httpStatus,

    -- ** DeleteReport
    deleteReport_arn,
    deleteReportResponse_httpStatus,

    -- ** DeleteReportGroup
    deleteReportGroup_deleteReports,
    deleteReportGroup_arn,
    deleteReportGroupResponse_httpStatus,

    -- ** DeleteResourcePolicy
    deleteResourcePolicy_resourceArn,
    deleteResourcePolicyResponse_httpStatus,

    -- ** DeleteSourceCredentials
    deleteSourceCredentials_arn,
    deleteSourceCredentialsResponse_arn,
    deleteSourceCredentialsResponse_httpStatus,

    -- ** DeleteWebhook
    deleteWebhook_projectName,
    deleteWebhookResponse_httpStatus,

    -- ** DescribeCodeCoverages
    describeCodeCoverages_maxLineCoveragePercentage,
    describeCodeCoverages_maxResults,
    describeCodeCoverages_minLineCoveragePercentage,
    describeCodeCoverages_nextToken,
    describeCodeCoverages_sortBy,
    describeCodeCoverages_sortOrder,
    describeCodeCoverages_reportArn,
    describeCodeCoveragesResponse_codeCoverages,
    describeCodeCoveragesResponse_nextToken,
    describeCodeCoveragesResponse_httpStatus,

    -- ** DescribeTestCases
    describeTestCases_filter,
    describeTestCases_maxResults,
    describeTestCases_nextToken,
    describeTestCases_reportArn,
    describeTestCasesResponse_nextToken,
    describeTestCasesResponse_testCases,
    describeTestCasesResponse_httpStatus,

    -- ** GetReportGroupTrend
    getReportGroupTrend_numOfReports,
    getReportGroupTrend_reportGroupArn,
    getReportGroupTrend_trendField,
    getReportGroupTrendResponse_rawData,
    getReportGroupTrendResponse_stats,
    getReportGroupTrendResponse_httpStatus,

    -- ** GetResourcePolicy
    getResourcePolicy_resourceArn,
    getResourcePolicyResponse_policy,
    getResourcePolicyResponse_httpStatus,

    -- ** ImportSourceCredentials
    importSourceCredentials_shouldOverwrite,
    importSourceCredentials_username,
    importSourceCredentials_token,
    importSourceCredentials_serverType,
    importSourceCredentials_authType,
    importSourceCredentialsResponse_arn,
    importSourceCredentialsResponse_httpStatus,

    -- ** InvalidateProjectCache
    invalidateProjectCache_projectName,
    invalidateProjectCacheResponse_httpStatus,

    -- ** ListBuildBatches
    listBuildBatches_filter,
    listBuildBatches_maxResults,
    listBuildBatches_nextToken,
    listBuildBatches_sortOrder,
    listBuildBatchesResponse_ids,
    listBuildBatchesResponse_nextToken,
    listBuildBatchesResponse_httpStatus,

    -- ** ListBuildBatchesForProject
    listBuildBatchesForProject_filter,
    listBuildBatchesForProject_maxResults,
    listBuildBatchesForProject_nextToken,
    listBuildBatchesForProject_projectName,
    listBuildBatchesForProject_sortOrder,
    listBuildBatchesForProjectResponse_ids,
    listBuildBatchesForProjectResponse_nextToken,
    listBuildBatchesForProjectResponse_httpStatus,

    -- ** ListBuilds
    listBuilds_nextToken,
    listBuilds_sortOrder,
    listBuildsResponse_ids,
    listBuildsResponse_nextToken,
    listBuildsResponse_httpStatus,

    -- ** ListBuildsForProject
    listBuildsForProject_nextToken,
    listBuildsForProject_sortOrder,
    listBuildsForProject_projectName,
    listBuildsForProjectResponse_ids,
    listBuildsForProjectResponse_nextToken,
    listBuildsForProjectResponse_httpStatus,

    -- ** ListCuratedEnvironmentImages
    listCuratedEnvironmentImagesResponse_platforms,
    listCuratedEnvironmentImagesResponse_httpStatus,

    -- ** ListProjects
    listProjects_nextToken,
    listProjects_sortBy,
    listProjects_sortOrder,
    listProjectsResponse_nextToken,
    listProjectsResponse_projects,
    listProjectsResponse_httpStatus,

    -- ** ListReportGroups
    listReportGroups_maxResults,
    listReportGroups_nextToken,
    listReportGroups_sortBy,
    listReportGroups_sortOrder,
    listReportGroupsResponse_nextToken,
    listReportGroupsResponse_reportGroups,
    listReportGroupsResponse_httpStatus,

    -- ** ListReports
    listReports_filter,
    listReports_maxResults,
    listReports_nextToken,
    listReports_sortOrder,
    listReportsResponse_nextToken,
    listReportsResponse_reports,
    listReportsResponse_httpStatus,

    -- ** ListReportsForReportGroup
    listReportsForReportGroup_filter,
    listReportsForReportGroup_maxResults,
    listReportsForReportGroup_nextToken,
    listReportsForReportGroup_sortOrder,
    listReportsForReportGroup_reportGroupArn,
    listReportsForReportGroupResponse_nextToken,
    listReportsForReportGroupResponse_reports,
    listReportsForReportGroupResponse_httpStatus,

    -- ** ListSharedProjects
    listSharedProjects_maxResults,
    listSharedProjects_nextToken,
    listSharedProjects_sortBy,
    listSharedProjects_sortOrder,
    listSharedProjectsResponse_nextToken,
    listSharedProjectsResponse_projects,
    listSharedProjectsResponse_httpStatus,

    -- ** ListSharedReportGroups
    listSharedReportGroups_maxResults,
    listSharedReportGroups_nextToken,
    listSharedReportGroups_sortBy,
    listSharedReportGroups_sortOrder,
    listSharedReportGroupsResponse_nextToken,
    listSharedReportGroupsResponse_reportGroups,
    listSharedReportGroupsResponse_httpStatus,

    -- ** ListSourceCredentials
    listSourceCredentialsResponse_sourceCredentialsInfos,
    listSourceCredentialsResponse_httpStatus,

    -- ** PutResourcePolicy
    putResourcePolicy_policy,
    putResourcePolicy_resourceArn,
    putResourcePolicyResponse_resourceArn,
    putResourcePolicyResponse_httpStatus,

    -- ** RetryBuild
    retryBuild_id,
    retryBuild_idempotencyToken,
    retryBuildResponse_build,
    retryBuildResponse_httpStatus,

    -- ** RetryBuildBatch
    retryBuildBatch_id,
    retryBuildBatch_idempotencyToken,
    retryBuildBatch_retryType,
    retryBuildBatchResponse_buildBatch,
    retryBuildBatchResponse_httpStatus,

    -- ** StartBuild
    startBuild_artifactsOverride,
    startBuild_buildStatusConfigOverride,
    startBuild_buildspecOverride,
    startBuild_cacheOverride,
    startBuild_certificateOverride,
    startBuild_computeTypeOverride,
    startBuild_debugSessionEnabled,
    startBuild_encryptionKeyOverride,
    startBuild_environmentTypeOverride,
    startBuild_environmentVariablesOverride,
    startBuild_gitCloneDepthOverride,
    startBuild_gitSubmodulesConfigOverride,
    startBuild_idempotencyToken,
    startBuild_imageOverride,
    startBuild_imagePullCredentialsTypeOverride,
    startBuild_insecureSslOverride,
    startBuild_logsConfigOverride,
    startBuild_privilegedModeOverride,
    startBuild_queuedTimeoutInMinutesOverride,
    startBuild_registryCredentialOverride,
    startBuild_reportBuildStatusOverride,
    startBuild_secondaryArtifactsOverride,
    startBuild_secondarySourcesOverride,
    startBuild_secondarySourcesVersionOverride,
    startBuild_serviceRoleOverride,
    startBuild_sourceAuthOverride,
    startBuild_sourceLocationOverride,
    startBuild_sourceTypeOverride,
    startBuild_sourceVersion,
    startBuild_timeoutInMinutesOverride,
    startBuild_projectName,
    startBuildResponse_build,
    startBuildResponse_httpStatus,

    -- ** StartBuildBatch
    startBuildBatch_artifactsOverride,
    startBuildBatch_buildBatchConfigOverride,
    startBuildBatch_buildTimeoutInMinutesOverride,
    startBuildBatch_buildspecOverride,
    startBuildBatch_cacheOverride,
    startBuildBatch_certificateOverride,
    startBuildBatch_computeTypeOverride,
    startBuildBatch_debugSessionEnabled,
    startBuildBatch_encryptionKeyOverride,
    startBuildBatch_environmentTypeOverride,
    startBuildBatch_environmentVariablesOverride,
    startBuildBatch_gitCloneDepthOverride,
    startBuildBatch_gitSubmodulesConfigOverride,
    startBuildBatch_idempotencyToken,
    startBuildBatch_imageOverride,
    startBuildBatch_imagePullCredentialsTypeOverride,
    startBuildBatch_insecureSslOverride,
    startBuildBatch_logsConfigOverride,
    startBuildBatch_privilegedModeOverride,
    startBuildBatch_queuedTimeoutInMinutesOverride,
    startBuildBatch_registryCredentialOverride,
    startBuildBatch_reportBuildBatchStatusOverride,
    startBuildBatch_secondaryArtifactsOverride,
    startBuildBatch_secondarySourcesOverride,
    startBuildBatch_secondarySourcesVersionOverride,
    startBuildBatch_serviceRoleOverride,
    startBuildBatch_sourceAuthOverride,
    startBuildBatch_sourceLocationOverride,
    startBuildBatch_sourceTypeOverride,
    startBuildBatch_sourceVersion,
    startBuildBatch_projectName,
    startBuildBatchResponse_buildBatch,
    startBuildBatchResponse_httpStatus,

    -- ** StopBuild
    stopBuild_id,
    stopBuildResponse_build,
    stopBuildResponse_httpStatus,

    -- ** StopBuildBatch
    stopBuildBatch_id,
    stopBuildBatchResponse_buildBatch,
    stopBuildBatchResponse_httpStatus,

    -- ** UpdateProject
    updateProject_artifacts,
    updateProject_badgeEnabled,
    updateProject_buildBatchConfig,
    updateProject_cache,
    updateProject_concurrentBuildLimit,
    updateProject_description,
    updateProject_encryptionKey,
    updateProject_environment,
    updateProject_fileSystemLocations,
    updateProject_logsConfig,
    updateProject_queuedTimeoutInMinutes,
    updateProject_secondaryArtifacts,
    updateProject_secondarySourceVersions,
    updateProject_secondarySources,
    updateProject_serviceRole,
    updateProject_source,
    updateProject_sourceVersion,
    updateProject_tags,
    updateProject_timeoutInMinutes,
    updateProject_vpcConfig,
    updateProject_name,
    updateProjectResponse_project,
    updateProjectResponse_httpStatus,

    -- ** UpdateProjectVisibility
    updateProjectVisibility_resourceAccessRole,
    updateProjectVisibility_projectArn,
    updateProjectVisibility_projectVisibility,
    updateProjectVisibilityResponse_projectArn,
    updateProjectVisibilityResponse_projectVisibility,
    updateProjectVisibilityResponse_publicProjectAlias,
    updateProjectVisibilityResponse_httpStatus,

    -- ** UpdateReportGroup
    updateReportGroup_exportConfig,
    updateReportGroup_tags,
    updateReportGroup_arn,
    updateReportGroupResponse_reportGroup,
    updateReportGroupResponse_httpStatus,

    -- ** UpdateWebhook
    updateWebhook_branchFilter,
    updateWebhook_buildType,
    updateWebhook_filterGroups,
    updateWebhook_rotateSecret,
    updateWebhook_projectName,
    updateWebhookResponse_webhook,
    updateWebhookResponse_httpStatus,

    -- * Types

    -- ** BatchRestrictions
    batchRestrictions_computeTypesAllowed,
    batchRestrictions_maximumBuildsAllowed,

    -- ** Build
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

    -- ** BuildArtifacts
    buildArtifacts_artifactIdentifier,
    buildArtifacts_bucketOwnerAccess,
    buildArtifacts_encryptionDisabled,
    buildArtifacts_location,
    buildArtifacts_md5sum,
    buildArtifacts_overrideArtifactName,
    buildArtifacts_sha256sum,

    -- ** BuildBatch
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

    -- ** BuildBatchFilter
    buildBatchFilter_status,

    -- ** BuildBatchPhase
    buildBatchPhase_contexts,
    buildBatchPhase_durationInSeconds,
    buildBatchPhase_endTime,
    buildBatchPhase_phaseStatus,
    buildBatchPhase_phaseType,
    buildBatchPhase_startTime,

    -- ** BuildGroup
    buildGroup_currentBuildSummary,
    buildGroup_dependsOn,
    buildGroup_identifier,
    buildGroup_ignoreFailure,
    buildGroup_priorBuildSummaryList,

    -- ** BuildNotDeleted
    buildNotDeleted_id,
    buildNotDeleted_statusCode,

    -- ** BuildPhase
    buildPhase_contexts,
    buildPhase_durationInSeconds,
    buildPhase_endTime,
    buildPhase_phaseStatus,
    buildPhase_phaseType,
    buildPhase_startTime,

    -- ** BuildStatusConfig
    buildStatusConfig_context,
    buildStatusConfig_targetUrl,

    -- ** BuildSummary
    buildSummary_arn,
    buildSummary_buildStatus,
    buildSummary_primaryArtifact,
    buildSummary_requestedOn,
    buildSummary_secondaryArtifacts,

    -- ** CloudWatchLogsConfig
    cloudWatchLogsConfig_groupName,
    cloudWatchLogsConfig_streamName,
    cloudWatchLogsConfig_status,

    -- ** CodeCoverage
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

    -- ** CodeCoverageReportSummary
    codeCoverageReportSummary_branchCoveragePercentage,
    codeCoverageReportSummary_branchesCovered,
    codeCoverageReportSummary_branchesMissed,
    codeCoverageReportSummary_lineCoveragePercentage,
    codeCoverageReportSummary_linesCovered,
    codeCoverageReportSummary_linesMissed,

    -- ** DebugSession
    debugSession_sessionEnabled,
    debugSession_sessionTarget,

    -- ** EnvironmentImage
    environmentImage_description,
    environmentImage_name,
    environmentImage_versions,

    -- ** EnvironmentLanguage
    environmentLanguage_images,
    environmentLanguage_language,

    -- ** EnvironmentPlatform
    environmentPlatform_languages,
    environmentPlatform_platform,

    -- ** EnvironmentVariable
    environmentVariable_type,
    environmentVariable_name,
    environmentVariable_value,

    -- ** ExportedEnvironmentVariable
    exportedEnvironmentVariable_name,
    exportedEnvironmentVariable_value,

    -- ** GitSubmodulesConfig
    gitSubmodulesConfig_fetchSubmodules,

    -- ** LogsConfig
    logsConfig_cloudWatchLogs,
    logsConfig_s3Logs,

    -- ** LogsLocation
    logsLocation_cloudWatchLogs,
    logsLocation_cloudWatchLogsArn,
    logsLocation_deepLink,
    logsLocation_groupName,
    logsLocation_s3DeepLink,
    logsLocation_s3Logs,
    logsLocation_s3LogsArn,
    logsLocation_streamName,

    -- ** NetworkInterface
    networkInterface_networkInterfaceId,
    networkInterface_subnetId,

    -- ** PhaseContext
    phaseContext_message,
    phaseContext_statusCode,

    -- ** Project
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

    -- ** ProjectArtifacts
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

    -- ** ProjectBadge
    projectBadge_badgeEnabled,
    projectBadge_badgeRequestUrl,

    -- ** ProjectBuildBatchConfig
    projectBuildBatchConfig_batchReportMode,
    projectBuildBatchConfig_combineArtifacts,
    projectBuildBatchConfig_restrictions,
    projectBuildBatchConfig_serviceRole,
    projectBuildBatchConfig_timeoutInMins,

    -- ** ProjectCache
    projectCache_location,
    projectCache_modes,
    projectCache_type,

    -- ** ProjectEnvironment
    projectEnvironment_certificate,
    projectEnvironment_environmentVariables,
    projectEnvironment_imagePullCredentialsType,
    projectEnvironment_privilegedMode,
    projectEnvironment_registryCredential,
    projectEnvironment_type,
    projectEnvironment_image,
    projectEnvironment_computeType,

    -- ** ProjectFileSystemLocation
    projectFileSystemLocation_identifier,
    projectFileSystemLocation_location,
    projectFileSystemLocation_mountOptions,
    projectFileSystemLocation_mountPoint,
    projectFileSystemLocation_type,

    -- ** ProjectSource
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

    -- ** ProjectSourceVersion
    projectSourceVersion_sourceIdentifier,
    projectSourceVersion_sourceVersion,

    -- ** RegistryCredential
    registryCredential_credential,
    registryCredential_credentialProvider,

    -- ** Report
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

    -- ** ReportExportConfig
    reportExportConfig_exportConfigType,
    reportExportConfig_s3Destination,

    -- ** ReportFilter
    reportFilter_status,

    -- ** ReportGroup
    reportGroup_arn,
    reportGroup_created,
    reportGroup_exportConfig,
    reportGroup_lastModified,
    reportGroup_name,
    reportGroup_status,
    reportGroup_tags,
    reportGroup_type,

    -- ** ReportGroupTrendStats
    reportGroupTrendStats_average,
    reportGroupTrendStats_max,
    reportGroupTrendStats_min,

    -- ** ReportWithRawData
    reportWithRawData_data,
    reportWithRawData_reportArn,

    -- ** ResolvedArtifact
    resolvedArtifact_identifier,
    resolvedArtifact_location,
    resolvedArtifact_type,

    -- ** S3LogsConfig
    s3LogsConfig_bucketOwnerAccess,
    s3LogsConfig_encryptionDisabled,
    s3LogsConfig_location,
    s3LogsConfig_status,

    -- ** S3ReportExportConfig
    s3ReportExportConfig_bucket,
    s3ReportExportConfig_bucketOwner,
    s3ReportExportConfig_encryptionDisabled,
    s3ReportExportConfig_encryptionKey,
    s3ReportExportConfig_packaging,
    s3ReportExportConfig_path,

    -- ** SourceAuth
    sourceAuth_resource,
    sourceAuth_type,

    -- ** SourceCredentialsInfo
    sourceCredentialsInfo_arn,
    sourceCredentialsInfo_authType,
    sourceCredentialsInfo_serverType,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TestCase
    testCase_durationInNanoSeconds,
    testCase_expired,
    testCase_message,
    testCase_name,
    testCase_prefix,
    testCase_reportArn,
    testCase_status,
    testCase_testRawDataPath,

    -- ** TestCaseFilter
    testCaseFilter_keyword,
    testCaseFilter_status,

    -- ** TestReportSummary
    testReportSummary_total,
    testReportSummary_statusCounts,
    testReportSummary_durationInNanoSeconds,

    -- ** VpcConfig
    vpcConfig_securityGroupIds,
    vpcConfig_subnets,
    vpcConfig_vpcId,

    -- ** Webhook
    webhook_branchFilter,
    webhook_buildType,
    webhook_filterGroups,
    webhook_lastModifiedSecret,
    webhook_payloadUrl,
    webhook_secret,
    webhook_url,

    -- ** WebhookFilter
    webhookFilter_excludeMatchedPattern,
    webhookFilter_type,
    webhookFilter_pattern,
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
import Amazonka.CodeBuild.Types.BatchRestrictions
import Amazonka.CodeBuild.Types.Build
import Amazonka.CodeBuild.Types.BuildArtifacts
import Amazonka.CodeBuild.Types.BuildBatch
import Amazonka.CodeBuild.Types.BuildBatchFilter
import Amazonka.CodeBuild.Types.BuildBatchPhase
import Amazonka.CodeBuild.Types.BuildGroup
import Amazonka.CodeBuild.Types.BuildNotDeleted
import Amazonka.CodeBuild.Types.BuildPhase
import Amazonka.CodeBuild.Types.BuildStatusConfig
import Amazonka.CodeBuild.Types.BuildSummary
import Amazonka.CodeBuild.Types.CloudWatchLogsConfig
import Amazonka.CodeBuild.Types.CodeCoverage
import Amazonka.CodeBuild.Types.CodeCoverageReportSummary
import Amazonka.CodeBuild.Types.DebugSession
import Amazonka.CodeBuild.Types.EnvironmentImage
import Amazonka.CodeBuild.Types.EnvironmentLanguage
import Amazonka.CodeBuild.Types.EnvironmentPlatform
import Amazonka.CodeBuild.Types.EnvironmentVariable
import Amazonka.CodeBuild.Types.ExportedEnvironmentVariable
import Amazonka.CodeBuild.Types.GitSubmodulesConfig
import Amazonka.CodeBuild.Types.LogsConfig
import Amazonka.CodeBuild.Types.LogsLocation
import Amazonka.CodeBuild.Types.NetworkInterface
import Amazonka.CodeBuild.Types.PhaseContext
import Amazonka.CodeBuild.Types.Project
import Amazonka.CodeBuild.Types.ProjectArtifacts
import Amazonka.CodeBuild.Types.ProjectBadge
import Amazonka.CodeBuild.Types.ProjectBuildBatchConfig
import Amazonka.CodeBuild.Types.ProjectCache
import Amazonka.CodeBuild.Types.ProjectEnvironment
import Amazonka.CodeBuild.Types.ProjectFileSystemLocation
import Amazonka.CodeBuild.Types.ProjectSource
import Amazonka.CodeBuild.Types.ProjectSourceVersion
import Amazonka.CodeBuild.Types.RegistryCredential
import Amazonka.CodeBuild.Types.Report
import Amazonka.CodeBuild.Types.ReportExportConfig
import Amazonka.CodeBuild.Types.ReportFilter
import Amazonka.CodeBuild.Types.ReportGroup
import Amazonka.CodeBuild.Types.ReportGroupTrendStats
import Amazonka.CodeBuild.Types.ReportWithRawData
import Amazonka.CodeBuild.Types.ResolvedArtifact
import Amazonka.CodeBuild.Types.S3LogsConfig
import Amazonka.CodeBuild.Types.S3ReportExportConfig
import Amazonka.CodeBuild.Types.SourceAuth
import Amazonka.CodeBuild.Types.SourceCredentialsInfo
import Amazonka.CodeBuild.Types.Tag
import Amazonka.CodeBuild.Types.TestCase
import Amazonka.CodeBuild.Types.TestCaseFilter
import Amazonka.CodeBuild.Types.TestReportSummary
import Amazonka.CodeBuild.Types.VpcConfig
import Amazonka.CodeBuild.Types.Webhook
import Amazonka.CodeBuild.Types.WebhookFilter
import Amazonka.CodeBuild.UpdateProject
import Amazonka.CodeBuild.UpdateProjectVisibility
import Amazonka.CodeBuild.UpdateReportGroup
import Amazonka.CodeBuild.UpdateWebhook

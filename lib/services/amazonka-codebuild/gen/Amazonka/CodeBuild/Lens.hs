{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodeBuild.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    batchGetBuildsResponse_buildsNotFound,
    batchGetBuildsResponse_builds,
    batchGetBuildsResponse_httpStatus,

    -- ** BatchGetProjects
    batchGetProjects_names,
    batchGetProjectsResponse_projects,
    batchGetProjectsResponse_projectsNotFound,
    batchGetProjectsResponse_httpStatus,

    -- ** BatchGetReportGroups
    batchGetReportGroups_reportGroupArns,
    batchGetReportGroupsResponse_reportGroupsNotFound,
    batchGetReportGroupsResponse_reportGroups,
    batchGetReportGroupsResponse_httpStatus,

    -- ** BatchGetReports
    batchGetReports_reportArns,
    batchGetReportsResponse_reportsNotFound,
    batchGetReportsResponse_reports,
    batchGetReportsResponse_httpStatus,

    -- ** CreateProject
    createProject_tags,
    createProject_secondarySources,
    createProject_badgeEnabled,
    createProject_fileSystemLocations,
    createProject_timeoutInMinutes,
    createProject_queuedTimeoutInMinutes,
    createProject_vpcConfig,
    createProject_secondaryArtifacts,
    createProject_sourceVersion,
    createProject_concurrentBuildLimit,
    createProject_description,
    createProject_cache,
    createProject_secondarySourceVersions,
    createProject_logsConfig,
    createProject_buildBatchConfig,
    createProject_encryptionKey,
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
    describeCodeCoverages_sortOrder,
    describeCodeCoverages_nextToken,
    describeCodeCoverages_minLineCoveragePercentage,
    describeCodeCoverages_sortBy,
    describeCodeCoverages_maxLineCoveragePercentage,
    describeCodeCoverages_maxResults,
    describeCodeCoverages_reportArn,
    describeCodeCoveragesResponse_nextToken,
    describeCodeCoveragesResponse_codeCoverages,
    describeCodeCoveragesResponse_httpStatus,

    -- ** DescribeTestCases
    describeTestCases_nextToken,
    describeTestCases_filter,
    describeTestCases_maxResults,
    describeTestCases_reportArn,
    describeTestCasesResponse_nextToken,
    describeTestCasesResponse_testCases,
    describeTestCasesResponse_httpStatus,

    -- ** GetReportGroupTrend
    getReportGroupTrend_numOfReports,
    getReportGroupTrend_reportGroupArn,
    getReportGroupTrend_trendField,
    getReportGroupTrendResponse_stats,
    getReportGroupTrendResponse_rawData,
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
    listBuildBatches_sortOrder,
    listBuildBatches_nextToken,
    listBuildBatches_filter,
    listBuildBatches_maxResults,
    listBuildBatchesResponse_nextToken,
    listBuildBatchesResponse_ids,
    listBuildBatchesResponse_httpStatus,

    -- ** ListBuildBatchesForProject
    listBuildBatchesForProject_sortOrder,
    listBuildBatchesForProject_nextToken,
    listBuildBatchesForProject_filter,
    listBuildBatchesForProject_maxResults,
    listBuildBatchesForProject_projectName,
    listBuildBatchesForProjectResponse_nextToken,
    listBuildBatchesForProjectResponse_ids,
    listBuildBatchesForProjectResponse_httpStatus,

    -- ** ListBuilds
    listBuilds_sortOrder,
    listBuilds_nextToken,
    listBuildsResponse_nextToken,
    listBuildsResponse_ids,
    listBuildsResponse_httpStatus,

    -- ** ListBuildsForProject
    listBuildsForProject_sortOrder,
    listBuildsForProject_nextToken,
    listBuildsForProject_projectName,
    listBuildsForProjectResponse_nextToken,
    listBuildsForProjectResponse_ids,
    listBuildsForProjectResponse_httpStatus,

    -- ** ListCuratedEnvironmentImages
    listCuratedEnvironmentImagesResponse_platforms,
    listCuratedEnvironmentImagesResponse_httpStatus,

    -- ** ListProjects
    listProjects_sortOrder,
    listProjects_nextToken,
    listProjects_sortBy,
    listProjectsResponse_projects,
    listProjectsResponse_nextToken,
    listProjectsResponse_httpStatus,

    -- ** ListReportGroups
    listReportGroups_sortOrder,
    listReportGroups_nextToken,
    listReportGroups_sortBy,
    listReportGroups_maxResults,
    listReportGroupsResponse_nextToken,
    listReportGroupsResponse_reportGroups,
    listReportGroupsResponse_httpStatus,

    -- ** ListReports
    listReports_sortOrder,
    listReports_nextToken,
    listReports_filter,
    listReports_maxResults,
    listReportsResponse_nextToken,
    listReportsResponse_reports,
    listReportsResponse_httpStatus,

    -- ** ListReportsForReportGroup
    listReportsForReportGroup_sortOrder,
    listReportsForReportGroup_nextToken,
    listReportsForReportGroup_filter,
    listReportsForReportGroup_maxResults,
    listReportsForReportGroup_reportGroupArn,
    listReportsForReportGroupResponse_nextToken,
    listReportsForReportGroupResponse_reports,
    listReportsForReportGroupResponse_httpStatus,

    -- ** ListSharedProjects
    listSharedProjects_sortOrder,
    listSharedProjects_nextToken,
    listSharedProjects_sortBy,
    listSharedProjects_maxResults,
    listSharedProjectsResponse_projects,
    listSharedProjectsResponse_nextToken,
    listSharedProjectsResponse_httpStatus,

    -- ** ListSharedReportGroups
    listSharedReportGroups_sortOrder,
    listSharedReportGroups_nextToken,
    listSharedReportGroups_sortBy,
    listSharedReportGroups_maxResults,
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
    retryBuild_idempotencyToken,
    retryBuild_id,
    retryBuildResponse_build,
    retryBuildResponse_httpStatus,

    -- ** RetryBuildBatch
    retryBuildBatch_idempotencyToken,
    retryBuildBatch_id,
    retryBuildBatch_retryType,
    retryBuildBatchResponse_buildBatch,
    retryBuildBatchResponse_httpStatus,

    -- ** StartBuild
    startBuild_timeoutInMinutesOverride,
    startBuild_sourceTypeOverride,
    startBuild_insecureSslOverride,
    startBuild_encryptionKeyOverride,
    startBuild_registryCredentialOverride,
    startBuild_secondarySourcesOverride,
    startBuild_sourceAuthOverride,
    startBuild_debugSessionEnabled,
    startBuild_sourceVersion,
    startBuild_serviceRoleOverride,
    startBuild_idempotencyToken,
    startBuild_reportBuildStatusOverride,
    startBuild_certificateOverride,
    startBuild_computeTypeOverride,
    startBuild_queuedTimeoutInMinutesOverride,
    startBuild_imagePullCredentialsTypeOverride,
    startBuild_cacheOverride,
    startBuild_privilegedModeOverride,
    startBuild_secondarySourcesVersionOverride,
    startBuild_environmentVariablesOverride,
    startBuild_gitSubmodulesConfigOverride,
    startBuild_artifactsOverride,
    startBuild_logsConfigOverride,
    startBuild_buildStatusConfigOverride,
    startBuild_gitCloneDepthOverride,
    startBuild_environmentTypeOverride,
    startBuild_secondaryArtifactsOverride,
    startBuild_sourceLocationOverride,
    startBuild_buildspecOverride,
    startBuild_imageOverride,
    startBuild_projectName,
    startBuildResponse_build,
    startBuildResponse_httpStatus,

    -- ** StartBuildBatch
    startBuildBatch_sourceTypeOverride,
    startBuildBatch_buildBatchConfigOverride,
    startBuildBatch_insecureSslOverride,
    startBuildBatch_encryptionKeyOverride,
    startBuildBatch_registryCredentialOverride,
    startBuildBatch_secondarySourcesOverride,
    startBuildBatch_sourceAuthOverride,
    startBuildBatch_debugSessionEnabled,
    startBuildBatch_sourceVersion,
    startBuildBatch_serviceRoleOverride,
    startBuildBatch_idempotencyToken,
    startBuildBatch_certificateOverride,
    startBuildBatch_computeTypeOverride,
    startBuildBatch_queuedTimeoutInMinutesOverride,
    startBuildBatch_imagePullCredentialsTypeOverride,
    startBuildBatch_cacheOverride,
    startBuildBatch_privilegedModeOverride,
    startBuildBatch_secondarySourcesVersionOverride,
    startBuildBatch_environmentVariablesOverride,
    startBuildBatch_gitSubmodulesConfigOverride,
    startBuildBatch_artifactsOverride,
    startBuildBatch_logsConfigOverride,
    startBuildBatch_gitCloneDepthOverride,
    startBuildBatch_environmentTypeOverride,
    startBuildBatch_secondaryArtifactsOverride,
    startBuildBatch_sourceLocationOverride,
    startBuildBatch_reportBuildBatchStatusOverride,
    startBuildBatch_buildTimeoutInMinutesOverride,
    startBuildBatch_buildspecOverride,
    startBuildBatch_imageOverride,
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
    updateProject_tags,
    updateProject_environment,
    updateProject_secondarySources,
    updateProject_badgeEnabled,
    updateProject_fileSystemLocations,
    updateProject_timeoutInMinutes,
    updateProject_queuedTimeoutInMinutes,
    updateProject_vpcConfig,
    updateProject_secondaryArtifacts,
    updateProject_sourceVersion,
    updateProject_concurrentBuildLimit,
    updateProject_description,
    updateProject_cache,
    updateProject_serviceRole,
    updateProject_secondarySourceVersions,
    updateProject_source,
    updateProject_logsConfig,
    updateProject_buildBatchConfig,
    updateProject_encryptionKey,
    updateProject_artifacts,
    updateProject_name,
    updateProjectResponse_project,
    updateProjectResponse_httpStatus,

    -- ** UpdateProjectVisibility
    updateProjectVisibility_resourceAccessRole,
    updateProjectVisibility_projectArn,
    updateProjectVisibility_projectVisibility,
    updateProjectVisibilityResponse_projectVisibility,
    updateProjectVisibilityResponse_publicProjectAlias,
    updateProjectVisibilityResponse_projectArn,
    updateProjectVisibilityResponse_httpStatus,

    -- ** UpdateReportGroup
    updateReportGroup_tags,
    updateReportGroup_exportConfig,
    updateReportGroup_arn,
    updateReportGroupResponse_reportGroup,
    updateReportGroupResponse_httpStatus,

    -- ** UpdateWebhook
    updateWebhook_rotateSecret,
    updateWebhook_branchFilter,
    updateWebhook_buildType,
    updateWebhook_filterGroups,
    updateWebhook_projectName,
    updateWebhookResponse_webhook,
    updateWebhookResponse_httpStatus,

    -- * Types

    -- ** BatchRestrictions
    batchRestrictions_computeTypesAllowed,
    batchRestrictions_maximumBuildsAllowed,

    -- ** Build
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

    -- ** BuildArtifacts
    buildArtifacts_encryptionDisabled,
    buildArtifacts_md5sum,
    buildArtifacts_artifactIdentifier,
    buildArtifacts_location,
    buildArtifacts_overrideArtifactName,
    buildArtifacts_bucketOwnerAccess,
    buildArtifacts_sha256sum,

    -- ** BuildBatch
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

    -- ** BuildBatchFilter
    buildBatchFilter_status,

    -- ** BuildBatchPhase
    buildBatchPhase_contexts,
    buildBatchPhase_phaseStatus,
    buildBatchPhase_endTime,
    buildBatchPhase_phaseType,
    buildBatchPhase_durationInSeconds,
    buildBatchPhase_startTime,

    -- ** BuildGroup
    buildGroup_dependsOn,
    buildGroup_priorBuildSummaryList,
    buildGroup_ignoreFailure,
    buildGroup_identifier,
    buildGroup_currentBuildSummary,

    -- ** BuildNotDeleted
    buildNotDeleted_id,
    buildNotDeleted_statusCode,

    -- ** BuildPhase
    buildPhase_contexts,
    buildPhase_phaseStatus,
    buildPhase_endTime,
    buildPhase_phaseType,
    buildPhase_durationInSeconds,
    buildPhase_startTime,

    -- ** BuildStatusConfig
    buildStatusConfig_targetUrl,
    buildStatusConfig_context,

    -- ** BuildSummary
    buildSummary_secondaryArtifacts,
    buildSummary_buildStatus,
    buildSummary_arn,
    buildSummary_primaryArtifact,
    buildSummary_requestedOn,

    -- ** CloudWatchLogsConfig
    cloudWatchLogsConfig_groupName,
    cloudWatchLogsConfig_streamName,
    cloudWatchLogsConfig_status,

    -- ** CodeCoverage
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

    -- ** CodeCoverageReportSummary
    codeCoverageReportSummary_linesCovered,
    codeCoverageReportSummary_lineCoveragePercentage,
    codeCoverageReportSummary_linesMissed,
    codeCoverageReportSummary_branchesMissed,
    codeCoverageReportSummary_branchesCovered,
    codeCoverageReportSummary_branchCoveragePercentage,

    -- ** DebugSession
    debugSession_sessionEnabled,
    debugSession_sessionTarget,

    -- ** EnvironmentImage
    environmentImage_name,
    environmentImage_description,
    environmentImage_versions,

    -- ** EnvironmentLanguage
    environmentLanguage_language,
    environmentLanguage_images,

    -- ** EnvironmentPlatform
    environmentPlatform_platform,
    environmentPlatform_languages,

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
    logsLocation_s3Logs,
    logsLocation_groupName,
    logsLocation_s3LogsArn,
    logsLocation_deepLink,
    logsLocation_cloudWatchLogsArn,
    logsLocation_s3DeepLink,
    logsLocation_streamName,

    -- ** NetworkInterface
    networkInterface_subnetId,
    networkInterface_networkInterfaceId,

    -- ** PhaseContext
    phaseContext_message,
    phaseContext_statusCode,

    -- ** Project
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

    -- ** ProjectArtifacts
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

    -- ** ProjectBadge
    projectBadge_badgeEnabled,
    projectBadge_badgeRequestUrl,

    -- ** ProjectBuildBatchConfig
    projectBuildBatchConfig_timeoutInMins,
    projectBuildBatchConfig_restrictions,
    projectBuildBatchConfig_batchReportMode,
    projectBuildBatchConfig_combineArtifacts,
    projectBuildBatchConfig_serviceRole,

    -- ** ProjectCache
    projectCache_modes,
    projectCache_location,
    projectCache_type,

    -- ** ProjectEnvironment
    projectEnvironment_privilegedMode,
    projectEnvironment_imagePullCredentialsType,
    projectEnvironment_certificate,
    projectEnvironment_environmentVariables,
    projectEnvironment_registryCredential,
    projectEnvironment_type,
    projectEnvironment_image,
    projectEnvironment_computeType,

    -- ** ProjectFileSystemLocation
    projectFileSystemLocation_type,
    projectFileSystemLocation_mountPoint,
    projectFileSystemLocation_mountOptions,
    projectFileSystemLocation_location,
    projectFileSystemLocation_identifier,

    -- ** ProjectSource
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

    -- ** ProjectSourceVersion
    projectSourceVersion_sourceIdentifier,
    projectSourceVersion_sourceVersion,

    -- ** RegistryCredential
    registryCredential_credential,
    registryCredential_credentialProvider,

    -- ** Report
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

    -- ** ReportExportConfig
    reportExportConfig_exportConfigType,
    reportExportConfig_s3Destination,

    -- ** ReportFilter
    reportFilter_status,

    -- ** ReportGroup
    reportGroup_tags,
    reportGroup_name,
    reportGroup_type,
    reportGroup_created,
    reportGroup_arn,
    reportGroup_status,
    reportGroup_exportConfig,
    reportGroup_lastModified,

    -- ** ReportGroupTrendStats
    reportGroupTrendStats_max,
    reportGroupTrendStats_average,
    reportGroupTrendStats_min,

    -- ** ReportWithRawData
    reportWithRawData_reportArn,
    reportWithRawData_data,

    -- ** ResolvedArtifact
    resolvedArtifact_type,
    resolvedArtifact_location,
    resolvedArtifact_identifier,

    -- ** S3LogsConfig
    s3LogsConfig_encryptionDisabled,
    s3LogsConfig_location,
    s3LogsConfig_bucketOwnerAccess,
    s3LogsConfig_status,

    -- ** S3ReportExportConfig
    s3ReportExportConfig_encryptionDisabled,
    s3ReportExportConfig_bucket,
    s3ReportExportConfig_bucketOwner,
    s3ReportExportConfig_path,
    s3ReportExportConfig_packaging,
    s3ReportExportConfig_encryptionKey,

    -- ** SourceAuth
    sourceAuth_resource,
    sourceAuth_type,

    -- ** SourceCredentialsInfo
    sourceCredentialsInfo_arn,
    sourceCredentialsInfo_serverType,
    sourceCredentialsInfo_authType,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TestCase
    testCase_message,
    testCase_name,
    testCase_reportArn,
    testCase_expired,
    testCase_status,
    testCase_durationInNanoSeconds,
    testCase_prefix,
    testCase_testRawDataPath,

    -- ** TestCaseFilter
    testCaseFilter_status,
    testCaseFilter_keyword,

    -- ** TestReportSummary
    testReportSummary_total,
    testReportSummary_statusCounts,
    testReportSummary_durationInNanoSeconds,

    -- ** VpcConfig
    vpcConfig_securityGroupIds,
    vpcConfig_subnets,
    vpcConfig_vpcId,

    -- ** Webhook
    webhook_lastModifiedSecret,
    webhook_url,
    webhook_payloadUrl,
    webhook_secret,
    webhook_branchFilter,
    webhook_buildType,
    webhook_filterGroups,

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

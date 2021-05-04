{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Lens
  ( -- * Operations

    -- ** DeleteReport
    deleteReport_arn,
    deleteReportResponse_httpStatus,

    -- ** BatchGetReports
    batchGetReports_reportArns,
    batchGetReportsResponse_reports,
    batchGetReportsResponse_reportsNotFound,
    batchGetReportsResponse_httpStatus,

    -- ** ListBuilds
    listBuilds_sortOrder,
    listBuilds_nextToken,
    listBuildsResponse_nextToken,
    listBuildsResponse_ids,
    listBuildsResponse_httpStatus,

    -- ** GetResourcePolicy
    getResourcePolicy_resourceArn,
    getResourcePolicyResponse_policy,
    getResourcePolicyResponse_httpStatus,

    -- ** ListProjects
    listProjects_sortOrder,
    listProjects_nextToken,
    listProjects_sortBy,
    listProjectsResponse_nextToken,
    listProjectsResponse_projects,
    listProjectsResponse_httpStatus,

    -- ** DescribeTestCases
    describeTestCases_nextToken,
    describeTestCases_maxResults,
    describeTestCases_filter,
    describeTestCases_reportArn,
    describeTestCasesResponse_nextToken,
    describeTestCasesResponse_testCases,
    describeTestCasesResponse_httpStatus,

    -- ** ListBuildsForProject
    listBuildsForProject_sortOrder,
    listBuildsForProject_nextToken,
    listBuildsForProject_projectName,
    listBuildsForProjectResponse_nextToken,
    listBuildsForProjectResponse_ids,
    listBuildsForProjectResponse_httpStatus,

    -- ** CreateProject
    createProject_vpcConfig,
    createProject_secondaryArtifacts,
    createProject_sourceVersion,
    createProject_cache,
    createProject_secondarySourceVersions,
    createProject_encryptionKey,
    createProject_badgeEnabled,
    createProject_concurrentBuildLimit,
    createProject_logsConfig,
    createProject_queuedTimeoutInMinutes,
    createProject_secondarySources,
    createProject_tags,
    createProject_timeoutInMinutes,
    createProject_description,
    createProject_buildBatchConfig,
    createProject_fileSystemLocations,
    createProject_name,
    createProject_source,
    createProject_artifacts,
    createProject_environment,
    createProject_serviceRole,
    createProjectResponse_project,
    createProjectResponse_httpStatus,

    -- ** ListBuildBatches
    listBuildBatches_sortOrder,
    listBuildBatches_nextToken,
    listBuildBatches_maxResults,
    listBuildBatches_filter,
    listBuildBatchesResponse_nextToken,
    listBuildBatchesResponse_ids,
    listBuildBatchesResponse_httpStatus,

    -- ** DeleteBuildBatch
    deleteBuildBatch_id,
    deleteBuildBatchResponse_statusCode,
    deleteBuildBatchResponse_buildsDeleted,
    deleteBuildBatchResponse_buildsNotDeleted,
    deleteBuildBatchResponse_httpStatus,

    -- ** PutResourcePolicy
    putResourcePolicy_policy,
    putResourcePolicy_resourceArn,
    putResourcePolicyResponse_resourceArn,
    putResourcePolicyResponse_httpStatus,

    -- ** DeleteReportGroup
    deleteReportGroup_deleteReports,
    deleteReportGroup_arn,
    deleteReportGroupResponse_httpStatus,

    -- ** BatchDeleteBuilds
    batchDeleteBuilds_ids,
    batchDeleteBuildsResponse_buildsDeleted,
    batchDeleteBuildsResponse_buildsNotDeleted,
    batchDeleteBuildsResponse_httpStatus,

    -- ** BatchGetReportGroups
    batchGetReportGroups_reportGroupArns,
    batchGetReportGroupsResponse_reportGroupsNotFound,
    batchGetReportGroupsResponse_reportGroups,
    batchGetReportGroupsResponse_httpStatus,

    -- ** UpdateReportGroup
    updateReportGroup_exportConfig,
    updateReportGroup_tags,
    updateReportGroup_arn,
    updateReportGroupResponse_reportGroup,
    updateReportGroupResponse_httpStatus,

    -- ** ListBuildBatchesForProject
    listBuildBatchesForProject_sortOrder,
    listBuildBatchesForProject_nextToken,
    listBuildBatchesForProject_maxResults,
    listBuildBatchesForProject_projectName,
    listBuildBatchesForProject_filter,
    listBuildBatchesForProjectResponse_nextToken,
    listBuildBatchesForProjectResponse_ids,
    listBuildBatchesForProjectResponse_httpStatus,

    -- ** CreateReportGroup
    createReportGroup_tags,
    createReportGroup_name,
    createReportGroup_type,
    createReportGroup_exportConfig,
    createReportGroupResponse_reportGroup,
    createReportGroupResponse_httpStatus,

    -- ** DescribeCodeCoverages
    describeCodeCoverages_sortOrder,
    describeCodeCoverages_nextToken,
    describeCodeCoverages_maxLineCoveragePercentage,
    describeCodeCoverages_maxResults,
    describeCodeCoverages_sortBy,
    describeCodeCoverages_minLineCoveragePercentage,
    describeCodeCoverages_reportArn,
    describeCodeCoveragesResponse_nextToken,
    describeCodeCoveragesResponse_codeCoverages,
    describeCodeCoveragesResponse_httpStatus,

    -- ** StartBuildBatch
    startBuildBatch_buildspecOverride,
    startBuildBatch_sourceVersion,
    startBuildBatch_environmentVariablesOverride,
    startBuildBatch_idempotencyToken,
    startBuildBatch_buildBatchConfigOverride,
    startBuildBatch_sourceLocationOverride,
    startBuildBatch_logsConfigOverride,
    startBuildBatch_artifactsOverride,
    startBuildBatch_sourceAuthOverride,
    startBuildBatch_buildTimeoutInMinutesOverride,
    startBuildBatch_imageOverride,
    startBuildBatch_queuedTimeoutInMinutesOverride,
    startBuildBatch_insecureSslOverride,
    startBuildBatch_secondarySourcesOverride,
    startBuildBatch_serviceRoleOverride,
    startBuildBatch_registryCredentialOverride,
    startBuildBatch_secondarySourcesVersionOverride,
    startBuildBatch_encryptionKeyOverride,
    startBuildBatch_privilegedModeOverride,
    startBuildBatch_reportBuildBatchStatusOverride,
    startBuildBatch_gitSubmodulesConfigOverride,
    startBuildBatch_computeTypeOverride,
    startBuildBatch_certificateOverride,
    startBuildBatch_sourceTypeOverride,
    startBuildBatch_environmentTypeOverride,
    startBuildBatch_imagePullCredentialsTypeOverride,
    startBuildBatch_secondaryArtifactsOverride,
    startBuildBatch_gitCloneDepthOverride,
    startBuildBatch_cacheOverride,
    startBuildBatch_debugSessionEnabled,
    startBuildBatch_projectName,
    startBuildBatchResponse_buildBatch,
    startBuildBatchResponse_httpStatus,

    -- ** UpdateWebhook
    updateWebhook_rotateSecret,
    updateWebhook_branchFilter,
    updateWebhook_filterGroups,
    updateWebhook_buildType,
    updateWebhook_projectName,
    updateWebhookResponse_webhook,
    updateWebhookResponse_httpStatus,

    -- ** RetryBuildBatch
    retryBuildBatch_idempotencyToken,
    retryBuildBatch_retryType,
    retryBuildBatch_id,
    retryBuildBatchResponse_buildBatch,
    retryBuildBatchResponse_httpStatus,

    -- ** DeleteWebhook
    deleteWebhook_projectName,
    deleteWebhookResponse_httpStatus,

    -- ** StopBuildBatch
    stopBuildBatch_id,
    stopBuildBatchResponse_buildBatch,
    stopBuildBatchResponse_httpStatus,

    -- ** ListSourceCredentials
    listSourceCredentialsResponse_sourceCredentialsInfos,
    listSourceCredentialsResponse_httpStatus,

    -- ** CreateWebhook
    createWebhook_branchFilter,
    createWebhook_filterGroups,
    createWebhook_buildType,
    createWebhook_projectName,
    createWebhookResponse_webhook,
    createWebhookResponse_httpStatus,

    -- ** BatchGetBuilds
    batchGetBuilds_ids,
    batchGetBuildsResponse_buildsNotFound,
    batchGetBuildsResponse_builds,
    batchGetBuildsResponse_httpStatus,

    -- ** ListReports
    listReports_sortOrder,
    listReports_nextToken,
    listReports_maxResults,
    listReports_filter,
    listReportsResponse_nextToken,
    listReportsResponse_reports,
    listReportsResponse_httpStatus,

    -- ** BatchGetProjects
    batchGetProjects_names,
    batchGetProjectsResponse_projects,
    batchGetProjectsResponse_projectsNotFound,
    batchGetProjectsResponse_httpStatus,

    -- ** DeleteProject
    deleteProject_name,
    deleteProjectResponse_httpStatus,

    -- ** UpdateProject
    updateProject_vpcConfig,
    updateProject_secondaryArtifacts,
    updateProject_sourceVersion,
    updateProject_cache,
    updateProject_serviceRole,
    updateProject_secondarySourceVersions,
    updateProject_encryptionKey,
    updateProject_badgeEnabled,
    updateProject_concurrentBuildLimit,
    updateProject_artifacts,
    updateProject_environment,
    updateProject_source,
    updateProject_logsConfig,
    updateProject_queuedTimeoutInMinutes,
    updateProject_secondarySources,
    updateProject_tags,
    updateProject_timeoutInMinutes,
    updateProject_description,
    updateProject_buildBatchConfig,
    updateProject_fileSystemLocations,
    updateProject_name,
    updateProjectResponse_project,
    updateProjectResponse_httpStatus,

    -- ** DeleteSourceCredentials
    deleteSourceCredentials_arn,
    deleteSourceCredentialsResponse_arn,
    deleteSourceCredentialsResponse_httpStatus,

    -- ** ListSharedReportGroups
    listSharedReportGroups_sortOrder,
    listSharedReportGroups_nextToken,
    listSharedReportGroups_maxResults,
    listSharedReportGroups_sortBy,
    listSharedReportGroupsResponse_nextToken,
    listSharedReportGroupsResponse_reportGroups,
    listSharedReportGroupsResponse_httpStatus,

    -- ** StopBuild
    stopBuild_id,
    stopBuildResponse_build,
    stopBuildResponse_httpStatus,

    -- ** RetryBuild
    retryBuild_idempotencyToken,
    retryBuild_id,
    retryBuildResponse_build,
    retryBuildResponse_httpStatus,

    -- ** BatchGetBuildBatches
    batchGetBuildBatches_ids,
    batchGetBuildBatchesResponse_buildBatchesNotFound,
    batchGetBuildBatchesResponse_buildBatches,
    batchGetBuildBatchesResponse_httpStatus,

    -- ** GetReportGroupTrend
    getReportGroupTrend_numOfReports,
    getReportGroupTrend_reportGroupArn,
    getReportGroupTrend_trendField,
    getReportGroupTrendResponse_rawData,
    getReportGroupTrendResponse_stats,
    getReportGroupTrendResponse_httpStatus,

    -- ** StartBuild
    startBuild_buildspecOverride,
    startBuild_sourceVersion,
    startBuild_environmentVariablesOverride,
    startBuild_idempotencyToken,
    startBuild_sourceLocationOverride,
    startBuild_logsConfigOverride,
    startBuild_artifactsOverride,
    startBuild_sourceAuthOverride,
    startBuild_imageOverride,
    startBuild_queuedTimeoutInMinutesOverride,
    startBuild_insecureSslOverride,
    startBuild_secondarySourcesOverride,
    startBuild_reportBuildStatusOverride,
    startBuild_serviceRoleOverride,
    startBuild_registryCredentialOverride,
    startBuild_secondarySourcesVersionOverride,
    startBuild_buildStatusConfigOverride,
    startBuild_encryptionKeyOverride,
    startBuild_privilegedModeOverride,
    startBuild_gitSubmodulesConfigOverride,
    startBuild_computeTypeOverride,
    startBuild_certificateOverride,
    startBuild_sourceTypeOverride,
    startBuild_environmentTypeOverride,
    startBuild_imagePullCredentialsTypeOverride,
    startBuild_secondaryArtifactsOverride,
    startBuild_gitCloneDepthOverride,
    startBuild_cacheOverride,
    startBuild_timeoutInMinutesOverride,
    startBuild_debugSessionEnabled,
    startBuild_projectName,
    startBuildResponse_build,
    startBuildResponse_httpStatus,

    -- ** DeleteResourcePolicy
    deleteResourcePolicy_resourceArn,
    deleteResourcePolicyResponse_httpStatus,

    -- ** ListCuratedEnvironmentImages
    listCuratedEnvironmentImagesResponse_platforms,
    listCuratedEnvironmentImagesResponse_httpStatus,

    -- ** ListReportGroups
    listReportGroups_sortOrder,
    listReportGroups_nextToken,
    listReportGroups_maxResults,
    listReportGroups_sortBy,
    listReportGroupsResponse_nextToken,
    listReportGroupsResponse_reportGroups,
    listReportGroupsResponse_httpStatus,

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

    -- ** ListReportsForReportGroup
    listReportsForReportGroup_sortOrder,
    listReportsForReportGroup_nextToken,
    listReportsForReportGroup_maxResults,
    listReportsForReportGroup_filter,
    listReportsForReportGroup_reportGroupArn,
    listReportsForReportGroupResponse_nextToken,
    listReportsForReportGroupResponse_reports,
    listReportsForReportGroupResponse_httpStatus,

    -- ** ListSharedProjects
    listSharedProjects_sortOrder,
    listSharedProjects_nextToken,
    listSharedProjects_maxResults,
    listSharedProjects_sortBy,
    listSharedProjectsResponse_nextToken,
    listSharedProjectsResponse_projects,
    listSharedProjectsResponse_httpStatus,

    -- * Types

    -- ** BatchRestrictions
    batchRestrictions_computeTypesAllowed,
    batchRestrictions_maximumBuildsAllowed,

    -- ** Build
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

    -- ** BuildArtifacts
    buildArtifacts_sha256sum,
    buildArtifacts_overrideArtifactName,
    buildArtifacts_artifactIdentifier,
    buildArtifacts_md5sum,
    buildArtifacts_encryptionDisabled,
    buildArtifacts_location,

    -- ** BuildBatch
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

    -- ** BuildBatchFilter
    buildBatchFilter_status,

    -- ** BuildBatchPhase
    buildBatchPhase_phaseType,
    buildBatchPhase_contexts,
    buildBatchPhase_startTime,
    buildBatchPhase_endTime,
    buildBatchPhase_durationInSeconds,
    buildBatchPhase_phaseStatus,

    -- ** BuildGroup
    buildGroup_dependsOn,
    buildGroup_currentBuildSummary,
    buildGroup_identifier,
    buildGroup_ignoreFailure,
    buildGroup_priorBuildSummaryList,

    -- ** BuildNotDeleted
    buildNotDeleted_id,
    buildNotDeleted_statusCode,

    -- ** BuildPhase
    buildPhase_phaseType,
    buildPhase_contexts,
    buildPhase_startTime,
    buildPhase_endTime,
    buildPhase_durationInSeconds,
    buildPhase_phaseStatus,

    -- ** BuildStatusConfig
    buildStatusConfig_context,
    buildStatusConfig_targetUrl,

    -- ** BuildSummary
    buildSummary_secondaryArtifacts,
    buildSummary_requestedOn,
    buildSummary_arn,
    buildSummary_buildStatus,
    buildSummary_primaryArtifact,

    -- ** CloudWatchLogsConfig
    cloudWatchLogsConfig_groupName,
    cloudWatchLogsConfig_streamName,
    cloudWatchLogsConfig_status,

    -- ** CodeCoverage
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

    -- ** CodeCoverageReportSummary
    codeCoverageReportSummary_branchesMissed,
    codeCoverageReportSummary_linesCovered,
    codeCoverageReportSummary_branchesCovered,
    codeCoverageReportSummary_lineCoveragePercentage,
    codeCoverageReportSummary_linesMissed,
    codeCoverageReportSummary_branchCoveragePercentage,

    -- ** DebugSession
    debugSession_sessionTarget,
    debugSession_sessionEnabled,

    -- ** EnvironmentImage
    environmentImage_versions,
    environmentImage_name,
    environmentImage_description,

    -- ** EnvironmentLanguage
    environmentLanguage_images,
    environmentLanguage_language,

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
    logsConfig_s3Logs,
    logsConfig_cloudWatchLogs,

    -- ** LogsLocation
    logsLocation_s3Logs,
    logsLocation_cloudWatchLogs,
    logsLocation_deepLink,
    logsLocation_groupName,
    logsLocation_cloudWatchLogsArn,
    logsLocation_s3LogsArn,
    logsLocation_s3DeepLink,
    logsLocation_streamName,

    -- ** NetworkInterface
    networkInterface_subnetId,
    networkInterface_networkInterfaceId,

    -- ** PhaseContext
    phaseContext_message,
    phaseContext_statusCode,

    -- ** Project
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

    -- ** ProjectArtifacts
    projectArtifacts_namespaceType,
    projectArtifacts_overrideArtifactName,
    projectArtifacts_artifactIdentifier,
    projectArtifacts_name,
    projectArtifacts_packaging,
    projectArtifacts_encryptionDisabled,
    projectArtifacts_location,
    projectArtifacts_path,
    projectArtifacts_type,

    -- ** ProjectBadge
    projectBadge_badgeRequestUrl,
    projectBadge_badgeEnabled,

    -- ** ProjectBuildBatchConfig
    projectBuildBatchConfig_combineArtifacts,
    projectBuildBatchConfig_serviceRole,
    projectBuildBatchConfig_timeoutInMins,
    projectBuildBatchConfig_restrictions,

    -- ** ProjectCache
    projectCache_modes,
    projectCache_location,
    projectCache_type,

    -- ** ProjectEnvironment
    projectEnvironment_privilegedMode,
    projectEnvironment_imagePullCredentialsType,
    projectEnvironment_registryCredential,
    projectEnvironment_environmentVariables,
    projectEnvironment_certificate,
    projectEnvironment_type,
    projectEnvironment_image,
    projectEnvironment_computeType,

    -- ** ProjectFileSystemLocation
    projectFileSystemLocation_identifier,
    projectFileSystemLocation_mountOptions,
    projectFileSystemLocation_mountPoint,
    projectFileSystemLocation_type,
    projectFileSystemLocation_location,

    -- ** ProjectSource
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

    -- ** ProjectSourceVersion
    projectSourceVersion_sourceIdentifier,
    projectSourceVersion_sourceVersion,

    -- ** RegistryCredential
    registryCredential_credential,
    registryCredential_credentialProvider,

    -- ** Report
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

    -- ** ReportExportConfig
    reportExportConfig_s3Destination,
    reportExportConfig_exportConfigType,

    -- ** ReportFilter
    reportFilter_status,

    -- ** ReportGroup
    reportGroup_status,
    reportGroup_exportConfig,
    reportGroup_arn,
    reportGroup_name,
    reportGroup_tags,
    reportGroup_lastModified,
    reportGroup_created,
    reportGroup_type,

    -- ** ReportGroupTrendStats
    reportGroupTrendStats_min,
    reportGroupTrendStats_max,
    reportGroupTrendStats_average,

    -- ** ReportWithRawData
    reportWithRawData_reportArn,
    reportWithRawData_data,

    -- ** ResolvedArtifact
    resolvedArtifact_identifier,
    resolvedArtifact_type,
    resolvedArtifact_location,

    -- ** S3LogsConfig
    s3LogsConfig_encryptionDisabled,
    s3LogsConfig_location,
    s3LogsConfig_status,

    -- ** S3ReportExportConfig
    s3ReportExportConfig_bucketOwner,
    s3ReportExportConfig_encryptionKey,
    s3ReportExportConfig_packaging,
    s3ReportExportConfig_encryptionDisabled,
    s3ReportExportConfig_bucket,
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
    testCase_testRawDataPath,
    testCase_status,
    testCase_message,
    testCase_reportArn,
    testCase_prefix,
    testCase_name,
    testCase_expired,
    testCase_durationInNanoSeconds,

    -- ** TestCaseFilter
    testCaseFilter_status,
    testCaseFilter_keyword,

    -- ** TestReportSummary
    testReportSummary_total,
    testReportSummary_statusCounts,
    testReportSummary_durationInNanoSeconds,

    -- ** VpcConfig
    vpcConfig_securityGroupIds,
    vpcConfig_vpcId,
    vpcConfig_subnets,

    -- ** Webhook
    webhook_branchFilter,
    webhook_payloadUrl,
    webhook_filterGroups,
    webhook_secret,
    webhook_buildType,
    webhook_url,
    webhook_lastModifiedSecret,

    -- ** WebhookFilter
    webhookFilter_excludeMatchedPattern,
    webhookFilter_type,
    webhookFilter_pattern,
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
import Network.AWS.CodeBuild.Types.BatchRestrictions
import Network.AWS.CodeBuild.Types.Build
import Network.AWS.CodeBuild.Types.BuildArtifacts
import Network.AWS.CodeBuild.Types.BuildBatch
import Network.AWS.CodeBuild.Types.BuildBatchFilter
import Network.AWS.CodeBuild.Types.BuildBatchPhase
import Network.AWS.CodeBuild.Types.BuildGroup
import Network.AWS.CodeBuild.Types.BuildNotDeleted
import Network.AWS.CodeBuild.Types.BuildPhase
import Network.AWS.CodeBuild.Types.BuildStatusConfig
import Network.AWS.CodeBuild.Types.BuildSummary
import Network.AWS.CodeBuild.Types.CloudWatchLogsConfig
import Network.AWS.CodeBuild.Types.CodeCoverage
import Network.AWS.CodeBuild.Types.CodeCoverageReportSummary
import Network.AWS.CodeBuild.Types.DebugSession
import Network.AWS.CodeBuild.Types.EnvironmentImage
import Network.AWS.CodeBuild.Types.EnvironmentLanguage
import Network.AWS.CodeBuild.Types.EnvironmentPlatform
import Network.AWS.CodeBuild.Types.EnvironmentVariable
import Network.AWS.CodeBuild.Types.ExportedEnvironmentVariable
import Network.AWS.CodeBuild.Types.GitSubmodulesConfig
import Network.AWS.CodeBuild.Types.LogsConfig
import Network.AWS.CodeBuild.Types.LogsLocation
import Network.AWS.CodeBuild.Types.NetworkInterface
import Network.AWS.CodeBuild.Types.PhaseContext
import Network.AWS.CodeBuild.Types.Project
import Network.AWS.CodeBuild.Types.ProjectArtifacts
import Network.AWS.CodeBuild.Types.ProjectBadge
import Network.AWS.CodeBuild.Types.ProjectBuildBatchConfig
import Network.AWS.CodeBuild.Types.ProjectCache
import Network.AWS.CodeBuild.Types.ProjectEnvironment
import Network.AWS.CodeBuild.Types.ProjectFileSystemLocation
import Network.AWS.CodeBuild.Types.ProjectSource
import Network.AWS.CodeBuild.Types.ProjectSourceVersion
import Network.AWS.CodeBuild.Types.RegistryCredential
import Network.AWS.CodeBuild.Types.Report
import Network.AWS.CodeBuild.Types.ReportExportConfig
import Network.AWS.CodeBuild.Types.ReportFilter
import Network.AWS.CodeBuild.Types.ReportGroup
import Network.AWS.CodeBuild.Types.ReportGroupTrendStats
import Network.AWS.CodeBuild.Types.ReportWithRawData
import Network.AWS.CodeBuild.Types.ResolvedArtifact
import Network.AWS.CodeBuild.Types.S3LogsConfig
import Network.AWS.CodeBuild.Types.S3ReportExportConfig
import Network.AWS.CodeBuild.Types.SourceAuth
import Network.AWS.CodeBuild.Types.SourceCredentialsInfo
import Network.AWS.CodeBuild.Types.Tag
import Network.AWS.CodeBuild.Types.TestCase
import Network.AWS.CodeBuild.Types.TestCaseFilter
import Network.AWS.CodeBuild.Types.TestReportSummary
import Network.AWS.CodeBuild.Types.VpcConfig
import Network.AWS.CodeBuild.Types.Webhook
import Network.AWS.CodeBuild.Types.WebhookFilter
import Network.AWS.CodeBuild.UpdateProject
import Network.AWS.CodeBuild.UpdateReportGroup
import Network.AWS.CodeBuild.UpdateWebhook

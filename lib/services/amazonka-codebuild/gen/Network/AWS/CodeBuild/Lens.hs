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

    -- ** ListProjects
    listProjects_sortOrder,
    listProjects_nextToken,
    listProjects_sortBy,
    listProjectsResponse_nextToken,
    listProjectsResponse_projects,
    listProjectsResponse_httpStatus,

    -- ** DeleteProject
    deleteProject_name,
    deleteProjectResponse_httpStatus,

    -- ** UpdateProject
    updateProject_secondaryArtifacts,
    updateProject_artifacts,
    updateProject_environment,
    updateProject_concurrentBuildLimit,
    updateProject_badgeEnabled,
    updateProject_secondarySourceVersions,
    updateProject_queuedTimeoutInMinutes,
    updateProject_cache,
    updateProject_secondarySources,
    updateProject_sourceVersion,
    updateProject_vpcConfig,
    updateProject_source,
    updateProject_logsConfig,
    updateProject_fileSystemLocations,
    updateProject_buildBatchConfig,
    updateProject_encryptionKey,
    updateProject_description,
    updateProject_serviceRole,
    updateProject_tags,
    updateProject_timeoutInMinutes,
    updateProject_name,
    updateProjectResponse_project,
    updateProjectResponse_httpStatus,

    -- ** DeleteSourceCredentials
    deleteSourceCredentials_arn,
    deleteSourceCredentialsResponse_arn,
    deleteSourceCredentialsResponse_httpStatus,

    -- ** ListBuilds
    listBuilds_sortOrder,
    listBuilds_nextToken,
    listBuildsResponse_ids,
    listBuildsResponse_nextToken,
    listBuildsResponse_httpStatus,

    -- ** ListSourceCredentials
    listSourceCredentialsResponse_sourceCredentialsInfos,
    listSourceCredentialsResponse_httpStatus,

    -- ** ListReports
    listReports_sortOrder,
    listReports_nextToken,
    listReports_filter,
    listReports_maxResults,
    listReportsResponse_reports,
    listReportsResponse_nextToken,
    listReportsResponse_httpStatus,

    -- ** DeleteReport
    deleteReport_arn,
    deleteReportResponse_httpStatus,

    -- ** CreateWebhook
    createWebhook_branchFilter,
    createWebhook_filterGroups,
    createWebhook_buildType,
    createWebhook_projectName,
    createWebhookResponse_webhook,
    createWebhookResponse_httpStatus,

    -- ** StopBuildBatch
    stopBuildBatch_id,
    stopBuildBatchResponse_buildBatch,
    stopBuildBatchResponse_httpStatus,

    -- ** ListSharedProjects
    listSharedProjects_sortOrder,
    listSharedProjects_nextToken,
    listSharedProjects_maxResults,
    listSharedProjects_sortBy,
    listSharedProjectsResponse_nextToken,
    listSharedProjectsResponse_projects,
    listSharedProjectsResponse_httpStatus,

    -- ** CreateReportGroup
    createReportGroup_tags,
    createReportGroup_name,
    createReportGroup_type,
    createReportGroup_exportConfig,
    createReportGroupResponse_reportGroup,
    createReportGroupResponse_httpStatus,

    -- ** DescribeCodeCoverages
    describeCodeCoverages_minLineCoveragePercentage,
    describeCodeCoverages_sortOrder,
    describeCodeCoverages_maxLineCoveragePercentage,
    describeCodeCoverages_nextToken,
    describeCodeCoverages_maxResults,
    describeCodeCoverages_sortBy,
    describeCodeCoverages_reportArn,
    describeCodeCoveragesResponse_codeCoverages,
    describeCodeCoveragesResponse_nextToken,
    describeCodeCoveragesResponse_httpStatus,

    -- ** ImportSourceCredentials
    importSourceCredentials_username,
    importSourceCredentials_shouldOverwrite,
    importSourceCredentials_token,
    importSourceCredentials_serverType,
    importSourceCredentials_authType,
    importSourceCredentialsResponse_arn,
    importSourceCredentialsResponse_httpStatus,

    -- ** ListBuildBatchesForProject
    listBuildBatchesForProject_sortOrder,
    listBuildBatchesForProject_nextToken,
    listBuildBatchesForProject_projectName,
    listBuildBatchesForProject_filter,
    listBuildBatchesForProject_maxResults,
    listBuildBatchesForProjectResponse_ids,
    listBuildBatchesForProjectResponse_nextToken,
    listBuildBatchesForProjectResponse_httpStatus,

    -- ** BatchGetReportGroups
    batchGetReportGroups_reportGroupArns,
    batchGetReportGroupsResponse_reportGroups,
    batchGetReportGroupsResponse_reportGroupsNotFound,
    batchGetReportGroupsResponse_httpStatus,

    -- ** DeleteBuildBatch
    deleteBuildBatch_id,
    deleteBuildBatchResponse_buildsNotDeleted,
    deleteBuildBatchResponse_buildsDeleted,
    deleteBuildBatchResponse_statusCode,
    deleteBuildBatchResponse_httpStatus,

    -- ** StartBuild
    startBuild_encryptionKeyOverride,
    startBuild_sourceLocationOverride,
    startBuild_environmentVariablesOverride,
    startBuild_buildStatusConfigOverride,
    startBuild_idempotencyToken,
    startBuild_debugSessionEnabled,
    startBuild_registryCredentialOverride,
    startBuild_timeoutInMinutesOverride,
    startBuild_serviceRoleOverride,
    startBuild_cacheOverride,
    startBuild_queuedTimeoutInMinutesOverride,
    startBuild_secondarySourcesOverride,
    startBuild_gitCloneDepthOverride,
    startBuild_imagePullCredentialsTypeOverride,
    startBuild_logsConfigOverride,
    startBuild_sourceAuthOverride,
    startBuild_gitSubmodulesConfigOverride,
    startBuild_environmentTypeOverride,
    startBuild_certificateOverride,
    startBuild_computeTypeOverride,
    startBuild_privilegedModeOverride,
    startBuild_sourceVersion,
    startBuild_buildspecOverride,
    startBuild_secondarySourcesVersionOverride,
    startBuild_reportBuildStatusOverride,
    startBuild_insecureSslOverride,
    startBuild_imageOverride,
    startBuild_secondaryArtifactsOverride,
    startBuild_artifactsOverride,
    startBuild_sourceTypeOverride,
    startBuild_projectName,
    startBuildResponse_build,
    startBuildResponse_httpStatus,

    -- ** BatchGetBuildBatches
    batchGetBuildBatches_ids,
    batchGetBuildBatchesResponse_buildBatches,
    batchGetBuildBatchesResponse_buildBatchesNotFound,
    batchGetBuildBatchesResponse_httpStatus,

    -- ** RetryBuild
    retryBuild_idempotencyToken,
    retryBuild_id,
    retryBuildResponse_build,
    retryBuildResponse_httpStatus,

    -- ** ListBuildsForProject
    listBuildsForProject_sortOrder,
    listBuildsForProject_nextToken,
    listBuildsForProject_projectName,
    listBuildsForProjectResponse_ids,
    listBuildsForProjectResponse_nextToken,
    listBuildsForProjectResponse_httpStatus,

    -- ** DescribeTestCases
    describeTestCases_nextToken,
    describeTestCases_filter,
    describeTestCases_maxResults,
    describeTestCases_reportArn,
    describeTestCasesResponse_nextToken,
    describeTestCasesResponse_testCases,
    describeTestCasesResponse_httpStatus,

    -- ** GetResourcePolicy
    getResourcePolicy_resourceArn,
    getResourcePolicyResponse_policy,
    getResourcePolicyResponse_httpStatus,

    -- ** BatchGetProjects
    batchGetProjects_names,
    batchGetProjectsResponse_projectsNotFound,
    batchGetProjectsResponse_projects,
    batchGetProjectsResponse_httpStatus,

    -- ** BatchGetBuilds
    batchGetBuilds_ids,
    batchGetBuildsResponse_builds,
    batchGetBuildsResponse_buildsNotFound,
    batchGetBuildsResponse_httpStatus,

    -- ** BatchGetReports
    batchGetReports_reportArns,
    batchGetReportsResponse_reports,
    batchGetReportsResponse_reportsNotFound,
    batchGetReportsResponse_httpStatus,

    -- ** UpdateWebhook
    updateWebhook_branchFilter,
    updateWebhook_rotateSecret,
    updateWebhook_filterGroups,
    updateWebhook_buildType,
    updateWebhook_projectName,
    updateWebhookResponse_webhook,
    updateWebhookResponse_httpStatus,

    -- ** DeleteWebhook
    deleteWebhook_projectName,
    deleteWebhookResponse_httpStatus,

    -- ** StartBuildBatch
    startBuildBatch_encryptionKeyOverride,
    startBuildBatch_sourceLocationOverride,
    startBuildBatch_buildBatchConfigOverride,
    startBuildBatch_environmentVariablesOverride,
    startBuildBatch_idempotencyToken,
    startBuildBatch_debugSessionEnabled,
    startBuildBatch_registryCredentialOverride,
    startBuildBatch_serviceRoleOverride,
    startBuildBatch_cacheOverride,
    startBuildBatch_queuedTimeoutInMinutesOverride,
    startBuildBatch_secondarySourcesOverride,
    startBuildBatch_gitCloneDepthOverride,
    startBuildBatch_imagePullCredentialsTypeOverride,
    startBuildBatch_logsConfigOverride,
    startBuildBatch_sourceAuthOverride,
    startBuildBatch_gitSubmodulesConfigOverride,
    startBuildBatch_environmentTypeOverride,
    startBuildBatch_certificateOverride,
    startBuildBatch_computeTypeOverride,
    startBuildBatch_reportBuildBatchStatusOverride,
    startBuildBatch_privilegedModeOverride,
    startBuildBatch_sourceVersion,
    startBuildBatch_buildspecOverride,
    startBuildBatch_secondarySourcesVersionOverride,
    startBuildBatch_insecureSslOverride,
    startBuildBatch_imageOverride,
    startBuildBatch_secondaryArtifactsOverride,
    startBuildBatch_buildTimeoutInMinutesOverride,
    startBuildBatch_artifactsOverride,
    startBuildBatch_sourceTypeOverride,
    startBuildBatch_projectName,
    startBuildBatchResponse_buildBatch,
    startBuildBatchResponse_httpStatus,

    -- ** RetryBuildBatch
    retryBuildBatch_idempotencyToken,
    retryBuildBatch_id,
    retryBuildBatch_retryType,
    retryBuildBatchResponse_buildBatch,
    retryBuildBatchResponse_httpStatus,

    -- ** UpdateProjectVisibility
    updateProjectVisibility_resourceAccessRole,
    updateProjectVisibility_projectArn,
    updateProjectVisibility_projectVisibility,
    updateProjectVisibilityResponse_publicProjectAlias,
    updateProjectVisibilityResponse_projectArn,
    updateProjectVisibilityResponse_projectVisibility,
    updateProjectVisibilityResponse_httpStatus,

    -- ** ListReportsForReportGroup
    listReportsForReportGroup_sortOrder,
    listReportsForReportGroup_nextToken,
    listReportsForReportGroup_filter,
    listReportsForReportGroup_maxResults,
    listReportsForReportGroup_reportGroupArn,
    listReportsForReportGroupResponse_reports,
    listReportsForReportGroupResponse_nextToken,
    listReportsForReportGroupResponse_httpStatus,

    -- ** InvalidateProjectCache
    invalidateProjectCache_projectName,
    invalidateProjectCacheResponse_httpStatus,

    -- ** UpdateReportGroup
    updateReportGroup_exportConfig,
    updateReportGroup_tags,
    updateReportGroup_arn,
    updateReportGroupResponse_reportGroup,
    updateReportGroupResponse_httpStatus,

    -- ** DeleteReportGroup
    deleteReportGroup_deleteReports,
    deleteReportGroup_arn,
    deleteReportGroupResponse_httpStatus,

    -- ** BatchDeleteBuilds
    batchDeleteBuilds_ids,
    batchDeleteBuildsResponse_buildsNotDeleted,
    batchDeleteBuildsResponse_buildsDeleted,
    batchDeleteBuildsResponse_httpStatus,

    -- ** ListReportGroups
    listReportGroups_sortOrder,
    listReportGroups_nextToken,
    listReportGroups_maxResults,
    listReportGroups_sortBy,
    listReportGroupsResponse_nextToken,
    listReportGroupsResponse_reportGroups,
    listReportGroupsResponse_httpStatus,

    -- ** PutResourcePolicy
    putResourcePolicy_policy,
    putResourcePolicy_resourceArn,
    putResourcePolicyResponse_resourceArn,
    putResourcePolicyResponse_httpStatus,

    -- ** DeleteResourcePolicy
    deleteResourcePolicy_resourceArn,
    deleteResourcePolicyResponse_httpStatus,

    -- ** ListCuratedEnvironmentImages
    listCuratedEnvironmentImagesResponse_platforms,
    listCuratedEnvironmentImagesResponse_httpStatus,

    -- ** GetReportGroupTrend
    getReportGroupTrend_numOfReports,
    getReportGroupTrend_reportGroupArn,
    getReportGroupTrend_trendField,
    getReportGroupTrendResponse_rawData,
    getReportGroupTrendResponse_stats,
    getReportGroupTrendResponse_httpStatus,

    -- ** StopBuild
    stopBuild_id,
    stopBuildResponse_build,
    stopBuildResponse_httpStatus,

    -- ** ListBuildBatches
    listBuildBatches_sortOrder,
    listBuildBatches_nextToken,
    listBuildBatches_filter,
    listBuildBatches_maxResults,
    listBuildBatchesResponse_ids,
    listBuildBatchesResponse_nextToken,
    listBuildBatchesResponse_httpStatus,

    -- ** CreateProject
    createProject_secondaryArtifacts,
    createProject_concurrentBuildLimit,
    createProject_badgeEnabled,
    createProject_secondarySourceVersions,
    createProject_queuedTimeoutInMinutes,
    createProject_cache,
    createProject_secondarySources,
    createProject_sourceVersion,
    createProject_vpcConfig,
    createProject_logsConfig,
    createProject_fileSystemLocations,
    createProject_buildBatchConfig,
    createProject_encryptionKey,
    createProject_description,
    createProject_tags,
    createProject_timeoutInMinutes,
    createProject_name,
    createProject_source,
    createProject_artifacts,
    createProject_environment,
    createProject_serviceRole,
    createProjectResponse_project,
    createProjectResponse_httpStatus,

    -- ** ListSharedReportGroups
    listSharedReportGroups_sortOrder,
    listSharedReportGroups_nextToken,
    listSharedReportGroups_maxResults,
    listSharedReportGroups_sortBy,
    listSharedReportGroupsResponse_nextToken,
    listSharedReportGroupsResponse_reportGroups,
    listSharedReportGroupsResponse_httpStatus,

    -- * Types

    -- ** BatchRestrictions
    batchRestrictions_maximumBuildsAllowed,
    batchRestrictions_computeTypesAllowed,

    -- ** Build
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

    -- ** BuildArtifacts
    buildArtifacts_location,
    buildArtifacts_md5sum,
    buildArtifacts_encryptionDisabled,
    buildArtifacts_overrideArtifactName,
    buildArtifacts_artifactIdentifier,
    buildArtifacts_sha256sum,
    buildArtifacts_bucketOwnerAccess,

    -- ** BuildBatch
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

    -- ** BuildBatchFilter
    buildBatchFilter_status,

    -- ** BuildBatchPhase
    buildBatchPhase_contexts,
    buildBatchPhase_startTime,
    buildBatchPhase_phaseStatus,
    buildBatchPhase_phaseType,
    buildBatchPhase_endTime,
    buildBatchPhase_durationInSeconds,

    -- ** BuildGroup
    buildGroup_identifier,
    buildGroup_dependsOn,
    buildGroup_ignoreFailure,
    buildGroup_currentBuildSummary,
    buildGroup_priorBuildSummaryList,

    -- ** BuildNotDeleted
    buildNotDeleted_id,
    buildNotDeleted_statusCode,

    -- ** BuildPhase
    buildPhase_contexts,
    buildPhase_startTime,
    buildPhase_phaseStatus,
    buildPhase_phaseType,
    buildPhase_endTime,
    buildPhase_durationInSeconds,

    -- ** BuildStatusConfig
    buildStatusConfig_context,
    buildStatusConfig_targetUrl,

    -- ** BuildSummary
    buildSummary_secondaryArtifacts,
    buildSummary_primaryArtifact,
    buildSummary_arn,
    buildSummary_buildStatus,
    buildSummary_requestedOn,

    -- ** CloudWatchLogsConfig
    cloudWatchLogsConfig_groupName,
    cloudWatchLogsConfig_streamName,
    cloudWatchLogsConfig_status,

    -- ** CodeCoverage
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

    -- ** CodeCoverageReportSummary
    codeCoverageReportSummary_branchesMissed,
    codeCoverageReportSummary_linesMissed,
    codeCoverageReportSummary_branchesCovered,
    codeCoverageReportSummary_linesCovered,
    codeCoverageReportSummary_branchCoveragePercentage,
    codeCoverageReportSummary_lineCoveragePercentage,

    -- ** DebugSession
    debugSession_sessionEnabled,
    debugSession_sessionTarget,

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
    exportedEnvironmentVariable_value,
    exportedEnvironmentVariable_name,

    -- ** GitSubmodulesConfig
    gitSubmodulesConfig_fetchSubmodules,

    -- ** LogsConfig
    logsConfig_s3Logs,
    logsConfig_cloudWatchLogs,

    -- ** LogsLocation
    logsLocation_deepLink,
    logsLocation_s3Logs,
    logsLocation_cloudWatchLogs,
    logsLocation_s3DeepLink,
    logsLocation_s3LogsArn,
    logsLocation_cloudWatchLogsArn,
    logsLocation_groupName,
    logsLocation_streamName,

    -- ** NetworkInterface
    networkInterface_subnetId,
    networkInterface_networkInterfaceId,

    -- ** PhaseContext
    phaseContext_message,
    phaseContext_statusCode,

    -- ** Project
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

    -- ** ProjectArtifacts
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

    -- ** ProjectBadge
    projectBadge_badgeEnabled,
    projectBadge_badgeRequestUrl,

    -- ** ProjectBuildBatchConfig
    projectBuildBatchConfig_combineArtifacts,
    projectBuildBatchConfig_timeoutInMins,
    projectBuildBatchConfig_restrictions,
    projectBuildBatchConfig_batchReportMode,
    projectBuildBatchConfig_serviceRole,

    -- ** ProjectCache
    projectCache_location,
    projectCache_modes,
    projectCache_type,

    -- ** ProjectEnvironment
    projectEnvironment_imagePullCredentialsType,
    projectEnvironment_privilegedMode,
    projectEnvironment_registryCredential,
    projectEnvironment_certificate,
    projectEnvironment_environmentVariables,
    projectEnvironment_type,
    projectEnvironment_image,
    projectEnvironment_computeType,

    -- ** ProjectFileSystemLocation
    projectFileSystemLocation_location,
    projectFileSystemLocation_identifier,
    projectFileSystemLocation_mountOptions,
    projectFileSystemLocation_type,
    projectFileSystemLocation_mountPoint,

    -- ** ProjectSource
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

    -- ** ProjectSourceVersion
    projectSourceVersion_sourceIdentifier,
    projectSourceVersion_sourceVersion,

    -- ** RegistryCredential
    registryCredential_credential,
    registryCredential_credentialProvider,

    -- ** Report
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

    -- ** ReportExportConfig
    reportExportConfig_exportConfigType,
    reportExportConfig_s3Destination,

    -- ** ReportFilter
    reportFilter_status,

    -- ** ReportGroup
    reportGroup_status,
    reportGroup_arn,
    reportGroup_created,
    reportGroup_name,
    reportGroup_type,
    reportGroup_lastModified,
    reportGroup_exportConfig,
    reportGroup_tags,

    -- ** ReportGroupTrendStats
    reportGroupTrendStats_max,
    reportGroupTrendStats_average,
    reportGroupTrendStats_min,

    -- ** ReportWithRawData
    reportWithRawData_data,
    reportWithRawData_reportArn,

    -- ** ResolvedArtifact
    resolvedArtifact_location,
    resolvedArtifact_identifier,
    resolvedArtifact_type,

    -- ** S3LogsConfig
    s3LogsConfig_location,
    s3LogsConfig_encryptionDisabled,
    s3LogsConfig_bucketOwnerAccess,
    s3LogsConfig_status,

    -- ** S3ReportExportConfig
    s3ReportExportConfig_packaging,
    s3ReportExportConfig_path,
    s3ReportExportConfig_bucket,
    s3ReportExportConfig_bucketOwner,
    s3ReportExportConfig_encryptionDisabled,
    s3ReportExportConfig_encryptionKey,

    -- ** SourceAuth
    sourceAuth_resource,
    sourceAuth_type,

    -- ** SourceCredentialsInfo
    sourceCredentialsInfo_arn,
    sourceCredentialsInfo_serverType,
    sourceCredentialsInfo_authType,

    -- ** Tag
    tag_value,
    tag_key,

    -- ** TestCase
    testCase_durationInNanoSeconds,
    testCase_status,
    testCase_expired,
    testCase_prefix,
    testCase_name,
    testCase_testRawDataPath,
    testCase_message,
    testCase_reportArn,

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
    webhook_lastModifiedSecret,
    webhook_url,
    webhook_secret,
    webhook_filterGroups,
    webhook_payloadUrl,
    webhook_buildType,

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
import Network.AWS.CodeBuild.UpdateProjectVisibility
import Network.AWS.CodeBuild.UpdateReportGroup
import Network.AWS.CodeBuild.UpdateWebhook

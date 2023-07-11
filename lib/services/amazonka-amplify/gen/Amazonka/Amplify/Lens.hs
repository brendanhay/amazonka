{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Amplify.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Amplify.Lens
  ( -- * Operations

    -- ** CreateApp
    createApp_accessToken,
    createApp_autoBranchCreationConfig,
    createApp_autoBranchCreationPatterns,
    createApp_basicAuthCredentials,
    createApp_buildSpec,
    createApp_customHeaders,
    createApp_customRules,
    createApp_description,
    createApp_enableAutoBranchCreation,
    createApp_enableBasicAuth,
    createApp_enableBranchAutoBuild,
    createApp_enableBranchAutoDeletion,
    createApp_environmentVariables,
    createApp_iamServiceRoleArn,
    createApp_oauthToken,
    createApp_platform,
    createApp_repository,
    createApp_tags,
    createApp_name,
    createAppResponse_httpStatus,
    createAppResponse_app,

    -- ** CreateBackendEnvironment
    createBackendEnvironment_deploymentArtifacts,
    createBackendEnvironment_stackName,
    createBackendEnvironment_appId,
    createBackendEnvironment_environmentName,
    createBackendEnvironmentResponse_httpStatus,
    createBackendEnvironmentResponse_backendEnvironment,

    -- ** CreateBranch
    createBranch_backendEnvironmentArn,
    createBranch_basicAuthCredentials,
    createBranch_buildSpec,
    createBranch_description,
    createBranch_displayName,
    createBranch_enableAutoBuild,
    createBranch_enableBasicAuth,
    createBranch_enableNotification,
    createBranch_enablePerformanceMode,
    createBranch_enablePullRequestPreview,
    createBranch_environmentVariables,
    createBranch_framework,
    createBranch_pullRequestEnvironmentName,
    createBranch_stage,
    createBranch_tags,
    createBranch_ttl,
    createBranch_appId,
    createBranch_branchName,
    createBranchResponse_httpStatus,
    createBranchResponse_branch,

    -- ** CreateDeployment
    createDeployment_fileMap,
    createDeployment_appId,
    createDeployment_branchName,
    createDeploymentResponse_jobId,
    createDeploymentResponse_httpStatus,
    createDeploymentResponse_fileUploadUrls,
    createDeploymentResponse_zipUploadUrl,

    -- ** CreateDomainAssociation
    createDomainAssociation_autoSubDomainCreationPatterns,
    createDomainAssociation_autoSubDomainIAMRole,
    createDomainAssociation_enableAutoSubDomain,
    createDomainAssociation_appId,
    createDomainAssociation_domainName,
    createDomainAssociation_subDomainSettings,
    createDomainAssociationResponse_httpStatus,
    createDomainAssociationResponse_domainAssociation,

    -- ** CreateWebhook
    createWebhook_description,
    createWebhook_appId,
    createWebhook_branchName,
    createWebhookResponse_httpStatus,
    createWebhookResponse_webhook,

    -- ** DeleteApp
    deleteApp_appId,
    deleteAppResponse_httpStatus,
    deleteAppResponse_app,

    -- ** DeleteBackendEnvironment
    deleteBackendEnvironment_appId,
    deleteBackendEnvironment_environmentName,
    deleteBackendEnvironmentResponse_httpStatus,
    deleteBackendEnvironmentResponse_backendEnvironment,

    -- ** DeleteBranch
    deleteBranch_appId,
    deleteBranch_branchName,
    deleteBranchResponse_httpStatus,
    deleteBranchResponse_branch,

    -- ** DeleteDomainAssociation
    deleteDomainAssociation_appId,
    deleteDomainAssociation_domainName,
    deleteDomainAssociationResponse_httpStatus,
    deleteDomainAssociationResponse_domainAssociation,

    -- ** DeleteJob
    deleteJob_appId,
    deleteJob_branchName,
    deleteJob_jobId,
    deleteJobResponse_httpStatus,
    deleteJobResponse_jobSummary,

    -- ** DeleteWebhook
    deleteWebhook_webhookId,
    deleteWebhookResponse_httpStatus,
    deleteWebhookResponse_webhook,

    -- ** GenerateAccessLogs
    generateAccessLogs_endTime,
    generateAccessLogs_startTime,
    generateAccessLogs_domainName,
    generateAccessLogs_appId,
    generateAccessLogsResponse_logUrl,
    generateAccessLogsResponse_httpStatus,

    -- ** GetApp
    getApp_appId,
    getAppResponse_httpStatus,
    getAppResponse_app,

    -- ** GetArtifactUrl
    getArtifactUrl_artifactId,
    getArtifactUrlResponse_httpStatus,
    getArtifactUrlResponse_artifactId,
    getArtifactUrlResponse_artifactUrl,

    -- ** GetBackendEnvironment
    getBackendEnvironment_appId,
    getBackendEnvironment_environmentName,
    getBackendEnvironmentResponse_httpStatus,
    getBackendEnvironmentResponse_backendEnvironment,

    -- ** GetBranch
    getBranch_appId,
    getBranch_branchName,
    getBranchResponse_httpStatus,
    getBranchResponse_branch,

    -- ** GetDomainAssociation
    getDomainAssociation_appId,
    getDomainAssociation_domainName,
    getDomainAssociationResponse_httpStatus,
    getDomainAssociationResponse_domainAssociation,

    -- ** GetJob
    getJob_appId,
    getJob_branchName,
    getJob_jobId,
    getJobResponse_httpStatus,
    getJobResponse_job,

    -- ** GetWebhook
    getWebhook_webhookId,
    getWebhookResponse_httpStatus,
    getWebhookResponse_webhook,

    -- ** ListApps
    listApps_maxResults,
    listApps_nextToken,
    listAppsResponse_nextToken,
    listAppsResponse_httpStatus,
    listAppsResponse_apps,

    -- ** ListArtifacts
    listArtifacts_maxResults,
    listArtifacts_nextToken,
    listArtifacts_appId,
    listArtifacts_branchName,
    listArtifacts_jobId,
    listArtifactsResponse_nextToken,
    listArtifactsResponse_httpStatus,
    listArtifactsResponse_artifacts,

    -- ** ListBackendEnvironments
    listBackendEnvironments_environmentName,
    listBackendEnvironments_maxResults,
    listBackendEnvironments_nextToken,
    listBackendEnvironments_appId,
    listBackendEnvironmentsResponse_nextToken,
    listBackendEnvironmentsResponse_httpStatus,
    listBackendEnvironmentsResponse_backendEnvironments,

    -- ** ListBranches
    listBranches_maxResults,
    listBranches_nextToken,
    listBranches_appId,
    listBranchesResponse_nextToken,
    listBranchesResponse_httpStatus,
    listBranchesResponse_branches,

    -- ** ListDomainAssociations
    listDomainAssociations_maxResults,
    listDomainAssociations_nextToken,
    listDomainAssociations_appId,
    listDomainAssociationsResponse_nextToken,
    listDomainAssociationsResponse_httpStatus,
    listDomainAssociationsResponse_domainAssociations,

    -- ** ListJobs
    listJobs_maxResults,
    listJobs_nextToken,
    listJobs_appId,
    listJobs_branchName,
    listJobsResponse_nextToken,
    listJobsResponse_httpStatus,
    listJobsResponse_jobSummaries,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListWebhooks
    listWebhooks_maxResults,
    listWebhooks_nextToken,
    listWebhooks_appId,
    listWebhooksResponse_nextToken,
    listWebhooksResponse_httpStatus,
    listWebhooksResponse_webhooks,

    -- ** StartDeployment
    startDeployment_jobId,
    startDeployment_sourceUrl,
    startDeployment_appId,
    startDeployment_branchName,
    startDeploymentResponse_httpStatus,
    startDeploymentResponse_jobSummary,

    -- ** StartJob
    startJob_commitId,
    startJob_commitMessage,
    startJob_commitTime,
    startJob_jobId,
    startJob_jobReason,
    startJob_appId,
    startJob_branchName,
    startJob_jobType,
    startJobResponse_httpStatus,
    startJobResponse_jobSummary,

    -- ** StopJob
    stopJob_appId,
    stopJob_branchName,
    stopJob_jobId,
    stopJobResponse_httpStatus,
    stopJobResponse_jobSummary,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateApp
    updateApp_accessToken,
    updateApp_autoBranchCreationConfig,
    updateApp_autoBranchCreationPatterns,
    updateApp_basicAuthCredentials,
    updateApp_buildSpec,
    updateApp_customHeaders,
    updateApp_customRules,
    updateApp_description,
    updateApp_enableAutoBranchCreation,
    updateApp_enableBasicAuth,
    updateApp_enableBranchAutoBuild,
    updateApp_enableBranchAutoDeletion,
    updateApp_environmentVariables,
    updateApp_iamServiceRoleArn,
    updateApp_name,
    updateApp_oauthToken,
    updateApp_platform,
    updateApp_repository,
    updateApp_appId,
    updateAppResponse_httpStatus,
    updateAppResponse_app,

    -- ** UpdateBranch
    updateBranch_backendEnvironmentArn,
    updateBranch_basicAuthCredentials,
    updateBranch_buildSpec,
    updateBranch_description,
    updateBranch_displayName,
    updateBranch_enableAutoBuild,
    updateBranch_enableBasicAuth,
    updateBranch_enableNotification,
    updateBranch_enablePerformanceMode,
    updateBranch_enablePullRequestPreview,
    updateBranch_environmentVariables,
    updateBranch_framework,
    updateBranch_pullRequestEnvironmentName,
    updateBranch_stage,
    updateBranch_ttl,
    updateBranch_appId,
    updateBranch_branchName,
    updateBranchResponse_httpStatus,
    updateBranchResponse_branch,

    -- ** UpdateDomainAssociation
    updateDomainAssociation_autoSubDomainCreationPatterns,
    updateDomainAssociation_autoSubDomainIAMRole,
    updateDomainAssociation_enableAutoSubDomain,
    updateDomainAssociation_subDomainSettings,
    updateDomainAssociation_appId,
    updateDomainAssociation_domainName,
    updateDomainAssociationResponse_httpStatus,
    updateDomainAssociationResponse_domainAssociation,

    -- ** UpdateWebhook
    updateWebhook_branchName,
    updateWebhook_description,
    updateWebhook_webhookId,
    updateWebhookResponse_httpStatus,
    updateWebhookResponse_webhook,

    -- * Types

    -- ** App
    app_autoBranchCreationConfig,
    app_autoBranchCreationPatterns,
    app_basicAuthCredentials,
    app_buildSpec,
    app_customHeaders,
    app_customRules,
    app_enableAutoBranchCreation,
    app_enableBranchAutoDeletion,
    app_iamServiceRoleArn,
    app_productionBranch,
    app_repositoryCloneMethod,
    app_tags,
    app_appId,
    app_appArn,
    app_name,
    app_description,
    app_repository,
    app_platform,
    app_createTime,
    app_updateTime,
    app_environmentVariables,
    app_defaultDomain,
    app_enableBranchAutoBuild,
    app_enableBasicAuth,

    -- ** Artifact
    artifact_artifactFileName,
    artifact_artifactId,

    -- ** AutoBranchCreationConfig
    autoBranchCreationConfig_basicAuthCredentials,
    autoBranchCreationConfig_buildSpec,
    autoBranchCreationConfig_enableAutoBuild,
    autoBranchCreationConfig_enableBasicAuth,
    autoBranchCreationConfig_enablePerformanceMode,
    autoBranchCreationConfig_enablePullRequestPreview,
    autoBranchCreationConfig_environmentVariables,
    autoBranchCreationConfig_framework,
    autoBranchCreationConfig_pullRequestEnvironmentName,
    autoBranchCreationConfig_stage,

    -- ** BackendEnvironment
    backendEnvironment_deploymentArtifacts,
    backendEnvironment_stackName,
    backendEnvironment_backendEnvironmentArn,
    backendEnvironment_environmentName,
    backendEnvironment_createTime,
    backendEnvironment_updateTime,

    -- ** Branch
    branch_associatedResources,
    branch_backendEnvironmentArn,
    branch_basicAuthCredentials,
    branch_buildSpec,
    branch_destinationBranch,
    branch_enablePerformanceMode,
    branch_pullRequestEnvironmentName,
    branch_sourceBranch,
    branch_tags,
    branch_thumbnailUrl,
    branch_branchArn,
    branch_branchName,
    branch_description,
    branch_stage,
    branch_displayName,
    branch_enableNotification,
    branch_createTime,
    branch_updateTime,
    branch_environmentVariables,
    branch_enableAutoBuild,
    branch_customDomains,
    branch_framework,
    branch_activeJobId,
    branch_totalNumberOfJobs,
    branch_enableBasicAuth,
    branch_ttl,
    branch_enablePullRequestPreview,

    -- ** CustomRule
    customRule_condition,
    customRule_status,
    customRule_source,
    customRule_target,

    -- ** DomainAssociation
    domainAssociation_autoSubDomainCreationPatterns,
    domainAssociation_autoSubDomainIAMRole,
    domainAssociation_certificateVerificationDNSRecord,
    domainAssociation_domainAssociationArn,
    domainAssociation_domainName,
    domainAssociation_enableAutoSubDomain,
    domainAssociation_domainStatus,
    domainAssociation_statusReason,
    domainAssociation_subDomains,

    -- ** Job
    job_summary,
    job_steps,

    -- ** JobSummary
    jobSummary_endTime,
    jobSummary_jobArn,
    jobSummary_jobId,
    jobSummary_commitId,
    jobSummary_commitMessage,
    jobSummary_commitTime,
    jobSummary_startTime,
    jobSummary_status,
    jobSummary_jobType,

    -- ** ProductionBranch
    productionBranch_branchName,
    productionBranch_lastDeployTime,
    productionBranch_status,
    productionBranch_thumbnailUrl,

    -- ** Step
    step_artifactsUrl,
    step_context,
    step_logUrl,
    step_screenshots,
    step_statusReason,
    step_testArtifactsUrl,
    step_testConfigUrl,
    step_stepName,
    step_startTime,
    step_status,
    step_endTime,

    -- ** SubDomain
    subDomain_subDomainSetting,
    subDomain_verified,
    subDomain_dnsRecord,

    -- ** SubDomainSetting
    subDomainSetting_prefix,
    subDomainSetting_branchName,

    -- ** Webhook
    webhook_webhookArn,
    webhook_webhookId,
    webhook_webhookUrl,
    webhook_branchName,
    webhook_description,
    webhook_createTime,
    webhook_updateTime,
  )
where

import Amazonka.Amplify.CreateApp
import Amazonka.Amplify.CreateBackendEnvironment
import Amazonka.Amplify.CreateBranch
import Amazonka.Amplify.CreateDeployment
import Amazonka.Amplify.CreateDomainAssociation
import Amazonka.Amplify.CreateWebhook
import Amazonka.Amplify.DeleteApp
import Amazonka.Amplify.DeleteBackendEnvironment
import Amazonka.Amplify.DeleteBranch
import Amazonka.Amplify.DeleteDomainAssociation
import Amazonka.Amplify.DeleteJob
import Amazonka.Amplify.DeleteWebhook
import Amazonka.Amplify.GenerateAccessLogs
import Amazonka.Amplify.GetApp
import Amazonka.Amplify.GetArtifactUrl
import Amazonka.Amplify.GetBackendEnvironment
import Amazonka.Amplify.GetBranch
import Amazonka.Amplify.GetDomainAssociation
import Amazonka.Amplify.GetJob
import Amazonka.Amplify.GetWebhook
import Amazonka.Amplify.ListApps
import Amazonka.Amplify.ListArtifacts
import Amazonka.Amplify.ListBackendEnvironments
import Amazonka.Amplify.ListBranches
import Amazonka.Amplify.ListDomainAssociations
import Amazonka.Amplify.ListJobs
import Amazonka.Amplify.ListTagsForResource
import Amazonka.Amplify.ListWebhooks
import Amazonka.Amplify.StartDeployment
import Amazonka.Amplify.StartJob
import Amazonka.Amplify.StopJob
import Amazonka.Amplify.TagResource
import Amazonka.Amplify.Types.App
import Amazonka.Amplify.Types.Artifact
import Amazonka.Amplify.Types.AutoBranchCreationConfig
import Amazonka.Amplify.Types.BackendEnvironment
import Amazonka.Amplify.Types.Branch
import Amazonka.Amplify.Types.CustomRule
import Amazonka.Amplify.Types.DomainAssociation
import Amazonka.Amplify.Types.Job
import Amazonka.Amplify.Types.JobSummary
import Amazonka.Amplify.Types.ProductionBranch
import Amazonka.Amplify.Types.Step
import Amazonka.Amplify.Types.SubDomain
import Amazonka.Amplify.Types.SubDomainSetting
import Amazonka.Amplify.Types.Webhook
import Amazonka.Amplify.UntagResource
import Amazonka.Amplify.UpdateApp
import Amazonka.Amplify.UpdateBranch
import Amazonka.Amplify.UpdateDomainAssociation
import Amazonka.Amplify.UpdateWebhook

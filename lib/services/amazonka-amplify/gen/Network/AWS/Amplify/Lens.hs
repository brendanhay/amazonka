{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Amplify.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Amplify.Lens
  ( -- * Operations

    -- ** GetDomainAssociation
    getDomainAssociation_appId,
    getDomainAssociation_domainName,
    getDomainAssociationResponse_httpStatus,
    getDomainAssociationResponse_domainAssociation,

    -- ** ListArtifacts
    listArtifacts_nextToken,
    listArtifacts_maxResults,
    listArtifacts_appId,
    listArtifacts_branchName,
    listArtifacts_jobId,
    listArtifactsResponse_nextToken,
    listArtifactsResponse_httpStatus,
    listArtifactsResponse_artifacts,

    -- ** StopJob
    stopJob_appId,
    stopJob_branchName,
    stopJob_jobId,
    stopJobResponse_httpStatus,
    stopJobResponse_jobSummary,

    -- ** GetBackendEnvironment
    getBackendEnvironment_appId,
    getBackendEnvironment_environmentName,
    getBackendEnvironmentResponse_httpStatus,
    getBackendEnvironmentResponse_backendEnvironment,

    -- ** CreateWebhook
    createWebhook_description,
    createWebhook_appId,
    createWebhook_branchName,
    createWebhookResponse_httpStatus,
    createWebhookResponse_webhook,

    -- ** GetBranch
    getBranch_appId,
    getBranch_branchName,
    getBranchResponse_httpStatus,
    getBranchResponse_branch,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** CreateDomainAssociation
    createDomainAssociation_enableAutoSubDomain,
    createDomainAssociation_autoSubDomainCreationPatterns,
    createDomainAssociation_autoSubDomainIAMRole,
    createDomainAssociation_appId,
    createDomainAssociation_domainName,
    createDomainAssociation_subDomainSettings,
    createDomainAssociationResponse_httpStatus,
    createDomainAssociationResponse_domainAssociation,

    -- ** GetWebhook
    getWebhook_webhookId,
    getWebhookResponse_httpStatus,
    getWebhookResponse_webhook,

    -- ** DeleteBranch
    deleteBranch_appId,
    deleteBranch_branchName,
    deleteBranchResponse_httpStatus,
    deleteBranchResponse_branch,

    -- ** UpdateBranch
    updateBranch_framework,
    updateBranch_ttl,
    updateBranch_enableNotification,
    updateBranch_stage,
    updateBranch_backendEnvironmentArn,
    updateBranch_enablePullRequestPreview,
    updateBranch_basicAuthCredentials,
    updateBranch_buildSpec,
    updateBranch_enablePerformanceMode,
    updateBranch_displayName,
    updateBranch_environmentVariables,
    updateBranch_enableAutoBuild,
    updateBranch_enableBasicAuth,
    updateBranch_pullRequestEnvironmentName,
    updateBranch_description,
    updateBranch_appId,
    updateBranch_branchName,
    updateBranchResponse_httpStatus,
    updateBranchResponse_branch,

    -- ** CreateBackendEnvironment
    createBackendEnvironment_deploymentArtifacts,
    createBackendEnvironment_stackName,
    createBackendEnvironment_appId,
    createBackendEnvironment_environmentName,
    createBackendEnvironmentResponse_httpStatus,
    createBackendEnvironmentResponse_backendEnvironment,

    -- ** CreateDeployment
    createDeployment_fileMap,
    createDeployment_appId,
    createDeployment_branchName,
    createDeploymentResponse_jobId,
    createDeploymentResponse_httpStatus,
    createDeploymentResponse_fileUploadUrls,
    createDeploymentResponse_zipUploadUrl,

    -- ** CreateBranch
    createBranch_framework,
    createBranch_ttl,
    createBranch_enableNotification,
    createBranch_stage,
    createBranch_backendEnvironmentArn,
    createBranch_enablePullRequestPreview,
    createBranch_basicAuthCredentials,
    createBranch_buildSpec,
    createBranch_enablePerformanceMode,
    createBranch_displayName,
    createBranch_environmentVariables,
    createBranch_enableAutoBuild,
    createBranch_enableBasicAuth,
    createBranch_pullRequestEnvironmentName,
    createBranch_description,
    createBranch_tags,
    createBranch_appId,
    createBranch_branchName,
    createBranchResponse_httpStatus,
    createBranchResponse_branch,

    -- ** GenerateAccessLogs
    generateAccessLogs_startTime,
    generateAccessLogs_endTime,
    generateAccessLogs_domainName,
    generateAccessLogs_appId,
    generateAccessLogsResponse_logUrl,
    generateAccessLogsResponse_httpStatus,

    -- ** ListApps
    listApps_nextToken,
    listApps_maxResults,
    listAppsResponse_nextToken,
    listAppsResponse_httpStatus,
    listAppsResponse_apps,

    -- ** ListBranches
    listBranches_nextToken,
    listBranches_maxResults,
    listBranches_appId,
    listBranchesResponse_nextToken,
    listBranchesResponse_httpStatus,
    listBranchesResponse_branches,

    -- ** DeleteBackendEnvironment
    deleteBackendEnvironment_appId,
    deleteBackendEnvironment_environmentName,
    deleteBackendEnvironmentResponse_httpStatus,
    deleteBackendEnvironmentResponse_backendEnvironment,

    -- ** DeleteApp
    deleteApp_appId,
    deleteAppResponse_httpStatus,
    deleteAppResponse_app,

    -- ** UpdateApp
    updateApp_enableBranchAutoBuild,
    updateApp_oauthToken,
    updateApp_accessToken,
    updateApp_customHeaders,
    updateApp_platform,
    updateApp_basicAuthCredentials,
    updateApp_repository,
    updateApp_buildSpec,
    updateApp_enableBranchAutoDeletion,
    updateApp_customRules,
    updateApp_iamServiceRoleArn,
    updateApp_autoBranchCreationPatterns,
    updateApp_name,
    updateApp_autoBranchCreationConfig,
    updateApp_environmentVariables,
    updateApp_enableAutoBranchCreation,
    updateApp_enableBasicAuth,
    updateApp_description,
    updateApp_appId,
    updateAppResponse_httpStatus,
    updateAppResponse_app,

    -- ** GetArtifactUrl
    getArtifactUrl_artifactId,
    getArtifactUrlResponse_httpStatus,
    getArtifactUrlResponse_artifactId,
    getArtifactUrlResponse_artifactUrl,

    -- ** ListJobs
    listJobs_nextToken,
    listJobs_maxResults,
    listJobs_appId,
    listJobs_branchName,
    listJobsResponse_nextToken,
    listJobsResponse_httpStatus,
    listJobsResponse_jobSummaries,

    -- ** DeleteJob
    deleteJob_appId,
    deleteJob_branchName,
    deleteJob_jobId,
    deleteJobResponse_httpStatus,
    deleteJobResponse_jobSummary,

    -- ** GetJob
    getJob_appId,
    getJob_branchName,
    getJob_jobId,
    getJobResponse_httpStatus,
    getJobResponse_job,

    -- ** StartJob
    startJob_commitId,
    startJob_jobId,
    startJob_jobReason,
    startJob_commitTime,
    startJob_commitMessage,
    startJob_appId,
    startJob_branchName,
    startJob_jobType,
    startJobResponse_httpStatus,
    startJobResponse_jobSummary,

    -- ** GetApp
    getApp_appId,
    getAppResponse_httpStatus,
    getAppResponse_app,

    -- ** UpdateWebhook
    updateWebhook_branchName,
    updateWebhook_description,
    updateWebhook_webhookId,
    updateWebhookResponse_httpStatus,
    updateWebhookResponse_webhook,

    -- ** DeleteWebhook
    deleteWebhook_webhookId,
    deleteWebhookResponse_httpStatus,
    deleteWebhookResponse_webhook,

    -- ** ListWebhooks
    listWebhooks_nextToken,
    listWebhooks_maxResults,
    listWebhooks_appId,
    listWebhooksResponse_nextToken,
    listWebhooksResponse_httpStatus,
    listWebhooksResponse_webhooks,

    -- ** CreateApp
    createApp_enableBranchAutoBuild,
    createApp_oauthToken,
    createApp_accessToken,
    createApp_customHeaders,
    createApp_platform,
    createApp_basicAuthCredentials,
    createApp_repository,
    createApp_buildSpec,
    createApp_enableBranchAutoDeletion,
    createApp_customRules,
    createApp_iamServiceRoleArn,
    createApp_autoBranchCreationPatterns,
    createApp_autoBranchCreationConfig,
    createApp_environmentVariables,
    createApp_enableAutoBranchCreation,
    createApp_enableBasicAuth,
    createApp_description,
    createApp_tags,
    createApp_name,
    createAppResponse_httpStatus,
    createAppResponse_app,

    -- ** DeleteDomainAssociation
    deleteDomainAssociation_appId,
    deleteDomainAssociation_domainName,
    deleteDomainAssociationResponse_httpStatus,
    deleteDomainAssociationResponse_domainAssociation,

    -- ** UpdateDomainAssociation
    updateDomainAssociation_enableAutoSubDomain,
    updateDomainAssociation_autoSubDomainCreationPatterns,
    updateDomainAssociation_autoSubDomainIAMRole,
    updateDomainAssociation_appId,
    updateDomainAssociation_domainName,
    updateDomainAssociation_subDomainSettings,
    updateDomainAssociationResponse_httpStatus,
    updateDomainAssociationResponse_domainAssociation,

    -- ** ListDomainAssociations
    listDomainAssociations_nextToken,
    listDomainAssociations_maxResults,
    listDomainAssociations_appId,
    listDomainAssociationsResponse_nextToken,
    listDomainAssociationsResponse_httpStatus,
    listDomainAssociationsResponse_domainAssociations,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** ListBackendEnvironments
    listBackendEnvironments_nextToken,
    listBackendEnvironments_environmentName,
    listBackendEnvironments_maxResults,
    listBackendEnvironments_appId,
    listBackendEnvironmentsResponse_nextToken,
    listBackendEnvironmentsResponse_httpStatus,
    listBackendEnvironmentsResponse_backendEnvironments,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** StartDeployment
    startDeployment_jobId,
    startDeployment_sourceUrl,
    startDeployment_appId,
    startDeployment_branchName,
    startDeploymentResponse_httpStatus,
    startDeploymentResponse_jobSummary,

    -- * Types

    -- ** App
    app_customHeaders,
    app_basicAuthCredentials,
    app_buildSpec,
    app_enableBranchAutoDeletion,
    app_customRules,
    app_iamServiceRoleArn,
    app_autoBranchCreationPatterns,
    app_productionBranch,
    app_autoBranchCreationConfig,
    app_enableAutoBranchCreation,
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
    autoBranchCreationConfig_framework,
    autoBranchCreationConfig_stage,
    autoBranchCreationConfig_enablePullRequestPreview,
    autoBranchCreationConfig_basicAuthCredentials,
    autoBranchCreationConfig_buildSpec,
    autoBranchCreationConfig_enablePerformanceMode,
    autoBranchCreationConfig_environmentVariables,
    autoBranchCreationConfig_enableAutoBuild,
    autoBranchCreationConfig_enableBasicAuth,
    autoBranchCreationConfig_pullRequestEnvironmentName,

    -- ** BackendEnvironment
    backendEnvironment_deploymentArtifacts,
    backendEnvironment_stackName,
    backendEnvironment_backendEnvironmentArn,
    backendEnvironment_environmentName,
    backendEnvironment_createTime,
    backendEnvironment_updateTime,

    -- ** Branch
    branch_backendEnvironmentArn,
    branch_thumbnailUrl,
    branch_basicAuthCredentials,
    branch_buildSpec,
    branch_sourceBranch,
    branch_enablePerformanceMode,
    branch_destinationBranch,
    branch_pullRequestEnvironmentName,
    branch_associatedResources,
    branch_tags,
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
    customRule_status,
    customRule_condition,
    customRule_source,
    customRule_target,

    -- ** DomainAssociation
    domainAssociation_certificateVerificationDNSRecord,
    domainAssociation_autoSubDomainCreationPatterns,
    domainAssociation_autoSubDomainIAMRole,
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
    productionBranch_lastDeployTime,
    productionBranch_status,
    productionBranch_thumbnailUrl,
    productionBranch_branchName,

    -- ** Step
    step_logUrl,
    step_context,
    step_testArtifactsUrl,
    step_artifactsUrl,
    step_testConfigUrl,
    step_screenshots,
    step_statusReason,
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

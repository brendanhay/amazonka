{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Amplify.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Amplify.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _DependentServiceFailureException,
    _NotFoundException,
    _InternalFailureException,
    _UnauthorizedException,
    _ResourceNotFoundException,
    _BadRequestException,
    _LimitExceededException,

    -- * DomainStatus
    DomainStatus (..),

    -- * JobStatus
    JobStatus (..),

    -- * JobType
    JobType (..),

    -- * Platform
    Platform (..),

    -- * Stage
    Stage (..),

    -- * App
    App (..),
    newApp,
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

    -- * Artifact
    Artifact (..),
    newArtifact,
    artifact_artifactFileName,
    artifact_artifactId,

    -- * AutoBranchCreationConfig
    AutoBranchCreationConfig (..),
    newAutoBranchCreationConfig,
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

    -- * BackendEnvironment
    BackendEnvironment (..),
    newBackendEnvironment,
    backendEnvironment_deploymentArtifacts,
    backendEnvironment_stackName,
    backendEnvironment_backendEnvironmentArn,
    backendEnvironment_environmentName,
    backendEnvironment_createTime,
    backendEnvironment_updateTime,

    -- * Branch
    Branch (..),
    newBranch,
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

    -- * CustomRule
    CustomRule (..),
    newCustomRule,
    customRule_status,
    customRule_condition,
    customRule_source,
    customRule_target,

    -- * DomainAssociation
    DomainAssociation (..),
    newDomainAssociation,
    domainAssociation_certificateVerificationDNSRecord,
    domainAssociation_autoSubDomainCreationPatterns,
    domainAssociation_autoSubDomainIAMRole,
    domainAssociation_domainAssociationArn,
    domainAssociation_domainName,
    domainAssociation_enableAutoSubDomain,
    domainAssociation_domainStatus,
    domainAssociation_statusReason,
    domainAssociation_subDomains,

    -- * Job
    Job (..),
    newJob,
    job_summary,
    job_steps,

    -- * JobSummary
    JobSummary (..),
    newJobSummary,
    jobSummary_endTime,
    jobSummary_jobArn,
    jobSummary_jobId,
    jobSummary_commitId,
    jobSummary_commitMessage,
    jobSummary_commitTime,
    jobSummary_startTime,
    jobSummary_status,
    jobSummary_jobType,

    -- * ProductionBranch
    ProductionBranch (..),
    newProductionBranch,
    productionBranch_lastDeployTime,
    productionBranch_status,
    productionBranch_thumbnailUrl,
    productionBranch_branchName,

    -- * Step
    Step (..),
    newStep,
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

    -- * SubDomain
    SubDomain (..),
    newSubDomain,
    subDomain_subDomainSetting,
    subDomain_verified,
    subDomain_dnsRecord,

    -- * SubDomainSetting
    SubDomainSetting (..),
    newSubDomainSetting,
    subDomainSetting_prefix,
    subDomainSetting_branchName,

    -- * Webhook
    Webhook (..),
    newWebhook,
    webhook_webhookArn,
    webhook_webhookId,
    webhook_webhookUrl,
    webhook_branchName,
    webhook_description,
    webhook_createTime,
    webhook_updateTime,
  )
where

import Network.AWS.Amplify.Types.App
import Network.AWS.Amplify.Types.Artifact
import Network.AWS.Amplify.Types.AutoBranchCreationConfig
import Network.AWS.Amplify.Types.BackendEnvironment
import Network.AWS.Amplify.Types.Branch
import Network.AWS.Amplify.Types.CustomRule
import Network.AWS.Amplify.Types.DomainAssociation
import Network.AWS.Amplify.Types.DomainStatus
import Network.AWS.Amplify.Types.Job
import Network.AWS.Amplify.Types.JobStatus
import Network.AWS.Amplify.Types.JobSummary
import Network.AWS.Amplify.Types.JobType
import Network.AWS.Amplify.Types.Platform
import Network.AWS.Amplify.Types.ProductionBranch
import Network.AWS.Amplify.Types.Stage
import Network.AWS.Amplify.Types.Step
import Network.AWS.Amplify.Types.SubDomain
import Network.AWS.Amplify.Types.SubDomainSetting
import Network.AWS.Amplify.Types.Webhook
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-07-25@ of the Amazon Amplify SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "Amplify",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "amplify",
      Core._serviceSigningName = "amplify",
      Core._serviceVersion = "2017-07-25",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "Amplify",
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

-- | An operation failed because a dependent service threw an exception.
_DependentServiceFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DependentServiceFailureException =
  Core._MatchServiceError
    defaultService
    "DependentServiceFailureException"
    Prelude.. Core.hasStatus 503

-- | An entity was not found during an operation.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | The service failed to perform an operation due to an internal issue.
_InternalFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalFailureException =
  Core._MatchServiceError
    defaultService
    "InternalFailureException"
    Prelude.. Core.hasStatus 500

-- | An operation failed due to a lack of access.
_UnauthorizedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnauthorizedException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedException"
    Prelude.. Core.hasStatus 401

-- | An operation failed due to a non-existent resource.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | A request contains unexpected data.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400

-- | A resource could not be created because service quotas were exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 429

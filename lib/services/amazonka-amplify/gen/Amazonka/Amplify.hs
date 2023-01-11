{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Amplify
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-07-25@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amplify enables developers to develop and deploy cloud-powered mobile
-- and web apps. The Amplify Console provides a continuous delivery and
-- hosting service for web applications. For more information, see the
-- <https://docs.aws.amazon.com/amplify/latest/userguide/welcome.html Amplify Console User Guide>.
-- The Amplify Framework is a comprehensive set of SDKs, libraries, tools,
-- and documentation for client app development. For more information, see
-- the <https://docs.amplify.aws/ Amplify Framework.>
module Amazonka.Amplify
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** BadRequestException
    _BadRequestException,

    -- ** DependentServiceFailureException
    _DependentServiceFailureException,

    -- ** InternalFailureException
    _InternalFailureException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** UnauthorizedException
    _UnauthorizedException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateApp
    CreateApp (CreateApp'),
    newCreateApp,
    CreateAppResponse (CreateAppResponse'),
    newCreateAppResponse,

    -- ** CreateBackendEnvironment
    CreateBackendEnvironment (CreateBackendEnvironment'),
    newCreateBackendEnvironment,
    CreateBackendEnvironmentResponse (CreateBackendEnvironmentResponse'),
    newCreateBackendEnvironmentResponse,

    -- ** CreateBranch
    CreateBranch (CreateBranch'),
    newCreateBranch,
    CreateBranchResponse (CreateBranchResponse'),
    newCreateBranchResponse,

    -- ** CreateDeployment
    CreateDeployment (CreateDeployment'),
    newCreateDeployment,
    CreateDeploymentResponse (CreateDeploymentResponse'),
    newCreateDeploymentResponse,

    -- ** CreateDomainAssociation
    CreateDomainAssociation (CreateDomainAssociation'),
    newCreateDomainAssociation,
    CreateDomainAssociationResponse (CreateDomainAssociationResponse'),
    newCreateDomainAssociationResponse,

    -- ** CreateWebhook
    CreateWebhook (CreateWebhook'),
    newCreateWebhook,
    CreateWebhookResponse (CreateWebhookResponse'),
    newCreateWebhookResponse,

    -- ** DeleteApp
    DeleteApp (DeleteApp'),
    newDeleteApp,
    DeleteAppResponse (DeleteAppResponse'),
    newDeleteAppResponse,

    -- ** DeleteBackendEnvironment
    DeleteBackendEnvironment (DeleteBackendEnvironment'),
    newDeleteBackendEnvironment,
    DeleteBackendEnvironmentResponse (DeleteBackendEnvironmentResponse'),
    newDeleteBackendEnvironmentResponse,

    -- ** DeleteBranch
    DeleteBranch (DeleteBranch'),
    newDeleteBranch,
    DeleteBranchResponse (DeleteBranchResponse'),
    newDeleteBranchResponse,

    -- ** DeleteDomainAssociation
    DeleteDomainAssociation (DeleteDomainAssociation'),
    newDeleteDomainAssociation,
    DeleteDomainAssociationResponse (DeleteDomainAssociationResponse'),
    newDeleteDomainAssociationResponse,

    -- ** DeleteJob
    DeleteJob (DeleteJob'),
    newDeleteJob,
    DeleteJobResponse (DeleteJobResponse'),
    newDeleteJobResponse,

    -- ** DeleteWebhook
    DeleteWebhook (DeleteWebhook'),
    newDeleteWebhook,
    DeleteWebhookResponse (DeleteWebhookResponse'),
    newDeleteWebhookResponse,

    -- ** GenerateAccessLogs
    GenerateAccessLogs (GenerateAccessLogs'),
    newGenerateAccessLogs,
    GenerateAccessLogsResponse (GenerateAccessLogsResponse'),
    newGenerateAccessLogsResponse,

    -- ** GetApp
    GetApp (GetApp'),
    newGetApp,
    GetAppResponse (GetAppResponse'),
    newGetAppResponse,

    -- ** GetArtifactUrl
    GetArtifactUrl (GetArtifactUrl'),
    newGetArtifactUrl,
    GetArtifactUrlResponse (GetArtifactUrlResponse'),
    newGetArtifactUrlResponse,

    -- ** GetBackendEnvironment
    GetBackendEnvironment (GetBackendEnvironment'),
    newGetBackendEnvironment,
    GetBackendEnvironmentResponse (GetBackendEnvironmentResponse'),
    newGetBackendEnvironmentResponse,

    -- ** GetBranch
    GetBranch (GetBranch'),
    newGetBranch,
    GetBranchResponse (GetBranchResponse'),
    newGetBranchResponse,

    -- ** GetDomainAssociation
    GetDomainAssociation (GetDomainAssociation'),
    newGetDomainAssociation,
    GetDomainAssociationResponse (GetDomainAssociationResponse'),
    newGetDomainAssociationResponse,

    -- ** GetJob
    GetJob (GetJob'),
    newGetJob,
    GetJobResponse (GetJobResponse'),
    newGetJobResponse,

    -- ** GetWebhook
    GetWebhook (GetWebhook'),
    newGetWebhook,
    GetWebhookResponse (GetWebhookResponse'),
    newGetWebhookResponse,

    -- ** ListApps (Paginated)
    ListApps (ListApps'),
    newListApps,
    ListAppsResponse (ListAppsResponse'),
    newListAppsResponse,

    -- ** ListArtifacts
    ListArtifacts (ListArtifacts'),
    newListArtifacts,
    ListArtifactsResponse (ListArtifactsResponse'),
    newListArtifactsResponse,

    -- ** ListBackendEnvironments
    ListBackendEnvironments (ListBackendEnvironments'),
    newListBackendEnvironments,
    ListBackendEnvironmentsResponse (ListBackendEnvironmentsResponse'),
    newListBackendEnvironmentsResponse,

    -- ** ListBranches (Paginated)
    ListBranches (ListBranches'),
    newListBranches,
    ListBranchesResponse (ListBranchesResponse'),
    newListBranchesResponse,

    -- ** ListDomainAssociations (Paginated)
    ListDomainAssociations (ListDomainAssociations'),
    newListDomainAssociations,
    ListDomainAssociationsResponse (ListDomainAssociationsResponse'),
    newListDomainAssociationsResponse,

    -- ** ListJobs (Paginated)
    ListJobs (ListJobs'),
    newListJobs,
    ListJobsResponse (ListJobsResponse'),
    newListJobsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListWebhooks
    ListWebhooks (ListWebhooks'),
    newListWebhooks,
    ListWebhooksResponse (ListWebhooksResponse'),
    newListWebhooksResponse,

    -- ** StartDeployment
    StartDeployment (StartDeployment'),
    newStartDeployment,
    StartDeploymentResponse (StartDeploymentResponse'),
    newStartDeploymentResponse,

    -- ** StartJob
    StartJob (StartJob'),
    newStartJob,
    StartJobResponse (StartJobResponse'),
    newStartJobResponse,

    -- ** StopJob
    StopJob (StopJob'),
    newStopJob,
    StopJobResponse (StopJobResponse'),
    newStopJobResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateApp
    UpdateApp (UpdateApp'),
    newUpdateApp,
    UpdateAppResponse (UpdateAppResponse'),
    newUpdateAppResponse,

    -- ** UpdateBranch
    UpdateBranch (UpdateBranch'),
    newUpdateBranch,
    UpdateBranchResponse (UpdateBranchResponse'),
    newUpdateBranchResponse,

    -- ** UpdateDomainAssociation
    UpdateDomainAssociation (UpdateDomainAssociation'),
    newUpdateDomainAssociation,
    UpdateDomainAssociationResponse (UpdateDomainAssociationResponse'),
    newUpdateDomainAssociationResponse,

    -- ** UpdateWebhook
    UpdateWebhook (UpdateWebhook'),
    newUpdateWebhook,
    UpdateWebhookResponse (UpdateWebhookResponse'),
    newUpdateWebhookResponse,

    -- * Types

    -- ** DomainStatus
    DomainStatus (..),

    -- ** JobStatus
    JobStatus (..),

    -- ** JobType
    JobType (..),

    -- ** Platform
    Platform (..),

    -- ** RepositoryCloneMethod
    RepositoryCloneMethod (..),

    -- ** Stage
    Stage (..),

    -- ** App
    App (App'),
    newApp,

    -- ** Artifact
    Artifact (Artifact'),
    newArtifact,

    -- ** AutoBranchCreationConfig
    AutoBranchCreationConfig (AutoBranchCreationConfig'),
    newAutoBranchCreationConfig,

    -- ** BackendEnvironment
    BackendEnvironment (BackendEnvironment'),
    newBackendEnvironment,

    -- ** Branch
    Branch (Branch'),
    newBranch,

    -- ** CustomRule
    CustomRule (CustomRule'),
    newCustomRule,

    -- ** DomainAssociation
    DomainAssociation (DomainAssociation'),
    newDomainAssociation,

    -- ** Job
    Job (Job'),
    newJob,

    -- ** JobSummary
    JobSummary (JobSummary'),
    newJobSummary,

    -- ** ProductionBranch
    ProductionBranch (ProductionBranch'),
    newProductionBranch,

    -- ** Step
    Step (Step'),
    newStep,

    -- ** SubDomain
    SubDomain (SubDomain'),
    newSubDomain,

    -- ** SubDomainSetting
    SubDomainSetting (SubDomainSetting'),
    newSubDomainSetting,

    -- ** Webhook
    Webhook (Webhook'),
    newWebhook,
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
import Amazonka.Amplify.Lens
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
import Amazonka.Amplify.Types
import Amazonka.Amplify.UntagResource
import Amazonka.Amplify.UpdateApp
import Amazonka.Amplify.UpdateBranch
import Amazonka.Amplify.UpdateDomainAssociation
import Amazonka.Amplify.UpdateWebhook
import Amazonka.Amplify.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Amplify'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.

{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.Amplify
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.Amplify
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** DependentServiceFailureException
    _DependentServiceFailureException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** InternalFailureException
    _InternalFailureException,

    -- ** UnauthorizedException
    _UnauthorizedException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** BadRequestException
    _BadRequestException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetDomainAssociation
    GetDomainAssociation (GetDomainAssociation'),
    newGetDomainAssociation,
    GetDomainAssociationResponse (GetDomainAssociationResponse'),
    newGetDomainAssociationResponse,

    -- ** ListArtifacts
    ListArtifacts (ListArtifacts'),
    newListArtifacts,
    ListArtifactsResponse (ListArtifactsResponse'),
    newListArtifactsResponse,

    -- ** StopJob
    StopJob (StopJob'),
    newStopJob,
    StopJobResponse (StopJobResponse'),
    newStopJobResponse,

    -- ** GetBackendEnvironment
    GetBackendEnvironment (GetBackendEnvironment'),
    newGetBackendEnvironment,
    GetBackendEnvironmentResponse (GetBackendEnvironmentResponse'),
    newGetBackendEnvironmentResponse,

    -- ** CreateWebhook
    CreateWebhook (CreateWebhook'),
    newCreateWebhook,
    CreateWebhookResponse (CreateWebhookResponse'),
    newCreateWebhookResponse,

    -- ** GetBranch
    GetBranch (GetBranch'),
    newGetBranch,
    GetBranchResponse (GetBranchResponse'),
    newGetBranchResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** CreateDomainAssociation
    CreateDomainAssociation (CreateDomainAssociation'),
    newCreateDomainAssociation,
    CreateDomainAssociationResponse (CreateDomainAssociationResponse'),
    newCreateDomainAssociationResponse,

    -- ** GetWebhook
    GetWebhook (GetWebhook'),
    newGetWebhook,
    GetWebhookResponse (GetWebhookResponse'),
    newGetWebhookResponse,

    -- ** DeleteBranch
    DeleteBranch (DeleteBranch'),
    newDeleteBranch,
    DeleteBranchResponse (DeleteBranchResponse'),
    newDeleteBranchResponse,

    -- ** UpdateBranch
    UpdateBranch (UpdateBranch'),
    newUpdateBranch,
    UpdateBranchResponse (UpdateBranchResponse'),
    newUpdateBranchResponse,

    -- ** CreateBackendEnvironment
    CreateBackendEnvironment (CreateBackendEnvironment'),
    newCreateBackendEnvironment,
    CreateBackendEnvironmentResponse (CreateBackendEnvironmentResponse'),
    newCreateBackendEnvironmentResponse,

    -- ** CreateDeployment
    CreateDeployment (CreateDeployment'),
    newCreateDeployment,
    CreateDeploymentResponse (CreateDeploymentResponse'),
    newCreateDeploymentResponse,

    -- ** CreateBranch
    CreateBranch (CreateBranch'),
    newCreateBranch,
    CreateBranchResponse (CreateBranchResponse'),
    newCreateBranchResponse,

    -- ** GenerateAccessLogs
    GenerateAccessLogs (GenerateAccessLogs'),
    newGenerateAccessLogs,
    GenerateAccessLogsResponse (GenerateAccessLogsResponse'),
    newGenerateAccessLogsResponse,

    -- ** ListApps (Paginated)
    ListApps (ListApps'),
    newListApps,
    ListAppsResponse (ListAppsResponse'),
    newListAppsResponse,

    -- ** ListBranches (Paginated)
    ListBranches (ListBranches'),
    newListBranches,
    ListBranchesResponse (ListBranchesResponse'),
    newListBranchesResponse,

    -- ** DeleteBackendEnvironment
    DeleteBackendEnvironment (DeleteBackendEnvironment'),
    newDeleteBackendEnvironment,
    DeleteBackendEnvironmentResponse (DeleteBackendEnvironmentResponse'),
    newDeleteBackendEnvironmentResponse,

    -- ** DeleteApp
    DeleteApp (DeleteApp'),
    newDeleteApp,
    DeleteAppResponse (DeleteAppResponse'),
    newDeleteAppResponse,

    -- ** UpdateApp
    UpdateApp (UpdateApp'),
    newUpdateApp,
    UpdateAppResponse (UpdateAppResponse'),
    newUpdateAppResponse,

    -- ** GetArtifactUrl
    GetArtifactUrl (GetArtifactUrl'),
    newGetArtifactUrl,
    GetArtifactUrlResponse (GetArtifactUrlResponse'),
    newGetArtifactUrlResponse,

    -- ** ListJobs (Paginated)
    ListJobs (ListJobs'),
    newListJobs,
    ListJobsResponse (ListJobsResponse'),
    newListJobsResponse,

    -- ** DeleteJob
    DeleteJob (DeleteJob'),
    newDeleteJob,
    DeleteJobResponse (DeleteJobResponse'),
    newDeleteJobResponse,

    -- ** GetJob
    GetJob (GetJob'),
    newGetJob,
    GetJobResponse (GetJobResponse'),
    newGetJobResponse,

    -- ** StartJob
    StartJob (StartJob'),
    newStartJob,
    StartJobResponse (StartJobResponse'),
    newStartJobResponse,

    -- ** GetApp
    GetApp (GetApp'),
    newGetApp,
    GetAppResponse (GetAppResponse'),
    newGetAppResponse,

    -- ** UpdateWebhook
    UpdateWebhook (UpdateWebhook'),
    newUpdateWebhook,
    UpdateWebhookResponse (UpdateWebhookResponse'),
    newUpdateWebhookResponse,

    -- ** DeleteWebhook
    DeleteWebhook (DeleteWebhook'),
    newDeleteWebhook,
    DeleteWebhookResponse (DeleteWebhookResponse'),
    newDeleteWebhookResponse,

    -- ** ListWebhooks
    ListWebhooks (ListWebhooks'),
    newListWebhooks,
    ListWebhooksResponse (ListWebhooksResponse'),
    newListWebhooksResponse,

    -- ** CreateApp
    CreateApp (CreateApp'),
    newCreateApp,
    CreateAppResponse (CreateAppResponse'),
    newCreateAppResponse,

    -- ** DeleteDomainAssociation
    DeleteDomainAssociation (DeleteDomainAssociation'),
    newDeleteDomainAssociation,
    DeleteDomainAssociationResponse (DeleteDomainAssociationResponse'),
    newDeleteDomainAssociationResponse,

    -- ** UpdateDomainAssociation
    UpdateDomainAssociation (UpdateDomainAssociation'),
    newUpdateDomainAssociation,
    UpdateDomainAssociationResponse (UpdateDomainAssociationResponse'),
    newUpdateDomainAssociationResponse,

    -- ** ListDomainAssociations (Paginated)
    ListDomainAssociations (ListDomainAssociations'),
    newListDomainAssociations,
    ListDomainAssociationsResponse (ListDomainAssociationsResponse'),
    newListDomainAssociationsResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** ListBackendEnvironments
    ListBackendEnvironments (ListBackendEnvironments'),
    newListBackendEnvironments,
    ListBackendEnvironmentsResponse (ListBackendEnvironmentsResponse'),
    newListBackendEnvironmentsResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** StartDeployment
    StartDeployment (StartDeployment'),
    newStartDeployment,
    StartDeploymentResponse (StartDeploymentResponse'),
    newStartDeploymentResponse,

    -- * Types

    -- ** DomainStatus
    DomainStatus (..),

    -- ** JobStatus
    JobStatus (..),

    -- ** JobType
    JobType (..),

    -- ** Platform
    Platform (..),

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

import Network.AWS.Amplify.CreateApp
import Network.AWS.Amplify.CreateBackendEnvironment
import Network.AWS.Amplify.CreateBranch
import Network.AWS.Amplify.CreateDeployment
import Network.AWS.Amplify.CreateDomainAssociation
import Network.AWS.Amplify.CreateWebhook
import Network.AWS.Amplify.DeleteApp
import Network.AWS.Amplify.DeleteBackendEnvironment
import Network.AWS.Amplify.DeleteBranch
import Network.AWS.Amplify.DeleteDomainAssociation
import Network.AWS.Amplify.DeleteJob
import Network.AWS.Amplify.DeleteWebhook
import Network.AWS.Amplify.GenerateAccessLogs
import Network.AWS.Amplify.GetApp
import Network.AWS.Amplify.GetArtifactUrl
import Network.AWS.Amplify.GetBackendEnvironment
import Network.AWS.Amplify.GetBranch
import Network.AWS.Amplify.GetDomainAssociation
import Network.AWS.Amplify.GetJob
import Network.AWS.Amplify.GetWebhook
import Network.AWS.Amplify.Lens
import Network.AWS.Amplify.ListApps
import Network.AWS.Amplify.ListArtifacts
import Network.AWS.Amplify.ListBackendEnvironments
import Network.AWS.Amplify.ListBranches
import Network.AWS.Amplify.ListDomainAssociations
import Network.AWS.Amplify.ListJobs
import Network.AWS.Amplify.ListTagsForResource
import Network.AWS.Amplify.ListWebhooks
import Network.AWS.Amplify.StartDeployment
import Network.AWS.Amplify.StartJob
import Network.AWS.Amplify.StopJob
import Network.AWS.Amplify.TagResource
import Network.AWS.Amplify.Types
import Network.AWS.Amplify.UntagResource
import Network.AWS.Amplify.UpdateApp
import Network.AWS.Amplify.UpdateBranch
import Network.AWS.Amplify.UpdateDomainAssociation
import Network.AWS.Amplify.UpdateWebhook
import Network.AWS.Amplify.Waiters

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

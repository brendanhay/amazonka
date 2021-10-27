{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.Proton
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-07-20@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- This is the AWS Proton Service API Reference. It provides descriptions,
-- syntax and usage examples for each of the
-- <https://docs.aws.amazon.com/proton/latest/APIReference/API_Operations.html actions>
-- and
-- <https://docs.aws.amazon.com/proton/latest/APIReference/API_Types.html data types>
-- for the AWS Proton service.
--
-- The documentation for each action shows the Query API request parameters
-- and the XML response.
--
-- Alternatively, you can use the AWS CLI to access an API. For more
-- information, see the
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-chap-welcome.html AWS Command Line Interface User Guide>.
--
-- The AWS Proton service is a two-pronged automation framework.
-- Administrators create service templates to provide standardized
-- infrastructure and deployment tooling for serverless and container based
-- applications. Developers, in turn, select from the available service
-- templates to automate their application or service deployments.
--
-- Because administrators define the infrastructure and tooling that AWS
-- Proton deploys and manages, they need permissions to use all of the
-- listed API operations.
--
-- When developers select a specific infrastructure and tooling set, AWS
-- Proton deploys their applications. To monitor their applications that
-- are running on AWS Proton, developers need permissions to the service
-- /create/, /list/, /update/ and /delete/ API operations and the service
-- instance /list/ and /update/ API operations.
--
-- To learn more about AWS Proton administration, see the
-- <https://docs.aws.amazon.com/proton/latest/adminguide/Welcome.html AWS Proton Administrator Guide>.
--
-- To learn more about deploying serverless and containerized applications
-- on AWS Proton, see the
-- <https://docs.aws.amazon.com/proton/latest/userguide/Welcome.html AWS Proton User Guide>.
--
-- __Ensuring Idempotency__
--
-- When you make a mutating API request, the request typically returns a
-- result before the asynchronous workflows of the operation are complete.
-- Operations might also time out or encounter other server issues before
-- they\'re complete, even if the request already returned a result. This
-- might make it difficult to determine whether the request succeeded.
-- Moreover, you might need to retry the request multiple times to ensure
-- that the operation completes successfully. However, if the original
-- request and the subsequent retries are successful, the operation occurs
-- multiple times. This means that you might create more resources than you
-- intended.
--
-- /Idempotency/ ensures that an API request action completes no more than
-- one time. With an idempotent request, if the original request action
-- completes successfully, any subsequent retries complete successfully
-- without performing any further actions. However, the result might
-- contain updated information, such as the current creation status.
--
-- The following lists of APIs are grouped according to methods that ensure
-- idempotency.
--
-- __Idempotent create APIs with a client token__
--
-- The API actions in this list support idempotency with the use of a
-- /client token/. The corresponding AWS CLI commands also support
-- idempotency using a client token. A client token is a unique,
-- case-sensitive string of up to 64 ASCII characters. To make an
-- idempotent API request using one of these actions, specify a client
-- token in the request. We recommend that you /don\'t/ reuse the same
-- client token for other API requests. If you don’t provide a client token
-- for these APIs, a default client token is automatically provided by
-- SDKs.
--
-- Given a request action that has succeeded:
--
-- If you retry the request using the same client token and the same
-- parameters, the retry succeeds without performing any further actions
-- other than returning the original resource detail data in the response.
--
-- If you retry the request using the same client token, but one or more of
-- the parameters are different, the retry throws a @ValidationException@
-- with an @IdempotentParameterMismatch@ error.
--
-- Client tokens expire eight hours after a request is made. If you retry
-- the request with the expired token, a new resource is created.
--
-- If the original resource is deleted and you retry the request, a new
-- resource is created.
--
-- Idempotent create APIs with a client token:
--
-- -   CreateEnvironmentTemplateVersion
--
-- -   CreateServiceTemplateVersion
--
-- -   CreateEnvironmentAccountConnection
--
-- __Idempotent create APIs__
--
-- Given a request action that has succeeded:
--
-- If you retry the request with an API from this group, and the original
-- resource /hasn\'t/ been modified, the retry succeeds without performing
-- any further actions other than returning the original resource detail
-- data in the response.
--
-- If the original resource has been modified, the retry throws a
-- @ConflictException@.
--
-- If you retry with different input parameters, the retry throws a
-- @ValidationException@ with an @IdempotentParameterMismatch@ error.
--
-- Idempotent create APIs:
--
-- -   CreateEnvironmentTemplate
--
-- -   CreateServiceTemplate
--
-- -   CreateEnvironment
--
-- -   CreateService
--
-- __Idempotent delete APIs__
--
-- Given a request action that has succeeded:
--
-- When you retry the request with an API from this group and the resource
-- was deleted, its metadata is returned in the response.
--
-- If you retry and the resource doesn\'t exist, the response is empty.
--
-- In both cases, the retry succeeds.
--
-- Idempotent delete APIs:
--
-- -   DeleteEnvironmentTemplate
--
-- -   DeleteEnvironmentTemplateVersion
--
-- -   DeleteServiceTemplate
--
-- -   DeleteServiceTemplateVersion
--
-- -   DeleteEnvironmentAccountConnection
--
-- __Asynchronous idempotent delete APIs__
--
-- Given a request action that has succeeded:
--
-- If you retry the request with an API from this group, if the original
-- request delete operation status is @DELETE_IN_PROGRESS@, the retry
-- returns the resource detail data in the response without performing any
-- further actions.
--
-- If the original request delete operation is complete, a retry returns an
-- empty response.
--
-- Asynchronous idempotent delete APIs:
--
-- -   DeleteEnvironment
--
-- -   DeleteService
module Network.AWS.Proton
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ValidationException
    _ValidationException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- ** EnvironmentDeployed
    newEnvironmentDeployed,

    -- ** ServiceTemplateVersionRegistered
    newServiceTemplateVersionRegistered,

    -- ** EnvironmentTemplateVersionRegistered
    newEnvironmentTemplateVersionRegistered,

    -- ** ServiceUpdated
    newServiceUpdated,

    -- ** ServiceDeleted
    newServiceDeleted,

    -- ** ServiceInstanceDeployed
    newServiceInstanceDeployed,

    -- ** ServicePipelineDeployed
    newServicePipelineDeployed,

    -- ** ServiceCreated
    newServiceCreated,

    -- * Operations
    -- $operations

    -- ** ListServices (Paginated)
    ListServices (ListServices'),
    newListServices,
    ListServicesResponse (ListServicesResponse'),
    newListServicesResponse,

    -- ** ListEnvironments (Paginated)
    ListEnvironments (ListEnvironments'),
    newListEnvironments,
    ListEnvironmentsResponse (ListEnvironmentsResponse'),
    newListEnvironmentsResponse,

    -- ** UpdateEnvironment
    UpdateEnvironment (UpdateEnvironment'),
    newUpdateEnvironment,
    UpdateEnvironmentResponse (UpdateEnvironmentResponse'),
    newUpdateEnvironmentResponse,

    -- ** DeleteEnvironment
    DeleteEnvironment (DeleteEnvironment'),
    newDeleteEnvironment,
    DeleteEnvironmentResponse (DeleteEnvironmentResponse'),
    newDeleteEnvironmentResponse,

    -- ** DeleteService
    DeleteService (DeleteService'),
    newDeleteService,
    DeleteServiceResponse (DeleteServiceResponse'),
    newDeleteServiceResponse,

    -- ** UpdateService
    UpdateService (UpdateService'),
    newUpdateService,
    UpdateServiceResponse (UpdateServiceResponse'),
    newUpdateServiceResponse,

    -- ** GetServiceInstance
    GetServiceInstance (GetServiceInstance'),
    newGetServiceInstance,
    GetServiceInstanceResponse (GetServiceInstanceResponse'),
    newGetServiceInstanceResponse,

    -- ** AcceptEnvironmentAccountConnection
    AcceptEnvironmentAccountConnection (AcceptEnvironmentAccountConnection'),
    newAcceptEnvironmentAccountConnection,
    AcceptEnvironmentAccountConnectionResponse (AcceptEnvironmentAccountConnectionResponse'),
    newAcceptEnvironmentAccountConnectionResponse,

    -- ** ListTagsForResource (Paginated)
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** UpdateAccountSettings
    UpdateAccountSettings (UpdateAccountSettings'),
    newUpdateAccountSettings,
    UpdateAccountSettingsResponse (UpdateAccountSettingsResponse'),
    newUpdateAccountSettingsResponse,

    -- ** RejectEnvironmentAccountConnection
    RejectEnvironmentAccountConnection (RejectEnvironmentAccountConnection'),
    newRejectEnvironmentAccountConnection,
    RejectEnvironmentAccountConnectionResponse (RejectEnvironmentAccountConnectionResponse'),
    newRejectEnvironmentAccountConnectionResponse,

    -- ** ListServiceInstances (Paginated)
    ListServiceInstances (ListServiceInstances'),
    newListServiceInstances,
    ListServiceInstancesResponse (ListServiceInstancesResponse'),
    newListServiceInstancesResponse,

    -- ** CancelServicePipelineDeployment
    CancelServicePipelineDeployment (CancelServicePipelineDeployment'),
    newCancelServicePipelineDeployment,
    CancelServicePipelineDeploymentResponse (CancelServicePipelineDeploymentResponse'),
    newCancelServicePipelineDeploymentResponse,

    -- ** CreateServiceTemplateVersion
    CreateServiceTemplateVersion (CreateServiceTemplateVersion'),
    newCreateServiceTemplateVersion,
    CreateServiceTemplateVersionResponse (CreateServiceTemplateVersionResponse'),
    newCreateServiceTemplateVersionResponse,

    -- ** GetServiceTemplate
    GetServiceTemplate (GetServiceTemplate'),
    newGetServiceTemplate,
    GetServiceTemplateResponse (GetServiceTemplateResponse'),
    newGetServiceTemplateResponse,

    -- ** CreateEnvironmentTemplateVersion
    CreateEnvironmentTemplateVersion (CreateEnvironmentTemplateVersion'),
    newCreateEnvironmentTemplateVersion,
    CreateEnvironmentTemplateVersionResponse (CreateEnvironmentTemplateVersionResponse'),
    newCreateEnvironmentTemplateVersionResponse,

    -- ** CancelServiceInstanceDeployment
    CancelServiceInstanceDeployment (CancelServiceInstanceDeployment'),
    newCancelServiceInstanceDeployment,
    CancelServiceInstanceDeploymentResponse (CancelServiceInstanceDeploymentResponse'),
    newCancelServiceInstanceDeploymentResponse,

    -- ** GetEnvironmentTemplate
    GetEnvironmentTemplate (GetEnvironmentTemplate'),
    newGetEnvironmentTemplate,
    GetEnvironmentTemplateResponse (GetEnvironmentTemplateResponse'),
    newGetEnvironmentTemplateResponse,

    -- ** UpdateServicePipeline
    UpdateServicePipeline (UpdateServicePipeline'),
    newUpdateServicePipeline,
    UpdateServicePipelineResponse (UpdateServicePipelineResponse'),
    newUpdateServicePipelineResponse,

    -- ** ListServiceTemplateVersions (Paginated)
    ListServiceTemplateVersions (ListServiceTemplateVersions'),
    newListServiceTemplateVersions,
    ListServiceTemplateVersionsResponse (ListServiceTemplateVersionsResponse'),
    newListServiceTemplateVersionsResponse,

    -- ** CreateEnvironmentAccountConnection
    CreateEnvironmentAccountConnection (CreateEnvironmentAccountConnection'),
    newCreateEnvironmentAccountConnection,
    CreateEnvironmentAccountConnectionResponse (CreateEnvironmentAccountConnectionResponse'),
    newCreateEnvironmentAccountConnectionResponse,

    -- ** ListEnvironmentTemplateVersions (Paginated)
    ListEnvironmentTemplateVersions (ListEnvironmentTemplateVersions'),
    newListEnvironmentTemplateVersions,
    ListEnvironmentTemplateVersionsResponse (ListEnvironmentTemplateVersionsResponse'),
    newListEnvironmentTemplateVersionsResponse,

    -- ** GetEnvironmentTemplateVersion
    GetEnvironmentTemplateVersion (GetEnvironmentTemplateVersion'),
    newGetEnvironmentTemplateVersion,
    GetEnvironmentTemplateVersionResponse (GetEnvironmentTemplateVersionResponse'),
    newGetEnvironmentTemplateVersionResponse,

    -- ** CreateServiceTemplate
    CreateServiceTemplate (CreateServiceTemplate'),
    newCreateServiceTemplate,
    CreateServiceTemplateResponse (CreateServiceTemplateResponse'),
    newCreateServiceTemplateResponse,

    -- ** GetServiceTemplateVersion
    GetServiceTemplateVersion (GetServiceTemplateVersion'),
    newGetServiceTemplateVersion,
    GetServiceTemplateVersionResponse (GetServiceTemplateVersionResponse'),
    newGetServiceTemplateVersionResponse,

    -- ** CreateEnvironmentTemplate
    CreateEnvironmentTemplate (CreateEnvironmentTemplate'),
    newCreateEnvironmentTemplate,
    CreateEnvironmentTemplateResponse (CreateEnvironmentTemplateResponse'),
    newCreateEnvironmentTemplateResponse,

    -- ** DeleteEnvironmentTemplate
    DeleteEnvironmentTemplate (DeleteEnvironmentTemplate'),
    newDeleteEnvironmentTemplate,
    DeleteEnvironmentTemplateResponse (DeleteEnvironmentTemplateResponse'),
    newDeleteEnvironmentTemplateResponse,

    -- ** UpdateEnvironmentTemplate
    UpdateEnvironmentTemplate (UpdateEnvironmentTemplate'),
    newUpdateEnvironmentTemplate,
    UpdateEnvironmentTemplateResponse (UpdateEnvironmentTemplateResponse'),
    newUpdateEnvironmentTemplateResponse,

    -- ** GetEnvironmentAccountConnection
    GetEnvironmentAccountConnection (GetEnvironmentAccountConnection'),
    newGetEnvironmentAccountConnection,
    GetEnvironmentAccountConnectionResponse (GetEnvironmentAccountConnectionResponse'),
    newGetEnvironmentAccountConnectionResponse,

    -- ** ListEnvironmentTemplates (Paginated)
    ListEnvironmentTemplates (ListEnvironmentTemplates'),
    newListEnvironmentTemplates,
    ListEnvironmentTemplatesResponse (ListEnvironmentTemplatesResponse'),
    newListEnvironmentTemplatesResponse,

    -- ** DeleteServiceTemplate
    DeleteServiceTemplate (DeleteServiceTemplate'),
    newDeleteServiceTemplate,
    DeleteServiceTemplateResponse (DeleteServiceTemplateResponse'),
    newDeleteServiceTemplateResponse,

    -- ** UpdateServiceTemplate
    UpdateServiceTemplate (UpdateServiceTemplate'),
    newUpdateServiceTemplate,
    UpdateServiceTemplateResponse (UpdateServiceTemplateResponse'),
    newUpdateServiceTemplateResponse,

    -- ** ListServiceTemplates (Paginated)
    ListServiceTemplates (ListServiceTemplates'),
    newListServiceTemplates,
    ListServiceTemplatesResponse (ListServiceTemplatesResponse'),
    newListServiceTemplatesResponse,

    -- ** GetAccountSettings
    GetAccountSettings (GetAccountSettings'),
    newGetAccountSettings,
    GetAccountSettingsResponse (GetAccountSettingsResponse'),
    newGetAccountSettingsResponse,

    -- ** CancelEnvironmentDeployment
    CancelEnvironmentDeployment (CancelEnvironmentDeployment'),
    newCancelEnvironmentDeployment,
    CancelEnvironmentDeploymentResponse (CancelEnvironmentDeploymentResponse'),
    newCancelEnvironmentDeploymentResponse,

    -- ** UpdateServiceInstance
    UpdateServiceInstance (UpdateServiceInstance'),
    newUpdateServiceInstance,
    UpdateServiceInstanceResponse (UpdateServiceInstanceResponse'),
    newUpdateServiceInstanceResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** GetEnvironment
    GetEnvironment (GetEnvironment'),
    newGetEnvironment,
    GetEnvironmentResponse (GetEnvironmentResponse'),
    newGetEnvironmentResponse,

    -- ** ListEnvironmentAccountConnections (Paginated)
    ListEnvironmentAccountConnections (ListEnvironmentAccountConnections'),
    newListEnvironmentAccountConnections,
    ListEnvironmentAccountConnectionsResponse (ListEnvironmentAccountConnectionsResponse'),
    newListEnvironmentAccountConnectionsResponse,

    -- ** GetService
    GetService (GetService'),
    newGetService,
    GetServiceResponse (GetServiceResponse'),
    newGetServiceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** DeleteEnvironmentAccountConnection
    DeleteEnvironmentAccountConnection (DeleteEnvironmentAccountConnection'),
    newDeleteEnvironmentAccountConnection,
    DeleteEnvironmentAccountConnectionResponse (DeleteEnvironmentAccountConnectionResponse'),
    newDeleteEnvironmentAccountConnectionResponse,

    -- ** UpdateEnvironmentAccountConnection
    UpdateEnvironmentAccountConnection (UpdateEnvironmentAccountConnection'),
    newUpdateEnvironmentAccountConnection,
    UpdateEnvironmentAccountConnectionResponse (UpdateEnvironmentAccountConnectionResponse'),
    newUpdateEnvironmentAccountConnectionResponse,

    -- ** DeleteServiceTemplateVersion
    DeleteServiceTemplateVersion (DeleteServiceTemplateVersion'),
    newDeleteServiceTemplateVersion,
    DeleteServiceTemplateVersionResponse (DeleteServiceTemplateVersionResponse'),
    newDeleteServiceTemplateVersionResponse,

    -- ** UpdateServiceTemplateVersion
    UpdateServiceTemplateVersion (UpdateServiceTemplateVersion'),
    newUpdateServiceTemplateVersion,
    UpdateServiceTemplateVersionResponse (UpdateServiceTemplateVersionResponse'),
    newUpdateServiceTemplateVersionResponse,

    -- ** UpdateEnvironmentTemplateVersion
    UpdateEnvironmentTemplateVersion (UpdateEnvironmentTemplateVersion'),
    newUpdateEnvironmentTemplateVersion,
    UpdateEnvironmentTemplateVersionResponse (UpdateEnvironmentTemplateVersionResponse'),
    newUpdateEnvironmentTemplateVersionResponse,

    -- ** DeleteEnvironmentTemplateVersion
    DeleteEnvironmentTemplateVersion (DeleteEnvironmentTemplateVersion'),
    newDeleteEnvironmentTemplateVersion,
    DeleteEnvironmentTemplateVersionResponse (DeleteEnvironmentTemplateVersionResponse'),
    newDeleteEnvironmentTemplateVersionResponse,

    -- ** CreateEnvironment
    CreateEnvironment (CreateEnvironment'),
    newCreateEnvironment,
    CreateEnvironmentResponse (CreateEnvironmentResponse'),
    newCreateEnvironmentResponse,

    -- ** CreateService
    CreateService (CreateService'),
    newCreateService,
    CreateServiceResponse (CreateServiceResponse'),
    newCreateServiceResponse,

    -- * Types

    -- ** DeploymentStatus
    DeploymentStatus (..),

    -- ** DeploymentUpdateType
    DeploymentUpdateType (..),

    -- ** EnvironmentAccountConnectionRequesterAccountType
    EnvironmentAccountConnectionRequesterAccountType (..),

    -- ** EnvironmentAccountConnectionStatus
    EnvironmentAccountConnectionStatus (..),

    -- ** Provisioning
    Provisioning (..),

    -- ** ServiceStatus
    ServiceStatus (..),

    -- ** TemplateVersionStatus
    TemplateVersionStatus (..),

    -- ** AccountSettings
    AccountSettings (AccountSettings'),
    newAccountSettings,

    -- ** CompatibleEnvironmentTemplate
    CompatibleEnvironmentTemplate (CompatibleEnvironmentTemplate'),
    newCompatibleEnvironmentTemplate,

    -- ** CompatibleEnvironmentTemplateInput
    CompatibleEnvironmentTemplateInput (CompatibleEnvironmentTemplateInput'),
    newCompatibleEnvironmentTemplateInput,

    -- ** Environment
    Environment (Environment'),
    newEnvironment,

    -- ** EnvironmentAccountConnection
    EnvironmentAccountConnection (EnvironmentAccountConnection'),
    newEnvironmentAccountConnection,

    -- ** EnvironmentAccountConnectionSummary
    EnvironmentAccountConnectionSummary (EnvironmentAccountConnectionSummary'),
    newEnvironmentAccountConnectionSummary,

    -- ** EnvironmentSummary
    EnvironmentSummary (EnvironmentSummary'),
    newEnvironmentSummary,

    -- ** EnvironmentTemplate
    EnvironmentTemplate (EnvironmentTemplate'),
    newEnvironmentTemplate,

    -- ** EnvironmentTemplateFilter
    EnvironmentTemplateFilter (EnvironmentTemplateFilter'),
    newEnvironmentTemplateFilter,

    -- ** EnvironmentTemplateSummary
    EnvironmentTemplateSummary (EnvironmentTemplateSummary'),
    newEnvironmentTemplateSummary,

    -- ** EnvironmentTemplateVersion
    EnvironmentTemplateVersion (EnvironmentTemplateVersion'),
    newEnvironmentTemplateVersion,

    -- ** EnvironmentTemplateVersionSummary
    EnvironmentTemplateVersionSummary (EnvironmentTemplateVersionSummary'),
    newEnvironmentTemplateVersionSummary,

    -- ** S3ObjectSource
    S3ObjectSource (S3ObjectSource'),
    newS3ObjectSource,

    -- ** Service
    Service (Service'),
    newService,

    -- ** ServiceInstance
    ServiceInstance (ServiceInstance'),
    newServiceInstance,

    -- ** ServiceInstanceSummary
    ServiceInstanceSummary (ServiceInstanceSummary'),
    newServiceInstanceSummary,

    -- ** ServicePipeline
    ServicePipeline (ServicePipeline'),
    newServicePipeline,

    -- ** ServiceSummary
    ServiceSummary (ServiceSummary'),
    newServiceSummary,

    -- ** ServiceTemplate
    ServiceTemplate (ServiceTemplate'),
    newServiceTemplate,

    -- ** ServiceTemplateSummary
    ServiceTemplateSummary (ServiceTemplateSummary'),
    newServiceTemplateSummary,

    -- ** ServiceTemplateVersion
    ServiceTemplateVersion (ServiceTemplateVersion'),
    newServiceTemplateVersion,

    -- ** ServiceTemplateVersionSummary
    ServiceTemplateVersionSummary (ServiceTemplateVersionSummary'),
    newServiceTemplateVersionSummary,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TemplateVersionSourceInput
    TemplateVersionSourceInput (TemplateVersionSourceInput'),
    newTemplateVersionSourceInput,
  )
where

import Network.AWS.Proton.AcceptEnvironmentAccountConnection
import Network.AWS.Proton.CancelEnvironmentDeployment
import Network.AWS.Proton.CancelServiceInstanceDeployment
import Network.AWS.Proton.CancelServicePipelineDeployment
import Network.AWS.Proton.CreateEnvironment
import Network.AWS.Proton.CreateEnvironmentAccountConnection
import Network.AWS.Proton.CreateEnvironmentTemplate
import Network.AWS.Proton.CreateEnvironmentTemplateVersion
import Network.AWS.Proton.CreateService
import Network.AWS.Proton.CreateServiceTemplate
import Network.AWS.Proton.CreateServiceTemplateVersion
import Network.AWS.Proton.DeleteEnvironment
import Network.AWS.Proton.DeleteEnvironmentAccountConnection
import Network.AWS.Proton.DeleteEnvironmentTemplate
import Network.AWS.Proton.DeleteEnvironmentTemplateVersion
import Network.AWS.Proton.DeleteService
import Network.AWS.Proton.DeleteServiceTemplate
import Network.AWS.Proton.DeleteServiceTemplateVersion
import Network.AWS.Proton.GetAccountSettings
import Network.AWS.Proton.GetEnvironment
import Network.AWS.Proton.GetEnvironmentAccountConnection
import Network.AWS.Proton.GetEnvironmentTemplate
import Network.AWS.Proton.GetEnvironmentTemplateVersion
import Network.AWS.Proton.GetService
import Network.AWS.Proton.GetServiceInstance
import Network.AWS.Proton.GetServiceTemplate
import Network.AWS.Proton.GetServiceTemplateVersion
import Network.AWS.Proton.Lens
import Network.AWS.Proton.ListEnvironmentAccountConnections
import Network.AWS.Proton.ListEnvironmentTemplateVersions
import Network.AWS.Proton.ListEnvironmentTemplates
import Network.AWS.Proton.ListEnvironments
import Network.AWS.Proton.ListServiceInstances
import Network.AWS.Proton.ListServiceTemplateVersions
import Network.AWS.Proton.ListServiceTemplates
import Network.AWS.Proton.ListServices
import Network.AWS.Proton.ListTagsForResource
import Network.AWS.Proton.RejectEnvironmentAccountConnection
import Network.AWS.Proton.TagResource
import Network.AWS.Proton.Types
import Network.AWS.Proton.UntagResource
import Network.AWS.Proton.UpdateAccountSettings
import Network.AWS.Proton.UpdateEnvironment
import Network.AWS.Proton.UpdateEnvironmentAccountConnection
import Network.AWS.Proton.UpdateEnvironmentTemplate
import Network.AWS.Proton.UpdateEnvironmentTemplateVersion
import Network.AWS.Proton.UpdateService
import Network.AWS.Proton.UpdateServiceInstance
import Network.AWS.Proton.UpdateServicePipeline
import Network.AWS.Proton.UpdateServiceTemplate
import Network.AWS.Proton.UpdateServiceTemplateVersion
import Network.AWS.Proton.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Proton'.

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

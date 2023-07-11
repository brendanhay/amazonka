{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.GreengrassV2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-11-30@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- IoT Greengrass brings local compute, messaging, data management, sync,
-- and ML inference capabilities to edge devices. This enables devices to
-- collect and analyze data closer to the source of information, react
-- autonomously to local events, and communicate securely with each other
-- on local networks. Local devices can also communicate securely with
-- Amazon Web Services IoT Core and export IoT data to the Amazon Web
-- Services Cloud. IoT Greengrass developers can use Lambda functions and
-- components to create and deploy applications to fleets of edge devices
-- for local operation.
--
-- IoT Greengrass Version 2 provides a new major version of the IoT
-- Greengrass Core software, new APIs, and a new console. Use this API
-- reference to learn how to use the IoT Greengrass V2 API operations to
-- manage components, manage deployments, and core devices.
--
-- For more information, see
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/what-is-iot-greengrass.html What is IoT Greengrass?>
-- in the /IoT Greengrass V2 Developer Guide/.
module Amazonka.GreengrassV2
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** RequestAlreadyInProgressException
    _RequestAlreadyInProgressException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateServiceRoleToAccount
    AssociateServiceRoleToAccount (AssociateServiceRoleToAccount'),
    newAssociateServiceRoleToAccount,
    AssociateServiceRoleToAccountResponse (AssociateServiceRoleToAccountResponse'),
    newAssociateServiceRoleToAccountResponse,

    -- ** BatchAssociateClientDeviceWithCoreDevice
    BatchAssociateClientDeviceWithCoreDevice (BatchAssociateClientDeviceWithCoreDevice'),
    newBatchAssociateClientDeviceWithCoreDevice,
    BatchAssociateClientDeviceWithCoreDeviceResponse (BatchAssociateClientDeviceWithCoreDeviceResponse'),
    newBatchAssociateClientDeviceWithCoreDeviceResponse,

    -- ** BatchDisassociateClientDeviceFromCoreDevice
    BatchDisassociateClientDeviceFromCoreDevice (BatchDisassociateClientDeviceFromCoreDevice'),
    newBatchDisassociateClientDeviceFromCoreDevice,
    BatchDisassociateClientDeviceFromCoreDeviceResponse (BatchDisassociateClientDeviceFromCoreDeviceResponse'),
    newBatchDisassociateClientDeviceFromCoreDeviceResponse,

    -- ** CancelDeployment
    CancelDeployment (CancelDeployment'),
    newCancelDeployment,
    CancelDeploymentResponse (CancelDeploymentResponse'),
    newCancelDeploymentResponse,

    -- ** CreateComponentVersion
    CreateComponentVersion (CreateComponentVersion'),
    newCreateComponentVersion,
    CreateComponentVersionResponse (CreateComponentVersionResponse'),
    newCreateComponentVersionResponse,

    -- ** CreateDeployment
    CreateDeployment (CreateDeployment'),
    newCreateDeployment,
    CreateDeploymentResponse (CreateDeploymentResponse'),
    newCreateDeploymentResponse,

    -- ** DeleteComponent
    DeleteComponent (DeleteComponent'),
    newDeleteComponent,
    DeleteComponentResponse (DeleteComponentResponse'),
    newDeleteComponentResponse,

    -- ** DeleteCoreDevice
    DeleteCoreDevice (DeleteCoreDevice'),
    newDeleteCoreDevice,
    DeleteCoreDeviceResponse (DeleteCoreDeviceResponse'),
    newDeleteCoreDeviceResponse,

    -- ** DeleteDeployment
    DeleteDeployment (DeleteDeployment'),
    newDeleteDeployment,
    DeleteDeploymentResponse (DeleteDeploymentResponse'),
    newDeleteDeploymentResponse,

    -- ** DescribeComponent
    DescribeComponent (DescribeComponent'),
    newDescribeComponent,
    DescribeComponentResponse (DescribeComponentResponse'),
    newDescribeComponentResponse,

    -- ** DisassociateServiceRoleFromAccount
    DisassociateServiceRoleFromAccount (DisassociateServiceRoleFromAccount'),
    newDisassociateServiceRoleFromAccount,
    DisassociateServiceRoleFromAccountResponse (DisassociateServiceRoleFromAccountResponse'),
    newDisassociateServiceRoleFromAccountResponse,

    -- ** GetComponent
    GetComponent (GetComponent'),
    newGetComponent,
    GetComponentResponse (GetComponentResponse'),
    newGetComponentResponse,

    -- ** GetComponentVersionArtifact
    GetComponentVersionArtifact (GetComponentVersionArtifact'),
    newGetComponentVersionArtifact,
    GetComponentVersionArtifactResponse (GetComponentVersionArtifactResponse'),
    newGetComponentVersionArtifactResponse,

    -- ** GetConnectivityInfo
    GetConnectivityInfo (GetConnectivityInfo'),
    newGetConnectivityInfo,
    GetConnectivityInfoResponse (GetConnectivityInfoResponse'),
    newGetConnectivityInfoResponse,

    -- ** GetCoreDevice
    GetCoreDevice (GetCoreDevice'),
    newGetCoreDevice,
    GetCoreDeviceResponse (GetCoreDeviceResponse'),
    newGetCoreDeviceResponse,

    -- ** GetDeployment
    GetDeployment (GetDeployment'),
    newGetDeployment,
    GetDeploymentResponse (GetDeploymentResponse'),
    newGetDeploymentResponse,

    -- ** GetServiceRoleForAccount
    GetServiceRoleForAccount (GetServiceRoleForAccount'),
    newGetServiceRoleForAccount,
    GetServiceRoleForAccountResponse (GetServiceRoleForAccountResponse'),
    newGetServiceRoleForAccountResponse,

    -- ** ListClientDevicesAssociatedWithCoreDevice (Paginated)
    ListClientDevicesAssociatedWithCoreDevice (ListClientDevicesAssociatedWithCoreDevice'),
    newListClientDevicesAssociatedWithCoreDevice,
    ListClientDevicesAssociatedWithCoreDeviceResponse (ListClientDevicesAssociatedWithCoreDeviceResponse'),
    newListClientDevicesAssociatedWithCoreDeviceResponse,

    -- ** ListComponentVersions (Paginated)
    ListComponentVersions (ListComponentVersions'),
    newListComponentVersions,
    ListComponentVersionsResponse (ListComponentVersionsResponse'),
    newListComponentVersionsResponse,

    -- ** ListComponents (Paginated)
    ListComponents (ListComponents'),
    newListComponents,
    ListComponentsResponse (ListComponentsResponse'),
    newListComponentsResponse,

    -- ** ListCoreDevices (Paginated)
    ListCoreDevices (ListCoreDevices'),
    newListCoreDevices,
    ListCoreDevicesResponse (ListCoreDevicesResponse'),
    newListCoreDevicesResponse,

    -- ** ListDeployments (Paginated)
    ListDeployments (ListDeployments'),
    newListDeployments,
    ListDeploymentsResponse (ListDeploymentsResponse'),
    newListDeploymentsResponse,

    -- ** ListEffectiveDeployments (Paginated)
    ListEffectiveDeployments (ListEffectiveDeployments'),
    newListEffectiveDeployments,
    ListEffectiveDeploymentsResponse (ListEffectiveDeploymentsResponse'),
    newListEffectiveDeploymentsResponse,

    -- ** ListInstalledComponents (Paginated)
    ListInstalledComponents (ListInstalledComponents'),
    newListInstalledComponents,
    ListInstalledComponentsResponse (ListInstalledComponentsResponse'),
    newListInstalledComponentsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ResolveComponentCandidates
    ResolveComponentCandidates (ResolveComponentCandidates'),
    newResolveComponentCandidates,
    ResolveComponentCandidatesResponse (ResolveComponentCandidatesResponse'),
    newResolveComponentCandidatesResponse,

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

    -- ** UpdateConnectivityInfo
    UpdateConnectivityInfo (UpdateConnectivityInfo'),
    newUpdateConnectivityInfo,
    UpdateConnectivityInfoResponse (UpdateConnectivityInfoResponse'),
    newUpdateConnectivityInfoResponse,

    -- * Types

    -- ** CloudComponentState
    CloudComponentState (..),

    -- ** ComponentDependencyType
    ComponentDependencyType (..),

    -- ** ComponentVisibilityScope
    ComponentVisibilityScope (..),

    -- ** CoreDeviceStatus
    CoreDeviceStatus (..),

    -- ** DeploymentComponentUpdatePolicyAction
    DeploymentComponentUpdatePolicyAction (..),

    -- ** DeploymentFailureHandlingPolicy
    DeploymentFailureHandlingPolicy (..),

    -- ** DeploymentHistoryFilter
    DeploymentHistoryFilter (..),

    -- ** DeploymentStatus
    DeploymentStatus (..),

    -- ** EffectiveDeploymentExecutionStatus
    EffectiveDeploymentExecutionStatus (..),

    -- ** InstalledComponentLifecycleState
    InstalledComponentLifecycleState (..),

    -- ** InstalledComponentTopologyFilter
    InstalledComponentTopologyFilter (..),

    -- ** IoTJobAbortAction
    IoTJobAbortAction (..),

    -- ** IoTJobExecutionFailureType
    IoTJobExecutionFailureType (..),

    -- ** LambdaEventSourceType
    LambdaEventSourceType (..),

    -- ** LambdaFilesystemPermission
    LambdaFilesystemPermission (..),

    -- ** LambdaInputPayloadEncodingType
    LambdaInputPayloadEncodingType (..),

    -- ** LambdaIsolationMode
    LambdaIsolationMode (..),

    -- ** RecipeOutputFormat
    RecipeOutputFormat (..),

    -- ** VendorGuidance
    VendorGuidance (..),

    -- ** AssociateClientDeviceWithCoreDeviceEntry
    AssociateClientDeviceWithCoreDeviceEntry (AssociateClientDeviceWithCoreDeviceEntry'),
    newAssociateClientDeviceWithCoreDeviceEntry,

    -- ** AssociateClientDeviceWithCoreDeviceErrorEntry
    AssociateClientDeviceWithCoreDeviceErrorEntry (AssociateClientDeviceWithCoreDeviceErrorEntry'),
    newAssociateClientDeviceWithCoreDeviceErrorEntry,

    -- ** AssociatedClientDevice
    AssociatedClientDevice (AssociatedClientDevice'),
    newAssociatedClientDevice,

    -- ** CloudComponentStatus
    CloudComponentStatus (CloudComponentStatus'),
    newCloudComponentStatus,

    -- ** Component
    Component (Component'),
    newComponent,

    -- ** ComponentCandidate
    ComponentCandidate (ComponentCandidate'),
    newComponentCandidate,

    -- ** ComponentConfigurationUpdate
    ComponentConfigurationUpdate (ComponentConfigurationUpdate'),
    newComponentConfigurationUpdate,

    -- ** ComponentDependencyRequirement
    ComponentDependencyRequirement (ComponentDependencyRequirement'),
    newComponentDependencyRequirement,

    -- ** ComponentDeploymentSpecification
    ComponentDeploymentSpecification (ComponentDeploymentSpecification'),
    newComponentDeploymentSpecification,

    -- ** ComponentLatestVersion
    ComponentLatestVersion (ComponentLatestVersion'),
    newComponentLatestVersion,

    -- ** ComponentPlatform
    ComponentPlatform (ComponentPlatform'),
    newComponentPlatform,

    -- ** ComponentRunWith
    ComponentRunWith (ComponentRunWith'),
    newComponentRunWith,

    -- ** ComponentVersionListItem
    ComponentVersionListItem (ComponentVersionListItem'),
    newComponentVersionListItem,

    -- ** ConnectivityInfo
    ConnectivityInfo (ConnectivityInfo'),
    newConnectivityInfo,

    -- ** CoreDevice
    CoreDevice (CoreDevice'),
    newCoreDevice,

    -- ** Deployment
    Deployment (Deployment'),
    newDeployment,

    -- ** DeploymentComponentUpdatePolicy
    DeploymentComponentUpdatePolicy (DeploymentComponentUpdatePolicy'),
    newDeploymentComponentUpdatePolicy,

    -- ** DeploymentConfigurationValidationPolicy
    DeploymentConfigurationValidationPolicy (DeploymentConfigurationValidationPolicy'),
    newDeploymentConfigurationValidationPolicy,

    -- ** DeploymentIoTJobConfiguration
    DeploymentIoTJobConfiguration (DeploymentIoTJobConfiguration'),
    newDeploymentIoTJobConfiguration,

    -- ** DeploymentPolicies
    DeploymentPolicies (DeploymentPolicies'),
    newDeploymentPolicies,

    -- ** DisassociateClientDeviceFromCoreDeviceEntry
    DisassociateClientDeviceFromCoreDeviceEntry (DisassociateClientDeviceFromCoreDeviceEntry'),
    newDisassociateClientDeviceFromCoreDeviceEntry,

    -- ** DisassociateClientDeviceFromCoreDeviceErrorEntry
    DisassociateClientDeviceFromCoreDeviceErrorEntry (DisassociateClientDeviceFromCoreDeviceErrorEntry'),
    newDisassociateClientDeviceFromCoreDeviceErrorEntry,

    -- ** EffectiveDeployment
    EffectiveDeployment (EffectiveDeployment'),
    newEffectiveDeployment,

    -- ** EffectiveDeploymentStatusDetails
    EffectiveDeploymentStatusDetails (EffectiveDeploymentStatusDetails'),
    newEffectiveDeploymentStatusDetails,

    -- ** InstalledComponent
    InstalledComponent (InstalledComponent'),
    newInstalledComponent,

    -- ** IoTJobAbortConfig
    IoTJobAbortConfig (IoTJobAbortConfig'),
    newIoTJobAbortConfig,

    -- ** IoTJobAbortCriteria
    IoTJobAbortCriteria (IoTJobAbortCriteria'),
    newIoTJobAbortCriteria,

    -- ** IoTJobExecutionsRolloutConfig
    IoTJobExecutionsRolloutConfig (IoTJobExecutionsRolloutConfig'),
    newIoTJobExecutionsRolloutConfig,

    -- ** IoTJobExponentialRolloutRate
    IoTJobExponentialRolloutRate (IoTJobExponentialRolloutRate'),
    newIoTJobExponentialRolloutRate,

    -- ** IoTJobRateIncreaseCriteria
    IoTJobRateIncreaseCriteria (IoTJobRateIncreaseCriteria'),
    newIoTJobRateIncreaseCriteria,

    -- ** IoTJobTimeoutConfig
    IoTJobTimeoutConfig (IoTJobTimeoutConfig'),
    newIoTJobTimeoutConfig,

    -- ** LambdaContainerParams
    LambdaContainerParams (LambdaContainerParams'),
    newLambdaContainerParams,

    -- ** LambdaDeviceMount
    LambdaDeviceMount (LambdaDeviceMount'),
    newLambdaDeviceMount,

    -- ** LambdaEventSource
    LambdaEventSource (LambdaEventSource'),
    newLambdaEventSource,

    -- ** LambdaExecutionParameters
    LambdaExecutionParameters (LambdaExecutionParameters'),
    newLambdaExecutionParameters,

    -- ** LambdaFunctionRecipeSource
    LambdaFunctionRecipeSource (LambdaFunctionRecipeSource'),
    newLambdaFunctionRecipeSource,

    -- ** LambdaLinuxProcessParams
    LambdaLinuxProcessParams (LambdaLinuxProcessParams'),
    newLambdaLinuxProcessParams,

    -- ** LambdaVolumeMount
    LambdaVolumeMount (LambdaVolumeMount'),
    newLambdaVolumeMount,

    -- ** ResolvedComponentVersion
    ResolvedComponentVersion (ResolvedComponentVersion'),
    newResolvedComponentVersion,

    -- ** SystemResourceLimits
    SystemResourceLimits (SystemResourceLimits'),
    newSystemResourceLimits,
  )
where

import Amazonka.GreengrassV2.AssociateServiceRoleToAccount
import Amazonka.GreengrassV2.BatchAssociateClientDeviceWithCoreDevice
import Amazonka.GreengrassV2.BatchDisassociateClientDeviceFromCoreDevice
import Amazonka.GreengrassV2.CancelDeployment
import Amazonka.GreengrassV2.CreateComponentVersion
import Amazonka.GreengrassV2.CreateDeployment
import Amazonka.GreengrassV2.DeleteComponent
import Amazonka.GreengrassV2.DeleteCoreDevice
import Amazonka.GreengrassV2.DeleteDeployment
import Amazonka.GreengrassV2.DescribeComponent
import Amazonka.GreengrassV2.DisassociateServiceRoleFromAccount
import Amazonka.GreengrassV2.GetComponent
import Amazonka.GreengrassV2.GetComponentVersionArtifact
import Amazonka.GreengrassV2.GetConnectivityInfo
import Amazonka.GreengrassV2.GetCoreDevice
import Amazonka.GreengrassV2.GetDeployment
import Amazonka.GreengrassV2.GetServiceRoleForAccount
import Amazonka.GreengrassV2.Lens
import Amazonka.GreengrassV2.ListClientDevicesAssociatedWithCoreDevice
import Amazonka.GreengrassV2.ListComponentVersions
import Amazonka.GreengrassV2.ListComponents
import Amazonka.GreengrassV2.ListCoreDevices
import Amazonka.GreengrassV2.ListDeployments
import Amazonka.GreengrassV2.ListEffectiveDeployments
import Amazonka.GreengrassV2.ListInstalledComponents
import Amazonka.GreengrassV2.ListTagsForResource
import Amazonka.GreengrassV2.ResolveComponentCandidates
import Amazonka.GreengrassV2.TagResource
import Amazonka.GreengrassV2.Types
import Amazonka.GreengrassV2.UntagResource
import Amazonka.GreengrassV2.UpdateConnectivityInfo
import Amazonka.GreengrassV2.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'GreengrassV2'.

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

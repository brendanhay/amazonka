{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.EKS
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-11-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Elastic Kubernetes Service (Amazon EKS) is a managed service that
-- makes it easy for you to run Kubernetes on Amazon Web Services without
-- needing to stand up or maintain your own Kubernetes control plane.
-- Kubernetes is an open-source system for automating the deployment,
-- scaling, and management of containerized applications.
--
-- Amazon EKS runs up-to-date versions of the open-source Kubernetes
-- software, so you can use all the existing plugins and tooling from the
-- Kubernetes community. Applications running on Amazon EKS are fully
-- compatible with applications running on any standard Kubernetes
-- environment, whether running in on-premises data centers or public
-- clouds. This means that you can easily migrate any standard Kubernetes
-- application to Amazon EKS without any code modification required.
module Amazonka.EKS
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** BadRequestException
    _BadRequestException,

    -- ** ClientException
    _ClientException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** ResourceLimitExceededException
    _ResourceLimitExceededException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ResourcePropagationDelayException
    _ResourcePropagationDelayException,

    -- ** ServerException
    _ServerException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** UnsupportedAvailabilityZoneException
    _UnsupportedAvailabilityZoneException,

    -- * Waiters
    -- $waiters

    -- ** AddonActive
    newAddonActive,

    -- ** AddonDeleted
    newAddonDeleted,

    -- ** ClusterActive
    newClusterActive,

    -- ** ClusterDeleted
    newClusterDeleted,

    -- ** FargateProfileActive
    newFargateProfileActive,

    -- ** FargateProfileDeleted
    newFargateProfileDeleted,

    -- ** NodegroupActive
    newNodegroupActive,

    -- ** NodegroupDeleted
    newNodegroupDeleted,

    -- * Operations
    -- $operations

    -- ** AssociateEncryptionConfig
    AssociateEncryptionConfig (AssociateEncryptionConfig'),
    newAssociateEncryptionConfig,
    AssociateEncryptionConfigResponse (AssociateEncryptionConfigResponse'),
    newAssociateEncryptionConfigResponse,

    -- ** AssociateIdentityProviderConfig
    AssociateIdentityProviderConfig (AssociateIdentityProviderConfig'),
    newAssociateIdentityProviderConfig,
    AssociateIdentityProviderConfigResponse (AssociateIdentityProviderConfigResponse'),
    newAssociateIdentityProviderConfigResponse,

    -- ** CreateAddon
    CreateAddon (CreateAddon'),
    newCreateAddon,
    CreateAddonResponse (CreateAddonResponse'),
    newCreateAddonResponse,

    -- ** CreateCluster
    CreateCluster (CreateCluster'),
    newCreateCluster,
    CreateClusterResponse (CreateClusterResponse'),
    newCreateClusterResponse,

    -- ** CreateFargateProfile
    CreateFargateProfile (CreateFargateProfile'),
    newCreateFargateProfile,
    CreateFargateProfileResponse (CreateFargateProfileResponse'),
    newCreateFargateProfileResponse,

    -- ** CreateNodegroup
    CreateNodegroup (CreateNodegroup'),
    newCreateNodegroup,
    CreateNodegroupResponse (CreateNodegroupResponse'),
    newCreateNodegroupResponse,

    -- ** DeleteAddon
    DeleteAddon (DeleteAddon'),
    newDeleteAddon,
    DeleteAddonResponse (DeleteAddonResponse'),
    newDeleteAddonResponse,

    -- ** DeleteCluster
    DeleteCluster (DeleteCluster'),
    newDeleteCluster,
    DeleteClusterResponse (DeleteClusterResponse'),
    newDeleteClusterResponse,

    -- ** DeleteFargateProfile
    DeleteFargateProfile (DeleteFargateProfile'),
    newDeleteFargateProfile,
    DeleteFargateProfileResponse (DeleteFargateProfileResponse'),
    newDeleteFargateProfileResponse,

    -- ** DeleteNodegroup
    DeleteNodegroup (DeleteNodegroup'),
    newDeleteNodegroup,
    DeleteNodegroupResponse (DeleteNodegroupResponse'),
    newDeleteNodegroupResponse,

    -- ** DeregisterCluster
    DeregisterCluster (DeregisterCluster'),
    newDeregisterCluster,
    DeregisterClusterResponse (DeregisterClusterResponse'),
    newDeregisterClusterResponse,

    -- ** DescribeAddon
    DescribeAddon (DescribeAddon'),
    newDescribeAddon,
    DescribeAddonResponse (DescribeAddonResponse'),
    newDescribeAddonResponse,

    -- ** DescribeAddonConfiguration
    DescribeAddonConfiguration (DescribeAddonConfiguration'),
    newDescribeAddonConfiguration,
    DescribeAddonConfigurationResponse (DescribeAddonConfigurationResponse'),
    newDescribeAddonConfigurationResponse,

    -- ** DescribeAddonVersions (Paginated)
    DescribeAddonVersions (DescribeAddonVersions'),
    newDescribeAddonVersions,
    DescribeAddonVersionsResponse (DescribeAddonVersionsResponse'),
    newDescribeAddonVersionsResponse,

    -- ** DescribeCluster
    DescribeCluster (DescribeCluster'),
    newDescribeCluster,
    DescribeClusterResponse (DescribeClusterResponse'),
    newDescribeClusterResponse,

    -- ** DescribeFargateProfile
    DescribeFargateProfile (DescribeFargateProfile'),
    newDescribeFargateProfile,
    DescribeFargateProfileResponse (DescribeFargateProfileResponse'),
    newDescribeFargateProfileResponse,

    -- ** DescribeIdentityProviderConfig
    DescribeIdentityProviderConfig (DescribeIdentityProviderConfig'),
    newDescribeIdentityProviderConfig,
    DescribeIdentityProviderConfigResponse (DescribeIdentityProviderConfigResponse'),
    newDescribeIdentityProviderConfigResponse,

    -- ** DescribeNodegroup
    DescribeNodegroup (DescribeNodegroup'),
    newDescribeNodegroup,
    DescribeNodegroupResponse (DescribeNodegroupResponse'),
    newDescribeNodegroupResponse,

    -- ** DescribeUpdate
    DescribeUpdate (DescribeUpdate'),
    newDescribeUpdate,
    DescribeUpdateResponse (DescribeUpdateResponse'),
    newDescribeUpdateResponse,

    -- ** DisassociateIdentityProviderConfig
    DisassociateIdentityProviderConfig (DisassociateIdentityProviderConfig'),
    newDisassociateIdentityProviderConfig,
    DisassociateIdentityProviderConfigResponse (DisassociateIdentityProviderConfigResponse'),
    newDisassociateIdentityProviderConfigResponse,

    -- ** ListAddons (Paginated)
    ListAddons (ListAddons'),
    newListAddons,
    ListAddonsResponse (ListAddonsResponse'),
    newListAddonsResponse,

    -- ** ListClusters (Paginated)
    ListClusters (ListClusters'),
    newListClusters,
    ListClustersResponse (ListClustersResponse'),
    newListClustersResponse,

    -- ** ListFargateProfiles (Paginated)
    ListFargateProfiles (ListFargateProfiles'),
    newListFargateProfiles,
    ListFargateProfilesResponse (ListFargateProfilesResponse'),
    newListFargateProfilesResponse,

    -- ** ListIdentityProviderConfigs (Paginated)
    ListIdentityProviderConfigs (ListIdentityProviderConfigs'),
    newListIdentityProviderConfigs,
    ListIdentityProviderConfigsResponse (ListIdentityProviderConfigsResponse'),
    newListIdentityProviderConfigsResponse,

    -- ** ListNodegroups (Paginated)
    ListNodegroups (ListNodegroups'),
    newListNodegroups,
    ListNodegroupsResponse (ListNodegroupsResponse'),
    newListNodegroupsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListUpdates (Paginated)
    ListUpdates (ListUpdates'),
    newListUpdates,
    ListUpdatesResponse (ListUpdatesResponse'),
    newListUpdatesResponse,

    -- ** RegisterCluster
    RegisterCluster (RegisterCluster'),
    newRegisterCluster,
    RegisterClusterResponse (RegisterClusterResponse'),
    newRegisterClusterResponse,

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

    -- ** UpdateAddon
    UpdateAddon (UpdateAddon'),
    newUpdateAddon,
    UpdateAddonResponse (UpdateAddonResponse'),
    newUpdateAddonResponse,

    -- ** UpdateClusterConfig
    UpdateClusterConfig (UpdateClusterConfig'),
    newUpdateClusterConfig,
    UpdateClusterConfigResponse (UpdateClusterConfigResponse'),
    newUpdateClusterConfigResponse,

    -- ** UpdateClusterVersion
    UpdateClusterVersion (UpdateClusterVersion'),
    newUpdateClusterVersion,
    UpdateClusterVersionResponse (UpdateClusterVersionResponse'),
    newUpdateClusterVersionResponse,

    -- ** UpdateNodegroupConfig
    UpdateNodegroupConfig (UpdateNodegroupConfig'),
    newUpdateNodegroupConfig,
    UpdateNodegroupConfigResponse (UpdateNodegroupConfigResponse'),
    newUpdateNodegroupConfigResponse,

    -- ** UpdateNodegroupVersion
    UpdateNodegroupVersion (UpdateNodegroupVersion'),
    newUpdateNodegroupVersion,
    UpdateNodegroupVersionResponse (UpdateNodegroupVersionResponse'),
    newUpdateNodegroupVersionResponse,

    -- * Types

    -- ** AMITypes
    AMITypes (..),

    -- ** AddonIssueCode
    AddonIssueCode (..),

    -- ** AddonStatus
    AddonStatus (..),

    -- ** CapacityTypes
    CapacityTypes (..),

    -- ** ClusterIssueCode
    ClusterIssueCode (..),

    -- ** ClusterStatus
    ClusterStatus (..),

    -- ** ConfigStatus
    ConfigStatus (..),

    -- ** ConnectorConfigProvider
    ConnectorConfigProvider (..),

    -- ** EKSErrorCode
    EKSErrorCode (..),

    -- ** FargateProfileStatus
    FargateProfileStatus (..),

    -- ** IpFamily
    IpFamily (..),

    -- ** LogType
    LogType (..),

    -- ** NodegroupIssueCode
    NodegroupIssueCode (..),

    -- ** NodegroupStatus
    NodegroupStatus (..),

    -- ** ResolveConflicts
    ResolveConflicts (..),

    -- ** TaintEffect
    TaintEffect (..),

    -- ** UpdateParamType
    UpdateParamType (..),

    -- ** UpdateStatus
    UpdateStatus (..),

    -- ** UpdateType
    UpdateType (..),

    -- ** Addon
    Addon (Addon'),
    newAddon,

    -- ** AddonHealth
    AddonHealth (AddonHealth'),
    newAddonHealth,

    -- ** AddonInfo
    AddonInfo (AddonInfo'),
    newAddonInfo,

    -- ** AddonIssue
    AddonIssue (AddonIssue'),
    newAddonIssue,

    -- ** AddonVersionInfo
    AddonVersionInfo (AddonVersionInfo'),
    newAddonVersionInfo,

    -- ** AutoScalingGroup
    AutoScalingGroup (AutoScalingGroup'),
    newAutoScalingGroup,

    -- ** Certificate
    Certificate (Certificate'),
    newCertificate,

    -- ** Cluster
    Cluster (Cluster'),
    newCluster,

    -- ** ClusterHealth
    ClusterHealth (ClusterHealth'),
    newClusterHealth,

    -- ** ClusterIssue
    ClusterIssue (ClusterIssue'),
    newClusterIssue,

    -- ** Compatibility
    Compatibility (Compatibility'),
    newCompatibility,

    -- ** ConnectorConfigRequest
    ConnectorConfigRequest (ConnectorConfigRequest'),
    newConnectorConfigRequest,

    -- ** ConnectorConfigResponse
    ConnectorConfigResponse (ConnectorConfigResponse'),
    newConnectorConfigResponse,

    -- ** ControlPlanePlacementRequest
    ControlPlanePlacementRequest (ControlPlanePlacementRequest'),
    newControlPlanePlacementRequest,

    -- ** ControlPlanePlacementResponse
    ControlPlanePlacementResponse (ControlPlanePlacementResponse'),
    newControlPlanePlacementResponse,

    -- ** EncryptionConfig
    EncryptionConfig (EncryptionConfig'),
    newEncryptionConfig,

    -- ** ErrorDetail
    ErrorDetail (ErrorDetail'),
    newErrorDetail,

    -- ** FargateProfile
    FargateProfile (FargateProfile'),
    newFargateProfile,

    -- ** FargateProfileSelector
    FargateProfileSelector (FargateProfileSelector'),
    newFargateProfileSelector,

    -- ** Identity
    Identity (Identity'),
    newIdentity,

    -- ** IdentityProviderConfig
    IdentityProviderConfig (IdentityProviderConfig'),
    newIdentityProviderConfig,

    -- ** IdentityProviderConfigResponse
    IdentityProviderConfigResponse (IdentityProviderConfigResponse'),
    newIdentityProviderConfigResponse,

    -- ** Issue
    Issue (Issue'),
    newIssue,

    -- ** KubernetesNetworkConfigRequest
    KubernetesNetworkConfigRequest (KubernetesNetworkConfigRequest'),
    newKubernetesNetworkConfigRequest,

    -- ** KubernetesNetworkConfigResponse
    KubernetesNetworkConfigResponse (KubernetesNetworkConfigResponse'),
    newKubernetesNetworkConfigResponse,

    -- ** LaunchTemplateSpecification
    LaunchTemplateSpecification (LaunchTemplateSpecification'),
    newLaunchTemplateSpecification,

    -- ** LogSetup
    LogSetup (LogSetup'),
    newLogSetup,

    -- ** Logging
    Logging (Logging'),
    newLogging,

    -- ** MarketplaceInformation
    MarketplaceInformation (MarketplaceInformation'),
    newMarketplaceInformation,

    -- ** Nodegroup
    Nodegroup (Nodegroup'),
    newNodegroup,

    -- ** NodegroupHealth
    NodegroupHealth (NodegroupHealth'),
    newNodegroupHealth,

    -- ** NodegroupResources
    NodegroupResources (NodegroupResources'),
    newNodegroupResources,

    -- ** NodegroupScalingConfig
    NodegroupScalingConfig (NodegroupScalingConfig'),
    newNodegroupScalingConfig,

    -- ** NodegroupUpdateConfig
    NodegroupUpdateConfig (NodegroupUpdateConfig'),
    newNodegroupUpdateConfig,

    -- ** OIDC
    OIDC (OIDC'),
    newOIDC,

    -- ** OidcIdentityProviderConfig
    OidcIdentityProviderConfig (OidcIdentityProviderConfig'),
    newOidcIdentityProviderConfig,

    -- ** OidcIdentityProviderConfigRequest
    OidcIdentityProviderConfigRequest (OidcIdentityProviderConfigRequest'),
    newOidcIdentityProviderConfigRequest,

    -- ** OutpostConfigRequest
    OutpostConfigRequest (OutpostConfigRequest'),
    newOutpostConfigRequest,

    -- ** OutpostConfigResponse
    OutpostConfigResponse (OutpostConfigResponse'),
    newOutpostConfigResponse,

    -- ** Provider
    Provider (Provider'),
    newProvider,

    -- ** RemoteAccessConfig
    RemoteAccessConfig (RemoteAccessConfig'),
    newRemoteAccessConfig,

    -- ** Taint
    Taint (Taint'),
    newTaint,

    -- ** Update
    Update (Update'),
    newUpdate,

    -- ** UpdateLabelsPayload
    UpdateLabelsPayload (UpdateLabelsPayload'),
    newUpdateLabelsPayload,

    -- ** UpdateParam
    UpdateParam (UpdateParam'),
    newUpdateParam,

    -- ** UpdateTaintsPayload
    UpdateTaintsPayload (UpdateTaintsPayload'),
    newUpdateTaintsPayload,

    -- ** VpcConfigRequest
    VpcConfigRequest (VpcConfigRequest'),
    newVpcConfigRequest,

    -- ** VpcConfigResponse
    VpcConfigResponse (VpcConfigResponse'),
    newVpcConfigResponse,
  )
where

import Amazonka.EKS.AssociateEncryptionConfig
import Amazonka.EKS.AssociateIdentityProviderConfig
import Amazonka.EKS.CreateAddon
import Amazonka.EKS.CreateCluster
import Amazonka.EKS.CreateFargateProfile
import Amazonka.EKS.CreateNodegroup
import Amazonka.EKS.DeleteAddon
import Amazonka.EKS.DeleteCluster
import Amazonka.EKS.DeleteFargateProfile
import Amazonka.EKS.DeleteNodegroup
import Amazonka.EKS.DeregisterCluster
import Amazonka.EKS.DescribeAddon
import Amazonka.EKS.DescribeAddonConfiguration
import Amazonka.EKS.DescribeAddonVersions
import Amazonka.EKS.DescribeCluster
import Amazonka.EKS.DescribeFargateProfile
import Amazonka.EKS.DescribeIdentityProviderConfig
import Amazonka.EKS.DescribeNodegroup
import Amazonka.EKS.DescribeUpdate
import Amazonka.EKS.DisassociateIdentityProviderConfig
import Amazonka.EKS.Lens
import Amazonka.EKS.ListAddons
import Amazonka.EKS.ListClusters
import Amazonka.EKS.ListFargateProfiles
import Amazonka.EKS.ListIdentityProviderConfigs
import Amazonka.EKS.ListNodegroups
import Amazonka.EKS.ListTagsForResource
import Amazonka.EKS.ListUpdates
import Amazonka.EKS.RegisterCluster
import Amazonka.EKS.TagResource
import Amazonka.EKS.Types
import Amazonka.EKS.UntagResource
import Amazonka.EKS.UpdateAddon
import Amazonka.EKS.UpdateClusterConfig
import Amazonka.EKS.UpdateClusterVersion
import Amazonka.EKS.UpdateNodegroupConfig
import Amazonka.EKS.UpdateNodegroupVersion
import Amazonka.EKS.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'EKS'.

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

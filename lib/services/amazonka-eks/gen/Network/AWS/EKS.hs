{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.EKS
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.EKS
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** NotFoundException
    _NotFoundException,

    -- ** BadRequestException
    _BadRequestException,

    -- ** ResourceLimitExceededException
    _ResourceLimitExceededException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ClientException
    _ClientException,

    -- ** UnsupportedAvailabilityZoneException
    _UnsupportedAvailabilityZoneException,

    -- ** ServerException
    _ServerException,

    -- * Waiters
    -- $waiters

    -- ** FargateProfileDeleted
    newFargateProfileDeleted,

    -- ** ClusterActive
    newClusterActive,

    -- ** NodegroupActive
    newNodegroupActive,

    -- ** NodegroupDeleted
    newNodegroupDeleted,

    -- ** ClusterDeleted
    newClusterDeleted,

    -- ** AddonActive
    newAddonActive,

    -- ** FargateProfileActive
    newFargateProfileActive,

    -- ** AddonDeleted
    newAddonDeleted,

    -- * Operations
    -- $operations

    -- ** DescribeUpdate
    DescribeUpdate (DescribeUpdate'),
    newDescribeUpdate,
    DescribeUpdateResponse (DescribeUpdateResponse'),
    newDescribeUpdateResponse,

    -- ** DescribeFargateProfile
    DescribeFargateProfile (DescribeFargateProfile'),
    newDescribeFargateProfile,
    DescribeFargateProfileResponse (DescribeFargateProfileResponse'),
    newDescribeFargateProfileResponse,

    -- ** DescribeAddonVersions (Paginated)
    DescribeAddonVersions (DescribeAddonVersions'),
    newDescribeAddonVersions,
    DescribeAddonVersionsResponse (DescribeAddonVersionsResponse'),
    newDescribeAddonVersionsResponse,

    -- ** ListIdentityProviderConfigs (Paginated)
    ListIdentityProviderConfigs (ListIdentityProviderConfigs'),
    newListIdentityProviderConfigs,
    ListIdentityProviderConfigsResponse (ListIdentityProviderConfigsResponse'),
    newListIdentityProviderConfigsResponse,

    -- ** ListAddons (Paginated)
    ListAddons (ListAddons'),
    newListAddons,
    ListAddonsResponse (ListAddonsResponse'),
    newListAddonsResponse,

    -- ** AssociateIdentityProviderConfig
    AssociateIdentityProviderConfig (AssociateIdentityProviderConfig'),
    newAssociateIdentityProviderConfig,
    AssociateIdentityProviderConfigResponse (AssociateIdentityProviderConfigResponse'),
    newAssociateIdentityProviderConfigResponse,

    -- ** UpdateClusterVersion
    UpdateClusterVersion (UpdateClusterVersion'),
    newUpdateClusterVersion,
    UpdateClusterVersionResponse (UpdateClusterVersionResponse'),
    newUpdateClusterVersionResponse,

    -- ** DeleteAddon
    DeleteAddon (DeleteAddon'),
    newDeleteAddon,
    DeleteAddonResponse (DeleteAddonResponse'),
    newDeleteAddonResponse,

    -- ** AssociateEncryptionConfig
    AssociateEncryptionConfig (AssociateEncryptionConfig'),
    newAssociateEncryptionConfig,
    AssociateEncryptionConfigResponse (AssociateEncryptionConfigResponse'),
    newAssociateEncryptionConfigResponse,

    -- ** UpdateAddon
    UpdateAddon (UpdateAddon'),
    newUpdateAddon,
    UpdateAddonResponse (UpdateAddonResponse'),
    newUpdateAddonResponse,

    -- ** ListNodegroups (Paginated)
    ListNodegroups (ListNodegroups'),
    newListNodegroups,
    ListNodegroupsResponse (ListNodegroupsResponse'),
    newListNodegroupsResponse,

    -- ** CreateCluster
    CreateCluster (CreateCluster'),
    newCreateCluster,
    CreateClusterResponse (CreateClusterResponse'),
    newCreateClusterResponse,

    -- ** RegisterCluster
    RegisterCluster (RegisterCluster'),
    newRegisterCluster,
    RegisterClusterResponse (RegisterClusterResponse'),
    newRegisterClusterResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** CreateNodegroup
    CreateNodegroup (CreateNodegroup'),
    newCreateNodegroup,
    CreateNodegroupResponse (CreateNodegroupResponse'),
    newCreateNodegroupResponse,

    -- ** DescribeIdentityProviderConfig
    DescribeIdentityProviderConfig (DescribeIdentityProviderConfig'),
    newDescribeIdentityProviderConfig,
    DescribeIdentityProviderConfigResponse (DescribeIdentityProviderConfigResponse'),
    newDescribeIdentityProviderConfigResponse,

    -- ** DeleteFargateProfile
    DeleteFargateProfile (DeleteFargateProfile'),
    newDeleteFargateProfile,
    DeleteFargateProfileResponse (DeleteFargateProfileResponse'),
    newDeleteFargateProfileResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** ListFargateProfiles (Paginated)
    ListFargateProfiles (ListFargateProfiles'),
    newListFargateProfiles,
    ListFargateProfilesResponse (ListFargateProfilesResponse'),
    newListFargateProfilesResponse,

    -- ** ListUpdates (Paginated)
    ListUpdates (ListUpdates'),
    newListUpdates,
    ListUpdatesResponse (ListUpdatesResponse'),
    newListUpdatesResponse,

    -- ** DisassociateIdentityProviderConfig
    DisassociateIdentityProviderConfig (DisassociateIdentityProviderConfig'),
    newDisassociateIdentityProviderConfig,
    DisassociateIdentityProviderConfigResponse (DisassociateIdentityProviderConfigResponse'),
    newDisassociateIdentityProviderConfigResponse,

    -- ** DescribeCluster
    DescribeCluster (DescribeCluster'),
    newDescribeCluster,
    DescribeClusterResponse (DescribeClusterResponse'),
    newDescribeClusterResponse,

    -- ** UpdateNodegroupVersion
    UpdateNodegroupVersion (UpdateNodegroupVersion'),
    newUpdateNodegroupVersion,
    UpdateNodegroupVersionResponse (UpdateNodegroupVersionResponse'),
    newUpdateNodegroupVersionResponse,

    -- ** CreateAddon
    CreateAddon (CreateAddon'),
    newCreateAddon,
    CreateAddonResponse (CreateAddonResponse'),
    newCreateAddonResponse,

    -- ** UpdateNodegroupConfig
    UpdateNodegroupConfig (UpdateNodegroupConfig'),
    newUpdateNodegroupConfig,
    UpdateNodegroupConfigResponse (UpdateNodegroupConfigResponse'),
    newUpdateNodegroupConfigResponse,

    -- ** UpdateClusterConfig
    UpdateClusterConfig (UpdateClusterConfig'),
    newUpdateClusterConfig,
    UpdateClusterConfigResponse (UpdateClusterConfigResponse'),
    newUpdateClusterConfigResponse,

    -- ** DeleteNodegroup
    DeleteNodegroup (DeleteNodegroup'),
    newDeleteNodegroup,
    DeleteNodegroupResponse (DeleteNodegroupResponse'),
    newDeleteNodegroupResponse,

    -- ** ListClusters (Paginated)
    ListClusters (ListClusters'),
    newListClusters,
    ListClustersResponse (ListClustersResponse'),
    newListClustersResponse,

    -- ** DeleteCluster
    DeleteCluster (DeleteCluster'),
    newDeleteCluster,
    DeleteClusterResponse (DeleteClusterResponse'),
    newDeleteClusterResponse,

    -- ** CreateFargateProfile
    CreateFargateProfile (CreateFargateProfile'),
    newCreateFargateProfile,
    CreateFargateProfileResponse (CreateFargateProfileResponse'),
    newCreateFargateProfileResponse,

    -- ** DescribeAddon
    DescribeAddon (DescribeAddon'),
    newDescribeAddon,
    DescribeAddonResponse (DescribeAddonResponse'),
    newDescribeAddonResponse,

    -- ** DescribeNodegroup
    DescribeNodegroup (DescribeNodegroup'),
    newDescribeNodegroup,
    DescribeNodegroupResponse (DescribeNodegroupResponse'),
    newDescribeNodegroupResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** DeregisterCluster
    DeregisterCluster (DeregisterCluster'),
    newDeregisterCluster,
    DeregisterClusterResponse (DeregisterClusterResponse'),
    newDeregisterClusterResponse,

    -- * Types

    -- ** AMITypes
    AMITypes (..),

    -- ** AddonIssueCode
    AddonIssueCode (..),

    -- ** AddonStatus
    AddonStatus (..),

    -- ** CapacityTypes
    CapacityTypes (..),

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

    -- ** Compatibility
    Compatibility (Compatibility'),
    newCompatibility,

    -- ** ConnectorConfigRequest
    ConnectorConfigRequest (ConnectorConfigRequest'),
    newConnectorConfigRequest,

    -- ** ConnectorConfigResponse
    ConnectorConfigResponse (ConnectorConfigResponse'),
    newConnectorConfigResponse,

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

import Network.AWS.EKS.AssociateEncryptionConfig
import Network.AWS.EKS.AssociateIdentityProviderConfig
import Network.AWS.EKS.CreateAddon
import Network.AWS.EKS.CreateCluster
import Network.AWS.EKS.CreateFargateProfile
import Network.AWS.EKS.CreateNodegroup
import Network.AWS.EKS.DeleteAddon
import Network.AWS.EKS.DeleteCluster
import Network.AWS.EKS.DeleteFargateProfile
import Network.AWS.EKS.DeleteNodegroup
import Network.AWS.EKS.DeregisterCluster
import Network.AWS.EKS.DescribeAddon
import Network.AWS.EKS.DescribeAddonVersions
import Network.AWS.EKS.DescribeCluster
import Network.AWS.EKS.DescribeFargateProfile
import Network.AWS.EKS.DescribeIdentityProviderConfig
import Network.AWS.EKS.DescribeNodegroup
import Network.AWS.EKS.DescribeUpdate
import Network.AWS.EKS.DisassociateIdentityProviderConfig
import Network.AWS.EKS.Lens
import Network.AWS.EKS.ListAddons
import Network.AWS.EKS.ListClusters
import Network.AWS.EKS.ListFargateProfiles
import Network.AWS.EKS.ListIdentityProviderConfigs
import Network.AWS.EKS.ListNodegroups
import Network.AWS.EKS.ListTagsForResource
import Network.AWS.EKS.ListUpdates
import Network.AWS.EKS.RegisterCluster
import Network.AWS.EKS.TagResource
import Network.AWS.EKS.Types
import Network.AWS.EKS.UntagResource
import Network.AWS.EKS.UpdateAddon
import Network.AWS.EKS.UpdateClusterConfig
import Network.AWS.EKS.UpdateClusterVersion
import Network.AWS.EKS.UpdateNodegroupConfig
import Network.AWS.EKS.UpdateNodegroupVersion
import Network.AWS.EKS.Waiters

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

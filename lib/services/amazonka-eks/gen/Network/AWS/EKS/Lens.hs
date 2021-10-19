{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EKS.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Lens
  ( -- * Operations

    -- ** CreateAddon
    createAddon_addonVersion,
    createAddon_serviceAccountRoleArn,
    createAddon_resolveConflicts,
    createAddon_clientRequestToken,
    createAddon_tags,
    createAddon_clusterName,
    createAddon_addonName,
    createAddonResponse_addon,
    createAddonResponse_httpStatus,

    -- ** DescribeFargateProfile
    describeFargateProfile_clusterName,
    describeFargateProfile_fargateProfileName,
    describeFargateProfileResponse_fargateProfile,
    describeFargateProfileResponse_httpStatus,

    -- ** DescribeUpdate
    describeUpdate_addonName,
    describeUpdate_nodegroupName,
    describeUpdate_name,
    describeUpdate_updateId,
    describeUpdateResponse_update,
    describeUpdateResponse_httpStatus,

    -- ** UpdateNodegroupConfig
    updateNodegroupConfig_taints,
    updateNodegroupConfig_scalingConfig,
    updateNodegroupConfig_labels,
    updateNodegroupConfig_clientRequestToken,
    updateNodegroupConfig_updateConfig,
    updateNodegroupConfig_clusterName,
    updateNodegroupConfig_nodegroupName,
    updateNodegroupConfigResponse_update,
    updateNodegroupConfigResponse_httpStatus,

    -- ** DescribeCluster
    describeCluster_name,
    describeClusterResponse_cluster,
    describeClusterResponse_httpStatus,

    -- ** DeregisterCluster
    deregisterCluster_name,
    deregisterClusterResponse_cluster,
    deregisterClusterResponse_httpStatus,

    -- ** DescribeNodegroup
    describeNodegroup_clusterName,
    describeNodegroup_nodegroupName,
    describeNodegroupResponse_nodegroup,
    describeNodegroupResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** CreateFargateProfile
    createFargateProfile_subnets,
    createFargateProfile_clientRequestToken,
    createFargateProfile_selectors,
    createFargateProfile_tags,
    createFargateProfile_fargateProfileName,
    createFargateProfile_clusterName,
    createFargateProfile_podExecutionRoleArn,
    createFargateProfileResponse_fargateProfile,
    createFargateProfileResponse_httpStatus,

    -- ** DescribeIdentityProviderConfig
    describeIdentityProviderConfig_clusterName,
    describeIdentityProviderConfig_identityProviderConfig,
    describeIdentityProviderConfigResponse_identityProviderConfig,
    describeIdentityProviderConfigResponse_httpStatus,

    -- ** DeleteFargateProfile
    deleteFargateProfile_clusterName,
    deleteFargateProfile_fargateProfileName,
    deleteFargateProfileResponse_fargateProfile,
    deleteFargateProfileResponse_httpStatus,

    -- ** DeleteCluster
    deleteCluster_name,
    deleteClusterResponse_cluster,
    deleteClusterResponse_httpStatus,

    -- ** CreateCluster
    createCluster_kubernetesNetworkConfig,
    createCluster_version,
    createCluster_encryptionConfig,
    createCluster_clientRequestToken,
    createCluster_logging,
    createCluster_tags,
    createCluster_name,
    createCluster_roleArn,
    createCluster_resourcesVpcConfig,
    createClusterResponse_cluster,
    createClusterResponse_httpStatus,

    -- ** UpdateClusterConfig
    updateClusterConfig_clientRequestToken,
    updateClusterConfig_logging,
    updateClusterConfig_resourcesVpcConfig,
    updateClusterConfig_name,
    updateClusterConfigResponse_update,
    updateClusterConfigResponse_httpStatus,

    -- ** ListAddons
    listAddons_nextToken,
    listAddons_maxResults,
    listAddons_clusterName,
    listAddonsResponse_addons,
    listAddonsResponse_nextToken,
    listAddonsResponse_httpStatus,

    -- ** UpdateClusterVersion
    updateClusterVersion_clientRequestToken,
    updateClusterVersion_name,
    updateClusterVersion_version,
    updateClusterVersionResponse_update,
    updateClusterVersionResponse_httpStatus,

    -- ** DescribeAddonVersions
    describeAddonVersions_addonName,
    describeAddonVersions_nextToken,
    describeAddonVersions_kubernetesVersion,
    describeAddonVersions_maxResults,
    describeAddonVersionsResponse_addons,
    describeAddonVersionsResponse_nextToken,
    describeAddonVersionsResponse_httpStatus,

    -- ** UpdateNodegroupVersion
    updateNodegroupVersion_force,
    updateNodegroupVersion_releaseVersion,
    updateNodegroupVersion_version,
    updateNodegroupVersion_launchTemplate,
    updateNodegroupVersion_clientRequestToken,
    updateNodegroupVersion_clusterName,
    updateNodegroupVersion_nodegroupName,
    updateNodegroupVersionResponse_update,
    updateNodegroupVersionResponse_httpStatus,

    -- ** ListIdentityProviderConfigs
    listIdentityProviderConfigs_nextToken,
    listIdentityProviderConfigs_maxResults,
    listIdentityProviderConfigs_clusterName,
    listIdentityProviderConfigsResponse_identityProviderConfigs,
    listIdentityProviderConfigsResponse_nextToken,
    listIdentityProviderConfigsResponse_httpStatus,

    -- ** DisassociateIdentityProviderConfig
    disassociateIdentityProviderConfig_clientRequestToken,
    disassociateIdentityProviderConfig_clusterName,
    disassociateIdentityProviderConfig_identityProviderConfig,
    disassociateIdentityProviderConfigResponse_update,
    disassociateIdentityProviderConfigResponse_httpStatus,

    -- ** DescribeAddon
    describeAddon_clusterName,
    describeAddon_addonName,
    describeAddonResponse_addon,
    describeAddonResponse_httpStatus,

    -- ** ListUpdates
    listUpdates_addonName,
    listUpdates_nextToken,
    listUpdates_nodegroupName,
    listUpdates_maxResults,
    listUpdates_name,
    listUpdatesResponse_nextToken,
    listUpdatesResponse_updateIds,
    listUpdatesResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** ListFargateProfiles
    listFargateProfiles_nextToken,
    listFargateProfiles_maxResults,
    listFargateProfiles_clusterName,
    listFargateProfilesResponse_nextToken,
    listFargateProfilesResponse_fargateProfileNames,
    listFargateProfilesResponse_httpStatus,

    -- ** RegisterCluster
    registerCluster_clientRequestToken,
    registerCluster_name,
    registerCluster_connectorConfig,
    registerClusterResponse_cluster,
    registerClusterResponse_httpStatus,

    -- ** ListClusters
    listClusters_include,
    listClusters_nextToken,
    listClusters_maxResults,
    listClustersResponse_nextToken,
    listClustersResponse_clusters,
    listClustersResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** CreateNodegroup
    createNodegroup_capacityType,
    createNodegroup_instanceTypes,
    createNodegroup_taints,
    createNodegroup_remoteAccess,
    createNodegroup_diskSize,
    createNodegroup_releaseVersion,
    createNodegroup_scalingConfig,
    createNodegroup_version,
    createNodegroup_launchTemplate,
    createNodegroup_labels,
    createNodegroup_amiType,
    createNodegroup_clientRequestToken,
    createNodegroup_updateConfig,
    createNodegroup_tags,
    createNodegroup_clusterName,
    createNodegroup_nodegroupName,
    createNodegroup_subnets,
    createNodegroup_nodeRole,
    createNodegroupResponse_nodegroup,
    createNodegroupResponse_httpStatus,

    -- ** ListNodegroups
    listNodegroups_nextToken,
    listNodegroups_maxResults,
    listNodegroups_clusterName,
    listNodegroupsResponse_nodegroups,
    listNodegroupsResponse_nextToken,
    listNodegroupsResponse_httpStatus,

    -- ** DeleteNodegroup
    deleteNodegroup_clusterName,
    deleteNodegroup_nodegroupName,
    deleteNodegroupResponse_nodegroup,
    deleteNodegroupResponse_httpStatus,

    -- ** AssociateEncryptionConfig
    associateEncryptionConfig_clientRequestToken,
    associateEncryptionConfig_clusterName,
    associateEncryptionConfig_encryptionConfig,
    associateEncryptionConfigResponse_update,
    associateEncryptionConfigResponse_httpStatus,

    -- ** AssociateIdentityProviderConfig
    associateIdentityProviderConfig_clientRequestToken,
    associateIdentityProviderConfig_tags,
    associateIdentityProviderConfig_clusterName,
    associateIdentityProviderConfig_oidc,
    associateIdentityProviderConfigResponse_update,
    associateIdentityProviderConfigResponse_tags,
    associateIdentityProviderConfigResponse_httpStatus,

    -- ** DeleteAddon
    deleteAddon_preserve,
    deleteAddon_clusterName,
    deleteAddon_addonName,
    deleteAddonResponse_addon,
    deleteAddonResponse_httpStatus,

    -- ** UpdateAddon
    updateAddon_addonVersion,
    updateAddon_serviceAccountRoleArn,
    updateAddon_resolveConflicts,
    updateAddon_clientRequestToken,
    updateAddon_clusterName,
    updateAddon_addonName,
    updateAddonResponse_update,
    updateAddonResponse_httpStatus,

    -- * Types

    -- ** Addon
    addon_modifiedAt,
    addon_status,
    addon_addonName,
    addon_addonVersion,
    addon_createdAt,
    addon_serviceAccountRoleArn,
    addon_health,
    addon_clusterName,
    addon_addonArn,
    addon_tags,

    -- ** AddonHealth
    addonHealth_issues,

    -- ** AddonInfo
    addonInfo_addonName,
    addonInfo_type,
    addonInfo_addonVersions,

    -- ** AddonIssue
    addonIssue_resourceIds,
    addonIssue_code,
    addonIssue_message,

    -- ** AddonVersionInfo
    addonVersionInfo_addonVersion,
    addonVersionInfo_architecture,
    addonVersionInfo_compatibilities,

    -- ** AutoScalingGroup
    autoScalingGroup_name,

    -- ** Certificate
    certificate_data,

    -- ** Cluster
    cluster_status,
    cluster_arn,
    cluster_createdAt,
    cluster_platformVersion,
    cluster_kubernetesNetworkConfig,
    cluster_connectorConfig,
    cluster_certificateAuthority,
    cluster_name,
    cluster_version,
    cluster_encryptionConfig,
    cluster_endpoint,
    cluster_clientRequestToken,
    cluster_logging,
    cluster_identity,
    cluster_resourcesVpcConfig,
    cluster_tags,
    cluster_roleArn,

    -- ** Compatibility
    compatibility_defaultVersion,
    compatibility_clusterVersion,
    compatibility_platformVersions,

    -- ** ConnectorConfigRequest
    connectorConfigRequest_roleArn,
    connectorConfigRequest_provider,

    -- ** ConnectorConfigResponse
    connectorConfigResponse_activationCode,
    connectorConfigResponse_activationId,
    connectorConfigResponse_activationExpiry,
    connectorConfigResponse_provider,
    connectorConfigResponse_roleArn,

    -- ** EncryptionConfig
    encryptionConfig_resources,
    encryptionConfig_provider,

    -- ** ErrorDetail
    errorDetail_resourceIds,
    errorDetail_errorCode,
    errorDetail_errorMessage,

    -- ** FargateProfile
    fargateProfile_fargateProfileArn,
    fargateProfile_status,
    fargateProfile_createdAt,
    fargateProfile_subnets,
    fargateProfile_clusterName,
    fargateProfile_podExecutionRoleArn,
    fargateProfile_fargateProfileName,
    fargateProfile_selectors,
    fargateProfile_tags,

    -- ** FargateProfileSelector
    fargateProfileSelector_namespace,
    fargateProfileSelector_labels,

    -- ** Identity
    identity_oidc,

    -- ** IdentityProviderConfig
    identityProviderConfig_type,
    identityProviderConfig_name,

    -- ** IdentityProviderConfigResponse
    identityProviderConfigResponse_oidc,

    -- ** Issue
    issue_resourceIds,
    issue_code,
    issue_message,

    -- ** KubernetesNetworkConfigRequest
    kubernetesNetworkConfigRequest_serviceIpv4Cidr,

    -- ** KubernetesNetworkConfigResponse
    kubernetesNetworkConfigResponse_serviceIpv4Cidr,

    -- ** LaunchTemplateSpecification
    launchTemplateSpecification_name,
    launchTemplateSpecification_version,
    launchTemplateSpecification_id,

    -- ** LogSetup
    logSetup_enabled,
    logSetup_types,

    -- ** Logging
    logging_clusterLogging,

    -- ** Nodegroup
    nodegroup_modifiedAt,
    nodegroup_capacityType,
    nodegroup_status,
    nodegroup_instanceTypes,
    nodegroup_createdAt,
    nodegroup_taints,
    nodegroup_subnets,
    nodegroup_remoteAccess,
    nodegroup_diskSize,
    nodegroup_releaseVersion,
    nodegroup_resources,
    nodegroup_health,
    nodegroup_nodeRole,
    nodegroup_scalingConfig,
    nodegroup_version,
    nodegroup_nodegroupArn,
    nodegroup_clusterName,
    nodegroup_launchTemplate,
    nodegroup_labels,
    nodegroup_amiType,
    nodegroup_nodegroupName,
    nodegroup_updateConfig,
    nodegroup_tags,

    -- ** NodegroupHealth
    nodegroupHealth_issues,

    -- ** NodegroupResources
    nodegroupResources_remoteAccessSecurityGroup,
    nodegroupResources_autoScalingGroups,

    -- ** NodegroupScalingConfig
    nodegroupScalingConfig_desiredSize,
    nodegroupScalingConfig_maxSize,
    nodegroupScalingConfig_minSize,

    -- ** NodegroupUpdateConfig
    nodegroupUpdateConfig_maxUnavailable,
    nodegroupUpdateConfig_maxUnavailablePercentage,

    -- ** OIDC
    oidc_issuer,

    -- ** OidcIdentityProviderConfig
    oidcIdentityProviderConfig_groupsPrefix,
    oidcIdentityProviderConfig_usernameClaim,
    oidcIdentityProviderConfig_clientId,
    oidcIdentityProviderConfig_status,
    oidcIdentityProviderConfig_identityProviderConfigName,
    oidcIdentityProviderConfig_identityProviderConfigArn,
    oidcIdentityProviderConfig_issuerUrl,
    oidcIdentityProviderConfig_requiredClaims,
    oidcIdentityProviderConfig_usernamePrefix,
    oidcIdentityProviderConfig_groupsClaim,
    oidcIdentityProviderConfig_clusterName,
    oidcIdentityProviderConfig_tags,

    -- ** OidcIdentityProviderConfigRequest
    oidcIdentityProviderConfigRequest_groupsPrefix,
    oidcIdentityProviderConfigRequest_usernameClaim,
    oidcIdentityProviderConfigRequest_requiredClaims,
    oidcIdentityProviderConfigRequest_usernamePrefix,
    oidcIdentityProviderConfigRequest_groupsClaim,
    oidcIdentityProviderConfigRequest_identityProviderConfigName,
    oidcIdentityProviderConfigRequest_issuerUrl,
    oidcIdentityProviderConfigRequest_clientId,

    -- ** Provider
    provider_keyArn,

    -- ** RemoteAccessConfig
    remoteAccessConfig_sourceSecurityGroups,
    remoteAccessConfig_ec2SshKey,

    -- ** Taint
    taint_effect,
    taint_value,
    taint_key,

    -- ** Update
    update_status,
    update_createdAt,
    update_params,
    update_id,
    update_type,
    update_errors,

    -- ** UpdateLabelsPayload
    updateLabelsPayload_removeLabels,
    updateLabelsPayload_addOrUpdateLabels,

    -- ** UpdateParam
    updateParam_value,
    updateParam_type,

    -- ** UpdateTaintsPayload
    updateTaintsPayload_addOrUpdateTaints,
    updateTaintsPayload_removeTaints,

    -- ** VpcConfigRequest
    vpcConfigRequest_securityGroupIds,
    vpcConfigRequest_endpointPrivateAccess,
    vpcConfigRequest_publicAccessCidrs,
    vpcConfigRequest_subnetIds,
    vpcConfigRequest_endpointPublicAccess,

    -- ** VpcConfigResponse
    vpcConfigResponse_securityGroupIds,
    vpcConfigResponse_endpointPrivateAccess,
    vpcConfigResponse_publicAccessCidrs,
    vpcConfigResponse_subnetIds,
    vpcConfigResponse_vpcId,
    vpcConfigResponse_clusterSecurityGroupId,
    vpcConfigResponse_endpointPublicAccess,
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
import Network.AWS.EKS.ListAddons
import Network.AWS.EKS.ListClusters
import Network.AWS.EKS.ListFargateProfiles
import Network.AWS.EKS.ListIdentityProviderConfigs
import Network.AWS.EKS.ListNodegroups
import Network.AWS.EKS.ListTagsForResource
import Network.AWS.EKS.ListUpdates
import Network.AWS.EKS.RegisterCluster
import Network.AWS.EKS.TagResource
import Network.AWS.EKS.Types.Addon
import Network.AWS.EKS.Types.AddonHealth
import Network.AWS.EKS.Types.AddonInfo
import Network.AWS.EKS.Types.AddonIssue
import Network.AWS.EKS.Types.AddonVersionInfo
import Network.AWS.EKS.Types.AutoScalingGroup
import Network.AWS.EKS.Types.Certificate
import Network.AWS.EKS.Types.Cluster
import Network.AWS.EKS.Types.Compatibility
import Network.AWS.EKS.Types.ConnectorConfigRequest
import Network.AWS.EKS.Types.ConnectorConfigResponse
import Network.AWS.EKS.Types.EncryptionConfig
import Network.AWS.EKS.Types.ErrorDetail
import Network.AWS.EKS.Types.FargateProfile
import Network.AWS.EKS.Types.FargateProfileSelector
import Network.AWS.EKS.Types.Identity
import Network.AWS.EKS.Types.IdentityProviderConfig
import Network.AWS.EKS.Types.IdentityProviderConfigResponse
import Network.AWS.EKS.Types.Issue
import Network.AWS.EKS.Types.KubernetesNetworkConfigRequest
import Network.AWS.EKS.Types.KubernetesNetworkConfigResponse
import Network.AWS.EKS.Types.LaunchTemplateSpecification
import Network.AWS.EKS.Types.LogSetup
import Network.AWS.EKS.Types.Logging
import Network.AWS.EKS.Types.Nodegroup
import Network.AWS.EKS.Types.NodegroupHealth
import Network.AWS.EKS.Types.NodegroupResources
import Network.AWS.EKS.Types.NodegroupScalingConfig
import Network.AWS.EKS.Types.NodegroupUpdateConfig
import Network.AWS.EKS.Types.OIDC
import Network.AWS.EKS.Types.OidcIdentityProviderConfig
import Network.AWS.EKS.Types.OidcIdentityProviderConfigRequest
import Network.AWS.EKS.Types.Provider
import Network.AWS.EKS.Types.RemoteAccessConfig
import Network.AWS.EKS.Types.Taint
import Network.AWS.EKS.Types.Update
import Network.AWS.EKS.Types.UpdateLabelsPayload
import Network.AWS.EKS.Types.UpdateParam
import Network.AWS.EKS.Types.UpdateTaintsPayload
import Network.AWS.EKS.Types.VpcConfigRequest
import Network.AWS.EKS.Types.VpcConfigResponse
import Network.AWS.EKS.UntagResource
import Network.AWS.EKS.UpdateAddon
import Network.AWS.EKS.UpdateClusterConfig
import Network.AWS.EKS.UpdateClusterVersion
import Network.AWS.EKS.UpdateNodegroupConfig
import Network.AWS.EKS.UpdateNodegroupVersion

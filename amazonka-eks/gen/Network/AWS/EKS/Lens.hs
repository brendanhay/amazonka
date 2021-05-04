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

    -- ** DescribeAddonVersions
    describeAddonVersions_nextToken,
    describeAddonVersions_maxResults,
    describeAddonVersions_kubernetesVersion,
    describeAddonVersions_addonName,
    describeAddonVersionsResponse_nextToken,
    describeAddonVersionsResponse_addons,
    describeAddonVersionsResponse_httpStatus,

    -- ** DescribeUpdate
    describeUpdate_nodegroupName,
    describeUpdate_addonName,
    describeUpdate_name,
    describeUpdate_updateId,
    describeUpdateResponse_update,
    describeUpdateResponse_httpStatus,

    -- ** ListIdentityProviderConfigs
    listIdentityProviderConfigs_nextToken,
    listIdentityProviderConfigs_maxResults,
    listIdentityProviderConfigs_clusterName,
    listIdentityProviderConfigsResponse_nextToken,
    listIdentityProviderConfigsResponse_identityProviderConfigs,
    listIdentityProviderConfigsResponse_httpStatus,

    -- ** DescribeFargateProfile
    describeFargateProfile_clusterName,
    describeFargateProfile_fargateProfileName,
    describeFargateProfileResponse_fargateProfile,
    describeFargateProfileResponse_httpStatus,

    -- ** UpdateAddon
    updateAddon_resolveConflicts,
    updateAddon_serviceAccountRoleArn,
    updateAddon_addonVersion,
    updateAddon_clientRequestToken,
    updateAddon_clusterName,
    updateAddon_addonName,
    updateAddonResponse_update,
    updateAddonResponse_httpStatus,

    -- ** AssociateEncryptionConfig
    associateEncryptionConfig_clientRequestToken,
    associateEncryptionConfig_clusterName,
    associateEncryptionConfig_encryptionConfig,
    associateEncryptionConfigResponse_update,
    associateEncryptionConfigResponse_httpStatus,

    -- ** ListAddons
    listAddons_nextToken,
    listAddons_maxResults,
    listAddons_clusterName,
    listAddonsResponse_nextToken,
    listAddonsResponse_addons,
    listAddonsResponse_httpStatus,

    -- ** DeleteAddon
    deleteAddon_clusterName,
    deleteAddon_addonName,
    deleteAddonResponse_addon,
    deleteAddonResponse_httpStatus,

    -- ** AssociateIdentityProviderConfig
    associateIdentityProviderConfig_tags,
    associateIdentityProviderConfig_clientRequestToken,
    associateIdentityProviderConfig_clusterName,
    associateIdentityProviderConfig_oidc,
    associateIdentityProviderConfigResponse_tags,
    associateIdentityProviderConfigResponse_update,
    associateIdentityProviderConfigResponse_httpStatus,

    -- ** UpdateClusterVersion
    updateClusterVersion_clientRequestToken,
    updateClusterVersion_name,
    updateClusterVersion_version,
    updateClusterVersionResponse_update,
    updateClusterVersionResponse_httpStatus,

    -- ** ListNodegroups
    listNodegroups_nextToken,
    listNodegroups_maxResults,
    listNodegroups_clusterName,
    listNodegroupsResponse_nextToken,
    listNodegroupsResponse_nodegroups,
    listNodegroupsResponse_httpStatus,

    -- ** CreateCluster
    createCluster_kubernetesNetworkConfig,
    createCluster_logging,
    createCluster_encryptionConfig,
    createCluster_version,
    createCluster_tags,
    createCluster_clientRequestToken,
    createCluster_name,
    createCluster_roleArn,
    createCluster_resourcesVpcConfig,
    createClusterResponse_cluster,
    createClusterResponse_httpStatus,

    -- ** CreateNodegroup
    createNodegroup_scalingConfig,
    createNodegroup_capacityType,
    createNodegroup_releaseVersion,
    createNodegroup_diskSize,
    createNodegroup_remoteAccess,
    createNodegroup_launchTemplate,
    createNodegroup_labels,
    createNodegroup_version,
    createNodegroup_tags,
    createNodegroup_clientRequestToken,
    createNodegroup_amiType,
    createNodegroup_instanceTypes,
    createNodegroup_clusterName,
    createNodegroup_nodegroupName,
    createNodegroup_subnets,
    createNodegroup_nodeRole,
    createNodegroupResponse_nodegroup,
    createNodegroupResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

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

    -- ** ListUpdates
    listUpdates_nextToken,
    listUpdates_maxResults,
    listUpdates_nodegroupName,
    listUpdates_addonName,
    listUpdates_name,
    listUpdatesResponse_updateIds,
    listUpdatesResponse_nextToken,
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

    -- ** DisassociateIdentityProviderConfig
    disassociateIdentityProviderConfig_clientRequestToken,
    disassociateIdentityProviderConfig_clusterName,
    disassociateIdentityProviderConfig_identityProviderConfig,
    disassociateIdentityProviderConfigResponse_update,
    disassociateIdentityProviderConfigResponse_httpStatus,

    -- ** DescribeCluster
    describeCluster_name,
    describeClusterResponse_cluster,
    describeClusterResponse_httpStatus,

    -- ** CreateAddon
    createAddon_resolveConflicts,
    createAddon_serviceAccountRoleArn,
    createAddon_addonVersion,
    createAddon_tags,
    createAddon_clientRequestToken,
    createAddon_clusterName,
    createAddon_addonName,
    createAddonResponse_addon,
    createAddonResponse_httpStatus,

    -- ** UpdateNodegroupConfig
    updateNodegroupConfig_scalingConfig,
    updateNodegroupConfig_labels,
    updateNodegroupConfig_clientRequestToken,
    updateNodegroupConfig_clusterName,
    updateNodegroupConfig_nodegroupName,
    updateNodegroupConfigResponse_update,
    updateNodegroupConfigResponse_httpStatus,

    -- ** UpdateNodegroupVersion
    updateNodegroupVersion_releaseVersion,
    updateNodegroupVersion_force,
    updateNodegroupVersion_launchTemplate,
    updateNodegroupVersion_version,
    updateNodegroupVersion_clientRequestToken,
    updateNodegroupVersion_clusterName,
    updateNodegroupVersion_nodegroupName,
    updateNodegroupVersionResponse_update,
    updateNodegroupVersionResponse_httpStatus,

    -- ** UpdateClusterConfig
    updateClusterConfig_resourcesVpcConfig,
    updateClusterConfig_logging,
    updateClusterConfig_clientRequestToken,
    updateClusterConfig_name,
    updateClusterConfigResponse_update,
    updateClusterConfigResponse_httpStatus,

    -- ** DeleteNodegroup
    deleteNodegroup_clusterName,
    deleteNodegroup_nodegroupName,
    deleteNodegroupResponse_nodegroup,
    deleteNodegroupResponse_httpStatus,

    -- ** DeleteCluster
    deleteCluster_name,
    deleteClusterResponse_cluster,
    deleteClusterResponse_httpStatus,

    -- ** ListClusters
    listClusters_nextToken,
    listClusters_maxResults,
    listClustersResponse_nextToken,
    listClustersResponse_clusters,
    listClustersResponse_httpStatus,

    -- ** DescribeAddon
    describeAddon_clusterName,
    describeAddon_addonName,
    describeAddonResponse_addon,
    describeAddonResponse_httpStatus,

    -- ** CreateFargateProfile
    createFargateProfile_tags,
    createFargateProfile_selectors,
    createFargateProfile_clientRequestToken,
    createFargateProfile_subnets,
    createFargateProfile_fargateProfileName,
    createFargateProfile_clusterName,
    createFargateProfile_podExecutionRoleArn,
    createFargateProfileResponse_fargateProfile,
    createFargateProfileResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** DescribeNodegroup
    describeNodegroup_clusterName,
    describeNodegroup_nodegroupName,
    describeNodegroupResponse_nodegroup,
    describeNodegroupResponse_httpStatus,

    -- * Types

    -- ** Addon
    addon_modifiedAt,
    addon_status,
    addon_addonArn,
    addon_serviceAccountRoleArn,
    addon_createdAt,
    addon_addonVersion,
    addon_addonName,
    addon_health,
    addon_tags,
    addon_clusterName,

    -- ** AddonHealth
    addonHealth_issues,

    -- ** AddonInfo
    addonInfo_addonVersions,
    addonInfo_addonName,
    addonInfo_type,

    -- ** AddonIssue
    addonIssue_message,
    addonIssue_code,
    addonIssue_resourceIds,

    -- ** AddonVersionInfo
    addonVersionInfo_compatibilities,
    addonVersionInfo_architecture,
    addonVersionInfo_addonVersion,

    -- ** AutoScalingGroup
    autoScalingGroup_name,

    -- ** Certificate
    certificate_data,

    -- ** Cluster
    cluster_status,
    cluster_roleArn,
    cluster_resourcesVpcConfig,
    cluster_kubernetesNetworkConfig,
    cluster_identity,
    cluster_logging,
    cluster_createdAt,
    cluster_platformVersion,
    cluster_arn,
    cluster_encryptionConfig,
    cluster_version,
    cluster_name,
    cluster_certificateAuthority,
    cluster_tags,
    cluster_endpoint,
    cluster_clientRequestToken,

    -- ** Compatibility
    compatibility_defaultVersion,
    compatibility_platformVersions,
    compatibility_clusterVersion,

    -- ** EncryptionConfig
    encryptionConfig_resources,
    encryptionConfig_provider,

    -- ** ErrorDetail
    errorDetail_resourceIds,
    errorDetail_errorMessage,
    errorDetail_errorCode,

    -- ** FargateProfile
    fargateProfile_status,
    fargateProfile_fargateProfileName,
    fargateProfile_podExecutionRoleArn,
    fargateProfile_createdAt,
    fargateProfile_fargateProfileArn,
    fargateProfile_tags,
    fargateProfile_selectors,
    fargateProfile_subnets,
    fargateProfile_clusterName,

    -- ** FargateProfileSelector
    fargateProfileSelector_labels,
    fargateProfileSelector_namespace,

    -- ** Identity
    identity_oidc,

    -- ** IdentityProviderConfig
    identityProviderConfig_type,
    identityProviderConfig_name,

    -- ** IdentityProviderConfigResponse
    identityProviderConfigResponse_oidc,

    -- ** Issue
    issue_message,
    issue_code,
    issue_resourceIds,

    -- ** KubernetesNetworkConfigRequest
    kubernetesNetworkConfigRequest_serviceIpv4Cidr,

    -- ** KubernetesNetworkConfigResponse
    kubernetesNetworkConfigResponse_serviceIpv4Cidr,

    -- ** LaunchTemplateSpecification
    launchTemplateSpecification_id,
    launchTemplateSpecification_version,
    launchTemplateSpecification_name,

    -- ** LogSetup
    logSetup_enabled,
    logSetup_types,

    -- ** Logging
    logging_clusterLogging,

    -- ** Nodegroup
    nodegroup_scalingConfig,
    nodegroup_modifiedAt,
    nodegroup_status,
    nodegroup_capacityType,
    nodegroup_releaseVersion,
    nodegroup_diskSize,
    nodegroup_nodegroupName,
    nodegroup_remoteAccess,
    nodegroup_createdAt,
    nodegroup_launchTemplate,
    nodegroup_labels,
    nodegroup_version,
    nodegroup_nodeRole,
    nodegroup_health,
    nodegroup_resources,
    nodegroup_tags,
    nodegroup_subnets,
    nodegroup_amiType,
    nodegroup_clusterName,
    nodegroup_nodegroupArn,
    nodegroup_instanceTypes,

    -- ** NodegroupHealth
    nodegroupHealth_issues,

    -- ** NodegroupResources
    nodegroupResources_remoteAccessSecurityGroup,
    nodegroupResources_autoScalingGroups,

    -- ** NodegroupScalingConfig
    nodegroupScalingConfig_minSize,
    nodegroupScalingConfig_desiredSize,
    nodegroupScalingConfig_maxSize,

    -- ** OIDC
    oidc_issuer,

    -- ** OidcIdentityProviderConfig
    oidcIdentityProviderConfig_clientId,
    oidcIdentityProviderConfig_groupsPrefix,
    oidcIdentityProviderConfig_status,
    oidcIdentityProviderConfig_groupsClaim,
    oidcIdentityProviderConfig_requiredClaims,
    oidcIdentityProviderConfig_identityProviderConfigName,
    oidcIdentityProviderConfig_usernameClaim,
    oidcIdentityProviderConfig_tags,
    oidcIdentityProviderConfig_usernamePrefix,
    oidcIdentityProviderConfig_issuerUrl,
    oidcIdentityProviderConfig_identityProviderConfigArn,
    oidcIdentityProviderConfig_clusterName,

    -- ** OidcIdentityProviderConfigRequest
    oidcIdentityProviderConfigRequest_groupsPrefix,
    oidcIdentityProviderConfigRequest_groupsClaim,
    oidcIdentityProviderConfigRequest_requiredClaims,
    oidcIdentityProviderConfigRequest_usernameClaim,
    oidcIdentityProviderConfigRequest_usernamePrefix,
    oidcIdentityProviderConfigRequest_identityProviderConfigName,
    oidcIdentityProviderConfigRequest_issuerUrl,
    oidcIdentityProviderConfigRequest_clientId,

    -- ** Provider
    provider_keyArn,

    -- ** RemoteAccessConfig
    remoteAccessConfig_ec2SshKey,
    remoteAccessConfig_sourceSecurityGroups,

    -- ** Update
    update_status,
    update_createdAt,
    update_id,
    update_params,
    update_errors,
    update_type,

    -- ** UpdateLabelsPayload
    updateLabelsPayload_removeLabels,
    updateLabelsPayload_addOrUpdateLabels,

    -- ** UpdateParam
    updateParam_value,
    updateParam_type,

    -- ** VpcConfigRequest
    vpcConfigRequest_securityGroupIds,
    vpcConfigRequest_endpointPublicAccess,
    vpcConfigRequest_subnetIds,
    vpcConfigRequest_endpointPrivateAccess,
    vpcConfigRequest_publicAccessCidrs,

    -- ** VpcConfigResponse
    vpcConfigResponse_securityGroupIds,
    vpcConfigResponse_endpointPublicAccess,
    vpcConfigResponse_subnetIds,
    vpcConfigResponse_clusterSecurityGroupId,
    vpcConfigResponse_vpcId,
    vpcConfigResponse_endpointPrivateAccess,
    vpcConfigResponse_publicAccessCidrs,
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
import Network.AWS.EKS.Types.OIDC
import Network.AWS.EKS.Types.OidcIdentityProviderConfig
import Network.AWS.EKS.Types.OidcIdentityProviderConfigRequest
import Network.AWS.EKS.Types.Provider
import Network.AWS.EKS.Types.RemoteAccessConfig
import Network.AWS.EKS.Types.Update
import Network.AWS.EKS.Types.UpdateLabelsPayload
import Network.AWS.EKS.Types.UpdateParam
import Network.AWS.EKS.Types.VpcConfigRequest
import Network.AWS.EKS.Types.VpcConfigResponse
import Network.AWS.EKS.UntagResource
import Network.AWS.EKS.UpdateAddon
import Network.AWS.EKS.UpdateClusterConfig
import Network.AWS.EKS.UpdateClusterVersion
import Network.AWS.EKS.UpdateNodegroupConfig
import Network.AWS.EKS.UpdateNodegroupVersion

{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EKS.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Lens
  ( -- * Operations

    -- ** AssociateEncryptionConfig
    associateEncryptionConfig_clientRequestToken,
    associateEncryptionConfig_clusterName,
    associateEncryptionConfig_encryptionConfig,
    associateEncryptionConfigResponse_update,
    associateEncryptionConfigResponse_httpStatus,

    -- ** AssociateIdentityProviderConfig
    associateIdentityProviderConfig_tags,
    associateIdentityProviderConfig_clientRequestToken,
    associateIdentityProviderConfig_clusterName,
    associateIdentityProviderConfig_oidc,
    associateIdentityProviderConfigResponse_tags,
    associateIdentityProviderConfigResponse_update,
    associateIdentityProviderConfigResponse_httpStatus,

    -- ** CreateAddon
    createAddon_tags,
    createAddon_clientRequestToken,
    createAddon_addonVersion,
    createAddon_serviceAccountRoleArn,
    createAddon_resolveConflicts,
    createAddon_clusterName,
    createAddon_addonName,
    createAddonResponse_addon,
    createAddonResponse_httpStatus,

    -- ** CreateCluster
    createCluster_encryptionConfig,
    createCluster_tags,
    createCluster_clientRequestToken,
    createCluster_outpostConfig,
    createCluster_logging,
    createCluster_kubernetesNetworkConfig,
    createCluster_version,
    createCluster_name,
    createCluster_roleArn,
    createCluster_resourcesVpcConfig,
    createClusterResponse_cluster,
    createClusterResponse_httpStatus,

    -- ** CreateFargateProfile
    createFargateProfile_tags,
    createFargateProfile_clientRequestToken,
    createFargateProfile_subnets,
    createFargateProfile_selectors,
    createFargateProfile_fargateProfileName,
    createFargateProfile_clusterName,
    createFargateProfile_podExecutionRoleArn,
    createFargateProfileResponse_fargateProfile,
    createFargateProfileResponse_httpStatus,

    -- ** CreateNodegroup
    createNodegroup_tags,
    createNodegroup_releaseVersion,
    createNodegroup_remoteAccess,
    createNodegroup_instanceTypes,
    createNodegroup_clientRequestToken,
    createNodegroup_updateConfig,
    createNodegroup_capacityType,
    createNodegroup_amiType,
    createNodegroup_diskSize,
    createNodegroup_launchTemplate,
    createNodegroup_taints,
    createNodegroup_labels,
    createNodegroup_scalingConfig,
    createNodegroup_version,
    createNodegroup_clusterName,
    createNodegroup_nodegroupName,
    createNodegroup_subnets,
    createNodegroup_nodeRole,
    createNodegroupResponse_nodegroup,
    createNodegroupResponse_httpStatus,

    -- ** DeleteAddon
    deleteAddon_preserve,
    deleteAddon_clusterName,
    deleteAddon_addonName,
    deleteAddonResponse_addon,
    deleteAddonResponse_httpStatus,

    -- ** DeleteCluster
    deleteCluster_name,
    deleteClusterResponse_cluster,
    deleteClusterResponse_httpStatus,

    -- ** DeleteFargateProfile
    deleteFargateProfile_clusterName,
    deleteFargateProfile_fargateProfileName,
    deleteFargateProfileResponse_fargateProfile,
    deleteFargateProfileResponse_httpStatus,

    -- ** DeleteNodegroup
    deleteNodegroup_clusterName,
    deleteNodegroup_nodegroupName,
    deleteNodegroupResponse_nodegroup,
    deleteNodegroupResponse_httpStatus,

    -- ** DeregisterCluster
    deregisterCluster_name,
    deregisterClusterResponse_cluster,
    deregisterClusterResponse_httpStatus,

    -- ** DescribeAddon
    describeAddon_clusterName,
    describeAddon_addonName,
    describeAddonResponse_addon,
    describeAddonResponse_httpStatus,

    -- ** DescribeAddonVersions
    describeAddonVersions_nextToken,
    describeAddonVersions_addonName,
    describeAddonVersions_maxResults,
    describeAddonVersions_kubernetesVersion,
    describeAddonVersionsResponse_nextToken,
    describeAddonVersionsResponse_addons,
    describeAddonVersionsResponse_httpStatus,

    -- ** DescribeCluster
    describeCluster_name,
    describeClusterResponse_cluster,
    describeClusterResponse_httpStatus,

    -- ** DescribeFargateProfile
    describeFargateProfile_clusterName,
    describeFargateProfile_fargateProfileName,
    describeFargateProfileResponse_fargateProfile,
    describeFargateProfileResponse_httpStatus,

    -- ** DescribeIdentityProviderConfig
    describeIdentityProviderConfig_clusterName,
    describeIdentityProviderConfig_identityProviderConfig,
    describeIdentityProviderConfigResponse_identityProviderConfig,
    describeIdentityProviderConfigResponse_httpStatus,

    -- ** DescribeNodegroup
    describeNodegroup_clusterName,
    describeNodegroup_nodegroupName,
    describeNodegroupResponse_nodegroup,
    describeNodegroupResponse_httpStatus,

    -- ** DescribeUpdate
    describeUpdate_nodegroupName,
    describeUpdate_addonName,
    describeUpdate_name,
    describeUpdate_updateId,
    describeUpdateResponse_update,
    describeUpdateResponse_httpStatus,

    -- ** DisassociateIdentityProviderConfig
    disassociateIdentityProviderConfig_clientRequestToken,
    disassociateIdentityProviderConfig_clusterName,
    disassociateIdentityProviderConfig_identityProviderConfig,
    disassociateIdentityProviderConfigResponse_update,
    disassociateIdentityProviderConfigResponse_httpStatus,

    -- ** ListAddons
    listAddons_nextToken,
    listAddons_maxResults,
    listAddons_clusterName,
    listAddonsResponse_nextToken,
    listAddonsResponse_addons,
    listAddonsResponse_httpStatus,

    -- ** ListClusters
    listClusters_nextToken,
    listClusters_maxResults,
    listClusters_include,
    listClustersResponse_nextToken,
    listClustersResponse_clusters,
    listClustersResponse_httpStatus,

    -- ** ListFargateProfiles
    listFargateProfiles_nextToken,
    listFargateProfiles_maxResults,
    listFargateProfiles_clusterName,
    listFargateProfilesResponse_nextToken,
    listFargateProfilesResponse_fargateProfileNames,
    listFargateProfilesResponse_httpStatus,

    -- ** ListIdentityProviderConfigs
    listIdentityProviderConfigs_nextToken,
    listIdentityProviderConfigs_maxResults,
    listIdentityProviderConfigs_clusterName,
    listIdentityProviderConfigsResponse_nextToken,
    listIdentityProviderConfigsResponse_identityProviderConfigs,
    listIdentityProviderConfigsResponse_httpStatus,

    -- ** ListNodegroups
    listNodegroups_nextToken,
    listNodegroups_maxResults,
    listNodegroups_clusterName,
    listNodegroupsResponse_nextToken,
    listNodegroupsResponse_nodegroups,
    listNodegroupsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListUpdates
    listUpdates_nextToken,
    listUpdates_nodegroupName,
    listUpdates_addonName,
    listUpdates_maxResults,
    listUpdates_name,
    listUpdatesResponse_nextToken,
    listUpdatesResponse_updateIds,
    listUpdatesResponse_httpStatus,

    -- ** RegisterCluster
    registerCluster_tags,
    registerCluster_clientRequestToken,
    registerCluster_name,
    registerCluster_connectorConfig,
    registerClusterResponse_cluster,
    registerClusterResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateAddon
    updateAddon_clientRequestToken,
    updateAddon_addonVersion,
    updateAddon_serviceAccountRoleArn,
    updateAddon_resolveConflicts,
    updateAddon_clusterName,
    updateAddon_addonName,
    updateAddonResponse_update,
    updateAddonResponse_httpStatus,

    -- ** UpdateClusterConfig
    updateClusterConfig_clientRequestToken,
    updateClusterConfig_logging,
    updateClusterConfig_resourcesVpcConfig,
    updateClusterConfig_name,
    updateClusterConfigResponse_update,
    updateClusterConfigResponse_httpStatus,

    -- ** UpdateClusterVersion
    updateClusterVersion_clientRequestToken,
    updateClusterVersion_name,
    updateClusterVersion_version,
    updateClusterVersionResponse_update,
    updateClusterVersionResponse_httpStatus,

    -- ** UpdateNodegroupConfig
    updateNodegroupConfig_clientRequestToken,
    updateNodegroupConfig_updateConfig,
    updateNodegroupConfig_taints,
    updateNodegroupConfig_labels,
    updateNodegroupConfig_scalingConfig,
    updateNodegroupConfig_clusterName,
    updateNodegroupConfig_nodegroupName,
    updateNodegroupConfigResponse_update,
    updateNodegroupConfigResponse_httpStatus,

    -- ** UpdateNodegroupVersion
    updateNodegroupVersion_releaseVersion,
    updateNodegroupVersion_clientRequestToken,
    updateNodegroupVersion_launchTemplate,
    updateNodegroupVersion_force,
    updateNodegroupVersion_version,
    updateNodegroupVersion_clusterName,
    updateNodegroupVersion_nodegroupName,
    updateNodegroupVersionResponse_update,
    updateNodegroupVersionResponse_httpStatus,

    -- * Types

    -- ** Addon
    addon_tags,
    addon_modifiedAt,
    addon_status,
    addon_addonName,
    addon_addonVersion,
    addon_health,
    addon_addonArn,
    addon_serviceAccountRoleArn,
    addon_clusterName,
    addon_createdAt,

    -- ** AddonHealth
    addonHealth_issues,

    -- ** AddonInfo
    addonInfo_type,
    addonInfo_addonVersions,
    addonInfo_addonName,

    -- ** AddonIssue
    addonIssue_message,
    addonIssue_code,
    addonIssue_resourceIds,

    -- ** AddonVersionInfo
    addonVersionInfo_addonVersion,
    addonVersionInfo_compatibilities,
    addonVersionInfo_architecture,

    -- ** AutoScalingGroup
    autoScalingGroup_name,

    -- ** Certificate
    certificate_data,

    -- ** Cluster
    cluster_encryptionConfig,
    cluster_tags,
    cluster_name,
    cluster_roleArn,
    cluster_clientRequestToken,
    cluster_arn,
    cluster_status,
    cluster_id,
    cluster_outpostConfig,
    cluster_logging,
    cluster_identity,
    cluster_kubernetesNetworkConfig,
    cluster_connectorConfig,
    cluster_health,
    cluster_platformVersion,
    cluster_certificateAuthority,
    cluster_endpoint,
    cluster_resourcesVpcConfig,
    cluster_createdAt,
    cluster_version,

    -- ** ClusterHealth
    clusterHealth_issues,

    -- ** ClusterIssue
    clusterIssue_message,
    clusterIssue_code,
    clusterIssue_resourceIds,

    -- ** Compatibility
    compatibility_clusterVersion,
    compatibility_defaultVersion,
    compatibility_platformVersions,

    -- ** ConnectorConfigRequest
    connectorConfigRequest_roleArn,
    connectorConfigRequest_provider,

    -- ** ConnectorConfigResponse
    connectorConfigResponse_roleArn,
    connectorConfigResponse_provider,
    connectorConfigResponse_activationId,
    connectorConfigResponse_activationExpiry,
    connectorConfigResponse_activationCode,

    -- ** ControlPlanePlacementRequest
    controlPlanePlacementRequest_groupName,

    -- ** ControlPlanePlacementResponse
    controlPlanePlacementResponse_groupName,

    -- ** EncryptionConfig
    encryptionConfig_provider,
    encryptionConfig_resources,

    -- ** ErrorDetail
    errorDetail_errorMessage,
    errorDetail_resourceIds,
    errorDetail_errorCode,

    -- ** FargateProfile
    fargateProfile_tags,
    fargateProfile_fargateProfileArn,
    fargateProfile_subnets,
    fargateProfile_fargateProfileName,
    fargateProfile_selectors,
    fargateProfile_status,
    fargateProfile_podExecutionRoleArn,
    fargateProfile_clusterName,
    fargateProfile_createdAt,

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
    kubernetesNetworkConfigRequest_ipFamily,
    kubernetesNetworkConfigRequest_serviceIpv4Cidr,

    -- ** KubernetesNetworkConfigResponse
    kubernetesNetworkConfigResponse_ipFamily,
    kubernetesNetworkConfigResponse_serviceIpv6Cidr,
    kubernetesNetworkConfigResponse_serviceIpv4Cidr,

    -- ** LaunchTemplateSpecification
    launchTemplateSpecification_name,
    launchTemplateSpecification_id,
    launchTemplateSpecification_version,

    -- ** LogSetup
    logSetup_enabled,
    logSetup_types,

    -- ** Logging
    logging_clusterLogging,

    -- ** Nodegroup
    nodegroup_tags,
    nodegroup_releaseVersion,
    nodegroup_modifiedAt,
    nodegroup_remoteAccess,
    nodegroup_instanceTypes,
    nodegroup_updateConfig,
    nodegroup_capacityType,
    nodegroup_amiType,
    nodegroup_diskSize,
    nodegroup_subnets,
    nodegroup_nodegroupArn,
    nodegroup_launchTemplate,
    nodegroup_nodegroupName,
    nodegroup_taints,
    nodegroup_nodeRole,
    nodegroup_status,
    nodegroup_labels,
    nodegroup_health,
    nodegroup_resources,
    nodegroup_scalingConfig,
    nodegroup_clusterName,
    nodegroup_createdAt,
    nodegroup_version,

    -- ** NodegroupHealth
    nodegroupHealth_issues,

    -- ** NodegroupResources
    nodegroupResources_autoScalingGroups,
    nodegroupResources_remoteAccessSecurityGroup,

    -- ** NodegroupScalingConfig
    nodegroupScalingConfig_desiredSize,
    nodegroupScalingConfig_minSize,
    nodegroupScalingConfig_maxSize,

    -- ** NodegroupUpdateConfig
    nodegroupUpdateConfig_maxUnavailable,
    nodegroupUpdateConfig_maxUnavailablePercentage,

    -- ** OIDC
    oidc_issuer,

    -- ** OidcIdentityProviderConfig
    oidcIdentityProviderConfig_tags,
    oidcIdentityProviderConfig_requiredClaims,
    oidcIdentityProviderConfig_clientId,
    oidcIdentityProviderConfig_identityProviderConfigName,
    oidcIdentityProviderConfig_status,
    oidcIdentityProviderConfig_usernamePrefix,
    oidcIdentityProviderConfig_groupsClaim,
    oidcIdentityProviderConfig_identityProviderConfigArn,
    oidcIdentityProviderConfig_issuerUrl,
    oidcIdentityProviderConfig_groupsPrefix,
    oidcIdentityProviderConfig_clusterName,
    oidcIdentityProviderConfig_usernameClaim,

    -- ** OidcIdentityProviderConfigRequest
    oidcIdentityProviderConfigRequest_requiredClaims,
    oidcIdentityProviderConfigRequest_usernamePrefix,
    oidcIdentityProviderConfigRequest_groupsClaim,
    oidcIdentityProviderConfigRequest_groupsPrefix,
    oidcIdentityProviderConfigRequest_usernameClaim,
    oidcIdentityProviderConfigRequest_identityProviderConfigName,
    oidcIdentityProviderConfigRequest_issuerUrl,
    oidcIdentityProviderConfigRequest_clientId,

    -- ** OutpostConfigRequest
    outpostConfigRequest_controlPlanePlacement,
    outpostConfigRequest_outpostArns,
    outpostConfigRequest_controlPlaneInstanceType,

    -- ** OutpostConfigResponse
    outpostConfigResponse_controlPlanePlacement,
    outpostConfigResponse_outpostArns,
    outpostConfigResponse_controlPlaneInstanceType,

    -- ** Provider
    provider_keyArn,

    -- ** RemoteAccessConfig
    remoteAccessConfig_sourceSecurityGroups,
    remoteAccessConfig_ec2SshKey,

    -- ** Taint
    taint_key,
    taint_effect,
    taint_value,

    -- ** Update
    update_type,
    update_status,
    update_id,
    update_errors,
    update_params,
    update_createdAt,

    -- ** UpdateLabelsPayload
    updateLabelsPayload_addOrUpdateLabels,
    updateLabelsPayload_removeLabels,

    -- ** UpdateParam
    updateParam_type,
    updateParam_value,

    -- ** UpdateTaintsPayload
    updateTaintsPayload_addOrUpdateTaints,
    updateTaintsPayload_removeTaints,

    -- ** VpcConfigRequest
    vpcConfigRequest_securityGroupIds,
    vpcConfigRequest_endpointPrivateAccess,
    vpcConfigRequest_publicAccessCidrs,
    vpcConfigRequest_endpointPublicAccess,
    vpcConfigRequest_subnetIds,

    -- ** VpcConfigResponse
    vpcConfigResponse_securityGroupIds,
    vpcConfigResponse_clusterSecurityGroupId,
    vpcConfigResponse_endpointPrivateAccess,
    vpcConfigResponse_publicAccessCidrs,
    vpcConfigResponse_vpcId,
    vpcConfigResponse_endpointPublicAccess,
    vpcConfigResponse_subnetIds,
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
import Amazonka.EKS.DescribeAddonVersions
import Amazonka.EKS.DescribeCluster
import Amazonka.EKS.DescribeFargateProfile
import Amazonka.EKS.DescribeIdentityProviderConfig
import Amazonka.EKS.DescribeNodegroup
import Amazonka.EKS.DescribeUpdate
import Amazonka.EKS.DisassociateIdentityProviderConfig
import Amazonka.EKS.ListAddons
import Amazonka.EKS.ListClusters
import Amazonka.EKS.ListFargateProfiles
import Amazonka.EKS.ListIdentityProviderConfigs
import Amazonka.EKS.ListNodegroups
import Amazonka.EKS.ListTagsForResource
import Amazonka.EKS.ListUpdates
import Amazonka.EKS.RegisterCluster
import Amazonka.EKS.TagResource
import Amazonka.EKS.Types.Addon
import Amazonka.EKS.Types.AddonHealth
import Amazonka.EKS.Types.AddonInfo
import Amazonka.EKS.Types.AddonIssue
import Amazonka.EKS.Types.AddonVersionInfo
import Amazonka.EKS.Types.AutoScalingGroup
import Amazonka.EKS.Types.Certificate
import Amazonka.EKS.Types.Cluster
import Amazonka.EKS.Types.ClusterHealth
import Amazonka.EKS.Types.ClusterIssue
import Amazonka.EKS.Types.Compatibility
import Amazonka.EKS.Types.ConnectorConfigRequest
import Amazonka.EKS.Types.ConnectorConfigResponse
import Amazonka.EKS.Types.ControlPlanePlacementRequest
import Amazonka.EKS.Types.ControlPlanePlacementResponse
import Amazonka.EKS.Types.EncryptionConfig
import Amazonka.EKS.Types.ErrorDetail
import Amazonka.EKS.Types.FargateProfile
import Amazonka.EKS.Types.FargateProfileSelector
import Amazonka.EKS.Types.Identity
import Amazonka.EKS.Types.IdentityProviderConfig
import Amazonka.EKS.Types.IdentityProviderConfigResponse
import Amazonka.EKS.Types.Issue
import Amazonka.EKS.Types.KubernetesNetworkConfigRequest
import Amazonka.EKS.Types.KubernetesNetworkConfigResponse
import Amazonka.EKS.Types.LaunchTemplateSpecification
import Amazonka.EKS.Types.LogSetup
import Amazonka.EKS.Types.Logging
import Amazonka.EKS.Types.Nodegroup
import Amazonka.EKS.Types.NodegroupHealth
import Amazonka.EKS.Types.NodegroupResources
import Amazonka.EKS.Types.NodegroupScalingConfig
import Amazonka.EKS.Types.NodegroupUpdateConfig
import Amazonka.EKS.Types.OIDC
import Amazonka.EKS.Types.OidcIdentityProviderConfig
import Amazonka.EKS.Types.OidcIdentityProviderConfigRequest
import Amazonka.EKS.Types.OutpostConfigRequest
import Amazonka.EKS.Types.OutpostConfigResponse
import Amazonka.EKS.Types.Provider
import Amazonka.EKS.Types.RemoteAccessConfig
import Amazonka.EKS.Types.Taint
import Amazonka.EKS.Types.Update
import Amazonka.EKS.Types.UpdateLabelsPayload
import Amazonka.EKS.Types.UpdateParam
import Amazonka.EKS.Types.UpdateTaintsPayload
import Amazonka.EKS.Types.VpcConfigRequest
import Amazonka.EKS.Types.VpcConfigResponse
import Amazonka.EKS.UntagResource
import Amazonka.EKS.UpdateAddon
import Amazonka.EKS.UpdateClusterConfig
import Amazonka.EKS.UpdateClusterVersion
import Amazonka.EKS.UpdateNodegroupConfig
import Amazonka.EKS.UpdateNodegroupVersion

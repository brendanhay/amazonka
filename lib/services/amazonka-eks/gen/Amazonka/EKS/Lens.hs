{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EKS.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    associateIdentityProviderConfig_clientRequestToken,
    associateIdentityProviderConfig_tags,
    associateIdentityProviderConfig_clusterName,
    associateIdentityProviderConfig_oidc,
    associateIdentityProviderConfigResponse_tags,
    associateIdentityProviderConfigResponse_update,
    associateIdentityProviderConfigResponse_httpStatus,

    -- ** CreateAddon
    createAddon_addonVersion,
    createAddon_clientRequestToken,
    createAddon_configurationValues,
    createAddon_resolveConflicts,
    createAddon_serviceAccountRoleArn,
    createAddon_tags,
    createAddon_clusterName,
    createAddon_addonName,
    createAddonResponse_addon,
    createAddonResponse_httpStatus,

    -- ** CreateCluster
    createCluster_clientRequestToken,
    createCluster_encryptionConfig,
    createCluster_kubernetesNetworkConfig,
    createCluster_logging,
    createCluster_outpostConfig,
    createCluster_tags,
    createCluster_version,
    createCluster_name,
    createCluster_roleArn,
    createCluster_resourcesVpcConfig,
    createClusterResponse_cluster,
    createClusterResponse_httpStatus,

    -- ** CreateFargateProfile
    createFargateProfile_clientRequestToken,
    createFargateProfile_selectors,
    createFargateProfile_subnets,
    createFargateProfile_tags,
    createFargateProfile_fargateProfileName,
    createFargateProfile_clusterName,
    createFargateProfile_podExecutionRoleArn,
    createFargateProfileResponse_fargateProfile,
    createFargateProfileResponse_httpStatus,

    -- ** CreateNodegroup
    createNodegroup_amiType,
    createNodegroup_capacityType,
    createNodegroup_clientRequestToken,
    createNodegroup_diskSize,
    createNodegroup_instanceTypes,
    createNodegroup_labels,
    createNodegroup_launchTemplate,
    createNodegroup_releaseVersion,
    createNodegroup_remoteAccess,
    createNodegroup_scalingConfig,
    createNodegroup_tags,
    createNodegroup_taints,
    createNodegroup_updateConfig,
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

    -- ** DescribeAddonConfiguration
    describeAddonConfiguration_addonName,
    describeAddonConfiguration_addonVersion,
    describeAddonConfigurationResponse_addonName,
    describeAddonConfigurationResponse_addonVersion,
    describeAddonConfigurationResponse_configurationSchema,
    describeAddonConfigurationResponse_httpStatus,

    -- ** DescribeAddonVersions
    describeAddonVersions_addonName,
    describeAddonVersions_kubernetesVersion,
    describeAddonVersions_maxResults,
    describeAddonVersions_nextToken,
    describeAddonVersions_owners,
    describeAddonVersions_publishers,
    describeAddonVersions_types,
    describeAddonVersionsResponse_addons,
    describeAddonVersionsResponse_nextToken,
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
    describeUpdate_addonName,
    describeUpdate_nodegroupName,
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
    listAddons_maxResults,
    listAddons_nextToken,
    listAddons_clusterName,
    listAddonsResponse_addons,
    listAddonsResponse_nextToken,
    listAddonsResponse_httpStatus,

    -- ** ListClusters
    listClusters_include,
    listClusters_maxResults,
    listClusters_nextToken,
    listClustersResponse_clusters,
    listClustersResponse_nextToken,
    listClustersResponse_httpStatus,

    -- ** ListFargateProfiles
    listFargateProfiles_maxResults,
    listFargateProfiles_nextToken,
    listFargateProfiles_clusterName,
    listFargateProfilesResponse_fargateProfileNames,
    listFargateProfilesResponse_nextToken,
    listFargateProfilesResponse_httpStatus,

    -- ** ListIdentityProviderConfigs
    listIdentityProviderConfigs_maxResults,
    listIdentityProviderConfigs_nextToken,
    listIdentityProviderConfigs_clusterName,
    listIdentityProviderConfigsResponse_identityProviderConfigs,
    listIdentityProviderConfigsResponse_nextToken,
    listIdentityProviderConfigsResponse_httpStatus,

    -- ** ListNodegroups
    listNodegroups_maxResults,
    listNodegroups_nextToken,
    listNodegroups_clusterName,
    listNodegroupsResponse_nextToken,
    listNodegroupsResponse_nodegroups,
    listNodegroupsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListUpdates
    listUpdates_addonName,
    listUpdates_maxResults,
    listUpdates_nextToken,
    listUpdates_nodegroupName,
    listUpdates_name,
    listUpdatesResponse_nextToken,
    listUpdatesResponse_updateIds,
    listUpdatesResponse_httpStatus,

    -- ** RegisterCluster
    registerCluster_clientRequestToken,
    registerCluster_tags,
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
    updateAddon_addonVersion,
    updateAddon_clientRequestToken,
    updateAddon_configurationValues,
    updateAddon_resolveConflicts,
    updateAddon_serviceAccountRoleArn,
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
    updateNodegroupConfig_labels,
    updateNodegroupConfig_scalingConfig,
    updateNodegroupConfig_taints,
    updateNodegroupConfig_updateConfig,
    updateNodegroupConfig_clusterName,
    updateNodegroupConfig_nodegroupName,
    updateNodegroupConfigResponse_update,
    updateNodegroupConfigResponse_httpStatus,

    -- ** UpdateNodegroupVersion
    updateNodegroupVersion_clientRequestToken,
    updateNodegroupVersion_force,
    updateNodegroupVersion_launchTemplate,
    updateNodegroupVersion_releaseVersion,
    updateNodegroupVersion_version,
    updateNodegroupVersion_clusterName,
    updateNodegroupVersion_nodegroupName,
    updateNodegroupVersionResponse_update,
    updateNodegroupVersionResponse_httpStatus,

    -- * Types

    -- ** Addon
    addon_addonArn,
    addon_addonName,
    addon_addonVersion,
    addon_clusterName,
    addon_configurationValues,
    addon_createdAt,
    addon_health,
    addon_marketplaceInformation,
    addon_modifiedAt,
    addon_owner,
    addon_publisher,
    addon_serviceAccountRoleArn,
    addon_status,
    addon_tags,

    -- ** AddonHealth
    addonHealth_issues,

    -- ** AddonInfo
    addonInfo_addonName,
    addonInfo_addonVersions,
    addonInfo_marketplaceInformation,
    addonInfo_owner,
    addonInfo_publisher,
    addonInfo_type,

    -- ** AddonIssue
    addonIssue_code,
    addonIssue_message,
    addonIssue_resourceIds,

    -- ** AddonVersionInfo
    addonVersionInfo_addonVersion,
    addonVersionInfo_architecture,
    addonVersionInfo_compatibilities,
    addonVersionInfo_requiresConfiguration,

    -- ** AutoScalingGroup
    autoScalingGroup_name,

    -- ** Certificate
    certificate_data,

    -- ** Cluster
    cluster_arn,
    cluster_certificateAuthority,
    cluster_clientRequestToken,
    cluster_connectorConfig,
    cluster_createdAt,
    cluster_encryptionConfig,
    cluster_endpoint,
    cluster_health,
    cluster_id,
    cluster_identity,
    cluster_kubernetesNetworkConfig,
    cluster_logging,
    cluster_name,
    cluster_outpostConfig,
    cluster_platformVersion,
    cluster_resourcesVpcConfig,
    cluster_roleArn,
    cluster_status,
    cluster_tags,
    cluster_version,

    -- ** ClusterHealth
    clusterHealth_issues,

    -- ** ClusterIssue
    clusterIssue_code,
    clusterIssue_message,
    clusterIssue_resourceIds,

    -- ** Compatibility
    compatibility_clusterVersion,
    compatibility_defaultVersion,
    compatibility_platformVersions,

    -- ** ConnectorConfigRequest
    connectorConfigRequest_roleArn,
    connectorConfigRequest_provider,

    -- ** ConnectorConfigResponse
    connectorConfigResponse_activationCode,
    connectorConfigResponse_activationExpiry,
    connectorConfigResponse_activationId,
    connectorConfigResponse_provider,
    connectorConfigResponse_roleArn,

    -- ** ControlPlanePlacementRequest
    controlPlanePlacementRequest_groupName,

    -- ** ControlPlanePlacementResponse
    controlPlanePlacementResponse_groupName,

    -- ** EncryptionConfig
    encryptionConfig_provider,
    encryptionConfig_resources,

    -- ** ErrorDetail
    errorDetail_errorCode,
    errorDetail_errorMessage,
    errorDetail_resourceIds,

    -- ** FargateProfile
    fargateProfile_clusterName,
    fargateProfile_createdAt,
    fargateProfile_fargateProfileArn,
    fargateProfile_fargateProfileName,
    fargateProfile_podExecutionRoleArn,
    fargateProfile_selectors,
    fargateProfile_status,
    fargateProfile_subnets,
    fargateProfile_tags,

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
    issue_code,
    issue_message,
    issue_resourceIds,

    -- ** KubernetesNetworkConfigRequest
    kubernetesNetworkConfigRequest_ipFamily,
    kubernetesNetworkConfigRequest_serviceIpv4Cidr,

    -- ** KubernetesNetworkConfigResponse
    kubernetesNetworkConfigResponse_ipFamily,
    kubernetesNetworkConfigResponse_serviceIpv4Cidr,
    kubernetesNetworkConfigResponse_serviceIpv6Cidr,

    -- ** LaunchTemplateSpecification
    launchTemplateSpecification_id,
    launchTemplateSpecification_name,
    launchTemplateSpecification_version,

    -- ** LogSetup
    logSetup_enabled,
    logSetup_types,

    -- ** Logging
    logging_clusterLogging,

    -- ** MarketplaceInformation
    marketplaceInformation_productId,
    marketplaceInformation_productUrl,

    -- ** Nodegroup
    nodegroup_amiType,
    nodegroup_capacityType,
    nodegroup_clusterName,
    nodegroup_createdAt,
    nodegroup_diskSize,
    nodegroup_health,
    nodegroup_instanceTypes,
    nodegroup_labels,
    nodegroup_launchTemplate,
    nodegroup_modifiedAt,
    nodegroup_nodeRole,
    nodegroup_nodegroupArn,
    nodegroup_nodegroupName,
    nodegroup_releaseVersion,
    nodegroup_remoteAccess,
    nodegroup_resources,
    nodegroup_scalingConfig,
    nodegroup_status,
    nodegroup_subnets,
    nodegroup_tags,
    nodegroup_taints,
    nodegroup_updateConfig,
    nodegroup_version,

    -- ** NodegroupHealth
    nodegroupHealth_issues,

    -- ** NodegroupResources
    nodegroupResources_autoScalingGroups,
    nodegroupResources_remoteAccessSecurityGroup,

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
    oidcIdentityProviderConfig_clientId,
    oidcIdentityProviderConfig_clusterName,
    oidcIdentityProviderConfig_groupsClaim,
    oidcIdentityProviderConfig_groupsPrefix,
    oidcIdentityProviderConfig_identityProviderConfigArn,
    oidcIdentityProviderConfig_identityProviderConfigName,
    oidcIdentityProviderConfig_issuerUrl,
    oidcIdentityProviderConfig_requiredClaims,
    oidcIdentityProviderConfig_status,
    oidcIdentityProviderConfig_tags,
    oidcIdentityProviderConfig_usernameClaim,
    oidcIdentityProviderConfig_usernamePrefix,

    -- ** OidcIdentityProviderConfigRequest
    oidcIdentityProviderConfigRequest_groupsClaim,
    oidcIdentityProviderConfigRequest_groupsPrefix,
    oidcIdentityProviderConfigRequest_requiredClaims,
    oidcIdentityProviderConfigRequest_usernameClaim,
    oidcIdentityProviderConfigRequest_usernamePrefix,
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
    remoteAccessConfig_ec2SshKey,
    remoteAccessConfig_sourceSecurityGroups,

    -- ** Taint
    taint_effect,
    taint_key,
    taint_value,

    -- ** Update
    update_createdAt,
    update_errors,
    update_id,
    update_params,
    update_status,
    update_type,

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
    vpcConfigRequest_endpointPrivateAccess,
    vpcConfigRequest_endpointPublicAccess,
    vpcConfigRequest_publicAccessCidrs,
    vpcConfigRequest_securityGroupIds,
    vpcConfigRequest_subnetIds,

    -- ** VpcConfigResponse
    vpcConfigResponse_clusterSecurityGroupId,
    vpcConfigResponse_endpointPrivateAccess,
    vpcConfigResponse_endpointPublicAccess,
    vpcConfigResponse_publicAccessCidrs,
    vpcConfigResponse_securityGroupIds,
    vpcConfigResponse_subnetIds,
    vpcConfigResponse_vpcId,
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
import Amazonka.EKS.Types.MarketplaceInformation
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

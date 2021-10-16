{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EKS.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _NotFoundException,
    _BadRequestException,
    _ResourceLimitExceededException,
    _ServiceUnavailableException,
    _InvalidParameterException,
    _InvalidRequestException,
    _ResourceInUseException,
    _ResourceNotFoundException,
    _ClientException,
    _UnsupportedAvailabilityZoneException,
    _ServerException,

    -- * AMITypes
    AMITypes (..),

    -- * AddonIssueCode
    AddonIssueCode (..),

    -- * AddonStatus
    AddonStatus (..),

    -- * CapacityTypes
    CapacityTypes (..),

    -- * ClusterStatus
    ClusterStatus (..),

    -- * ConfigStatus
    ConfigStatus (..),

    -- * ConnectorConfigProvider
    ConnectorConfigProvider (..),

    -- * EKSErrorCode
    EKSErrorCode (..),

    -- * FargateProfileStatus
    FargateProfileStatus (..),

    -- * LogType
    LogType (..),

    -- * NodegroupIssueCode
    NodegroupIssueCode (..),

    -- * NodegroupStatus
    NodegroupStatus (..),

    -- * ResolveConflicts
    ResolveConflicts (..),

    -- * TaintEffect
    TaintEffect (..),

    -- * UpdateParamType
    UpdateParamType (..),

    -- * UpdateStatus
    UpdateStatus (..),

    -- * UpdateType
    UpdateType (..),

    -- * Addon
    Addon (..),
    newAddon,
    addon_status,
    addon_modifiedAt,
    addon_serviceAccountRoleArn,
    addon_createdAt,
    addon_addonArn,
    addon_addonVersion,
    addon_addonName,
    addon_health,
    addon_tags,
    addon_clusterName,

    -- * AddonHealth
    AddonHealth (..),
    newAddonHealth,
    addonHealth_issues,

    -- * AddonInfo
    AddonInfo (..),
    newAddonInfo,
    addonInfo_addonVersions,
    addonInfo_addonName,
    addonInfo_type,

    -- * AddonIssue
    AddonIssue (..),
    newAddonIssue,
    addonIssue_message,
    addonIssue_code,
    addonIssue_resourceIds,

    -- * AddonVersionInfo
    AddonVersionInfo (..),
    newAddonVersionInfo,
    addonVersionInfo_compatibilities,
    addonVersionInfo_architecture,
    addonVersionInfo_addonVersion,

    -- * AutoScalingGroup
    AutoScalingGroup (..),
    newAutoScalingGroup,
    autoScalingGroup_name,

    -- * Certificate
    Certificate (..),
    newCertificate,
    certificate_data,

    -- * Cluster
    Cluster (..),
    newCluster,
    cluster_status,
    cluster_roleArn,
    cluster_identity,
    cluster_kubernetesNetworkConfig,
    cluster_resourcesVpcConfig,
    cluster_logging,
    cluster_encryptionConfig,
    cluster_createdAt,
    cluster_platformVersion,
    cluster_arn,
    cluster_version,
    cluster_name,
    cluster_certificateAuthority,
    cluster_connectorConfig,
    cluster_tags,
    cluster_endpoint,
    cluster_clientRequestToken,

    -- * Compatibility
    Compatibility (..),
    newCompatibility,
    compatibility_defaultVersion,
    compatibility_platformVersions,
    compatibility_clusterVersion,

    -- * ConnectorConfigRequest
    ConnectorConfigRequest (..),
    newConnectorConfigRequest,
    connectorConfigRequest_roleArn,
    connectorConfigRequest_provider,

    -- * ConnectorConfigResponse
    ConnectorConfigResponse (..),
    newConnectorConfigResponse,
    connectorConfigResponse_activationCode,
    connectorConfigResponse_roleArn,
    connectorConfigResponse_activationId,
    connectorConfigResponse_provider,
    connectorConfigResponse_activationExpiry,

    -- * EncryptionConfig
    EncryptionConfig (..),
    newEncryptionConfig,
    encryptionConfig_resources,
    encryptionConfig_provider,

    -- * ErrorDetail
    ErrorDetail (..),
    newErrorDetail,
    errorDetail_resourceIds,
    errorDetail_errorMessage,
    errorDetail_errorCode,

    -- * FargateProfile
    FargateProfile (..),
    newFargateProfile,
    fargateProfile_status,
    fargateProfile_fargateProfileName,
    fargateProfile_podExecutionRoleArn,
    fargateProfile_createdAt,
    fargateProfile_fargateProfileArn,
    fargateProfile_tags,
    fargateProfile_selectors,
    fargateProfile_subnets,
    fargateProfile_clusterName,

    -- * FargateProfileSelector
    FargateProfileSelector (..),
    newFargateProfileSelector,
    fargateProfileSelector_labels,
    fargateProfileSelector_namespace,

    -- * Identity
    Identity (..),
    newIdentity,
    identity_oidc,

    -- * IdentityProviderConfig
    IdentityProviderConfig (..),
    newIdentityProviderConfig,
    identityProviderConfig_type,
    identityProviderConfig_name,

    -- * IdentityProviderConfigResponse
    IdentityProviderConfigResponse (..),
    newIdentityProviderConfigResponse,
    identityProviderConfigResponse_oidc,

    -- * Issue
    Issue (..),
    newIssue,
    issue_message,
    issue_code,
    issue_resourceIds,

    -- * KubernetesNetworkConfigRequest
    KubernetesNetworkConfigRequest (..),
    newKubernetesNetworkConfigRequest,
    kubernetesNetworkConfigRequest_serviceIpv4Cidr,

    -- * KubernetesNetworkConfigResponse
    KubernetesNetworkConfigResponse (..),
    newKubernetesNetworkConfigResponse,
    kubernetesNetworkConfigResponse_serviceIpv4Cidr,

    -- * LaunchTemplateSpecification
    LaunchTemplateSpecification (..),
    newLaunchTemplateSpecification,
    launchTemplateSpecification_id,
    launchTemplateSpecification_version,
    launchTemplateSpecification_name,

    -- * LogSetup
    LogSetup (..),
    newLogSetup,
    logSetup_enabled,
    logSetup_types,

    -- * Logging
    Logging (..),
    newLogging,
    logging_clusterLogging,

    -- * Nodegroup
    Nodegroup (..),
    newNodegroup,
    nodegroup_scalingConfig,
    nodegroup_capacityType,
    nodegroup_status,
    nodegroup_modifiedAt,
    nodegroup_releaseVersion,
    nodegroup_nodegroupName,
    nodegroup_remoteAccess,
    nodegroup_diskSize,
    nodegroup_createdAt,
    nodegroup_labels,
    nodegroup_launchTemplate,
    nodegroup_version,
    nodegroup_nodeRole,
    nodegroup_health,
    nodegroup_resources,
    nodegroup_tags,
    nodegroup_updateConfig,
    nodegroup_subnets,
    nodegroup_clusterName,
    nodegroup_amiType,
    nodegroup_taints,
    nodegroup_instanceTypes,
    nodegroup_nodegroupArn,

    -- * NodegroupHealth
    NodegroupHealth (..),
    newNodegroupHealth,
    nodegroupHealth_issues,

    -- * NodegroupResources
    NodegroupResources (..),
    newNodegroupResources,
    nodegroupResources_remoteAccessSecurityGroup,
    nodegroupResources_autoScalingGroups,

    -- * NodegroupScalingConfig
    NodegroupScalingConfig (..),
    newNodegroupScalingConfig,
    nodegroupScalingConfig_minSize,
    nodegroupScalingConfig_desiredSize,
    nodegroupScalingConfig_maxSize,

    -- * NodegroupUpdateConfig
    NodegroupUpdateConfig (..),
    newNodegroupUpdateConfig,
    nodegroupUpdateConfig_maxUnavailablePercentage,
    nodegroupUpdateConfig_maxUnavailable,

    -- * OIDC
    OIDC (..),
    newOIDC,
    oidc_issuer,

    -- * OidcIdentityProviderConfig
    OidcIdentityProviderConfig (..),
    newOidcIdentityProviderConfig,
    oidcIdentityProviderConfig_groupsPrefix,
    oidcIdentityProviderConfig_status,
    oidcIdentityProviderConfig_clientId,
    oidcIdentityProviderConfig_groupsClaim,
    oidcIdentityProviderConfig_requiredClaims,
    oidcIdentityProviderConfig_identityProviderConfigName,
    oidcIdentityProviderConfig_usernameClaim,
    oidcIdentityProviderConfig_tags,
    oidcIdentityProviderConfig_usernamePrefix,
    oidcIdentityProviderConfig_identityProviderConfigArn,
    oidcIdentityProviderConfig_issuerUrl,
    oidcIdentityProviderConfig_clusterName,

    -- * OidcIdentityProviderConfigRequest
    OidcIdentityProviderConfigRequest (..),
    newOidcIdentityProviderConfigRequest,
    oidcIdentityProviderConfigRequest_groupsPrefix,
    oidcIdentityProviderConfigRequest_groupsClaim,
    oidcIdentityProviderConfigRequest_requiredClaims,
    oidcIdentityProviderConfigRequest_usernameClaim,
    oidcIdentityProviderConfigRequest_usernamePrefix,
    oidcIdentityProviderConfigRequest_identityProviderConfigName,
    oidcIdentityProviderConfigRequest_issuerUrl,
    oidcIdentityProviderConfigRequest_clientId,

    -- * Provider
    Provider (..),
    newProvider,
    provider_keyArn,

    -- * RemoteAccessConfig
    RemoteAccessConfig (..),
    newRemoteAccessConfig,
    remoteAccessConfig_ec2SshKey,
    remoteAccessConfig_sourceSecurityGroups,

    -- * Taint
    Taint (..),
    newTaint,
    taint_effect,
    taint_key,
    taint_value,

    -- * Update
    Update (..),
    newUpdate,
    update_status,
    update_id,
    update_createdAt,
    update_params,
    update_errors,
    update_type,

    -- * UpdateLabelsPayload
    UpdateLabelsPayload (..),
    newUpdateLabelsPayload,
    updateLabelsPayload_removeLabels,
    updateLabelsPayload_addOrUpdateLabels,

    -- * UpdateParam
    UpdateParam (..),
    newUpdateParam,
    updateParam_value,
    updateParam_type,

    -- * UpdateTaintsPayload
    UpdateTaintsPayload (..),
    newUpdateTaintsPayload,
    updateTaintsPayload_removeTaints,
    updateTaintsPayload_addOrUpdateTaints,

    -- * VpcConfigRequest
    VpcConfigRequest (..),
    newVpcConfigRequest,
    vpcConfigRequest_securityGroupIds,
    vpcConfigRequest_endpointPublicAccess,
    vpcConfigRequest_subnetIds,
    vpcConfigRequest_endpointPrivateAccess,
    vpcConfigRequest_publicAccessCidrs,

    -- * VpcConfigResponse
    VpcConfigResponse (..),
    newVpcConfigResponse,
    vpcConfigResponse_securityGroupIds,
    vpcConfigResponse_endpointPublicAccess,
    vpcConfigResponse_subnetIds,
    vpcConfigResponse_clusterSecurityGroupId,
    vpcConfigResponse_vpcId,
    vpcConfigResponse_endpointPrivateAccess,
    vpcConfigResponse_publicAccessCidrs,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EKS.Types.AMITypes
import Network.AWS.EKS.Types.Addon
import Network.AWS.EKS.Types.AddonHealth
import Network.AWS.EKS.Types.AddonInfo
import Network.AWS.EKS.Types.AddonIssue
import Network.AWS.EKS.Types.AddonIssueCode
import Network.AWS.EKS.Types.AddonStatus
import Network.AWS.EKS.Types.AddonVersionInfo
import Network.AWS.EKS.Types.AutoScalingGroup
import Network.AWS.EKS.Types.CapacityTypes
import Network.AWS.EKS.Types.Certificate
import Network.AWS.EKS.Types.Cluster
import Network.AWS.EKS.Types.ClusterStatus
import Network.AWS.EKS.Types.Compatibility
import Network.AWS.EKS.Types.ConfigStatus
import Network.AWS.EKS.Types.ConnectorConfigProvider
import Network.AWS.EKS.Types.ConnectorConfigRequest
import Network.AWS.EKS.Types.ConnectorConfigResponse
import Network.AWS.EKS.Types.EKSErrorCode
import Network.AWS.EKS.Types.EncryptionConfig
import Network.AWS.EKS.Types.ErrorDetail
import Network.AWS.EKS.Types.FargateProfile
import Network.AWS.EKS.Types.FargateProfileSelector
import Network.AWS.EKS.Types.FargateProfileStatus
import Network.AWS.EKS.Types.Identity
import Network.AWS.EKS.Types.IdentityProviderConfig
import Network.AWS.EKS.Types.IdentityProviderConfigResponse
import Network.AWS.EKS.Types.Issue
import Network.AWS.EKS.Types.KubernetesNetworkConfigRequest
import Network.AWS.EKS.Types.KubernetesNetworkConfigResponse
import Network.AWS.EKS.Types.LaunchTemplateSpecification
import Network.AWS.EKS.Types.LogSetup
import Network.AWS.EKS.Types.LogType
import Network.AWS.EKS.Types.Logging
import Network.AWS.EKS.Types.Nodegroup
import Network.AWS.EKS.Types.NodegroupHealth
import Network.AWS.EKS.Types.NodegroupIssueCode
import Network.AWS.EKS.Types.NodegroupResources
import Network.AWS.EKS.Types.NodegroupScalingConfig
import Network.AWS.EKS.Types.NodegroupStatus
import Network.AWS.EKS.Types.NodegroupUpdateConfig
import Network.AWS.EKS.Types.OIDC
import Network.AWS.EKS.Types.OidcIdentityProviderConfig
import Network.AWS.EKS.Types.OidcIdentityProviderConfigRequest
import Network.AWS.EKS.Types.Provider
import Network.AWS.EKS.Types.RemoteAccessConfig
import Network.AWS.EKS.Types.ResolveConflicts
import Network.AWS.EKS.Types.Taint
import Network.AWS.EKS.Types.TaintEffect
import Network.AWS.EKS.Types.Update
import Network.AWS.EKS.Types.UpdateLabelsPayload
import Network.AWS.EKS.Types.UpdateParam
import Network.AWS.EKS.Types.UpdateParamType
import Network.AWS.EKS.Types.UpdateStatus
import Network.AWS.EKS.Types.UpdateTaintsPayload
import Network.AWS.EKS.Types.UpdateType
import Network.AWS.EKS.Types.VpcConfigRequest
import Network.AWS.EKS.Types.VpcConfigResponse
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-11-01@ of the Amazon Elastic Kubernetes Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "EKS",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "eks",
      Core._serviceSigningName = "eks",
      Core._serviceVersion = "2017-11-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "EKS",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | A service resource associated with the request could not be found.
-- Clients should not retry such requests.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | This exception is thrown if the request contains a semantic error. The
-- precise meaning will depend on the API, and will be documented in the
-- error message.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400

-- | You have encountered a service limit on the specified resource.
_ResourceLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ResourceLimitExceededException"
    Prelude.. Core.hasStatus 400

-- | The service is unavailable. Back off and retry the operation.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | The specified parameter is invalid. Review the available parameters for
-- the API request.
_InvalidParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterException"
    Prelude.. Core.hasStatus 400

-- | The request is invalid given the state of the cluster. Check the state
-- of the cluster and the associated operations.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"
    Prelude.. Core.hasStatus 400

-- | The specified resource is in use.
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"
    Prelude.. Core.hasStatus 409

-- | The specified resource could not be found. You can view your available
-- clusters with ListClusters. You can view your available managed node
-- groups with ListNodegroups. Amazon EKS clusters and node groups are
-- Region-specific.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | These errors are usually caused by a client action. Actions can include
-- using an action or resource on behalf of a user that doesn\'t have
-- permissions to use the action or resource or specifying an identifier
-- that is not valid.
_ClientException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClientException =
  Core._MatchServiceError
    defaultService
    "ClientException"
    Prelude.. Core.hasStatus 400

-- | At least one of your specified cluster subnets is in an Availability
-- Zone that does not support Amazon EKS. The exception output specifies
-- the supported Availability Zones for your account, from which you can
-- choose subnets for your cluster.
_UnsupportedAvailabilityZoneException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnsupportedAvailabilityZoneException =
  Core._MatchServiceError
    defaultService
    "UnsupportedAvailabilityZoneException"
    Prelude.. Core.hasStatus 400

-- | These errors are usually caused by a server-side issue.
_ServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServerException =
  Core._MatchServiceError
    defaultService
    "ServerException"
    Prelude.. Core.hasStatus 500

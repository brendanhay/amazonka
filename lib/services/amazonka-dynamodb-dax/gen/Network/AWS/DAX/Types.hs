{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _SubnetInUse,
    _ServiceQuotaExceededException,
    _InvalidVPCNetworkStateFault,
    _SubnetQuotaExceededFault,
    _ParameterGroupQuotaExceededFault,
    _InsufficientClusterCapacityFault,
    _ServiceLinkedRoleNotFoundFault,
    _InvalidParameterCombinationException,
    _ClusterNotFoundFault,
    _SubnetGroupQuotaExceededFault,
    _TagNotFoundFault,
    _SubnetGroupAlreadyExistsFault,
    _NodeQuotaForCustomerExceededFault,
    _SubnetGroupNotFoundFault,
    _ParameterGroupAlreadyExistsFault,
    _ParameterGroupNotFoundFault,
    _InvalidParameterValueException,
    _NodeNotFoundFault,
    _NodeQuotaForClusterExceededFault,
    _ClusterAlreadyExistsFault,
    _InvalidARNFault,
    _InvalidClusterStateFault,
    _TagQuotaPerResourceExceeded,
    _SubnetGroupInUseFault,
    _InvalidParameterGroupStateFault,
    _ClusterQuotaForCustomerExceededFault,
    _InvalidSubnet,

    -- * ChangeType
    ChangeType (..),

    -- * ClusterEndpointEncryptionType
    ClusterEndpointEncryptionType (..),

    -- * IsModifiable
    IsModifiable (..),

    -- * ParameterType
    ParameterType (..),

    -- * SSEStatus
    SSEStatus (..),

    -- * SourceType
    SourceType (..),

    -- * Cluster
    Cluster (..),
    newCluster,
    cluster_clusterArn,
    cluster_status,
    cluster_iamRoleArn,
    cluster_subnetGroup,
    cluster_totalNodes,
    cluster_parameterGroup,
    cluster_clusterEndpointEncryptionType,
    cluster_nodes,
    cluster_notificationConfiguration,
    cluster_securityGroups,
    cluster_activeNodes,
    cluster_preferredMaintenanceWindow,
    cluster_description,
    cluster_sSEDescription,
    cluster_nodeIdsToRemove,
    cluster_clusterDiscoveryEndpoint,
    cluster_nodeType,
    cluster_clusterName,

    -- * Endpoint
    Endpoint (..),
    newEndpoint,
    endpoint_address,
    endpoint_port,
    endpoint_url,

    -- * Event
    Event (..),
    newEvent,
    event_message,
    event_sourceName,
    event_date,
    event_sourceType,

    -- * Node
    Node (..),
    newNode,
    node_nodeStatus,
    node_nodeId,
    node_parameterGroupStatus,
    node_availabilityZone,
    node_nodeCreateTime,
    node_endpoint,

    -- * NodeTypeSpecificValue
    NodeTypeSpecificValue (..),
    newNodeTypeSpecificValue,
    nodeTypeSpecificValue_value,
    nodeTypeSpecificValue_nodeType,

    -- * NotificationConfiguration
    NotificationConfiguration (..),
    newNotificationConfiguration,
    notificationConfiguration_topicStatus,
    notificationConfiguration_topicArn,

    -- * Parameter
    Parameter (..),
    newParameter,
    parameter_changeType,
    parameter_allowedValues,
    parameter_source,
    parameter_parameterValue,
    parameter_parameterType,
    parameter_parameterName,
    parameter_description,
    parameter_nodeTypeSpecificValues,
    parameter_dataType,
    parameter_isModifiable,

    -- * ParameterGroup
    ParameterGroup (..),
    newParameterGroup,
    parameterGroup_parameterGroupName,
    parameterGroup_description,

    -- * ParameterGroupStatus
    ParameterGroupStatus (..),
    newParameterGroupStatus,
    parameterGroupStatus_nodeIdsToReboot,
    parameterGroupStatus_parameterGroupName,
    parameterGroupStatus_parameterApplyStatus,

    -- * ParameterNameValue
    ParameterNameValue (..),
    newParameterNameValue,
    parameterNameValue_parameterValue,
    parameterNameValue_parameterName,

    -- * SSEDescription
    SSEDescription (..),
    newSSEDescription,
    sSEDescription_status,

    -- * SSESpecification
    SSESpecification (..),
    newSSESpecification,
    sSESpecification_enabled,

    -- * SecurityGroupMembership
    SecurityGroupMembership (..),
    newSecurityGroupMembership,
    securityGroupMembership_status,
    securityGroupMembership_securityGroupIdentifier,

    -- * Subnet
    Subnet (..),
    newSubnet,
    subnet_subnetIdentifier,
    subnet_subnetAvailabilityZone,

    -- * SubnetGroup
    SubnetGroup (..),
    newSubnetGroup,
    subnetGroup_description,
    subnetGroup_subnetGroupName,
    subnetGroup_vpcId,
    subnetGroup_subnets,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DAX.Types.ChangeType
import Network.AWS.DAX.Types.Cluster
import Network.AWS.DAX.Types.ClusterEndpointEncryptionType
import Network.AWS.DAX.Types.Endpoint
import Network.AWS.DAX.Types.Event
import Network.AWS.DAX.Types.IsModifiable
import Network.AWS.DAX.Types.Node
import Network.AWS.DAX.Types.NodeTypeSpecificValue
import Network.AWS.DAX.Types.NotificationConfiguration
import Network.AWS.DAX.Types.Parameter
import Network.AWS.DAX.Types.ParameterGroup
import Network.AWS.DAX.Types.ParameterGroupStatus
import Network.AWS.DAX.Types.ParameterNameValue
import Network.AWS.DAX.Types.ParameterType
import Network.AWS.DAX.Types.SSEDescription
import Network.AWS.DAX.Types.SSESpecification
import Network.AWS.DAX.Types.SSEStatus
import Network.AWS.DAX.Types.SecurityGroupMembership
import Network.AWS.DAX.Types.SourceType
import Network.AWS.DAX.Types.Subnet
import Network.AWS.DAX.Types.SubnetGroup
import Network.AWS.DAX.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-04-19@ of the Amazon DynamoDB Accelerator (DAX) SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "DAX",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "dax",
      Core._serviceSigningName = "dax",
      Core._serviceVersion = "2017-04-19",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "DAX",
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

-- | The requested subnet is being used by another subnet group.
_SubnetInUse :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubnetInUse =
  Core._MatchServiceError
    defaultService
    "SubnetInUse"

-- | You have reached the maximum number of x509 certificates that can be
-- created for encrypted clusters in a 30 day period. Contact AWS customer
-- support to discuss options for continuing to create encrypted clusters.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"

-- | The VPC network is in an invalid state.
_InvalidVPCNetworkStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidVPCNetworkStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidVPCNetworkStateFault"

-- | The request cannot be processed because it would exceed the allowed
-- number of subnets in a subnet group.
_SubnetQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubnetQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "SubnetQuotaExceededFault"

-- | You have attempted to exceed the maximum number of parameter groups.
_ParameterGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ParameterGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "ParameterGroupQuotaExceededFault"

-- | There are not enough system resources to create the cluster you
-- requested (or to resize an already-existing cluster).
_InsufficientClusterCapacityFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InsufficientClusterCapacityFault =
  Core._MatchServiceError
    defaultService
    "InsufficientClusterCapacityFault"

-- | The specified service linked role (SLR) was not found.
_ServiceLinkedRoleNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceLinkedRoleNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ServiceLinkedRoleNotFoundFault"

-- | Two or more incompatible parameters were specified.
_InvalidParameterCombinationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterCombinationException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterCombinationException"

-- | The requested cluster ID does not refer to an existing DAX cluster.
_ClusterNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClusterNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ClusterNotFoundFault"

-- | The request cannot be processed because it would exceed the allowed
-- number of subnets in a subnet group.
_SubnetGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubnetGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "SubnetGroupQuotaExceededFault"

-- | The tag does not exist.
_TagNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TagNotFoundFault =
  Core._MatchServiceError
    defaultService
    "TagNotFoundFault"

-- | The specified subnet group already exists.
_SubnetGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubnetGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "SubnetGroupAlreadyExistsFault"

-- | You have attempted to exceed the maximum number of nodes for your AWS
-- account.
_NodeQuotaForCustomerExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NodeQuotaForCustomerExceededFault =
  Core._MatchServiceError
    defaultService
    "NodeQuotaForCustomerExceededFault"

-- | The requested subnet group name does not refer to an existing subnet
-- group.
_SubnetGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubnetGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SubnetGroupNotFoundFault"

-- | The specified parameter group already exists.
_ParameterGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ParameterGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ParameterGroupAlreadyExistsFault"

-- | The specified parameter group does not exist.
_ParameterGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ParameterGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ParameterGroupNotFoundFault"

-- | The value for a parameter is invalid.
_InvalidParameterValueException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterValueException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterValueException"

-- | None of the nodes in the cluster have the given node ID.
_NodeNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NodeNotFoundFault =
  Core._MatchServiceError
    defaultService
    "NodeNotFoundFault"

-- | You have attempted to exceed the maximum number of nodes for a DAX
-- cluster.
_NodeQuotaForClusterExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NodeQuotaForClusterExceededFault =
  Core._MatchServiceError
    defaultService
    "NodeQuotaForClusterExceededFault"

-- | You already have a DAX cluster with the given identifier.
_ClusterAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClusterAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ClusterAlreadyExistsFault"

-- | The Amazon Resource Name (ARN) supplied in the request is not valid.
_InvalidARNFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidARNFault =
  Core._MatchServiceError
    defaultService
    "InvalidARNFault"

-- | The requested DAX cluster is not in the /available/ state.
_InvalidClusterStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidClusterStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidClusterStateFault"

-- | You have exceeded the maximum number of tags for this DAX cluster.
_TagQuotaPerResourceExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TagQuotaPerResourceExceeded =
  Core._MatchServiceError
    defaultService
    "TagQuotaPerResourceExceeded"

-- | The specified subnet group is currently in use.
_SubnetGroupInUseFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubnetGroupInUseFault =
  Core._MatchServiceError
    defaultService
    "SubnetGroupInUseFault"

-- | One or more parameters in a parameter group are in an invalid state.
_InvalidParameterGroupStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidParameterGroupStateFault"

-- | You have attempted to exceed the maximum number of DAX clusters for your
-- AWS account.
_ClusterQuotaForCustomerExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClusterQuotaForCustomerExceededFault =
  Core._MatchServiceError
    defaultService
    "ClusterQuotaForCustomerExceededFault"

-- | An invalid subnet identifier was specified.
_InvalidSubnet :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSubnet =
  Core._MatchServiceError
    defaultService
    "InvalidSubnet"

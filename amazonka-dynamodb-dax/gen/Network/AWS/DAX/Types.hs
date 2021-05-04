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
    _InvalidVPCNetworkStateFault,
    _SubnetQuotaExceededFault,
    _ParameterGroupQuotaExceededFault,
    _InsufficientClusterCapacityFault,
    _ServiceLinkedRoleNotFoundFault,
    _InvalidParameterCombinationException,
    _SubnetGroupQuotaExceededFault,
    _TagNotFoundFault,
    _ClusterNotFoundFault,
    _SubnetGroupAlreadyExistsFault,
    _NodeQuotaForCustomerExceededFault,
    _SubnetGroupNotFoundFault,
    _ParameterGroupAlreadyExistsFault,
    _ParameterGroupNotFoundFault,
    _InvalidParameterValueException,
    _NodeNotFoundFault,
    _InvalidARNFault,
    _ClusterAlreadyExistsFault,
    _InvalidClusterStateFault,
    _NodeQuotaForClusterExceededFault,
    _InvalidSubnet,
    _ClusterQuotaForCustomerExceededFault,
    _SubnetGroupInUseFault,
    _InvalidParameterGroupStateFault,
    _TagQuotaPerResourceExceeded,

    -- * ChangeType
    ChangeType (..),

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
    cluster_subnetGroup,
    cluster_iamRoleArn,
    cluster_status,
    cluster_totalNodes,
    cluster_parameterGroup,
    cluster_nodes,
    cluster_notificationConfiguration,
    cluster_securityGroups,
    cluster_activeNodes,
    cluster_preferredMaintenanceWindow,
    cluster_description,
    cluster_sSEDescription,
    cluster_clusterDiscoveryEndpoint,
    cluster_nodeIdsToRemove,
    cluster_nodeType,
    cluster_clusterName,

    -- * Endpoint
    Endpoint (..),
    newEndpoint,
    endpoint_address,
    endpoint_port,

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
    parameter_dataType,
    parameter_isModifiable,
    parameter_nodeTypeSpecificValues,

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
    subnetGroup_subnets,
    subnetGroup_vpcId,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,
  )
where

import Network.AWS.DAX.Types.ChangeType
import Network.AWS.DAX.Types.Cluster
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
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "DAX",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcEndpointPrefix = "dax",
      Prelude._svcSigningName = "dax",
      Prelude._svcVersion = "2017-04-19",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError = Prelude.parseJSONError "DAX",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The requested subnet is being used by another subnet group.
_SubnetInUse :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SubnetInUse =
  Prelude._MatchServiceError
    defaultService
    "SubnetInUse"

-- | The VPC network is in an invalid state.
_InvalidVPCNetworkStateFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidVPCNetworkStateFault =
  Prelude._MatchServiceError
    defaultService
    "InvalidVPCNetworkStateFault"

-- | The request cannot be processed because it would exceed the allowed
-- number of subnets in a subnet group.
_SubnetQuotaExceededFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SubnetQuotaExceededFault =
  Prelude._MatchServiceError
    defaultService
    "SubnetQuotaExceededFault"

-- | You have attempted to exceed the maximum number of parameter groups.
_ParameterGroupQuotaExceededFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ParameterGroupQuotaExceededFault =
  Prelude._MatchServiceError
    defaultService
    "ParameterGroupQuotaExceededFault"

-- | There are not enough system resources to create the cluster you
-- requested (or to resize an already-existing cluster).
_InsufficientClusterCapacityFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InsufficientClusterCapacityFault =
  Prelude._MatchServiceError
    defaultService
    "InsufficientClusterCapacityFault"

-- | The specified service linked role (SLR) was not found.
_ServiceLinkedRoleNotFoundFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ServiceLinkedRoleNotFoundFault =
  Prelude._MatchServiceError
    defaultService
    "ServiceLinkedRoleNotFoundFault"

-- | Two or more incompatible parameters were specified.
_InvalidParameterCombinationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidParameterCombinationException =
  Prelude._MatchServiceError
    defaultService
    "InvalidParameterCombinationException"

-- | The request cannot be processed because it would exceed the allowed
-- number of subnets in a subnet group.
_SubnetGroupQuotaExceededFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SubnetGroupQuotaExceededFault =
  Prelude._MatchServiceError
    defaultService
    "SubnetGroupQuotaExceededFault"

-- | The tag does not exist.
_TagNotFoundFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TagNotFoundFault =
  Prelude._MatchServiceError
    defaultService
    "TagNotFoundFault"

-- | The requested cluster ID does not refer to an existing DAX cluster.
_ClusterNotFoundFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ClusterNotFoundFault =
  Prelude._MatchServiceError
    defaultService
    "ClusterNotFoundFault"

-- | The specified subnet group already exists.
_SubnetGroupAlreadyExistsFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SubnetGroupAlreadyExistsFault =
  Prelude._MatchServiceError
    defaultService
    "SubnetGroupAlreadyExistsFault"

-- | You have attempted to exceed the maximum number of nodes for your AWS
-- account.
_NodeQuotaForCustomerExceededFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NodeQuotaForCustomerExceededFault =
  Prelude._MatchServiceError
    defaultService
    "NodeQuotaForCustomerExceededFault"

-- | The requested subnet group name does not refer to an existing subnet
-- group.
_SubnetGroupNotFoundFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SubnetGroupNotFoundFault =
  Prelude._MatchServiceError
    defaultService
    "SubnetGroupNotFoundFault"

-- | The specified parameter group already exists.
_ParameterGroupAlreadyExistsFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ParameterGroupAlreadyExistsFault =
  Prelude._MatchServiceError
    defaultService
    "ParameterGroupAlreadyExistsFault"

-- | The specified parameter group does not exist.
_ParameterGroupNotFoundFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ParameterGroupNotFoundFault =
  Prelude._MatchServiceError
    defaultService
    "ParameterGroupNotFoundFault"

-- | The value for a parameter is invalid.
_InvalidParameterValueException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidParameterValueException =
  Prelude._MatchServiceError
    defaultService
    "InvalidParameterValueException"

-- | None of the nodes in the cluster have the given node ID.
_NodeNotFoundFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NodeNotFoundFault =
  Prelude._MatchServiceError
    defaultService
    "NodeNotFoundFault"

-- | The Amazon Resource Name (ARN) supplied in the request is not valid.
_InvalidARNFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidARNFault =
  Prelude._MatchServiceError
    defaultService
    "InvalidARNFault"

-- | You already have a DAX cluster with the given identifier.
_ClusterAlreadyExistsFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ClusterAlreadyExistsFault =
  Prelude._MatchServiceError
    defaultService
    "ClusterAlreadyExistsFault"

-- | The requested DAX cluster is not in the /available/ state.
_InvalidClusterStateFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidClusterStateFault =
  Prelude._MatchServiceError
    defaultService
    "InvalidClusterStateFault"

-- | You have attempted to exceed the maximum number of nodes for a DAX
-- cluster.
_NodeQuotaForClusterExceededFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NodeQuotaForClusterExceededFault =
  Prelude._MatchServiceError
    defaultService
    "NodeQuotaForClusterExceededFault"

-- | An invalid subnet identifier was specified.
_InvalidSubnet :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidSubnet =
  Prelude._MatchServiceError
    defaultService
    "InvalidSubnet"

-- | You have attempted to exceed the maximum number of DAX clusters for your
-- AWS account.
_ClusterQuotaForCustomerExceededFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ClusterQuotaForCustomerExceededFault =
  Prelude._MatchServiceError
    defaultService
    "ClusterQuotaForCustomerExceededFault"

-- | The specified subnet group is currently in use.
_SubnetGroupInUseFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SubnetGroupInUseFault =
  Prelude._MatchServiceError
    defaultService
    "SubnetGroupInUseFault"

-- | One or more parameters in a parameter group are in an invalid state.
_InvalidParameterGroupStateFault :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidParameterGroupStateFault =
  Prelude._MatchServiceError
    defaultService
    "InvalidParameterGroupStateFault"

-- | You have exceeded the maximum number of tags for this DAX cluster.
_TagQuotaPerResourceExceeded :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TagQuotaPerResourceExceeded =
  Prelude._MatchServiceError
    defaultService
    "TagQuotaPerResourceExceeded"

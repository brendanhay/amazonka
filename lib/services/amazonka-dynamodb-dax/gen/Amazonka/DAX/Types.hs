{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DAX.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DAX.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ClusterAlreadyExistsFault,
    _ClusterNotFoundFault,
    _ClusterQuotaForCustomerExceededFault,
    _InsufficientClusterCapacityFault,
    _InvalidARNFault,
    _InvalidClusterStateFault,
    _InvalidParameterCombinationException,
    _InvalidParameterGroupStateFault,
    _InvalidParameterValueException,
    _InvalidSubnet,
    _InvalidVPCNetworkStateFault,
    _NodeNotFoundFault,
    _NodeQuotaForClusterExceededFault,
    _NodeQuotaForCustomerExceededFault,
    _ParameterGroupAlreadyExistsFault,
    _ParameterGroupNotFoundFault,
    _ParameterGroupQuotaExceededFault,
    _ServiceLinkedRoleNotFoundFault,
    _ServiceQuotaExceededException,
    _SubnetGroupAlreadyExistsFault,
    _SubnetGroupInUseFault,
    _SubnetGroupNotFoundFault,
    _SubnetGroupQuotaExceededFault,
    _SubnetInUse,
    _SubnetQuotaExceededFault,
    _TagNotFoundFault,
    _TagQuotaPerResourceExceeded,

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
    cluster_activeNodes,
    cluster_clusterArn,
    cluster_clusterDiscoveryEndpoint,
    cluster_clusterEndpointEncryptionType,
    cluster_clusterName,
    cluster_description,
    cluster_iamRoleArn,
    cluster_nodeIdsToRemove,
    cluster_nodeType,
    cluster_nodes,
    cluster_notificationConfiguration,
    cluster_parameterGroup,
    cluster_preferredMaintenanceWindow,
    cluster_sSEDescription,
    cluster_securityGroups,
    cluster_status,
    cluster_subnetGroup,
    cluster_totalNodes,

    -- * Endpoint
    Endpoint (..),
    newEndpoint,
    endpoint_address,
    endpoint_port,
    endpoint_url,

    -- * Event
    Event (..),
    newEvent,
    event_date,
    event_message,
    event_sourceName,
    event_sourceType,

    -- * Node
    Node (..),
    newNode,
    node_availabilityZone,
    node_endpoint,
    node_nodeCreateTime,
    node_nodeId,
    node_nodeStatus,
    node_parameterGroupStatus,

    -- * NodeTypeSpecificValue
    NodeTypeSpecificValue (..),
    newNodeTypeSpecificValue,
    nodeTypeSpecificValue_nodeType,
    nodeTypeSpecificValue_value,

    -- * NotificationConfiguration
    NotificationConfiguration (..),
    newNotificationConfiguration,
    notificationConfiguration_topicArn,
    notificationConfiguration_topicStatus,

    -- * Parameter
    Parameter (..),
    newParameter,
    parameter_allowedValues,
    parameter_changeType,
    parameter_dataType,
    parameter_description,
    parameter_isModifiable,
    parameter_nodeTypeSpecificValues,
    parameter_parameterName,
    parameter_parameterType,
    parameter_parameterValue,
    parameter_source,

    -- * ParameterGroup
    ParameterGroup (..),
    newParameterGroup,
    parameterGroup_description,
    parameterGroup_parameterGroupName,

    -- * ParameterGroupStatus
    ParameterGroupStatus (..),
    newParameterGroupStatus,
    parameterGroupStatus_nodeIdsToReboot,
    parameterGroupStatus_parameterApplyStatus,
    parameterGroupStatus_parameterGroupName,

    -- * ParameterNameValue
    ParameterNameValue (..),
    newParameterNameValue,
    parameterNameValue_parameterName,
    parameterNameValue_parameterValue,

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
    securityGroupMembership_securityGroupIdentifier,
    securityGroupMembership_status,

    -- * Subnet
    Subnet (..),
    newSubnet,
    subnet_subnetAvailabilityZone,
    subnet_subnetIdentifier,

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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DAX.Types.ChangeType
import Amazonka.DAX.Types.Cluster
import Amazonka.DAX.Types.ClusterEndpointEncryptionType
import Amazonka.DAX.Types.Endpoint
import Amazonka.DAX.Types.Event
import Amazonka.DAX.Types.IsModifiable
import Amazonka.DAX.Types.Node
import Amazonka.DAX.Types.NodeTypeSpecificValue
import Amazonka.DAX.Types.NotificationConfiguration
import Amazonka.DAX.Types.Parameter
import Amazonka.DAX.Types.ParameterGroup
import Amazonka.DAX.Types.ParameterGroupStatus
import Amazonka.DAX.Types.ParameterNameValue
import Amazonka.DAX.Types.ParameterType
import Amazonka.DAX.Types.SSEDescription
import Amazonka.DAX.Types.SSESpecification
import Amazonka.DAX.Types.SSEStatus
import Amazonka.DAX.Types.SecurityGroupMembership
import Amazonka.DAX.Types.SourceType
import Amazonka.DAX.Types.Subnet
import Amazonka.DAX.Types.SubnetGroup
import Amazonka.DAX.Types.Tag
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2017-04-19@ of the Amazon DynamoDB Accelerator (DAX) SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "DAX",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "dax",
      Core.signingName = "dax",
      Core.version = "2017-04-19",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "DAX",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | You already have a DAX cluster with the given identifier.
_ClusterAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ClusterAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ClusterAlreadyExistsFault"

-- | The requested cluster ID does not refer to an existing DAX cluster.
_ClusterNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ClusterNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ClusterNotFoundFault"

-- | You have attempted to exceed the maximum number of DAX clusters for your
-- AWS account.
_ClusterQuotaForCustomerExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ClusterQuotaForCustomerExceededFault =
  Core._MatchServiceError
    defaultService
    "ClusterQuotaForCustomerExceededFault"

-- | There are not enough system resources to create the cluster you
-- requested (or to resize an already-existing cluster).
_InsufficientClusterCapacityFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InsufficientClusterCapacityFault =
  Core._MatchServiceError
    defaultService
    "InsufficientClusterCapacityFault"

-- | The Amazon Resource Name (ARN) supplied in the request is not valid.
_InvalidARNFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidARNFault =
  Core._MatchServiceError
    defaultService
    "InvalidARNFault"

-- | The requested DAX cluster is not in the /available/ state.
_InvalidClusterStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidClusterStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidClusterStateFault"

-- | Two or more incompatible parameters were specified.
_InvalidParameterCombinationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidParameterCombinationException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterCombinationException"

-- | One or more parameters in a parameter group are in an invalid state.
_InvalidParameterGroupStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidParameterGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidParameterGroupStateFault"

-- | The value for a parameter is invalid.
_InvalidParameterValueException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidParameterValueException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterValueException"

-- | An invalid subnet identifier was specified.
_InvalidSubnet :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidSubnet =
  Core._MatchServiceError
    defaultService
    "InvalidSubnet"

-- | The VPC network is in an invalid state.
_InvalidVPCNetworkStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidVPCNetworkStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidVPCNetworkStateFault"

-- | None of the nodes in the cluster have the given node ID.
_NodeNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NodeNotFoundFault =
  Core._MatchServiceError
    defaultService
    "NodeNotFoundFault"

-- | You have attempted to exceed the maximum number of nodes for a DAX
-- cluster.
_NodeQuotaForClusterExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NodeQuotaForClusterExceededFault =
  Core._MatchServiceError
    defaultService
    "NodeQuotaForClusterExceededFault"

-- | You have attempted to exceed the maximum number of nodes for your AWS
-- account.
_NodeQuotaForCustomerExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NodeQuotaForCustomerExceededFault =
  Core._MatchServiceError
    defaultService
    "NodeQuotaForCustomerExceededFault"

-- | The specified parameter group already exists.
_ParameterGroupAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ParameterGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ParameterGroupAlreadyExistsFault"

-- | The specified parameter group does not exist.
_ParameterGroupNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ParameterGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ParameterGroupNotFoundFault"

-- | You have attempted to exceed the maximum number of parameter groups.
_ParameterGroupQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ParameterGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "ParameterGroupQuotaExceededFault"

-- | The specified service linked role (SLR) was not found.
_ServiceLinkedRoleNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceLinkedRoleNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ServiceLinkedRoleNotFoundFault"

-- | You have reached the maximum number of x509 certificates that can be
-- created for encrypted clusters in a 30 day period. Contact AWS customer
-- support to discuss options for continuing to create encrypted clusters.
_ServiceQuotaExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"

-- | The specified subnet group already exists.
_SubnetGroupAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SubnetGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "SubnetGroupAlreadyExistsFault"

-- | The specified subnet group is currently in use.
_SubnetGroupInUseFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SubnetGroupInUseFault =
  Core._MatchServiceError
    defaultService
    "SubnetGroupInUseFault"

-- | The requested subnet group name does not refer to an existing subnet
-- group.
_SubnetGroupNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SubnetGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SubnetGroupNotFoundFault"

-- | The request cannot be processed because it would exceed the allowed
-- number of subnets in a subnet group.
_SubnetGroupQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SubnetGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "SubnetGroupQuotaExceededFault"

-- | The requested subnet is being used by another subnet group.
_SubnetInUse :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SubnetInUse =
  Core._MatchServiceError
    defaultService
    "SubnetInUse"

-- | The request cannot be processed because it would exceed the allowed
-- number of subnets in a subnet group.
_SubnetQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SubnetQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "SubnetQuotaExceededFault"

-- | The tag does not exist.
_TagNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TagNotFoundFault =
  Core._MatchServiceError
    defaultService
    "TagNotFoundFault"

-- | You have exceeded the maximum number of tags for this DAX cluster.
_TagQuotaPerResourceExceeded :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TagQuotaPerResourceExceeded =
  Core._MatchServiceError
    defaultService
    "TagQuotaPerResourceExceeded"

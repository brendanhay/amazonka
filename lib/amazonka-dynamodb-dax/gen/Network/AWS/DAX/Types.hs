-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _SubnetQuotaExceededFault,
    _ParameterGroupNotFoundFault,
    _InvalidParameterGroupStateFault,
    _SubnetGroupInUseFault,
    _ParameterGroupAlreadyExistsFault,
    _InvalidSubnet,
    _TagQuotaPerResourceExceeded,
    _ClusterNotFoundFault,
    _TagNotFoundFault,
    _NodeQuotaForClusterExceededFault,
    _InvalidClusterStateFault,
    _ServiceLinkedRoleNotFoundFault,
    _InsufficientClusterCapacityFault,
    _NodeNotFoundFault,
    _ParameterGroupQuotaExceededFault,
    _InvalidParameterValueException,
    _InvalidVPCNetworkStateFault,
    _SubnetInUse,
    _ClusterQuotaForCustomerExceededFault,
    _SubnetGroupNotFoundFault,
    _SubnetGroupAlreadyExistsFault,
    _NodeQuotaForCustomerExceededFault,
    _SubnetGroupQuotaExceededFault,
    _ClusterAlreadyExistsFault,
    _InvalidARNFault,
    _InvalidParameterCombinationException,

    -- * Event
    Event (..),
    mkEvent,
    eDate,
    eMessage,
    eSourceName,
    eSourceType,

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * Cluster
    Cluster (..),
    mkCluster,
    cActiveNodes,
    cClusterArn,
    cClusterDiscoveryEndpoint,
    cClusterName,
    cDescription,
    cIamRoleArn,
    cNodeIdsToRemove,
    cNodeType,
    cNodes,
    cNotificationConfiguration,
    cParameterGroup,
    cPreferredMaintenanceWindow,
    cSSEDescription,
    cSecurityGroups,
    cStatus,
    cSubnetGroup,
    cTotalNodes,

    -- * ParameterGroupStatus
    ParameterGroupStatus (..),
    mkParameterGroupStatus,
    pgsNodeIdsToReboot,
    pgsParameterApplyStatus,
    pgsParameterGroupName,

    -- * NotificationConfiguration
    NotificationConfiguration (..),
    mkNotificationConfiguration,
    ncTopicArn,
    ncTopicStatus,

    -- * ParameterNameValue
    ParameterNameValue (..),
    mkParameterNameValue,
    pnvParameterName,
    pnvParameterValue,

    -- * String
    String (..),

    -- * SourceType
    SourceType (..),

    -- * Node
    Node (..),
    mkNode,
    nAvailabilityZone,
    nEndpoint,
    nNodeCreateTime,
    nNodeId,
    nNodeStatus,
    nParameterGroupStatus,

    -- * SSESpecification
    SSESpecification (..),
    mkSSESpecification,
    ssesEnabled,

    -- * Subnet
    Subnet (..),
    mkSubnet,
    sSubnetAvailabilityZone,
    sSubnetIdentifier,

    -- * SecurityGroupMembership
    SecurityGroupMembership (..),
    mkSecurityGroupMembership,
    sgmSecurityGroupIdentifier,
    sgmStatus,

    -- * SSEStatus
    SSEStatus (..),

    -- * ParameterType
    ParameterType (..),

    -- * SubnetGroup
    SubnetGroup (..),
    mkSubnetGroup,
    sgDescription,
    sgSubnetGroupName,
    sgSubnets,
    sgVpcId,

    -- * IsModifiable
    IsModifiable (..),

    -- * NodeTypeSpecificValue
    NodeTypeSpecificValue (..),
    mkNodeTypeSpecificValue,
    ntsvNodeType,
    ntsvValue,

    -- * SSEDescription
    SSEDescription (..),
    mkSSEDescription,
    ssedStatus,

    -- * Endpoint
    Endpoint (..),
    mkEndpoint,
    eAddress,
    ePort,

    -- * ParameterGroup
    ParameterGroup (..),
    mkParameterGroup,
    pgDescription,
    pgParameterGroupName,

    -- * ChangeType
    ChangeType (..),

    -- * Parameter
    Parameter (..),
    mkParameter,
    pAllowedValues,
    pChangeType,
    pDataType,
    pDescription,
    pIsModifiable,
    pNodeTypeSpecificValues,
    pParameterName,
    pParameterType,
    pParameterValue,
    pSource,

    -- * ClusterName
    ClusterName (..),

    -- * NextToken
    NextToken (..),

    -- * Message
    Message (..),

    -- * SourceName
    SourceName (..),

    -- * NodeType
    NodeType (..),

    -- * IamRoleArn
    IamRoleArn (..),

    -- * Description
    Description (..),

    -- * NotificationTopicArn
    NotificationTopicArn (..),

    -- * ParameterGroupName
    ParameterGroupName (..),

    -- * PreferredMaintenanceWindow
    PreferredMaintenanceWindow (..),

    -- * SubnetGroupName
    SubnetGroupName (..),

    -- * Key
    Key (..),

    -- * Value
    Value (..),

    -- * NotificationTopicStatus
    NotificationTopicStatus (..),

    -- * ClusterArn
    ClusterArn (..),

    -- * Status
    Status (..),

    -- * ParameterApplyStatus
    ParameterApplyStatus (..),

    -- * TopicArn
    TopicArn (..),

    -- * TopicStatus
    TopicStatus (..),

    -- * ParameterName
    ParameterName (..),

    -- * ParameterValue
    ParameterValue (..),
  )
where

import Network.AWS.DAX.Types.ChangeType
import Network.AWS.DAX.Types.Cluster
import Network.AWS.DAX.Types.ClusterArn
import Network.AWS.DAX.Types.ClusterName
import Network.AWS.DAX.Types.Description
import Network.AWS.DAX.Types.Endpoint
import Network.AWS.DAX.Types.Event
import Network.AWS.DAX.Types.IamRoleArn
import Network.AWS.DAX.Types.IsModifiable
import Network.AWS.DAX.Types.Key
import Network.AWS.DAX.Types.Message
import Network.AWS.DAX.Types.NextToken
import Network.AWS.DAX.Types.Node
import Network.AWS.DAX.Types.NodeType
import Network.AWS.DAX.Types.NodeTypeSpecificValue
import Network.AWS.DAX.Types.NotificationConfiguration
import Network.AWS.DAX.Types.NotificationTopicArn
import Network.AWS.DAX.Types.NotificationTopicStatus
import Network.AWS.DAX.Types.Parameter
import Network.AWS.DAX.Types.ParameterApplyStatus
import Network.AWS.DAX.Types.ParameterGroup
import Network.AWS.DAX.Types.ParameterGroupName
import Network.AWS.DAX.Types.ParameterGroupStatus
import Network.AWS.DAX.Types.ParameterName
import Network.AWS.DAX.Types.ParameterNameValue
import Network.AWS.DAX.Types.ParameterType
import Network.AWS.DAX.Types.ParameterValue
import Network.AWS.DAX.Types.PreferredMaintenanceWindow
import Network.AWS.DAX.Types.SSEDescription
import Network.AWS.DAX.Types.SSESpecification
import Network.AWS.DAX.Types.SSEStatus
import Network.AWS.DAX.Types.SecurityGroupMembership
import Network.AWS.DAX.Types.SourceName
import Network.AWS.DAX.Types.SourceType
import Network.AWS.DAX.Types.Status
import Network.AWS.DAX.Types.String
import Network.AWS.DAX.Types.Subnet
import Network.AWS.DAX.Types.SubnetGroup
import Network.AWS.DAX.Types.SubnetGroupName
import Network.AWS.DAX.Types.Tag
import Network.AWS.DAX.Types.TopicArn
import Network.AWS.DAX.Types.TopicStatus
import Network.AWS.DAX.Types.Value
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-04-19@ of the Amazon DynamoDB Accelerator (DAX) SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "DAX",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "dax",
      Core._svcVersion = "2017-04-19",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "DAX",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
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
      | Lens.has
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | The request cannot be processed because it would exceed the allowed number of subnets in a subnet group.
_SubnetQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SubnetQuotaExceededFault =
  Core._MatchServiceError
    mkServiceConfig
    "SubnetQuotaExceededFault"
{-# DEPRECATED _SubnetQuotaExceededFault "Use generic-lens or generic-optics instead." #-}

-- | The specified parameter group does not exist.
_ParameterGroupNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ParameterGroupNotFoundFault =
  Core._MatchServiceError
    mkServiceConfig
    "ParameterGroupNotFoundFault"
{-# DEPRECATED _ParameterGroupNotFoundFault "Use generic-lens or generic-optics instead." #-}

-- | One or more parameters in a parameter group are in an invalid state.
_InvalidParameterGroupStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterGroupStateFault =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidParameterGroupStateFault"
{-# DEPRECATED _InvalidParameterGroupStateFault "Use generic-lens or generic-optics instead." #-}

-- | The specified subnet group is currently in use.
_SubnetGroupInUseFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SubnetGroupInUseFault =
  Core._MatchServiceError mkServiceConfig "SubnetGroupInUseFault"
{-# DEPRECATED _SubnetGroupInUseFault "Use generic-lens or generic-optics instead." #-}

-- | The specified parameter group already exists.
_ParameterGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ParameterGroupAlreadyExistsFault =
  Core._MatchServiceError
    mkServiceConfig
    "ParameterGroupAlreadyExistsFault"
{-# DEPRECATED _ParameterGroupAlreadyExistsFault "Use generic-lens or generic-optics instead." #-}

-- | An invalid subnet identifier was specified.
_InvalidSubnet :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSubnet =
  Core._MatchServiceError mkServiceConfig "InvalidSubnet"
{-# DEPRECATED _InvalidSubnet "Use generic-lens or generic-optics instead." #-}

-- | You have exceeded the maximum number of tags for this DAX cluster.
_TagQuotaPerResourceExceeded :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TagQuotaPerResourceExceeded =
  Core._MatchServiceError
    mkServiceConfig
    "TagQuotaPerResourceExceeded"
{-# DEPRECATED _TagQuotaPerResourceExceeded "Use generic-lens or generic-optics instead." #-}

-- | The requested cluster ID does not refer to an existing DAX cluster.
_ClusterNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ClusterNotFoundFault =
  Core._MatchServiceError mkServiceConfig "ClusterNotFoundFault"
{-# DEPRECATED _ClusterNotFoundFault "Use generic-lens or generic-optics instead." #-}

-- | The tag does not exist.
_TagNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TagNotFoundFault =
  Core._MatchServiceError mkServiceConfig "TagNotFoundFault"
{-# DEPRECATED _TagNotFoundFault "Use generic-lens or generic-optics instead." #-}

-- | You have attempted to exceed the maximum number of nodes for a DAX cluster.
_NodeQuotaForClusterExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NodeQuotaForClusterExceededFault =
  Core._MatchServiceError
    mkServiceConfig
    "NodeQuotaForClusterExceededFault"
{-# DEPRECATED _NodeQuotaForClusterExceededFault "Use generic-lens or generic-optics instead." #-}

-- | The requested DAX cluster is not in the /available/ state.
_InvalidClusterStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidClusterStateFault =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidClusterStateFault"
{-# DEPRECATED _InvalidClusterStateFault "Use generic-lens or generic-optics instead." #-}

-- | The specified service linked role (SLR) was not found.
_ServiceLinkedRoleNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ServiceLinkedRoleNotFoundFault =
  Core._MatchServiceError
    mkServiceConfig
    "ServiceLinkedRoleNotFoundFault"
{-# DEPRECATED _ServiceLinkedRoleNotFoundFault "Use generic-lens or generic-optics instead." #-}

-- | There are not enough system resources to create the cluster you requested (or to resize an already-existing cluster).
_InsufficientClusterCapacityFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InsufficientClusterCapacityFault =
  Core._MatchServiceError
    mkServiceConfig
    "InsufficientClusterCapacityFault"
{-# DEPRECATED _InsufficientClusterCapacityFault "Use generic-lens or generic-optics instead." #-}

-- | None of the nodes in the cluster have the given node ID.
_NodeNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NodeNotFoundFault =
  Core._MatchServiceError mkServiceConfig "NodeNotFoundFault"
{-# DEPRECATED _NodeNotFoundFault "Use generic-lens or generic-optics instead." #-}

-- | You have attempted to exceed the maximum number of parameter groups.
_ParameterGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ParameterGroupQuotaExceededFault =
  Core._MatchServiceError
    mkServiceConfig
    "ParameterGroupQuotaExceededFault"
{-# DEPRECATED _ParameterGroupQuotaExceededFault "Use generic-lens or generic-optics instead." #-}

-- | The value for a parameter is invalid.
_InvalidParameterValueException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterValueException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidParameterValueException"
{-# DEPRECATED _InvalidParameterValueException "Use generic-lens or generic-optics instead." #-}

-- | The VPC network is in an invalid state.
_InvalidVPCNetworkStateFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidVPCNetworkStateFault =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidVPCNetworkStateFault"
{-# DEPRECATED _InvalidVPCNetworkStateFault "Use generic-lens or generic-optics instead." #-}

-- | The requested subnet is being used by another subnet group.
_SubnetInUse :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SubnetInUse =
  Core._MatchServiceError mkServiceConfig "SubnetInUse"
{-# DEPRECATED _SubnetInUse "Use generic-lens or generic-optics instead." #-}

-- | You have attempted to exceed the maximum number of DAX clusters for your AWS account.
_ClusterQuotaForCustomerExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ClusterQuotaForCustomerExceededFault =
  Core._MatchServiceError
    mkServiceConfig
    "ClusterQuotaForCustomerExceededFault"
{-# DEPRECATED _ClusterQuotaForCustomerExceededFault "Use generic-lens or generic-optics instead." #-}

-- | The requested subnet group name does not refer to an existing subnet group.
_SubnetGroupNotFoundFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SubnetGroupNotFoundFault =
  Core._MatchServiceError
    mkServiceConfig
    "SubnetGroupNotFoundFault"
{-# DEPRECATED _SubnetGroupNotFoundFault "Use generic-lens or generic-optics instead." #-}

-- | The specified subnet group already exists.
_SubnetGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SubnetGroupAlreadyExistsFault =
  Core._MatchServiceError
    mkServiceConfig
    "SubnetGroupAlreadyExistsFault"
{-# DEPRECATED _SubnetGroupAlreadyExistsFault "Use generic-lens or generic-optics instead." #-}

-- | You have attempted to exceed the maximum number of nodes for your AWS account.
_NodeQuotaForCustomerExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NodeQuotaForCustomerExceededFault =
  Core._MatchServiceError
    mkServiceConfig
    "NodeQuotaForCustomerExceededFault"
{-# DEPRECATED _NodeQuotaForCustomerExceededFault "Use generic-lens or generic-optics instead." #-}

-- | The request cannot be processed because it would exceed the allowed number of subnets in a subnet group.
_SubnetGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SubnetGroupQuotaExceededFault =
  Core._MatchServiceError
    mkServiceConfig
    "SubnetGroupQuotaExceededFault"
{-# DEPRECATED _SubnetGroupQuotaExceededFault "Use generic-lens or generic-optics instead." #-}

-- | You already have a DAX cluster with the given identifier.
_ClusterAlreadyExistsFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ClusterAlreadyExistsFault =
  Core._MatchServiceError
    mkServiceConfig
    "ClusterAlreadyExistsFault"
{-# DEPRECATED _ClusterAlreadyExistsFault "Use generic-lens or generic-optics instead." #-}

-- | The Amazon Resource Name (ARN) supplied in the request is not valid.
_InvalidARNFault :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidARNFault =
  Core._MatchServiceError mkServiceConfig "InvalidARNFault"
{-# DEPRECATED _InvalidARNFault "Use generic-lens or generic-optics instead." #-}

-- | Two or more incompatible parameters were specified.
_InvalidParameterCombinationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParameterCombinationException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidParameterCombinationException"
{-# DEPRECATED _InvalidParameterCombinationException "Use generic-lens or generic-optics instead." #-}

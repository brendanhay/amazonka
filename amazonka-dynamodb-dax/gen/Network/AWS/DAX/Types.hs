{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DAX.Types
    (
    -- * Service Configuration
      dax

    -- * Errors
    , _SubnetQuotaExceededFault
    , _ParameterGroupNotFoundFault
    , _InvalidParameterGroupStateFault
    , _SubnetGroupInUseFault
    , _ParameterGroupAlreadyExistsFault
    , _InvalidSubnet
    , _TagQuotaPerResourceExceeded
    , _ClusterNotFoundFault
    , _TagNotFoundFault
    , _NodeQuotaForClusterExceededFault
    , _InvalidClusterStateFault
    , _InsufficientClusterCapacityFault
    , _NodeNotFoundFault
    , _ParameterGroupQuotaExceededFault
    , _InvalidParameterValueException
    , _InvalidVPCNetworkStateFault
    , _SubnetInUse
    , _ClusterQuotaForCustomerExceededFault
    , _SubnetGroupNotFoundFault
    , _SubnetGroupAlreadyExistsFault
    , _NodeQuotaForCustomerExceededFault
    , _SubnetGroupQuotaExceededFault
    , _ClusterAlreadyExistsFault
    , _InvalidARNFault
    , _InvalidParameterCombinationException

    -- * ChangeType
    , ChangeType (..)

    -- * IsModifiable
    , IsModifiable (..)

    -- * ParameterType
    , ParameterType (..)

    -- * SourceType
    , SourceType (..)

    -- * Cluster
    , Cluster
    , cluster
    , cStatus
    , cIAMRoleARN
    , cClusterARN
    , cActiveNodes
    , cSecurityGroups
    , cNotificationConfiguration
    , cNodeIdsToRemove
    , cTotalNodes
    , cPreferredMaintenanceWindow
    , cSubnetGroup
    , cClusterName
    , cNodeType
    , cNodes
    , cClusterDiscoveryEndpoint
    , cDescription
    , cParameterGroup

    -- * Endpoint
    , Endpoint
    , endpoint
    , eAddress
    , ePort

    -- * Event
    , Event
    , event
    , eSourceName
    , eSourceType
    , eDate
    , eMessage

    -- * Node
    , Node
    , node
    , nNodeStatus
    , nParameterGroupStatus
    , nAvailabilityZone
    , nNodeId
    , nEndpoint
    , nNodeCreateTime

    -- * NodeTypeSpecificValue
    , NodeTypeSpecificValue
    , nodeTypeSpecificValue
    , ntsvValue
    , ntsvNodeType

    -- * NotificationConfiguration
    , NotificationConfiguration
    , notificationConfiguration
    , ncTopicStatus
    , ncTopicARN

    -- * Parameter
    , Parameter
    , parameter
    , pParameterValue
    , pParameterType
    , pSource
    , pIsModifiable
    , pDataType
    , pNodeTypeSpecificValues
    , pAllowedValues
    , pParameterName
    , pDescription
    , pChangeType

    -- * ParameterGroup
    , ParameterGroup
    , parameterGroup
    , pgDescription
    , pgParameterGroupName

    -- * ParameterGroupStatus
    , ParameterGroupStatus
    , parameterGroupStatus
    , pgsNodeIdsToReboot
    , pgsParameterApplyStatus
    , pgsParameterGroupName

    -- * ParameterNameValue
    , ParameterNameValue
    , parameterNameValue
    , pnvParameterValue
    , pnvParameterName

    -- * SecurityGroupMembership
    , SecurityGroupMembership
    , securityGroupMembership
    , sgmStatus
    , sgmSecurityGroupIdentifier

    -- * Subnet
    , Subnet
    , subnet
    , sSubnetIdentifier
    , sSubnetAvailabilityZone

    -- * SubnetGroup
    , SubnetGroup
    , subnetGroup
    , sgVPCId
    , sgSubnets
    , sgSubnetGroupName
    , sgDescription

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey
    ) where

import Network.AWS.DAX.Types.Product
import Network.AWS.DAX.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-04-19@ of the Amazon DynamoDB Accelerator (DAX) SDK configuration.
dax :: Service
dax =
  Service
    { _svcAbbrev = "DAX"
    , _svcSigner = v4
    , _svcPrefix = "dax"
    , _svcVersion = "2017-04-19"
    , _svcEndpoint = defaultEndpoint dax
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "DAX"
    , _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | The request cannot be processed because it would exceed the allowed number of subnets in a subnet group.
--
--
_SubnetQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_SubnetQuotaExceededFault = _MatchServiceError dax "SubnetQuotaExceededFault"


-- | The specified parameter group does not exist.
--
--
_ParameterGroupNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ParameterGroupNotFoundFault =
  _MatchServiceError dax "ParameterGroupNotFoundFault"


-- | One or more parameters in a parameter group are in an invalid state.
--
--
_InvalidParameterGroupStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterGroupStateFault =
  _MatchServiceError dax "InvalidParameterGroupStateFault"


-- | The specified subnet group is currently in use.
--
--
_SubnetGroupInUseFault :: AsError a => Getting (First ServiceError) a ServiceError
_SubnetGroupInUseFault = _MatchServiceError dax "SubnetGroupInUseFault"


-- | The specified parameter group already exists.
--
--
_ParameterGroupAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_ParameterGroupAlreadyExistsFault =
  _MatchServiceError dax "ParameterGroupAlreadyExistsFault"


-- | An invalid subnet identifier was specified.
--
--
_InvalidSubnet :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSubnet = _MatchServiceError dax "InvalidSubnet"


-- | You have exceeded the maximum number of tags for this DAX cluster.
--
--
_TagQuotaPerResourceExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_TagQuotaPerResourceExceeded =
  _MatchServiceError dax "TagQuotaPerResourceExceeded"


-- | The requested cluster ID does not refer to an existing DAX cluster.
--
--
_ClusterNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterNotFoundFault = _MatchServiceError dax "ClusterNotFoundFault"


-- | The tag does not exist.
--
--
_TagNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_TagNotFoundFault = _MatchServiceError dax "TagNotFoundFault"


-- | You have attempted to exceed the maximum number of nodes for a DAX cluster.
--
--
_NodeQuotaForClusterExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_NodeQuotaForClusterExceededFault =
  _MatchServiceError dax "NodeQuotaForClusterExceededFault"


-- | The requested DAX cluster is not in the /available/ state.
--
--
_InvalidClusterStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidClusterStateFault = _MatchServiceError dax "InvalidClusterStateFault"


-- | There are not enough system resources to create the cluster you requested (or to resize an already-existing cluster).
--
--
_InsufficientClusterCapacityFault :: AsError a => Getting (First ServiceError) a ServiceError
_InsufficientClusterCapacityFault =
  _MatchServiceError dax "InsufficientClusterCapacityFault"


-- | None of the nodes in the cluster have the given node ID.
--
--
_NodeNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_NodeNotFoundFault = _MatchServiceError dax "NodeNotFoundFault"


-- | You have attempted to exceed the maximum number of parameter groups.
--
--
_ParameterGroupQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_ParameterGroupQuotaExceededFault =
  _MatchServiceError dax "ParameterGroupQuotaExceededFault"


-- | The value for a parameter is invalid.
--
--
_InvalidParameterValueException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterValueException =
  _MatchServiceError dax "InvalidParameterValueException"


-- | The VPC network is in an invalid state.
--
--
_InvalidVPCNetworkStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidVPCNetworkStateFault =
  _MatchServiceError dax "InvalidVPCNetworkStateFault"


-- | The requested subnet is being used by another subnet group.
--
--
_SubnetInUse :: AsError a => Getting (First ServiceError) a ServiceError
_SubnetInUse = _MatchServiceError dax "SubnetInUse"


-- | You have attempted to exceed the maximum number of DAX clusters for your AWS account.
--
--
_ClusterQuotaForCustomerExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterQuotaForCustomerExceededFault =
  _MatchServiceError dax "ClusterQuotaForCustomerExceededFault"


-- | The requested subnet group name does not refer to an existing subnet group.
--
--
_SubnetGroupNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_SubnetGroupNotFoundFault = _MatchServiceError dax "SubnetGroupNotFoundFault"


-- | The specified subnet group already exists.
--
--
_SubnetGroupAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_SubnetGroupAlreadyExistsFault =
  _MatchServiceError dax "SubnetGroupAlreadyExistsFault"


-- | You have attempted to exceed the maximum number of nodes for your AWS account.
--
--
_NodeQuotaForCustomerExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_NodeQuotaForCustomerExceededFault =
  _MatchServiceError dax "NodeQuotaForCustomerExceededFault"


-- | The request cannot be processed because it would exceed the allowed number of subnets in a subnet group.
--
--
_SubnetGroupQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_SubnetGroupQuotaExceededFault =
  _MatchServiceError dax "SubnetGroupQuotaExceededFault"


-- | You already have a DAX cluster with the given identifier.
--
--
_ClusterAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterAlreadyExistsFault = _MatchServiceError dax "ClusterAlreadyExistsFault"


-- | The Amazon Resource Name (ARN) supplied in the request is not valid.
--
--
_InvalidARNFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidARNFault = _MatchServiceError dax "InvalidARNFault"


-- | Two or more incompatible parameters were specified.
--
--
_InvalidParameterCombinationException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterCombinationException =
  _MatchServiceError dax "InvalidParameterCombinationException"


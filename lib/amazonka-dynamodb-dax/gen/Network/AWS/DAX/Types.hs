{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types
  ( -- * Service Configuration
    dax,

    -- * Errors

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
    Cluster,
    cluster,
    cStatus,
    cIAMRoleARN,
    cClusterARN,
    cActiveNodes,
    cSecurityGroups,
    cNotificationConfiguration,
    cNodeIdsToRemove,
    cTotalNodes,
    cPreferredMaintenanceWindow,
    cSubnetGroup,
    cClusterName,
    cNodeType,
    cNodes,
    cClusterDiscoveryEndpoint,
    cSSEDescription,
    cDescription,
    cParameterGroup,

    -- * Endpoint
    Endpoint,
    endpoint,
    eAddress,
    ePort,

    -- * Event
    Event,
    event,
    eSourceName,
    eSourceType,
    eDate,
    eMessage,

    -- * Node
    Node,
    node,
    nNodeStatus,
    nParameterGroupStatus,
    nAvailabilityZone,
    nNodeId,
    nEndpoint,
    nNodeCreateTime,

    -- * NodeTypeSpecificValue
    NodeTypeSpecificValue,
    nodeTypeSpecificValue,
    ntsvValue,
    ntsvNodeType,

    -- * NotificationConfiguration
    NotificationConfiguration,
    notificationConfiguration,
    ncTopicStatus,
    ncTopicARN,

    -- * Parameter
    Parameter,
    parameter,
    pParameterValue,
    pParameterType,
    pSource,
    pIsModifiable,
    pDataType,
    pNodeTypeSpecificValues,
    pAllowedValues,
    pParameterName,
    pDescription,
    pChangeType,

    -- * ParameterGroup
    ParameterGroup,
    parameterGroup,
    pgDescription,
    pgParameterGroupName,

    -- * ParameterGroupStatus
    ParameterGroupStatus,
    parameterGroupStatus,
    pgsNodeIdsToReboot,
    pgsParameterApplyStatus,
    pgsParameterGroupName,

    -- * ParameterNameValue
    ParameterNameValue,
    parameterNameValue,
    pnvParameterValue,
    pnvParameterName,

    -- * SSEDescription
    SSEDescription,
    sSEDescription,
    ssedStatus,

    -- * SSESpecification
    SSESpecification,
    sSESpecification,
    ssesEnabled,

    -- * SecurityGroupMembership
    SecurityGroupMembership,
    securityGroupMembership,
    sgmStatus,
    sgmSecurityGroupIdentifier,

    -- * Subnet
    Subnet,
    subnet,
    sSubnetIdentifier,
    sSubnetAvailabilityZone,

    -- * SubnetGroup
    SubnetGroup,
    subnetGroup,
    sgVPCId,
    sgSubnets,
    sgSubnetGroupName,
    sgDescription,

    -- * Tag
    Tag,
    tag,
    tagValue,
    tagKey,
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-04-19@ of the Amazon DynamoDB Accelerator (DAX) SDK configuration.
dax :: Service
dax =
  Service
    { _svcAbbrev = "DAX",
      _svcSigner = v4,
      _svcPrefix = "dax",
      _svcVersion = "2017-04-19",
      _svcEndpoint = defaultEndpoint dax,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "DAX",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

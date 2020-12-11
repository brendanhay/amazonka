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
    daxService,

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
    Cluster (..),
    mkCluster,
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
    Endpoint (..),
    mkEndpoint,
    eAddress,
    ePort,

    -- * Event
    Event (..),
    mkEvent,
    eSourceName,
    eSourceType,
    eDate,
    eMessage,

    -- * Node
    Node (..),
    mkNode,
    nNodeStatus,
    nParameterGroupStatus,
    nAvailabilityZone,
    nNodeId,
    nEndpoint,
    nNodeCreateTime,

    -- * NodeTypeSpecificValue
    NodeTypeSpecificValue (..),
    mkNodeTypeSpecificValue,
    ntsvValue,
    ntsvNodeType,

    -- * NotificationConfiguration
    NotificationConfiguration (..),
    mkNotificationConfiguration,
    ncTopicStatus,
    ncTopicARN,

    -- * Parameter
    Parameter (..),
    mkParameter,
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
    ParameterGroup (..),
    mkParameterGroup,
    pgDescription,
    pgParameterGroupName,

    -- * ParameterGroupStatus
    ParameterGroupStatus (..),
    mkParameterGroupStatus,
    pgsNodeIdsToReboot,
    pgsParameterApplyStatus,
    pgsParameterGroupName,

    -- * ParameterNameValue
    ParameterNameValue (..),
    mkParameterNameValue,
    pnvParameterValue,
    pnvParameterName,

    -- * SSEDescription
    SSEDescription (..),
    mkSSEDescription,
    ssedStatus,

    -- * SSESpecification
    SSESpecification (..),
    mkSSESpecification,
    ssesEnabled,

    -- * SecurityGroupMembership
    SecurityGroupMembership (..),
    mkSecurityGroupMembership,
    sgmStatus,
    sgmSecurityGroupIdentifier,

    -- * Subnet
    Subnet (..),
    mkSubnet,
    sSubnetIdentifier,
    sSubnetAvailabilityZone,

    -- * SubnetGroup
    SubnetGroup (..),
    mkSubnetGroup,
    sgVPCId,
    sgSubnets,
    sgSubnetGroupName,
    sgDescription,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,
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
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-04-19@ of the Amazon DynamoDB Accelerator (DAX) SDK configuration.
daxService :: Lude.Service
daxService =
  Lude.Service
    { Lude._svcAbbrev = "DAX",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "dax",
      Lude._svcVersion = "2017-04-19",
      Lude._svcEndpoint = Lude.defaultEndpoint daxService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "DAX",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing

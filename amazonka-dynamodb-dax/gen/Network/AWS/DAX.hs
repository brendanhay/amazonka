{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- DAX is a managed caching service engineered for Amazon DynamoDB. DAX dramatically speeds up database reads by caching frequently-accessed data from DynamoDB, so applications can access that data with sub-millisecond latency. You can create a DAX cluster easily, using the AWS Management Console. With a few simple modifications to your code, your application can begin taking advantage of the DAX cluster and realize significant improvements in read performance.
--
--
module Network.AWS.DAX
    (
    -- * Service Configuration
      dax

    -- * Errors
    -- $errors

    -- ** SubnetQuotaExceededFault
    , _SubnetQuotaExceededFault

    -- ** ParameterGroupNotFoundFault
    , _ParameterGroupNotFoundFault

    -- ** InvalidParameterGroupStateFault
    , _InvalidParameterGroupStateFault

    -- ** SubnetGroupInUseFault
    , _SubnetGroupInUseFault

    -- ** ParameterGroupAlreadyExistsFault
    , _ParameterGroupAlreadyExistsFault

    -- ** InvalidSubnet
    , _InvalidSubnet

    -- ** TagQuotaPerResourceExceeded
    , _TagQuotaPerResourceExceeded

    -- ** ClusterNotFoundFault
    , _ClusterNotFoundFault

    -- ** TagNotFoundFault
    , _TagNotFoundFault

    -- ** NodeQuotaForClusterExceededFault
    , _NodeQuotaForClusterExceededFault

    -- ** InvalidClusterStateFault
    , _InvalidClusterStateFault

    -- ** InsufficientClusterCapacityFault
    , _InsufficientClusterCapacityFault

    -- ** NodeNotFoundFault
    , _NodeNotFoundFault

    -- ** ParameterGroupQuotaExceededFault
    , _ParameterGroupQuotaExceededFault

    -- ** InvalidParameterValueException
    , _InvalidParameterValueException

    -- ** InvalidVPCNetworkStateFault
    , _InvalidVPCNetworkStateFault

    -- ** SubnetInUse
    , _SubnetInUse

    -- ** ClusterQuotaForCustomerExceededFault
    , _ClusterQuotaForCustomerExceededFault

    -- ** SubnetGroupNotFoundFault
    , _SubnetGroupNotFoundFault

    -- ** SubnetGroupAlreadyExistsFault
    , _SubnetGroupAlreadyExistsFault

    -- ** NodeQuotaForCustomerExceededFault
    , _NodeQuotaForCustomerExceededFault

    -- ** SubnetGroupQuotaExceededFault
    , _SubnetGroupQuotaExceededFault

    -- ** ClusterAlreadyExistsFault
    , _ClusterAlreadyExistsFault

    -- ** InvalidARNFault
    , _InvalidARNFault

    -- ** InvalidParameterCombinationException
    , _InvalidParameterCombinationException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** DescribeClusters
    , module Network.AWS.DAX.DescribeClusters

    -- ** DescribeParameters
    , module Network.AWS.DAX.DescribeParameters

    -- ** DescribeEvents
    , module Network.AWS.DAX.DescribeEvents

    -- ** IncreaseReplicationFactor
    , module Network.AWS.DAX.IncreaseReplicationFactor

    -- ** CreateSubnetGroup
    , module Network.AWS.DAX.CreateSubnetGroup

    -- ** DeleteCluster
    , module Network.AWS.DAX.DeleteCluster

    -- ** UpdateCluster
    , module Network.AWS.DAX.UpdateCluster

    -- ** CreateCluster
    , module Network.AWS.DAX.CreateCluster

    -- ** DescribeDefaultParameters
    , module Network.AWS.DAX.DescribeDefaultParameters

    -- ** DeleteParameterGroup
    , module Network.AWS.DAX.DeleteParameterGroup

    -- ** UpdateParameterGroup
    , module Network.AWS.DAX.UpdateParameterGroup

    -- ** DescribeSubnetGroups
    , module Network.AWS.DAX.DescribeSubnetGroups

    -- ** CreateParameterGroup
    , module Network.AWS.DAX.CreateParameterGroup

    -- ** UpdateSubnetGroup
    , module Network.AWS.DAX.UpdateSubnetGroup

    -- ** DeleteSubnetGroup
    , module Network.AWS.DAX.DeleteSubnetGroup

    -- ** DescribeParameterGroups
    , module Network.AWS.DAX.DescribeParameterGroups

    -- ** TagResource
    , module Network.AWS.DAX.TagResource

    -- ** ListTags
    , module Network.AWS.DAX.ListTags

    -- ** DecreaseReplicationFactor
    , module Network.AWS.DAX.DecreaseReplicationFactor

    -- ** UntagResource
    , module Network.AWS.DAX.UntagResource

    -- ** RebootNode
    , module Network.AWS.DAX.RebootNode

    -- * Types

    -- ** ChangeType
    , ChangeType (..)

    -- ** IsModifiable
    , IsModifiable (..)

    -- ** ParameterType
    , ParameterType (..)

    -- ** SourceType
    , SourceType (..)

    -- ** Cluster
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

    -- ** Endpoint
    , Endpoint
    , endpoint
    , eAddress
    , ePort

    -- ** Event
    , Event
    , event
    , eSourceName
    , eSourceType
    , eDate
    , eMessage

    -- ** Node
    , Node
    , node
    , nNodeStatus
    , nParameterGroupStatus
    , nAvailabilityZone
    , nNodeId
    , nEndpoint
    , nNodeCreateTime

    -- ** NodeTypeSpecificValue
    , NodeTypeSpecificValue
    , nodeTypeSpecificValue
    , ntsvValue
    , ntsvNodeType

    -- ** NotificationConfiguration
    , NotificationConfiguration
    , notificationConfiguration
    , ncTopicStatus
    , ncTopicARN

    -- ** Parameter
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

    -- ** ParameterGroup
    , ParameterGroup
    , parameterGroup
    , pgDescription
    , pgParameterGroupName

    -- ** ParameterGroupStatus
    , ParameterGroupStatus
    , parameterGroupStatus
    , pgsNodeIdsToReboot
    , pgsParameterApplyStatus
    , pgsParameterGroupName

    -- ** ParameterNameValue
    , ParameterNameValue
    , parameterNameValue
    , pnvParameterValue
    , pnvParameterName

    -- ** SecurityGroupMembership
    , SecurityGroupMembership
    , securityGroupMembership
    , sgmStatus
    , sgmSecurityGroupIdentifier

    -- ** Subnet
    , Subnet
    , subnet
    , sSubnetIdentifier
    , sSubnetAvailabilityZone

    -- ** SubnetGroup
    , SubnetGroup
    , subnetGroup
    , sgVPCId
    , sgSubnets
    , sgSubnetGroupName
    , sgDescription

    -- ** Tag
    , Tag
    , tag
    , tagValue
    , tagKey
    ) where

import Network.AWS.DAX.CreateCluster
import Network.AWS.DAX.CreateParameterGroup
import Network.AWS.DAX.CreateSubnetGroup
import Network.AWS.DAX.DecreaseReplicationFactor
import Network.AWS.DAX.DeleteCluster
import Network.AWS.DAX.DeleteParameterGroup
import Network.AWS.DAX.DeleteSubnetGroup
import Network.AWS.DAX.DescribeClusters
import Network.AWS.DAX.DescribeDefaultParameters
import Network.AWS.DAX.DescribeEvents
import Network.AWS.DAX.DescribeParameterGroups
import Network.AWS.DAX.DescribeParameters
import Network.AWS.DAX.DescribeSubnetGroups
import Network.AWS.DAX.IncreaseReplicationFactor
import Network.AWS.DAX.ListTags
import Network.AWS.DAX.RebootNode
import Network.AWS.DAX.TagResource
import Network.AWS.DAX.Types
import Network.AWS.DAX.UntagResource
import Network.AWS.DAX.UpdateCluster
import Network.AWS.DAX.UpdateParameterGroup
import Network.AWS.DAX.UpdateSubnetGroup
import Network.AWS.DAX.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'DAX'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}

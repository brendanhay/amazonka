{-# LANGUAGE TemplateHaskell             #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_03_24.Lenses
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.ElastiCache.V2014_03_24.Lenses where

import Control.Lens.TH
import Network.AWS.ElastiCache.V2014_03_24.Types
import Network.AWS.ElastiCache.V2014_03_24.DeleteCacheSecurityGroup
import Network.AWS.ElastiCache.V2014_03_24.CreateReplicationGroup
import Network.AWS.ElastiCache.V2014_03_24.DeleteCacheCluster
import Network.AWS.ElastiCache.V2014_03_24.RebootCacheCluster
import Network.AWS.ElastiCache.V2014_03_24.RevokeCacheSecurityGroupIngress
import Network.AWS.ElastiCache.V2014_03_24.CreateCacheCluster
import Network.AWS.ElastiCache.V2014_03_24.DescribeEvents
import Network.AWS.ElastiCache.V2014_03_24.DescribeEngineDefaultParameters
import Network.AWS.ElastiCache.V2014_03_24.ModifyCacheParameterGroup
import Network.AWS.ElastiCache.V2014_03_24.DeleteReplicationGroup
import Network.AWS.ElastiCache.V2014_03_24.DescribeCacheClusters
import Network.AWS.ElastiCache.V2014_03_24.PurchaseReservedCacheNodesOffering
import Network.AWS.ElastiCache.V2014_03_24.ModifyReplicationGroup
import Network.AWS.ElastiCache.V2014_03_24.DescribeCacheParameters
import Network.AWS.ElastiCache.V2014_03_24.DescribeCacheSubnetGroups
import Network.AWS.ElastiCache.V2014_03_24.CreateCacheSecurityGroup
import Network.AWS.ElastiCache.V2014_03_24.AuthorizeCacheSecurityGroupIngress
import Network.AWS.ElastiCache.V2014_03_24.CopySnapshot
import Network.AWS.ElastiCache.V2014_03_24.CreateCacheSubnetGroup
import Network.AWS.ElastiCache.V2014_03_24.DescribeCacheParameterGroups
import Network.AWS.ElastiCache.V2014_03_24.ResetCacheParameterGroup
import Network.AWS.ElastiCache.V2014_03_24.DescribeSnapshots
import Network.AWS.ElastiCache.V2014_03_24.DescribeReplicationGroups
import Network.AWS.ElastiCache.V2014_03_24.DeleteSnapshot
import Network.AWS.ElastiCache.V2014_03_24.DescribeReservedCacheNodesOfferings
import Network.AWS.ElastiCache.V2014_03_24.ModifyCacheSubnetGroup
import Network.AWS.ElastiCache.V2014_03_24.CreateSnapshot
import Network.AWS.ElastiCache.V2014_03_24.DeleteCacheParameterGroup
import Network.AWS.ElastiCache.V2014_03_24.DescribeCacheSecurityGroups
import Network.AWS.ElastiCache.V2014_03_24.ModifyCacheCluster
import Network.AWS.ElastiCache.V2014_03_24.DescribeCacheEngineVersions
import Network.AWS.ElastiCache.V2014_03_24.CreateCacheParameterGroup
import Network.AWS.ElastiCache.V2014_03_24.DescribeReservedCacheNodes
import Network.AWS.ElastiCache.V2014_03_24.DeleteCacheSubnetGroup

-- Newtypes
makeIso ''AvailabilityZone
makeIso ''ReplicationGroupPendingModifiedValues

-- Products
makeLenses ''CacheCluster
makeLenses ''CacheEngineVersion
makeLenses ''CacheNode
makeLenses ''CacheNodeTypeSpecificParameter
makeLenses ''CacheNodeTypeSpecificValue
makeLenses ''CacheParameterGroup
makeLenses ''CacheParameterGroupStatus
makeLenses ''CacheSecurityGroup
makeLenses ''CacheSecurityGroupMembership
makeLenses ''CacheSubnetGroup
makeLenses ''EC2SecurityGroup
makeLenses ''Endpoint
makeLenses ''EngineDefaults
makeLenses ''Event
makeLenses ''NodeGroup
makeLenses ''NodeGroupMember
makeLenses ''NodeSnapshot
makeLenses ''NotificationConfiguration
makeLenses ''Parameter
makeLenses ''ParameterNameValue
makeLenses ''PendingModifiedValues
makeLenses ''RecurringCharge
makeLenses ''ReplicationGroup
makeLenses ''ReservedCacheNode
makeLenses ''ReservedCacheNodesOffering
makeLenses ''SecurityGroupMembership
makeLenses ''Snapshot
makeLenses ''Subnet

-- Requests
makeLenses ''DeleteCacheSecurityGroup
makeLenses ''CreateReplicationGroup
makeLenses ''DeleteCacheCluster
makeLenses ''RebootCacheCluster
makeLenses ''RevokeCacheSecurityGroupIngress
makeLenses ''CreateCacheCluster
makeLenses ''DescribeEvents
makeLenses ''DescribeEngineDefaultParameters
makeLenses ''ModifyCacheParameterGroup
makeLenses ''DeleteReplicationGroup
makeLenses ''DescribeCacheClusters
makeLenses ''PurchaseReservedCacheNodesOffering
makeLenses ''ModifyReplicationGroup
makeLenses ''DescribeCacheParameters
makeLenses ''DescribeCacheSubnetGroups
makeLenses ''CreateCacheSecurityGroup
makeLenses ''AuthorizeCacheSecurityGroupIngress
makeLenses ''CopySnapshot
makeLenses ''CreateCacheSubnetGroup
makeLenses ''DescribeCacheParameterGroups
makeLenses ''ResetCacheParameterGroup
makeLenses ''DescribeSnapshots
makeLenses ''DescribeReplicationGroups
makeLenses ''DeleteSnapshot
makeLenses ''DescribeReservedCacheNodesOfferings
makeLenses ''ModifyCacheSubnetGroup
makeLenses ''CreateSnapshot
makeLenses ''DeleteCacheParameterGroup
makeLenses ''DescribeCacheSecurityGroups
makeLenses ''ModifyCacheCluster
makeLenses ''DescribeCacheEngineVersions
makeLenses ''CreateCacheParameterGroup
makeLenses ''DescribeReservedCacheNodes
makeLenses ''DeleteCacheSubnetGroup

-- Responses
makeLenses ''DeleteCacheSecurityGroupResponse
makeLenses ''CreateReplicationGroupResponse
makeLenses ''DeleteCacheClusterResponse
makeLenses ''RebootCacheClusterResponse
makeLenses ''RevokeCacheSecurityGroupIngressResponse
makeLenses ''CreateCacheClusterResponse
makeLenses ''DescribeEventsResponse
makeLenses ''DescribeEngineDefaultParametersResponse
makeLenses ''ModifyCacheParameterGroupResponse
makeLenses ''DeleteReplicationGroupResponse
makeLenses ''DescribeCacheClustersResponse
makeLenses ''PurchaseReservedCacheNodesOfferingResponse
makeLenses ''ModifyReplicationGroupResponse
makeLenses ''DescribeCacheParametersResponse
makeLenses ''DescribeCacheSubnetGroupsResponse
makeLenses ''CreateCacheSecurityGroupResponse
makeLenses ''AuthorizeCacheSecurityGroupIngressResponse
makeLenses ''CopySnapshotResponse
makeLenses ''CreateCacheSubnetGroupResponse
makeLenses ''DescribeCacheParameterGroupsResponse
makeLenses ''ResetCacheParameterGroupResponse
makeLenses ''DescribeSnapshotsResponse
makeLenses ''DescribeReplicationGroupsResponse
makeLenses ''DeleteSnapshotResponse
makeLenses ''DescribeReservedCacheNodesOfferingsResponse
makeLenses ''ModifyCacheSubnetGroupResponse
makeLenses ''CreateSnapshotResponse
makeLenses ''DeleteCacheParameterGroupResponse
makeLenses ''DescribeCacheSecurityGroupsResponse
makeLenses ''ModifyCacheClusterResponse
makeLenses ''DescribeCacheEngineVersionsResponse
makeLenses ''CreateCacheParameterGroupResponse
makeLenses ''DescribeReservedCacheNodesResponse
makeLenses ''DeleteCacheSubnetGroupResponse

-- Module      : Network.AWS.ElastiCache
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | ElastiCache is a web service that makes it easy to deploy, operate, and scale
-- an in-memory cache in the cloud. The service improves the performance of web
-- applications by allowing you to retrieve information from fast, managed,
-- in-memory caches, instead of relying entirely on slower disk-based databases.
-- Amazon ElastiCache automatically detects and replaces failed nodes, reducing
-- the overhead associated with self-managed infrastructures and provides a
-- resilient system that mitigates the risk of overloaded databases, which slow
-- website and application load times. Through integration with Amazon
-- CloudWatch, Amazon ElastiCache provides enhanced visibility into key
-- performance metrics associated with your Memcached or Redis nodes.
module Network.AWS.ElastiCache
    ( module Network.AWS.ElastiCache.AuthorizeCacheSecurityGroupIngress
    , module Network.AWS.ElastiCache.CopySnapshot
    , module Network.AWS.ElastiCache.CreateCacheCluster
    , module Network.AWS.ElastiCache.CreateCacheParameterGroup
    , module Network.AWS.ElastiCache.CreateCacheSecurityGroup
    , module Network.AWS.ElastiCache.CreateCacheSubnetGroup
    , module Network.AWS.ElastiCache.CreateReplicationGroup
    , module Network.AWS.ElastiCache.CreateSnapshot
    , module Network.AWS.ElastiCache.DeleteCacheCluster
    , module Network.AWS.ElastiCache.DeleteCacheParameterGroup
    , module Network.AWS.ElastiCache.DeleteCacheSecurityGroup
    , module Network.AWS.ElastiCache.DeleteCacheSubnetGroup
    , module Network.AWS.ElastiCache.DeleteReplicationGroup
    , module Network.AWS.ElastiCache.DeleteSnapshot
    , module Network.AWS.ElastiCache.DescribeCacheClusters
    , module Network.AWS.ElastiCache.DescribeCacheEngineVersions
    , module Network.AWS.ElastiCache.DescribeCacheParameterGroups
    , module Network.AWS.ElastiCache.DescribeCacheParameters
    , module Network.AWS.ElastiCache.DescribeCacheSecurityGroups
    , module Network.AWS.ElastiCache.DescribeCacheSubnetGroups
    , module Network.AWS.ElastiCache.DescribeEngineDefaultParameters
    , module Network.AWS.ElastiCache.DescribeEvents
    , module Network.AWS.ElastiCache.DescribeReplicationGroups
    , module Network.AWS.ElastiCache.DescribeReservedCacheNodes
    , module Network.AWS.ElastiCache.DescribeReservedCacheNodesOfferings
    , module Network.AWS.ElastiCache.DescribeSnapshots
    , module Network.AWS.ElastiCache.ModifyCacheCluster
    , module Network.AWS.ElastiCache.ModifyCacheParameterGroup
    , module Network.AWS.ElastiCache.ModifyCacheSubnetGroup
    , module Network.AWS.ElastiCache.ModifyReplicationGroup
    , module Network.AWS.ElastiCache.PurchaseReservedCacheNodesOffering
    , module Network.AWS.ElastiCache.RebootCacheCluster
    , module Network.AWS.ElastiCache.ResetCacheParameterGroup
    , module Network.AWS.ElastiCache.RevokeCacheSecurityGroupIngress
    , module Network.AWS.ElastiCache.Types
    ) where

import Network.AWS.ElastiCache.AuthorizeCacheSecurityGroupIngress
import Network.AWS.ElastiCache.CopySnapshot
import Network.AWS.ElastiCache.CreateCacheCluster
import Network.AWS.ElastiCache.CreateCacheParameterGroup
import Network.AWS.ElastiCache.CreateCacheSecurityGroup
import Network.AWS.ElastiCache.CreateCacheSubnetGroup
import Network.AWS.ElastiCache.CreateReplicationGroup
import Network.AWS.ElastiCache.CreateSnapshot
import Network.AWS.ElastiCache.DeleteCacheCluster
import Network.AWS.ElastiCache.DeleteCacheParameterGroup
import Network.AWS.ElastiCache.DeleteCacheSecurityGroup
import Network.AWS.ElastiCache.DeleteCacheSubnetGroup
import Network.AWS.ElastiCache.DeleteReplicationGroup
import Network.AWS.ElastiCache.DeleteSnapshot
import Network.AWS.ElastiCache.DescribeCacheClusters
import Network.AWS.ElastiCache.DescribeCacheEngineVersions
import Network.AWS.ElastiCache.DescribeCacheParameterGroups
import Network.AWS.ElastiCache.DescribeCacheParameters
import Network.AWS.ElastiCache.DescribeCacheSecurityGroups
import Network.AWS.ElastiCache.DescribeCacheSubnetGroups
import Network.AWS.ElastiCache.DescribeEngineDefaultParameters
import Network.AWS.ElastiCache.DescribeEvents
import Network.AWS.ElastiCache.DescribeReplicationGroups
import Network.AWS.ElastiCache.DescribeReservedCacheNodes
import Network.AWS.ElastiCache.DescribeReservedCacheNodesOfferings
import Network.AWS.ElastiCache.DescribeSnapshots
import Network.AWS.ElastiCache.ModifyCacheCluster
import Network.AWS.ElastiCache.ModifyCacheParameterGroup
import Network.AWS.ElastiCache.ModifyCacheSubnetGroup
import Network.AWS.ElastiCache.ModifyReplicationGroup
import Network.AWS.ElastiCache.PurchaseReservedCacheNodesOffering
import Network.AWS.ElastiCache.RebootCacheCluster
import Network.AWS.ElastiCache.ResetCacheParameterGroup
import Network.AWS.ElastiCache.RevokeCacheSecurityGroupIngress
import Network.AWS.ElastiCache.Types

-- Module      : Network.AWS.ElastiCache.V2014_07_15
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | ElastiCache is a web service that makes it easy to deploy, operate, and
-- scale an in-memory cache in the cloud. The service improves the performance
-- of web applications by allowing you to retrieve information from fast,
-- managed, in-memory caches, instead of relying entirely on slower disk-based
-- databases. Amazon ElastiCache automatically detects and replaces failed
-- nodes, reducing the overhead associated with self-managed infrastructures
-- and provides a resilient system that mitigates the risk of overloaded
-- databases, which slow website and application load times. Through
-- integration with Amazon CloudWatch, Amazon ElastiCache provides enhanced
-- visibility into key performance metrics associated with your Memcached or
-- Redis nodes.
module Network.AWS.ElastiCache.V2014_07_15
    ( module Network.AWS.ElastiCache.V2014_07_15.AuthorizeCacheSecurityGroupIngress
    , module Network.AWS.ElastiCache.V2014_07_15.CopySnapshot
    , module Network.AWS.ElastiCache.V2014_07_15.CreateCacheCluster
    , module Network.AWS.ElastiCache.V2014_07_15.CreateCacheParameterGroup
    , module Network.AWS.ElastiCache.V2014_07_15.CreateCacheSecurityGroup
    , module Network.AWS.ElastiCache.V2014_07_15.CreateCacheSubnetGroup
    , module Network.AWS.ElastiCache.V2014_07_15.CreateReplicationGroup
    , module Network.AWS.ElastiCache.V2014_07_15.CreateSnapshot
    , module Network.AWS.ElastiCache.V2014_07_15.DeleteCacheCluster
    , module Network.AWS.ElastiCache.V2014_07_15.DeleteCacheParameterGroup
    , module Network.AWS.ElastiCache.V2014_07_15.DeleteCacheSecurityGroup
    , module Network.AWS.ElastiCache.V2014_07_15.DeleteCacheSubnetGroup
    , module Network.AWS.ElastiCache.V2014_07_15.DeleteReplicationGroup
    , module Network.AWS.ElastiCache.V2014_07_15.DeleteSnapshot
    , module Network.AWS.ElastiCache.V2014_07_15.DescribeCacheClusters
    , module Network.AWS.ElastiCache.V2014_07_15.DescribeCacheEngineVersions
    , module Network.AWS.ElastiCache.V2014_07_15.DescribeCacheParameterGroups
    , module Network.AWS.ElastiCache.V2014_07_15.DescribeCacheParameters
    , module Network.AWS.ElastiCache.V2014_07_15.DescribeCacheSecurityGroups
    , module Network.AWS.ElastiCache.V2014_07_15.DescribeCacheSubnetGroups
    , module Network.AWS.ElastiCache.V2014_07_15.DescribeEngineDefaultParameters
    , module Network.AWS.ElastiCache.V2014_07_15.DescribeEvents
    , module Network.AWS.ElastiCache.V2014_07_15.DescribeReplicationGroups
    , module Network.AWS.ElastiCache.V2014_07_15.DescribeReservedCacheNodes
    , module Network.AWS.ElastiCache.V2014_07_15.DescribeReservedCacheNodesOfferings
    , module Network.AWS.ElastiCache.V2014_07_15.DescribeSnapshots
    , module Network.AWS.ElastiCache.V2014_07_15.ModifyCacheCluster
    , module Network.AWS.ElastiCache.V2014_07_15.ModifyCacheParameterGroup
    , module Network.AWS.ElastiCache.V2014_07_15.ModifyCacheSubnetGroup
    , module Network.AWS.ElastiCache.V2014_07_15.ModifyReplicationGroup
    , module Network.AWS.ElastiCache.V2014_07_15.PurchaseReservedCacheNodesOffering
    , module Network.AWS.ElastiCache.V2014_07_15.RebootCacheCluster
    , module Network.AWS.ElastiCache.V2014_07_15.ResetCacheParameterGroup
    , module Network.AWS.ElastiCache.V2014_07_15.RevokeCacheSecurityGroupIngress
    , module Network.AWS.ElastiCache.V2014_07_15.Types
    ) where

import Network.AWS.ElastiCache.V2014_07_15.AuthorizeCacheSecurityGroupIngress
import Network.AWS.ElastiCache.V2014_07_15.CopySnapshot
import Network.AWS.ElastiCache.V2014_07_15.CreateCacheCluster
import Network.AWS.ElastiCache.V2014_07_15.CreateCacheParameterGroup
import Network.AWS.ElastiCache.V2014_07_15.CreateCacheSecurityGroup
import Network.AWS.ElastiCache.V2014_07_15.CreateCacheSubnetGroup
import Network.AWS.ElastiCache.V2014_07_15.CreateReplicationGroup
import Network.AWS.ElastiCache.V2014_07_15.CreateSnapshot
import Network.AWS.ElastiCache.V2014_07_15.DeleteCacheCluster
import Network.AWS.ElastiCache.V2014_07_15.DeleteCacheParameterGroup
import Network.AWS.ElastiCache.V2014_07_15.DeleteCacheSecurityGroup
import Network.AWS.ElastiCache.V2014_07_15.DeleteCacheSubnetGroup
import Network.AWS.ElastiCache.V2014_07_15.DeleteReplicationGroup
import Network.AWS.ElastiCache.V2014_07_15.DeleteSnapshot
import Network.AWS.ElastiCache.V2014_07_15.DescribeCacheClusters
import Network.AWS.ElastiCache.V2014_07_15.DescribeCacheEngineVersions
import Network.AWS.ElastiCache.V2014_07_15.DescribeCacheParameterGroups
import Network.AWS.ElastiCache.V2014_07_15.DescribeCacheParameters
import Network.AWS.ElastiCache.V2014_07_15.DescribeCacheSecurityGroups
import Network.AWS.ElastiCache.V2014_07_15.DescribeCacheSubnetGroups
import Network.AWS.ElastiCache.V2014_07_15.DescribeEngineDefaultParameters
import Network.AWS.ElastiCache.V2014_07_15.DescribeEvents
import Network.AWS.ElastiCache.V2014_07_15.DescribeReplicationGroups
import Network.AWS.ElastiCache.V2014_07_15.DescribeReservedCacheNodes
import Network.AWS.ElastiCache.V2014_07_15.DescribeReservedCacheNodesOfferings
import Network.AWS.ElastiCache.V2014_07_15.DescribeSnapshots
import Network.AWS.ElastiCache.V2014_07_15.ModifyCacheCluster
import Network.AWS.ElastiCache.V2014_07_15.ModifyCacheParameterGroup
import Network.AWS.ElastiCache.V2014_07_15.ModifyCacheSubnetGroup
import Network.AWS.ElastiCache.V2014_07_15.ModifyReplicationGroup
import Network.AWS.ElastiCache.V2014_07_15.PurchaseReservedCacheNodesOffering
import Network.AWS.ElastiCache.V2014_07_15.RebootCacheCluster
import Network.AWS.ElastiCache.V2014_07_15.ResetCacheParameterGroup
import Network.AWS.ElastiCache.V2014_07_15.RevokeCacheSecurityGroupIngress
import Network.AWS.ElastiCache.V2014_07_15.Types

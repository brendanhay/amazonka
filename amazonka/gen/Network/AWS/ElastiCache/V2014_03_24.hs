-- Module      : Network.AWS.ElastiCache.V2014_03_24
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
module Network.AWS.ElastiCache.V2014_03_24
    ( module Network.AWS.ElastiCache.V2014_03_24.AuthorizeCacheSecurityGroupIngress
    , module Network.AWS.ElastiCache.V2014_03_24.CopySnapshot
    , module Network.AWS.ElastiCache.V2014_03_24.CreateCacheCluster
    , module Network.AWS.ElastiCache.V2014_03_24.CreateCacheParameterGroup
    , module Network.AWS.ElastiCache.V2014_03_24.CreateCacheSecurityGroup
    , module Network.AWS.ElastiCache.V2014_03_24.CreateCacheSubnetGroup
    , module Network.AWS.ElastiCache.V2014_03_24.CreateReplicationGroup
    , module Network.AWS.ElastiCache.V2014_03_24.CreateSnapshot
    , module Network.AWS.ElastiCache.V2014_03_24.DeleteCacheCluster
    , module Network.AWS.ElastiCache.V2014_03_24.DeleteCacheParameterGroup
    , module Network.AWS.ElastiCache.V2014_03_24.DeleteCacheSecurityGroup
    , module Network.AWS.ElastiCache.V2014_03_24.DeleteCacheSubnetGroup
    , module Network.AWS.ElastiCache.V2014_03_24.DeleteReplicationGroup
    , module Network.AWS.ElastiCache.V2014_03_24.DeleteSnapshot
    , module Network.AWS.ElastiCache.V2014_03_24.DescribeCacheClusters
    , module Network.AWS.ElastiCache.V2014_03_24.DescribeCacheEngineVersions
    , module Network.AWS.ElastiCache.V2014_03_24.DescribeCacheParameterGroups
    , module Network.AWS.ElastiCache.V2014_03_24.DescribeCacheParameters
    , module Network.AWS.ElastiCache.V2014_03_24.DescribeCacheSecurityGroups
    , module Network.AWS.ElastiCache.V2014_03_24.DescribeCacheSubnetGroups
    , module Network.AWS.ElastiCache.V2014_03_24.DescribeEngineDefaultParameters
    , module Network.AWS.ElastiCache.V2014_03_24.DescribeEvents
    , module Network.AWS.ElastiCache.V2014_03_24.DescribeReplicationGroups
    , module Network.AWS.ElastiCache.V2014_03_24.DescribeReservedCacheNodes
    , module Network.AWS.ElastiCache.V2014_03_24.DescribeReservedCacheNodesOfferings
    , module Network.AWS.ElastiCache.V2014_03_24.DescribeSnapshots
    , module Network.AWS.ElastiCache.V2014_03_24.ModifyCacheCluster
    , module Network.AWS.ElastiCache.V2014_03_24.ModifyCacheParameterGroup
    , module Network.AWS.ElastiCache.V2014_03_24.ModifyCacheSubnetGroup
    , module Network.AWS.ElastiCache.V2014_03_24.ModifyReplicationGroup
    , module Network.AWS.ElastiCache.V2014_03_24.PurchaseReservedCacheNodesOffering
    , module Network.AWS.ElastiCache.V2014_03_24.RebootCacheCluster
    , module Network.AWS.ElastiCache.V2014_03_24.ResetCacheParameterGroup
    , module Network.AWS.ElastiCache.V2014_03_24.RevokeCacheSecurityGroupIngress
    , module Network.AWS.ElastiCache.V2014_03_24.Types
    ) where

import Network.AWS.ElastiCache.V2014_03_24.AuthorizeCacheSecurityGroupIngress
import Network.AWS.ElastiCache.V2014_03_24.CopySnapshot
import Network.AWS.ElastiCache.V2014_03_24.CreateCacheCluster
import Network.AWS.ElastiCache.V2014_03_24.CreateCacheParameterGroup
import Network.AWS.ElastiCache.V2014_03_24.CreateCacheSecurityGroup
import Network.AWS.ElastiCache.V2014_03_24.CreateCacheSubnetGroup
import Network.AWS.ElastiCache.V2014_03_24.CreateReplicationGroup
import Network.AWS.ElastiCache.V2014_03_24.CreateSnapshot
import Network.AWS.ElastiCache.V2014_03_24.DeleteCacheCluster
import Network.AWS.ElastiCache.V2014_03_24.DeleteCacheParameterGroup
import Network.AWS.ElastiCache.V2014_03_24.DeleteCacheSecurityGroup
import Network.AWS.ElastiCache.V2014_03_24.DeleteCacheSubnetGroup
import Network.AWS.ElastiCache.V2014_03_24.DeleteReplicationGroup
import Network.AWS.ElastiCache.V2014_03_24.DeleteSnapshot
import Network.AWS.ElastiCache.V2014_03_24.DescribeCacheClusters
import Network.AWS.ElastiCache.V2014_03_24.DescribeCacheEngineVersions
import Network.AWS.ElastiCache.V2014_03_24.DescribeCacheParameterGroups
import Network.AWS.ElastiCache.V2014_03_24.DescribeCacheParameters
import Network.AWS.ElastiCache.V2014_03_24.DescribeCacheSecurityGroups
import Network.AWS.ElastiCache.V2014_03_24.DescribeCacheSubnetGroups
import Network.AWS.ElastiCache.V2014_03_24.DescribeEngineDefaultParameters
import Network.AWS.ElastiCache.V2014_03_24.DescribeEvents
import Network.AWS.ElastiCache.V2014_03_24.DescribeReplicationGroups
import Network.AWS.ElastiCache.V2014_03_24.DescribeReservedCacheNodes
import Network.AWS.ElastiCache.V2014_03_24.DescribeReservedCacheNodesOfferings
import Network.AWS.ElastiCache.V2014_03_24.DescribeSnapshots
import Network.AWS.ElastiCache.V2014_03_24.ModifyCacheCluster
import Network.AWS.ElastiCache.V2014_03_24.ModifyCacheParameterGroup
import Network.AWS.ElastiCache.V2014_03_24.ModifyCacheSubnetGroup
import Network.AWS.ElastiCache.V2014_03_24.ModifyReplicationGroup
import Network.AWS.ElastiCache.V2014_03_24.PurchaseReservedCacheNodesOffering
import Network.AWS.ElastiCache.V2014_03_24.RebootCacheCluster
import Network.AWS.ElastiCache.V2014_03_24.ResetCacheParameterGroup
import Network.AWS.ElastiCache.V2014_03_24.RevokeCacheSecurityGroupIngress
import Network.AWS.ElastiCache.V2014_03_24.Types

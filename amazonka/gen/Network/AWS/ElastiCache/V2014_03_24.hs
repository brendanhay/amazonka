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
module Network.AWS.ElastiCache.V2014_03_24 (module Export) where

import Network.AWS.ElastiCache.V2014_03_24.AuthorizeCacheSecurityGroupIngress as Export
import Network.AWS.ElastiCache.V2014_03_24.CopySnapshot as Export
import Network.AWS.ElastiCache.V2014_03_24.CreateCacheCluster as Export
import Network.AWS.ElastiCache.V2014_03_24.CreateCacheParameterGroup as Export
import Network.AWS.ElastiCache.V2014_03_24.CreateCacheSecurityGroup as Export
import Network.AWS.ElastiCache.V2014_03_24.CreateCacheSubnetGroup as Export
import Network.AWS.ElastiCache.V2014_03_24.CreateReplicationGroup as Export
import Network.AWS.ElastiCache.V2014_03_24.CreateSnapshot as Export
import Network.AWS.ElastiCache.V2014_03_24.DeleteCacheCluster as Export
import Network.AWS.ElastiCache.V2014_03_24.DeleteCacheParameterGroup as Export
import Network.AWS.ElastiCache.V2014_03_24.DeleteCacheSecurityGroup as Export
import Network.AWS.ElastiCache.V2014_03_24.DeleteCacheSubnetGroup as Export
import Network.AWS.ElastiCache.V2014_03_24.DeleteReplicationGroup as Export
import Network.AWS.ElastiCache.V2014_03_24.DeleteSnapshot as Export
import Network.AWS.ElastiCache.V2014_03_24.DescribeCacheClusters as Export
import Network.AWS.ElastiCache.V2014_03_24.DescribeCacheEngineVersions as Export
import Network.AWS.ElastiCache.V2014_03_24.DescribeCacheParameterGroups as Export
import Network.AWS.ElastiCache.V2014_03_24.DescribeCacheParameters as Export
import Network.AWS.ElastiCache.V2014_03_24.DescribeCacheSecurityGroups as Export
import Network.AWS.ElastiCache.V2014_03_24.DescribeCacheSubnetGroups as Export
import Network.AWS.ElastiCache.V2014_03_24.DescribeEngineDefaultParameters as Export
import Network.AWS.ElastiCache.V2014_03_24.DescribeEvents as Export
import Network.AWS.ElastiCache.V2014_03_24.DescribeReplicationGroups as Export
import Network.AWS.ElastiCache.V2014_03_24.DescribeReservedCacheNodes as Export
import Network.AWS.ElastiCache.V2014_03_24.DescribeReservedCacheNodesOfferings as Export
import Network.AWS.ElastiCache.V2014_03_24.DescribeSnapshots as Export
import Network.AWS.ElastiCache.V2014_03_24.ModifyCacheCluster as Export
import Network.AWS.ElastiCache.V2014_03_24.ModifyCacheParameterGroup as Export
import Network.AWS.ElastiCache.V2014_03_24.ModifyCacheSubnetGroup as Export
import Network.AWS.ElastiCache.V2014_03_24.ModifyReplicationGroup as Export
import Network.AWS.ElastiCache.V2014_03_24.PurchaseReservedCacheNodesOffering as Export
import Network.AWS.ElastiCache.V2014_03_24.RebootCacheCluster as Export
import Network.AWS.ElastiCache.V2014_03_24.ResetCacheParameterGroup as Export
import Network.AWS.ElastiCache.V2014_03_24.RevokeCacheSecurityGroupIngress as Export
import Network.AWS.ElastiCache.V2014_03_24.Types as Export

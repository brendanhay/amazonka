{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Amazon ElastiCache
--
-- Amazon ElastiCache is a web service that makes it easier to set up,
-- operate, and scale a distributed cache in the cloud.
--
-- With ElastiCache, customers gain all of the benefits of a
-- high-performance, in-memory cache with far less of the administrative
-- burden of launching and managing a distributed cache. The service makes
-- setup, scaling, and cluster failure handling much simpler than in a
-- self-managed cache deployment.
--
-- In addition, through integration with Amazon CloudWatch, customers get
-- enhanced visibility into the key performance statistics associated with
-- their cache and can receive alarms if a part of their cache runs hot.
module Network.AWS.ElastiCache
    ( module Export
    ) where

import           Network.AWS.ElastiCache.AddTagsToResource                   as Export
import           Network.AWS.ElastiCache.AuthorizeCacheSecurityGroupIngress  as Export
import           Network.AWS.ElastiCache.CopySnapshot                        as Export
import           Network.AWS.ElastiCache.CreateCacheCluster                  as Export
import           Network.AWS.ElastiCache.CreateCacheParameterGroup           as Export
import           Network.AWS.ElastiCache.CreateCacheSecurityGroup            as Export
import           Network.AWS.ElastiCache.CreateCacheSubnetGroup              as Export
import           Network.AWS.ElastiCache.CreateReplicationGroup              as Export
import           Network.AWS.ElastiCache.CreateSnapshot                      as Export
import           Network.AWS.ElastiCache.DeleteCacheCluster                  as Export
import           Network.AWS.ElastiCache.DeleteCacheParameterGroup           as Export
import           Network.AWS.ElastiCache.DeleteCacheSecurityGroup            as Export
import           Network.AWS.ElastiCache.DeleteCacheSubnetGroup              as Export
import           Network.AWS.ElastiCache.DeleteReplicationGroup              as Export
import           Network.AWS.ElastiCache.DeleteSnapshot                      as Export
import           Network.AWS.ElastiCache.DescribeCacheClusters               as Export
import           Network.AWS.ElastiCache.DescribeCacheEngineVersions         as Export
import           Network.AWS.ElastiCache.DescribeCacheParameterGroups        as Export
import           Network.AWS.ElastiCache.DescribeCacheParameters             as Export
import           Network.AWS.ElastiCache.DescribeCacheSecurityGroups         as Export
import           Network.AWS.ElastiCache.DescribeCacheSubnetGroups           as Export
import           Network.AWS.ElastiCache.DescribeEngineDefaultParameters     as Export
import           Network.AWS.ElastiCache.DescribeEvents                      as Export
import           Network.AWS.ElastiCache.DescribeReplicationGroups           as Export
import           Network.AWS.ElastiCache.DescribeReservedCacheNodes          as Export
import           Network.AWS.ElastiCache.DescribeReservedCacheNodesOfferings as Export
import           Network.AWS.ElastiCache.DescribeSnapshots                   as Export
import           Network.AWS.ElastiCache.ListTagsForResource                 as Export
import           Network.AWS.ElastiCache.ModifyCacheCluster                  as Export
import           Network.AWS.ElastiCache.ModifyCacheParameterGroup           as Export
import           Network.AWS.ElastiCache.ModifyCacheSubnetGroup              as Export
import           Network.AWS.ElastiCache.ModifyReplicationGroup              as Export
import           Network.AWS.ElastiCache.PurchaseReservedCacheNodesOffering  as Export
import           Network.AWS.ElastiCache.RebootCacheCluster                  as Export
import           Network.AWS.ElastiCache.RemoveTagsFromResource              as Export
import           Network.AWS.ElastiCache.ResetCacheParameterGroup            as Export
import           Network.AWS.ElastiCache.RevokeCacheSecurityGroupIngress     as Export
import           Network.AWS.ElastiCache.Types                               as Export
import           Network.AWS.ElastiCache.Types.Product                       as Export
import           Network.AWS.ElastiCache.Types.Sum                           as Export
import           Network.AWS.ElastiCache.Waiters                             as Export

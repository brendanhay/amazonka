{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ElastiCache.Waiters
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

module Network.AWS.ElastiCache.Waiters where

import Network.AWS.ElastiCache.DescribeCacheClusters
import Network.AWS.ElastiCache.DescribeCacheClusters
import Network.AWS.ElastiCache.DescribeReplicationGroups
import Network.AWS.ElastiCache.DescribeReplicationGroups
import Network.AWS.ElastiCache.Types
import Network.AWS.Prelude
import Network.AWS.Waiters

cacheClusterAvailable :: Wait DescribeCacheClusters
cacheClusterAvailable = Wait{_waitName = "CacheClusterAvailable", _waitAttempts = 60, _waitDelay = 30, _waitAcceptors = [matchAll "available" AcceptSuccess (folding (concatOf dccrCacheClusters) . ccCacheClusterStatus . _Just . to toText), matchAny "deleted" AcceptFailure (folding (concatOf dccrCacheClusters) . ccCacheClusterStatus . _Just . to toText), matchAny "deleting" AcceptFailure (folding (concatOf dccrCacheClusters) . ccCacheClusterStatus . _Just . to toText), matchAny "incompatible-network" AcceptFailure (folding (concatOf dccrCacheClusters) . ccCacheClusterStatus . _Just . to toText), matchAny "restore-failed" AcceptFailure (folding (concatOf dccrCacheClusters) . ccCacheClusterStatus . _Just . to toText)]};

cacheClusterDeleted :: Wait DescribeCacheClusters
cacheClusterDeleted = Wait{_waitName = "CacheClusterDeleted", _waitAttempts = 60, _waitDelay = 30, _waitAcceptors = [matchError "CacheClusterNotFound" AcceptSuccess, matchAny "creating" AcceptFailure (folding (concatOf dccrCacheClusters) . ccCacheClusterStatus . _Just . to toText), matchAny "modifying" AcceptFailure (folding (concatOf dccrCacheClusters) . ccCacheClusterStatus . _Just . to toText), matchAny "rebooting" AcceptFailure (folding (concatOf dccrCacheClusters) . ccCacheClusterStatus . _Just . to toText)]};

replicationGroupDeleted :: Wait DescribeReplicationGroups
replicationGroupDeleted = Wait{_waitName = "ReplicationGroupDeleted", _waitAttempts = 60, _waitDelay = 30, _waitAcceptors = [matchError "ReplicationGroupNotFoundFault" AcceptSuccess, matchAny "creating" AcceptFailure (folding (concatOf drgrReplicationGroups) . rgStatus . _Just . to toText), matchAny "modifying" AcceptFailure (folding (concatOf drgrReplicationGroups) . rgStatus . _Just . to toText), matchAny "rebooting" AcceptFailure (folding (concatOf drgrReplicationGroups) . rgStatus . _Just . to toText)]};

replicationGroupAvailable :: Wait DescribeReplicationGroups
replicationGroupAvailable = Wait{_waitName = "ReplicationGroupAvailable", _waitAttempts = 60, _waitDelay = 30, _waitAcceptors = [matchAll "available" AcceptSuccess (folding (concatOf drgrReplicationGroups) . rgStatus . _Just . to toText), matchAny "deleted" AcceptFailure (folding (concatOf drgrReplicationGroups) . rgStatus . _Just . to toText), matchAny "deleting" AcceptFailure (folding (concatOf drgrReplicationGroups) . rgStatus . _Just . to toText), matchAny "incompatible-network" AcceptFailure (folding (concatOf drgrReplicationGroups) . rgStatus . _Just . to toText), matchAny "restore-failed" AcceptFailure (folding (concatOf drgrReplicationGroups) . rgStatus . _Just . to toText)]};

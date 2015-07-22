{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Waiters
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Waiters where

import           Network.AWS.ElastiCache.DescribeCacheClusters
import           Network.AWS.ElastiCache.DescribeCacheClusters
import           Network.AWS.ElastiCache.DescribeReplicationGroups
import           Network.AWS.ElastiCache.DescribeReplicationGroups
import           Network.AWS.ElastiCache.Types
import           Network.AWS.Prelude
import           Network.AWS.Waiter

cacheClusterAvailable :: Wait DescribeCacheClusters
cacheClusterAvailable =
    Wait
    { _waitName = "CacheClusterAvailable"
    , _waitAttempts = 60
    , _waitDelay = 30
    , _waitAcceptors = [ matchAll
                             "available"
                             AcceptSuccess
                             (folding (concatOf drsCacheClusters) .
                              ccCacheClusterStatus . _Just . to toText)
                       , matchAny
                             "deleted"
                             AcceptFailure
                             (folding (concatOf drsCacheClusters) .
                              ccCacheClusterStatus . _Just . to toText)
                       , matchAny
                             "deleting"
                             AcceptFailure
                             (folding (concatOf drsCacheClusters) .
                              ccCacheClusterStatus . _Just . to toText)
                       , matchAny
                             "incompatible-network"
                             AcceptFailure
                             (folding (concatOf drsCacheClusters) .
                              ccCacheClusterStatus . _Just . to toText)
                       , matchAny
                             "restore-failed"
                             AcceptFailure
                             (folding (concatOf drsCacheClusters) .
                              ccCacheClusterStatus . _Just . to toText)]
    }

cacheClusterDeleted :: Wait DescribeCacheClusters
cacheClusterDeleted =
    Wait
    { _waitName = "CacheClusterDeleted"
    , _waitAttempts = 60
    , _waitDelay = 30
    , _waitAcceptors = [ matchError "CacheClusterNotFound" AcceptSuccess
                       , matchAny
                             "creating"
                             AcceptFailure
                             (folding (concatOf drsCacheClusters) .
                              ccCacheClusterStatus . _Just . to toText)
                       , matchAny
                             "modifying"
                             AcceptFailure
                             (folding (concatOf drsCacheClusters) .
                              ccCacheClusterStatus . _Just . to toText)
                       , matchAny
                             "rebooting"
                             AcceptFailure
                             (folding (concatOf drsCacheClusters) .
                              ccCacheClusterStatus . _Just . to toText)]
    }

replicationGroupDeleted :: Wait DescribeReplicationGroups
replicationGroupDeleted =
    Wait
    { _waitName = "ReplicationGroupDeleted"
    , _waitAttempts = 60
    , _waitDelay = 30
    , _waitAcceptors = [ matchError
                             "ReplicationGroupNotFoundFault"
                             AcceptSuccess
                       , matchAny
                             "creating"
                             AcceptFailure
                             (folding (concatOf drgsrsReplicationGroups) .
                              rgStatus . _Just . to toText)
                       , matchAny
                             "modifying"
                             AcceptFailure
                             (folding (concatOf drgsrsReplicationGroups) .
                              rgStatus . _Just . to toText)
                       , matchAny
                             "rebooting"
                             AcceptFailure
                             (folding (concatOf drgsrsReplicationGroups) .
                              rgStatus . _Just . to toText)]
    }

replicationGroupAvailable :: Wait DescribeReplicationGroups
replicationGroupAvailable =
    Wait
    { _waitName = "ReplicationGroupAvailable"
    , _waitAttempts = 60
    , _waitDelay = 30
    , _waitAcceptors = [ matchAll
                             "available"
                             AcceptSuccess
                             (folding (concatOf drgsrsReplicationGroups) .
                              rgStatus . _Just . to toText)
                       , matchAny
                             "deleted"
                             AcceptFailure
                             (folding (concatOf drgsrsReplicationGroups) .
                              rgStatus . _Just . to toText)
                       , matchAny
                             "deleting"
                             AcceptFailure
                             (folding (concatOf drgsrsReplicationGroups) .
                              rgStatus . _Just . to toText)
                       , matchAny
                             "incompatible-network"
                             AcceptFailure
                             (folding (concatOf drgsrsReplicationGroups) .
                              rgStatus . _Just . to toText)
                       , matchAny
                             "restore-failed"
                             AcceptFailure
                             (folding (concatOf drgsrsReplicationGroups) .
                              rgStatus . _Just . to toText)]
    }

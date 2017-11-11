{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Waiters
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Waiters where

import Network.AWS.ElastiCache.DescribeCacheClusters
import Network.AWS.ElastiCache.DescribeCacheClusters
import Network.AWS.ElastiCache.DescribeReplicationGroups
import Network.AWS.ElastiCache.DescribeReplicationGroups
import Network.AWS.ElastiCache.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Waiter

-- | Polls 'Network.AWS.ElastiCache.DescribeCacheClusters' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
cacheClusterAvailable :: Wait DescribeCacheClusters
cacheClusterAvailable =
  Wait
  { _waitName = "CacheClusterAvailable"
  , _waitAttempts = 60
  , _waitDelay = 30
  , _waitAcceptors =
      [ matchAll
          "available"
          AcceptSuccess
          (folding (concatOf drsCacheClusters) .
           ccCacheClusterStatus . _Just . to toTextCI)
      , matchAny
          "deleted"
          AcceptFailure
          (folding (concatOf drsCacheClusters) .
           ccCacheClusterStatus . _Just . to toTextCI)
      , matchAny
          "deleting"
          AcceptFailure
          (folding (concatOf drsCacheClusters) .
           ccCacheClusterStatus . _Just . to toTextCI)
      , matchAny
          "incompatible-network"
          AcceptFailure
          (folding (concatOf drsCacheClusters) .
           ccCacheClusterStatus . _Just . to toTextCI)
      , matchAny
          "restore-failed"
          AcceptFailure
          (folding (concatOf drsCacheClusters) .
           ccCacheClusterStatus . _Just . to toTextCI)
      ]
  }


-- | Polls 'Network.AWS.ElastiCache.DescribeCacheClusters' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
cacheClusterDeleted :: Wait DescribeCacheClusters
cacheClusterDeleted =
  Wait
  { _waitName = "CacheClusterDeleted"
  , _waitAttempts = 60
  , _waitDelay = 30
  , _waitAcceptors =
      [ matchError "CacheClusterNotFound" AcceptSuccess
      , matchAny
          "creating"
          AcceptFailure
          (folding (concatOf drsCacheClusters) .
           ccCacheClusterStatus . _Just . to toTextCI)
      , matchAny
          "modifying"
          AcceptFailure
          (folding (concatOf drsCacheClusters) .
           ccCacheClusterStatus . _Just . to toTextCI)
      , matchAny
          "rebooting"
          AcceptFailure
          (folding (concatOf drsCacheClusters) .
           ccCacheClusterStatus . _Just . to toTextCI)
      ]
  }


-- | Polls 'Network.AWS.ElastiCache.DescribeReplicationGroups' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
replicationGroupDeleted :: Wait DescribeReplicationGroups
replicationGroupDeleted =
  Wait
  { _waitName = "ReplicationGroupDeleted"
  , _waitAttempts = 60
  , _waitDelay = 30
  , _waitAcceptors =
      [ matchError "ReplicationGroupNotFoundFault" AcceptSuccess
      , matchAny
          "creating"
          AcceptFailure
          (folding (concatOf drgrsReplicationGroups) .
           rgStatus . _Just . to toTextCI)
      , matchAny
          "modifying"
          AcceptFailure
          (folding (concatOf drgrsReplicationGroups) .
           rgStatus . _Just . to toTextCI)
      , matchAny
          "rebooting"
          AcceptFailure
          (folding (concatOf drgrsReplicationGroups) .
           rgStatus . _Just . to toTextCI)
      ]
  }


-- | Polls 'Network.AWS.ElastiCache.DescribeReplicationGroups' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
replicationGroupAvailable :: Wait DescribeReplicationGroups
replicationGroupAvailable =
  Wait
  { _waitName = "ReplicationGroupAvailable"
  , _waitAttempts = 60
  , _waitDelay = 30
  , _waitAcceptors =
      [ matchAll
          "available"
          AcceptSuccess
          (folding (concatOf drgrsReplicationGroups) .
           rgStatus . _Just . to toTextCI)
      , matchAny
          "deleted"
          AcceptFailure
          (folding (concatOf drgrsReplicationGroups) .
           rgStatus . _Just . to toTextCI)
      , matchAny
          "deleting"
          AcceptFailure
          (folding (concatOf drgrsReplicationGroups) .
           rgStatus . _Just . to toTextCI)
      , matchAny
          "incompatible-network"
          AcceptFailure
          (folding (concatOf drgrsReplicationGroups) .
           rgStatus . _Just . to toTextCI)
      , matchAny
          "restore-failed"
          AcceptFailure
          (folding (concatOf drgrsReplicationGroups) .
           rgStatus . _Just . to toTextCI)
      ]
  }


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Waiters where

import Network.AWS.ElastiCache.DescribeCacheClusters
import Network.AWS.ElastiCache.DescribeReplicationGroups
import Network.AWS.ElastiCache.Lens
import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.ElastiCache.DescribeReplicationGroups' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newReplicationGroupDeleted :: Waiter.Wait DescribeReplicationGroups
newReplicationGroupDeleted =
  Waiter.Wait
    { Waiter._waitName =
        "ReplicationGroupDeleted",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "deleted"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationGroupsResponse_replicationGroups
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationGroup_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "available"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationGroupsResponse_replicationGroups
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationGroup_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchError
            "ReplicationGroupNotFoundFault"
            Waiter.AcceptSuccess
        ]
    }

-- | Polls 'Network.AWS.ElastiCache.DescribeReplicationGroups' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newReplicationGroupAvailable :: Waiter.Wait DescribeReplicationGroups
newReplicationGroupAvailable =
  Waiter.Wait
    { Waiter._waitName =
        "ReplicationGroupAvailable",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "available"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationGroupsResponse_replicationGroups
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationGroup_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "deleted"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationGroupsResponse_replicationGroups
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationGroup_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.ElastiCache.DescribeCacheClusters' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newCacheClusterAvailable :: Waiter.Wait DescribeCacheClusters
newCacheClusterAvailable =
  Waiter.Wait
    { Waiter._waitName =
        "CacheClusterAvailable",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "available"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeCacheClustersResponse_cacheClusters
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. cacheCluster_cacheClusterStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "deleted"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeCacheClustersResponse_cacheClusters
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. cacheCluster_cacheClusterStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "deleting"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeCacheClustersResponse_cacheClusters
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. cacheCluster_cacheClusterStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "incompatible-network"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeCacheClustersResponse_cacheClusters
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. cacheCluster_cacheClusterStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "restore-failed"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeCacheClustersResponse_cacheClusters
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. cacheCluster_cacheClusterStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.ElastiCache.DescribeCacheClusters' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newCacheClusterDeleted :: Waiter.Wait DescribeCacheClusters
newCacheClusterDeleted =
  Waiter.Wait
    { Waiter._waitName =
        "CacheClusterDeleted",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "deleted"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeCacheClustersResponse_cacheClusters
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. cacheCluster_cacheClusterStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchError
            "CacheClusterNotFound"
            Waiter.AcceptSuccess,
          Waiter.matchAny
            "available"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeCacheClustersResponse_cacheClusters
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. cacheCluster_cacheClusterStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "creating"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeCacheClustersResponse_cacheClusters
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. cacheCluster_cacheClusterStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "incompatible-network"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeCacheClustersResponse_cacheClusters
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. cacheCluster_cacheClusterStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "modifying"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeCacheClustersResponse_cacheClusters
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. cacheCluster_cacheClusterStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "restore-failed"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeCacheClustersResponse_cacheClusters
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. cacheCluster_cacheClusterStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "snapshotting"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeCacheClustersResponse_cacheClusters
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. cacheCluster_cacheClusterStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

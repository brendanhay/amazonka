{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Waiters
  ( -- * CacheClusterAvailable
    mkCacheClusterAvailable,

    -- * CacheClusterDeleted
    mkCacheClusterDeleted,

    -- * ReplicationGroupDeleted
    mkReplicationGroupDeleted,

    -- * ReplicationGroupAvailable
    mkReplicationGroupAvailable,
  )
where

import Network.AWS.ElastiCache.DescribeCacheClusters
import Network.AWS.ElastiCache.DescribeReplicationGroups
import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.ElastiCache.DescribeCacheClusters' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkCacheClusterAvailable :: Waiter.Wait DescribeCacheClusters
mkCacheClusterAvailable =
  Waiter.Wait
    { Waiter._waitName = "CacheClusterAvailable",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "available"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"cacheClusters" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"cacheClusterStatus"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "deleted"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"cacheClusters" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"cacheClusterStatus"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "deleting"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"cacheClusters" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"cacheClusterStatus"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "incompatible-network"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"cacheClusters" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"cacheClusterStatus"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "restore-failed"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"cacheClusters" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"cacheClusterStatus"
                Core.. Lens._Just
            )
        ]
    }

-- | Polls 'Network.AWS.ElastiCache.DescribeCacheClusters' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkCacheClusterDeleted :: Waiter.Wait DescribeCacheClusters
mkCacheClusterDeleted =
  Waiter.Wait
    { Waiter._waitName = "CacheClusterDeleted",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "deleted"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"cacheClusters" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"cacheClusterStatus"
                Core.. Lens._Just
            ),
          Waiter.matchError "CacheClusterNotFound" Waiter.AcceptSuccess,
          Waiter.matchAny
            "available"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"cacheClusters" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"cacheClusterStatus"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "creating"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"cacheClusters" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"cacheClusterStatus"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "incompatible-network"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"cacheClusters" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"cacheClusterStatus"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "modifying"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"cacheClusters" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"cacheClusterStatus"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "restore-failed"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"cacheClusters" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"cacheClusterStatus"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "snapshotting"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"cacheClusters" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"cacheClusterStatus"
                Core.. Lens._Just
            )
        ]
    }

-- | Polls 'Network.AWS.ElastiCache.DescribeReplicationGroups' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkReplicationGroupDeleted :: Waiter.Wait DescribeReplicationGroups
mkReplicationGroupDeleted =
  Waiter.Wait
    { Waiter._waitName = "ReplicationGroupDeleted",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "deleted"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationGroups" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "available"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationGroups" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            ),
          Waiter.matchError
            "ReplicationGroupNotFoundFault"
            Waiter.AcceptSuccess
        ]
    }

-- | Polls 'Network.AWS.ElastiCache.DescribeReplicationGroups' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkReplicationGroupAvailable :: Waiter.Wait DescribeReplicationGroups
mkReplicationGroupAvailable =
  Waiter.Wait
    { Waiter._waitName = "ReplicationGroupAvailable",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "available"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationGroups" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "deleted"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationGroups" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            )
        ]
    }

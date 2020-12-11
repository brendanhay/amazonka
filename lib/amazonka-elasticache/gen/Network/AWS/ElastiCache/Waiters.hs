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
import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Waiter as Wait

-- | Polls 'Network.AWS.ElastiCache.DescribeCacheClusters' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkCacheClusterAvailable :: Wait.Wait DescribeCacheClusters
mkCacheClusterAvailable =
  Wait.Wait
    { Wait._waitName = "CacheClusterAvailable",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "available"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (drsCacheClusters Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. ccCacheClusterStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "deleted"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (drsCacheClusters Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. ccCacheClusterStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "deleting"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (drsCacheClusters Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. ccCacheClusterStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "incompatible-network"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (drsCacheClusters Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. ccCacheClusterStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "restore-failed"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (drsCacheClusters Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. ccCacheClusterStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.ElastiCache.DescribeCacheClusters' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkCacheClusterDeleted :: Wait.Wait DescribeCacheClusters
mkCacheClusterDeleted =
  Wait.Wait
    { Wait._waitName = "CacheClusterDeleted",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "deleted"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (drsCacheClusters Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. ccCacheClusterStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchError "CacheClusterNotFound" Wait.AcceptSuccess,
          Wait.matchAny
            "available"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (drsCacheClusters Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. ccCacheClusterStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "creating"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (drsCacheClusters Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. ccCacheClusterStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "incompatible-network"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (drsCacheClusters Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. ccCacheClusterStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "modifying"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (drsCacheClusters Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. ccCacheClusterStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "restore-failed"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (drsCacheClusters Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. ccCacheClusterStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "snapshotting"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (drsCacheClusters Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. ccCacheClusterStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.ElastiCache.DescribeReplicationGroups' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkReplicationGroupDeleted :: Wait.Wait DescribeReplicationGroups
mkReplicationGroupDeleted =
  Wait.Wait
    { Wait._waitName = "ReplicationGroupDeleted",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "deleted"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( drgrsReplicationGroups Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. rgStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "available"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( drgrsReplicationGroups Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. rgStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchError
            "ReplicationGroupNotFoundFault"
            Wait.AcceptSuccess
        ]
    }

-- | Polls 'Network.AWS.ElastiCache.DescribeReplicationGroups' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkReplicationGroupAvailable :: Wait.Wait DescribeReplicationGroups
mkReplicationGroupAvailable =
  Wait.Wait
    { Wait._waitName = "ReplicationGroupAvailable",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "available"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( drgrsReplicationGroups Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. rgStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "deleted"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( drgrsReplicationGroups Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. rgStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

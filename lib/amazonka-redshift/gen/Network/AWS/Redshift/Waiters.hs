{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Waiters
  (
    -- * ClusterRestored
    mkClusterRestored,
    -- * ClusterDeleted
    mkClusterDeleted,
    -- * SnapshotAvailable
    mkSnapshotAvailable,
    -- * ClusterAvailable
    mkClusterAvailable,
  ) where

import Network.AWS.Redshift.DescribeClusterSnapshots
import Network.AWS.Redshift.DescribeClusters
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.Redshift.DescribeClusters' every 60 seconds until a successful state is reached. An error is returned after 30 failed checks.
mkClusterRestored :: Waiter.Wait DescribeClusters
mkClusterRestored
  = Waiter.Wait{Waiter._waitName = "ClusterRestored",
                Waiter._waitAttempts = 30, Waiter._waitDelay = 60,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "completed" Waiter.AcceptSuccess
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"clusters" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core..
                        Lens.field @"restoreStatus" Core.. Lens._Just Core..
                          Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "deleting" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"clusters" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"clusterStatus" Core.. Lens._Just)]}

-- | Polls 'Network.AWS.Redshift.DescribeClusters' every 60 seconds until a successful state is reached. An error is returned after 30 failed checks.
mkClusterDeleted :: Waiter.Wait DescribeClusters
mkClusterDeleted
  = Waiter.Wait{Waiter._waitName = "ClusterDeleted",
                Waiter._waitAttempts = 30, Waiter._waitDelay = 60,
                Waiter._waitAcceptors =
                  [Waiter.matchError "ClusterNotFound" Waiter.AcceptSuccess,
                   Waiter.matchAny "creating" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"clusters" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"clusterStatus" Core.. Lens._Just),
                   Waiter.matchAny "modifying" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"clusters" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"clusterStatus" Core.. Lens._Just)]}

-- | Polls 'Network.AWS.Redshift.DescribeClusterSnapshots' every 15 seconds until a successful state is reached. An error is returned after 20 failed checks.
mkSnapshotAvailable :: Waiter.Wait DescribeClusterSnapshots
mkSnapshotAvailable
  = Waiter.Wait{Waiter._waitName = "SnapshotAvailable",
                Waiter._waitAttempts = 20, Waiter._waitDelay = 15,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "available" Waiter.AcceptSuccess
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"snapshots" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "failed" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"snapshots" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "deleted" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"snapshots" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just)]}

-- | Polls 'Network.AWS.Redshift.DescribeClusters' every 60 seconds until a successful state is reached. An error is returned after 30 failed checks.
mkClusterAvailable :: Waiter.Wait DescribeClusters
mkClusterAvailable
  = Waiter.Wait{Waiter._waitName = "ClusterAvailable",
                Waiter._waitAttempts = 30, Waiter._waitDelay = 60,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "available" Waiter.AcceptSuccess
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"clusters" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"clusterStatus" Core.. Lens._Just),
                   Waiter.matchAny "deleting" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"clusters" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"clusterStatus" Core.. Lens._Just),
                   Waiter.matchError "ClusterNotFound" Waiter.AcceptRetry]}

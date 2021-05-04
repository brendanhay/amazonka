{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Waiters where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.DescribeClusterSnapshots
import Network.AWS.Redshift.DescribeClusters
import Network.AWS.Redshift.Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.Redshift.DescribeClusters' every 60 seconds until a successful state is reached. An error is returned after 30 failed checks.
newClusterRestored :: Waiter.Wait DescribeClusters
newClusterRestored =
  Waiter.Wait
    { Waiter._waitName = "ClusterRestored",
      Waiter._waitAttempts = 30,
      Waiter._waitDelay = 60,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "completed"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeClustersResponse_clusters
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. cluster_restoreStatus
                Prelude.. Lens._Just
                Prelude.. restoreStatus_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "deleting"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeClustersResponse_clusters
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. cluster_clusterStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.Redshift.DescribeClusters' every 60 seconds until a successful state is reached. An error is returned after 30 failed checks.
newClusterDeleted :: Waiter.Wait DescribeClusters
newClusterDeleted =
  Waiter.Wait
    { Waiter._waitName = "ClusterDeleted",
      Waiter._waitAttempts = 30,
      Waiter._waitDelay = 60,
      Waiter._waitAcceptors =
        [ Waiter.matchError
            "ClusterNotFound"
            Waiter.AcceptSuccess,
          Waiter.matchAny
            "creating"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeClustersResponse_clusters
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. cluster_clusterStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "modifying"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeClustersResponse_clusters
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. cluster_clusterStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.Redshift.DescribeClusters' every 60 seconds until a successful state is reached. An error is returned after 30 failed checks.
newClusterAvailable :: Waiter.Wait DescribeClusters
newClusterAvailable =
  Waiter.Wait
    { Waiter._waitName = "ClusterAvailable",
      Waiter._waitAttempts = 30,
      Waiter._waitDelay = 60,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "available"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeClustersResponse_clusters
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. cluster_clusterStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "deleting"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeClustersResponse_clusters
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. cluster_clusterStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchError
            "ClusterNotFound"
            Waiter.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.Redshift.DescribeClusterSnapshots' every 15 seconds until a successful state is reached. An error is returned after 20 failed checks.
newSnapshotAvailable :: Waiter.Wait DescribeClusterSnapshots
newSnapshotAvailable =
  Waiter.Wait
    { Waiter._waitName = "SnapshotAvailable",
      Waiter._waitAttempts = 20,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "available"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeClusterSnapshotsResponse_snapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. snapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "failed"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeClusterSnapshotsResponse_snapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. snapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "deleted"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeClusterSnapshotsResponse_snapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. snapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

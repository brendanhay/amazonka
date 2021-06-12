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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.DescribeClusterSnapshots
import Network.AWS.Redshift.DescribeClusters
import Network.AWS.Redshift.Lens
import Network.AWS.Redshift.Types

-- | Polls 'Network.AWS.Redshift.DescribeClusters' every 60 seconds until a successful state is reached. An error is returned after 30 failed checks.
newClusterRestored :: Core.Wait DescribeClusters
newClusterRestored =
  Core.Wait
    { Core._waitName = "ClusterRestored",
      Core._waitAttempts = 30,
      Core._waitDelay = 60,
      Core._waitAcceptors =
        [ Core.matchAll
            "completed"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeClustersResponse_clusters
                        Core.. Lens._Just
                    )
                )
                Core.. cluster_restoreStatus
                Core.. Lens._Just
                Core.. restoreStatus_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "deleting"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeClustersResponse_clusters
                        Core.. Lens._Just
                    )
                )
                Core.. cluster_clusterStatus
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.Redshift.DescribeClusters' every 60 seconds until a successful state is reached. An error is returned after 30 failed checks.
newClusterDeleted :: Core.Wait DescribeClusters
newClusterDeleted =
  Core.Wait
    { Core._waitName = "ClusterDeleted",
      Core._waitAttempts = 30,
      Core._waitDelay = 60,
      Core._waitAcceptors =
        [ Core.matchError
            "ClusterNotFound"
            Core.AcceptSuccess,
          Core.matchAny
            "creating"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeClustersResponse_clusters
                        Core.. Lens._Just
                    )
                )
                Core.. cluster_clusterStatus
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "modifying"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeClustersResponse_clusters
                        Core.. Lens._Just
                    )
                )
                Core.. cluster_clusterStatus
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.Redshift.DescribeClusters' every 60 seconds until a successful state is reached. An error is returned after 30 failed checks.
newClusterAvailable :: Core.Wait DescribeClusters
newClusterAvailable =
  Core.Wait
    { Core._waitName = "ClusterAvailable",
      Core._waitAttempts = 30,
      Core._waitDelay = 60,
      Core._waitAcceptors =
        [ Core.matchAll
            "available"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeClustersResponse_clusters
                        Core.. Lens._Just
                    )
                )
                Core.. cluster_clusterStatus
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "deleting"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeClustersResponse_clusters
                        Core.. Lens._Just
                    )
                )
                Core.. cluster_clusterStatus
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchError "ClusterNotFound" Core.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.Redshift.DescribeClusterSnapshots' every 15 seconds until a successful state is reached. An error is returned after 20 failed checks.
newSnapshotAvailable :: Core.Wait DescribeClusterSnapshots
newSnapshotAvailable =
  Core.Wait
    { Core._waitName = "SnapshotAvailable",
      Core._waitAttempts = 20,
      Core._waitDelay = 15,
      Core._waitAcceptors =
        [ Core.matchAll
            "available"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeClusterSnapshotsResponse_snapshots
                        Core.. Lens._Just
                    )
                )
                Core.. snapshot_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "failed"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeClusterSnapshotsResponse_snapshots
                        Core.. Lens._Just
                    )
                )
                Core.. snapshot_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "deleted"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeClusterSnapshotsResponse_snapshots
                        Core.. Lens._Just
                    )
                )
                Core.. snapshot_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            )
        ]
    }

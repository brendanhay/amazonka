{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Redshift.Waiters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Waiters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.DescribeClusterSnapshots
import Amazonka.Redshift.DescribeClusters
import Amazonka.Redshift.Lens
import Amazonka.Redshift.Types

-- | Polls 'Amazonka.Redshift.DescribeClusters' every 60 seconds until a successful state is reached. An error is returned after 30 failed checks.
newClusterAvailable :: Core.Wait DescribeClusters
newClusterAvailable =
  Core.Wait
    { Core.name = "ClusterAvailable",
      Core.attempts = 30,
      Core.delay = 60,
      Core.acceptors =
        [ Core.matchAll
            "available"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeClustersResponse_clusters
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. cluster_clusterStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "deleting"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeClustersResponse_clusters
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. cluster_clusterStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchError "ClusterNotFound" Core.AcceptRetry
        ]
    }

-- | Polls 'Amazonka.Redshift.DescribeClusters' every 60 seconds until a successful state is reached. An error is returned after 30 failed checks.
newClusterRestored :: Core.Wait DescribeClusters
newClusterRestored =
  Core.Wait
    { Core.name = "ClusterRestored",
      Core.attempts = 30,
      Core.delay = 60,
      Core.acceptors =
        [ Core.matchAll
            "completed"
            Core.AcceptSuccess
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
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "deleting"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeClustersResponse_clusters
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. cluster_clusterStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Redshift.DescribeClusters' every 60 seconds until a successful state is reached. An error is returned after 30 failed checks.
newClusterDeleted :: Core.Wait DescribeClusters
newClusterDeleted =
  Core.Wait
    { Core.name = "ClusterDeleted",
      Core.attempts = 30,
      Core.delay = 60,
      Core.acceptors =
        [ Core.matchError
            "ClusterNotFound"
            Core.AcceptSuccess,
          Core.matchAny
            "creating"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeClustersResponse_clusters
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. cluster_clusterStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "modifying"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeClustersResponse_clusters
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. cluster_clusterStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Redshift.DescribeClusterSnapshots' every 15 seconds until a successful state is reached. An error is returned after 20 failed checks.
newSnapshotAvailable :: Core.Wait DescribeClusterSnapshots
newSnapshotAvailable =
  Core.Wait
    { Core.name = "SnapshotAvailable",
      Core.attempts = 20,
      Core.delay = 15,
      Core.acceptors =
        [ Core.matchAll
            "available"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeClusterSnapshotsResponse_snapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. snapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "failed"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeClusterSnapshotsResponse_snapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. snapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "deleted"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeClusterSnapshotsResponse_snapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. snapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

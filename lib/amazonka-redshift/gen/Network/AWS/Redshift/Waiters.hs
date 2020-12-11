{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Waiters
  ( -- * ClusterRestored
    mkClusterRestored,

    -- * ClusterDeleted
    mkClusterDeleted,

    -- * SnapshotAvailable
    mkSnapshotAvailable,

    -- * ClusterAvailable
    mkClusterAvailable,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.DescribeClusterSnapshots
import Network.AWS.Redshift.DescribeClusters
import Network.AWS.Redshift.Types
import qualified Network.AWS.Waiter as Wait

-- | Polls 'Network.AWS.Redshift.DescribeClusters' every 60 seconds until a successful state is reached. An error is returned after 30 failed checks.
mkClusterRestored :: Wait.Wait DescribeClusters
mkClusterRestored =
  Wait.Wait
    { Wait._waitName = "ClusterRestored",
      Wait._waitAttempts = 30,
      Wait._waitDelay = 60,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "completed"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (dcrsClusters Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. cRestoreStatus
                Lude.. Lens._Just
                Lude.. rsStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "deleting"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dcrsClusters Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. cClusterStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.Redshift.DescribeClusters' every 60 seconds until a successful state is reached. An error is returned after 30 failed checks.
mkClusterDeleted :: Wait.Wait DescribeClusters
mkClusterDeleted =
  Wait.Wait
    { Wait._waitName = "ClusterDeleted",
      Wait._waitAttempts = 30,
      Wait._waitDelay = 60,
      Wait._waitAcceptors =
        [ Wait.matchError "ClusterNotFound" Wait.AcceptSuccess,
          Wait.matchAny
            "creating"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dcrsClusters Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. cClusterStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "modifying"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dcrsClusters Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. cClusterStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.Redshift.DescribeClusterSnapshots' every 15 seconds until a successful state is reached. An error is returned after 20 failed checks.
mkSnapshotAvailable :: Wait.Wait DescribeClusterSnapshots
mkSnapshotAvailable =
  Wait.Wait
    { Wait._waitName = "SnapshotAvailable",
      Wait._waitAttempts = 20,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "available"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (dcssrsSnapshots Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. sStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "failed"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dcssrsSnapshots Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. sStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "deleted"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dcssrsSnapshots Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. sStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.Redshift.DescribeClusters' every 60 seconds until a successful state is reached. An error is returned after 30 failed checks.
mkClusterAvailable :: Wait.Wait DescribeClusters
mkClusterAvailable =
  Wait.Wait
    { Wait._waitName = "ClusterAvailable",
      Wait._waitAttempts = 30,
      Wait._waitDelay = 60,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "available"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (dcrsClusters Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. cClusterStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "deleting"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dcrsClusters Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. cClusterStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchError "ClusterNotFound" Wait.AcceptRetry
        ]
    }

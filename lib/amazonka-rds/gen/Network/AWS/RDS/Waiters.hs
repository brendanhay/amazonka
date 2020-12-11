{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Waiters
  ( -- * DBInstanceAvailable
    mkDBInstanceAvailable,

    -- * DBSnapshotCompleted
    mkDBSnapshotCompleted,

    -- * DBSnapshotDeleted
    mkDBSnapshotDeleted,

    -- * DBInstanceDeleted
    mkDBInstanceDeleted,

    -- * DBClusterSnapshotDeleted
    mkDBClusterSnapshotDeleted,

    -- * DBSnapshotAvailable
    mkDBSnapshotAvailable,

    -- * DBClusterSnapshotAvailable
    mkDBClusterSnapshotAvailable,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.DescribeDBClusterSnapshots
import Network.AWS.RDS.DescribeDBInstances
import Network.AWS.RDS.DescribeDBSnapshots
import Network.AWS.RDS.Types
import qualified Network.AWS.Waiter as Wait

-- | Polls 'Network.AWS.RDS.DescribeDBInstances' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkDBInstanceAvailable :: Wait.Wait DescribeDBInstances
mkDBInstanceAvailable =
  Wait.Wait
    { Wait._waitName = "DBInstanceAvailable",
      Wait._waitAttempts = 60,
      Wait._waitDelay = 30,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "available"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (ddbirsDBInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. diDBInstanceStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "deleted"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (ddbirsDBInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. diDBInstanceStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "deleting"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (ddbirsDBInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. diDBInstanceStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "failed"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (ddbirsDBInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. diDBInstanceStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "incompatible-restore"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (ddbirsDBInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. diDBInstanceStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "incompatible-parameters"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (ddbirsDBInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. diDBInstanceStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.RDS.DescribeDBSnapshots' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkDBSnapshotCompleted :: Wait.Wait DescribeDBSnapshots
mkDBSnapshotCompleted =
  Wait.Wait
    { Wait._waitName = "DBSnapshotCompleted",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchError "DBSnapshotNotFound" Wait.AcceptSuccess,
          Wait.matchAll
            "available"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (ddsrsDBSnapshots Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. dsStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.RDS.DescribeDBSnapshots' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkDBSnapshotDeleted :: Wait.Wait DescribeDBSnapshots
mkDBSnapshotDeleted =
  Wait.Wait
    { Wait._waitName = "DBSnapshotDeleted",
      Wait._waitAttempts = 60,
      Wait._waitDelay = 30,
      Wait._waitAcceptors =
        [ Lude.matchEmpty
            Lude.True
            Wait.AcceptSuccess
            (ddsrsDBSnapshots Lude.. Lens._Just),
          Wait.matchError "DBSnapshotNotFound" Wait.AcceptSuccess,
          Wait.matchAny
            "creating"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (ddsrsDBSnapshots Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. dsStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "modifying"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (ddsrsDBSnapshots Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. dsStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "rebooting"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (ddsrsDBSnapshots Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. dsStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "resetting-master-credentials"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (ddsrsDBSnapshots Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. dsStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.RDS.DescribeDBInstances' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkDBInstanceDeleted :: Wait.Wait DescribeDBInstances
mkDBInstanceDeleted =
  Wait.Wait
    { Wait._waitName = "DBInstanceDeleted",
      Wait._waitAttempts = 60,
      Wait._waitDelay = 30,
      Wait._waitAcceptors =
        [ Lude.matchEmpty
            Lude.True
            Wait.AcceptSuccess
            (ddbirsDBInstances Lude.. Lens._Just),
          Wait.matchError "DBInstanceNotFound" Wait.AcceptSuccess,
          Wait.matchAny
            "creating"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (ddbirsDBInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. diDBInstanceStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "modifying"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (ddbirsDBInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. diDBInstanceStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "rebooting"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (ddbirsDBInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. diDBInstanceStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "resetting-master-credentials"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (ddbirsDBInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. diDBInstanceStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.RDS.DescribeDBClusterSnapshots' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkDBClusterSnapshotDeleted :: Wait.Wait DescribeDBClusterSnapshots
mkDBClusterSnapshotDeleted =
  Wait.Wait
    { Wait._waitName = "DBClusterSnapshotDeleted",
      Wait._waitAttempts = 60,
      Wait._waitDelay = 30,
      Wait._waitAcceptors =
        [ Lude.matchEmpty
            Lude.True
            Wait.AcceptSuccess
            (ddbcsrsDBClusterSnapshots Lude.. Lens._Just),
          Wait.matchError
            "DBClusterSnapshotNotFoundFault"
            Wait.AcceptSuccess,
          Wait.matchAny
            "creating"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( ddbcsrsDBClusterSnapshots Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. dcsStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "modifying"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( ddbcsrsDBClusterSnapshots Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. dcsStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "rebooting"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( ddbcsrsDBClusterSnapshots Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. dcsStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "resetting-master-credentials"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( ddbcsrsDBClusterSnapshots Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. dcsStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.RDS.DescribeDBSnapshots' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkDBSnapshotAvailable :: Wait.Wait DescribeDBSnapshots
mkDBSnapshotAvailable =
  Wait.Wait
    { Wait._waitName = "DBSnapshotAvailable",
      Wait._waitAttempts = 60,
      Wait._waitDelay = 30,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "available"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (ddsrsDBSnapshots Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. dsStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "deleted"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (ddsrsDBSnapshots Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. dsStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "deleting"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (ddsrsDBSnapshots Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. dsStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "failed"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (ddsrsDBSnapshots Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. dsStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "incompatible-restore"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (ddsrsDBSnapshots Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. dsStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "incompatible-parameters"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (ddsrsDBSnapshots Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. dsStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.RDS.DescribeDBClusterSnapshots' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkDBClusterSnapshotAvailable :: Wait.Wait DescribeDBClusterSnapshots
mkDBClusterSnapshotAvailable =
  Wait.Wait
    { Wait._waitName = "DBClusterSnapshotAvailable",
      Wait._waitAttempts = 60,
      Wait._waitDelay = 30,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "available"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( ddbcsrsDBClusterSnapshots Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. dcsStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "deleted"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( ddbcsrsDBClusterSnapshots Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. dcsStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "deleting"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( ddbcsrsDBClusterSnapshots Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. dcsStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "failed"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( ddbcsrsDBClusterSnapshots Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. dcsStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "incompatible-restore"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( ddbcsrsDBClusterSnapshots Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. dcsStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "incompatible-parameters"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( ddbcsrsDBClusterSnapshots Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. dcsStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

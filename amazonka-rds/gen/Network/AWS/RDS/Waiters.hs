{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Waiters where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.DescribeDBClusterSnapshots
import Network.AWS.RDS.DescribeDBInstances
import Network.AWS.RDS.DescribeDBSnapshots
import Network.AWS.RDS.Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.RDS.DescribeDBSnapshots' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newDBSnapshotDeleted :: Waiter.Wait DescribeDBSnapshots
newDBSnapshotDeleted =
  Waiter.Wait
    { Waiter._waitName = "DBSnapshotDeleted",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 30,
      Waiter._waitAcceptors =
        [ Waiter.matchNonEmpty
            Prelude.True
            Waiter.AcceptSuccess
            ( describeDBSnapshotsResponse_dbSnapshots
                Prelude.. Lens._Just
            ),
          Waiter.matchError
            "DBSnapshotNotFound"
            Waiter.AcceptSuccess,
          Waiter.matchAny
            "creating"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBSnapshotsResponse_dbSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "modifying"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBSnapshotsResponse_dbSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "rebooting"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBSnapshotsResponse_dbSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "resetting-master-credentials"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBSnapshotsResponse_dbSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.RDS.DescribeDBSnapshots' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newDBSnapshotCompleted :: Waiter.Wait DescribeDBSnapshots
newDBSnapshotCompleted =
  Waiter.Wait
    { Waiter._waitName =
        "DBSnapshotCompleted",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchError
            "DBSnapshotNotFound"
            Waiter.AcceptSuccess,
          Waiter.matchAll
            "available"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBSnapshotsResponse_dbSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.RDS.DescribeDBInstances' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newDBInstanceAvailable :: Waiter.Wait DescribeDBInstances
newDBInstanceAvailable =
  Waiter.Wait
    { Waiter._waitName =
        "DBInstanceAvailable",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 30,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "available"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBInstancesResponse_dbInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbInstance_dbInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "deleted"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBInstancesResponse_dbInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbInstance_dbInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "deleting"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBInstancesResponse_dbInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbInstance_dbInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "failed"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBInstancesResponse_dbInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbInstance_dbInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "incompatible-restore"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBInstancesResponse_dbInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbInstance_dbInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "incompatible-parameters"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBInstancesResponse_dbInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbInstance_dbInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.RDS.DescribeDBSnapshots' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newDBSnapshotAvailable :: Waiter.Wait DescribeDBSnapshots
newDBSnapshotAvailable =
  Waiter.Wait
    { Waiter._waitName =
        "DBSnapshotAvailable",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 30,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "available"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBSnapshotsResponse_dbSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "deleted"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBSnapshotsResponse_dbSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "deleting"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBSnapshotsResponse_dbSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "failed"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBSnapshotsResponse_dbSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "incompatible-restore"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBSnapshotsResponse_dbSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "incompatible-parameters"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBSnapshotsResponse_dbSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.RDS.DescribeDBClusterSnapshots' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newDBClusterSnapshotDeleted :: Waiter.Wait DescribeDBClusterSnapshots
newDBClusterSnapshotDeleted =
  Waiter.Wait
    { Waiter._waitName =
        "DBClusterSnapshotDeleted",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 30,
      Waiter._waitAcceptors =
        [ Waiter.matchNonEmpty
            Prelude.True
            Waiter.AcceptSuccess
            ( describeDBClusterSnapshotsResponse_dbClusterSnapshots
                Prelude.. Lens._Just
            ),
          Waiter.matchError
            "DBClusterSnapshotNotFoundFault"
            Waiter.AcceptSuccess,
          Waiter.matchAny
            "creating"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBClusterSnapshotsResponse_dbClusterSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbClusterSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "modifying"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBClusterSnapshotsResponse_dbClusterSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbClusterSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "rebooting"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBClusterSnapshotsResponse_dbClusterSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbClusterSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "resetting-master-credentials"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBClusterSnapshotsResponse_dbClusterSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbClusterSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.RDS.DescribeDBInstances' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newDBInstanceDeleted :: Waiter.Wait DescribeDBInstances
newDBInstanceDeleted =
  Waiter.Wait
    { Waiter._waitName = "DBInstanceDeleted",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 30,
      Waiter._waitAcceptors =
        [ Waiter.matchNonEmpty
            Prelude.True
            Waiter.AcceptSuccess
            ( describeDBInstancesResponse_dbInstances
                Prelude.. Lens._Just
            ),
          Waiter.matchError
            "DBInstanceNotFound"
            Waiter.AcceptSuccess,
          Waiter.matchAny
            "creating"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBInstancesResponse_dbInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbInstance_dbInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "modifying"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBInstancesResponse_dbInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbInstance_dbInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "rebooting"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBInstancesResponse_dbInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbInstance_dbInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "resetting-master-credentials"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBInstancesResponse_dbInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbInstance_dbInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.RDS.DescribeDBClusterSnapshots' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newDBClusterSnapshotAvailable :: Waiter.Wait DescribeDBClusterSnapshots
newDBClusterSnapshotAvailable =
  Waiter.Wait
    { Waiter._waitName =
        "DBClusterSnapshotAvailable",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 30,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "available"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBClusterSnapshotsResponse_dbClusterSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbClusterSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "deleted"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBClusterSnapshotsResponse_dbClusterSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbClusterSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "deleting"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBClusterSnapshotsResponse_dbClusterSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbClusterSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "failed"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBClusterSnapshotsResponse_dbClusterSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbClusterSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "incompatible-restore"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBClusterSnapshotsResponse_dbClusterSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbClusterSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "incompatible-parameters"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBClusterSnapshotsResponse_dbClusterSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbClusterSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

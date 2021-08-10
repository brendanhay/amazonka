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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.DescribeDBClusterSnapshots
import Network.AWS.RDS.DescribeDBInstances
import Network.AWS.RDS.DescribeDBSnapshots
import Network.AWS.RDS.Lens
import Network.AWS.RDS.Types

-- | Polls 'Network.AWS.RDS.DescribeDBSnapshots' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newDBSnapshotDeleted :: Core.Wait DescribeDBSnapshots
newDBSnapshotDeleted =
  Core.Wait
    { Core._waitName = "DBSnapshotDeleted",
      Core._waitAttempts = 60,
      Core._waitDelay = 30,
      Core._waitAcceptors =
        [ Core.matchNonEmpty
            Prelude.True
            Core.AcceptSuccess
            ( describeDBSnapshotsResponse_dbSnapshots
                Prelude.. Lens._Just
            ),
          Core.matchError
            "DBSnapshotNotFound"
            Core.AcceptSuccess,
          Core.matchAny
            "creating"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBSnapshotsResponse_dbSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "modifying"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBSnapshotsResponse_dbSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "rebooting"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBSnapshotsResponse_dbSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "resetting-master-credentials"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBSnapshotsResponse_dbSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.RDS.DescribeDBSnapshots' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newDBSnapshotCompleted :: Core.Wait DescribeDBSnapshots
newDBSnapshotCompleted =
  Core.Wait
    { Core._waitName = "DBSnapshotCompleted",
      Core._waitAttempts = 40,
      Core._waitDelay = 15,
      Core._waitAcceptors =
        [ Core.matchError
            "DBSnapshotNotFound"
            Core.AcceptSuccess,
          Core.matchAll
            "available"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBSnapshotsResponse_dbSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.RDS.DescribeDBInstances' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newDBInstanceAvailable :: Core.Wait DescribeDBInstances
newDBInstanceAvailable =
  Core.Wait
    { Core._waitName = "DBInstanceAvailable",
      Core._waitAttempts = 60,
      Core._waitDelay = 30,
      Core._waitAcceptors =
        [ Core.matchAll
            "available"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBInstancesResponse_dbInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbInstance_dbInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "deleted"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBInstancesResponse_dbInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbInstance_dbInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "deleting"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBInstancesResponse_dbInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbInstance_dbInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "failed"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBInstancesResponse_dbInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbInstance_dbInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "incompatible-restore"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBInstancesResponse_dbInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbInstance_dbInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "incompatible-parameters"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBInstancesResponse_dbInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbInstance_dbInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.RDS.DescribeDBSnapshots' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newDBSnapshotAvailable :: Core.Wait DescribeDBSnapshots
newDBSnapshotAvailable =
  Core.Wait
    { Core._waitName = "DBSnapshotAvailable",
      Core._waitAttempts = 60,
      Core._waitDelay = 30,
      Core._waitAcceptors =
        [ Core.matchAll
            "available"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBSnapshotsResponse_dbSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "deleted"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBSnapshotsResponse_dbSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "deleting"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBSnapshotsResponse_dbSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "failed"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBSnapshotsResponse_dbSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "incompatible-restore"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBSnapshotsResponse_dbSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "incompatible-parameters"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBSnapshotsResponse_dbSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.RDS.DescribeDBClusterSnapshots' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newDBClusterSnapshotDeleted :: Core.Wait DescribeDBClusterSnapshots
newDBClusterSnapshotDeleted =
  Core.Wait
    { Core._waitName =
        "DBClusterSnapshotDeleted",
      Core._waitAttempts = 60,
      Core._waitDelay = 30,
      Core._waitAcceptors =
        [ Core.matchNonEmpty
            Prelude.True
            Core.AcceptSuccess
            ( describeDBClusterSnapshotsResponse_dbClusterSnapshots
                Prelude.. Lens._Just
            ),
          Core.matchError
            "DBClusterSnapshotNotFoundFault"
            Core.AcceptSuccess,
          Core.matchAny
            "creating"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBClusterSnapshotsResponse_dbClusterSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbClusterSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "modifying"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBClusterSnapshotsResponse_dbClusterSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbClusterSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "rebooting"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBClusterSnapshotsResponse_dbClusterSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbClusterSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "resetting-master-credentials"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBClusterSnapshotsResponse_dbClusterSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbClusterSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.RDS.DescribeDBInstances' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newDBInstanceDeleted :: Core.Wait DescribeDBInstances
newDBInstanceDeleted =
  Core.Wait
    { Core._waitName = "DBInstanceDeleted",
      Core._waitAttempts = 60,
      Core._waitDelay = 30,
      Core._waitAcceptors =
        [ Core.matchNonEmpty
            Prelude.True
            Core.AcceptSuccess
            ( describeDBInstancesResponse_dbInstances
                Prelude.. Lens._Just
            ),
          Core.matchError
            "DBInstanceNotFound"
            Core.AcceptSuccess,
          Core.matchAny
            "creating"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBInstancesResponse_dbInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbInstance_dbInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "modifying"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBInstancesResponse_dbInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbInstance_dbInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "rebooting"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBInstancesResponse_dbInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbInstance_dbInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "resetting-master-credentials"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBInstancesResponse_dbInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbInstance_dbInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.RDS.DescribeDBClusterSnapshots' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newDBClusterSnapshotAvailable :: Core.Wait DescribeDBClusterSnapshots
newDBClusterSnapshotAvailable =
  Core.Wait
    { Core._waitName =
        "DBClusterSnapshotAvailable",
      Core._waitAttempts = 60,
      Core._waitDelay = 30,
      Core._waitAcceptors =
        [ Core.matchAll
            "available"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBClusterSnapshotsResponse_dbClusterSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbClusterSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "deleted"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBClusterSnapshotsResponse_dbClusterSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbClusterSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "deleting"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBClusterSnapshotsResponse_dbClusterSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbClusterSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "failed"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBClusterSnapshotsResponse_dbClusterSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbClusterSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "incompatible-restore"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBClusterSnapshotsResponse_dbClusterSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbClusterSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "incompatible-parameters"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBClusterSnapshotsResponse_dbClusterSnapshots
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbClusterSnapshot_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

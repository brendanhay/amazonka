{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.RDS.Waiters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Waiters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.DescribeDBClusterSnapshots
import Amazonka.RDS.DescribeDBClusters
import Amazonka.RDS.DescribeDBInstances
import Amazonka.RDS.DescribeDBSnapshots
import Amazonka.RDS.Lens
import Amazonka.RDS.Types

-- | Polls 'Amazonka.RDS.DescribeDBClusters' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newDBClusterDeleted :: Core.Wait DescribeDBClusters
newDBClusterDeleted =
  Core.Wait
    { Core.name = "DBClusterDeleted",
      Core.attempts = 60,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchNonEmpty
            Prelude.True
            Core.AcceptSuccess
            ( describeDBClustersResponse_dbClusters
                Prelude.. Lens._Just
            ),
          Core.matchError
            "DBClusterNotFoundFault"
            Core.AcceptSuccess,
          Core.matchAny
            "creating"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBClustersResponse_dbClusters
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbCluster_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "modifying"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBClustersResponse_dbClusters
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbCluster_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "rebooting"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBClustersResponse_dbClusters
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbCluster_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "resetting-master-credentials"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBClustersResponse_dbClusters
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbCluster_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.RDS.DescribeDBClusterSnapshots' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newDBClusterSnapshotDeleted :: Core.Wait DescribeDBClusterSnapshots
newDBClusterSnapshotDeleted =
  Core.Wait
    { Core.name = "DBClusterSnapshotDeleted",
      Core.attempts = 60,
      Core.delay = 30,
      Core.acceptors =
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.RDS.DescribeDBClusterSnapshots' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newDBClusterSnapshotAvailable :: Core.Wait DescribeDBClusterSnapshots
newDBClusterSnapshotAvailable =
  Core.Wait
    { Core.name = "DBClusterSnapshotAvailable",
      Core.attempts = 60,
      Core.delay = 30,
      Core.acceptors =
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.RDS.DescribeDBInstances' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newDBInstanceAvailable :: Core.Wait DescribeDBInstances
newDBInstanceAvailable =
  Core.Wait
    { Core.name = "DBInstanceAvailable",
      Core.attempts = 60,
      Core.delay = 30,
      Core.acceptors =
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.RDS.DescribeDBSnapshots' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newDBSnapshotCompleted :: Core.Wait DescribeDBSnapshots
newDBSnapshotCompleted =
  Core.Wait
    { Core.name = "DBSnapshotCompleted",
      Core.attempts = 40,
      Core.delay = 15,
      Core.acceptors =
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
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.RDS.DescribeDBSnapshots' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newDBSnapshotAvailable :: Core.Wait DescribeDBSnapshots
newDBSnapshotAvailable =
  Core.Wait
    { Core.name = "DBSnapshotAvailable",
      Core.attempts = 60,
      Core.delay = 30,
      Core.acceptors =
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.RDS.DescribeDBInstances' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newDBInstanceDeleted :: Core.Wait DescribeDBInstances
newDBInstanceDeleted =
  Core.Wait
    { Core.name = "DBInstanceDeleted",
      Core.attempts = 60,
      Core.delay = 30,
      Core.acceptors =
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.RDS.DescribeDBClusters' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newDBClusterAvailable :: Core.Wait DescribeDBClusters
newDBClusterAvailable =
  Core.Wait
    { Core.name = "DBClusterAvailable",
      Core.attempts = 60,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "available"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBClustersResponse_dbClusters
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbCluster_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "deleted"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBClustersResponse_dbClusters
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbCluster_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "deleting"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBClustersResponse_dbClusters
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbCluster_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "failed"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBClustersResponse_dbClusters
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbCluster_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "incompatible-restore"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBClustersResponse_dbClusters
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbCluster_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "incompatible-parameters"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBClustersResponse_dbClusters
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbCluster_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.RDS.DescribeDBSnapshots' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newDBSnapshotDeleted :: Core.Wait DescribeDBSnapshots
newDBSnapshotDeleted =
  Core.Wait
    { Core.name = "DBSnapshotDeleted",
      Core.attempts = 60,
      Core.delay = 30,
      Core.acceptors =
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

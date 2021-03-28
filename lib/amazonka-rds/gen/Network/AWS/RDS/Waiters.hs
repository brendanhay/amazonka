{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Waiters
  (
    -- * DBInstanceAvailable
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
  ) where

import Network.AWS.RDS.DescribeDBClusterSnapshots
import Network.AWS.RDS.DescribeDBInstances
import Network.AWS.RDS.DescribeDBSnapshots
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.RDS.DescribeDBInstances' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkDBInstanceAvailable :: Waiter.Wait DescribeDBInstances
mkDBInstanceAvailable
  = Waiter.Wait{Waiter._waitName = "DBInstanceAvailable",
                Waiter._waitAttempts = 60, Waiter._waitDelay = 30,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "available" Waiter.AcceptSuccess
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"dBInstances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"dBInstanceStatus" Core.. Lens._Just),
                   Waiter.matchAny "deleted" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"dBInstances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"dBInstanceStatus" Core.. Lens._Just),
                   Waiter.matchAny "deleting" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"dBInstances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"dBInstanceStatus" Core.. Lens._Just),
                   Waiter.matchAny "failed" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"dBInstances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"dBInstanceStatus" Core.. Lens._Just),
                   Waiter.matchAny "incompatible-restore" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"dBInstances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"dBInstanceStatus" Core.. Lens._Just),
                   Waiter.matchAny "incompatible-parameters" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"dBInstances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"dBInstanceStatus" Core.. Lens._Just)]}

-- | Polls 'Network.AWS.RDS.DescribeDBSnapshots' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkDBSnapshotCompleted :: Waiter.Wait DescribeDBSnapshots
mkDBSnapshotCompleted
  = Waiter.Wait{Waiter._waitName = "DBSnapshotCompleted",
                Waiter._waitAttempts = 40, Waiter._waitDelay = 15,
                Waiter._waitAcceptors =
                  [Waiter.matchError "DBSnapshotNotFound" Waiter.AcceptSuccess,
                   Waiter.matchAll "available" Waiter.AcceptSuccess
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"dBSnapshots" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just)]}

-- | Polls 'Network.AWS.RDS.DescribeDBSnapshots' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkDBSnapshotDeleted :: Waiter.Wait DescribeDBSnapshots
mkDBSnapshotDeleted
  = Waiter.Wait{Waiter._waitName = "DBSnapshotDeleted",
                Waiter._waitAttempts = 60, Waiter._waitDelay = 30,
                Waiter._waitAcceptors =
                  [Waiter.matchEmpty Core.True Waiter.AcceptSuccess
                     (Lens.field @"dBSnapshots" Core.. Lens._Just Core.. Core._isEmpty),
                   Waiter.matchError "DBSnapshotNotFound" Waiter.AcceptSuccess,
                   Waiter.matchAny "creating" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"dBSnapshots" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "modifying" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"dBSnapshots" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "rebooting" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"dBSnapshots" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "resetting-master-credentials" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"dBSnapshots" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just)]}

-- | Polls 'Network.AWS.RDS.DescribeDBInstances' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkDBInstanceDeleted :: Waiter.Wait DescribeDBInstances
mkDBInstanceDeleted
  = Waiter.Wait{Waiter._waitName = "DBInstanceDeleted",
                Waiter._waitAttempts = 60, Waiter._waitDelay = 30,
                Waiter._waitAcceptors =
                  [Waiter.matchEmpty Core.True Waiter.AcceptSuccess
                     (Lens.field @"dBInstances" Core.. Lens._Just Core.. Core._isEmpty),
                   Waiter.matchError "DBInstanceNotFound" Waiter.AcceptSuccess,
                   Waiter.matchAny "creating" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"dBInstances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"dBInstanceStatus" Core.. Lens._Just),
                   Waiter.matchAny "modifying" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"dBInstances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"dBInstanceStatus" Core.. Lens._Just),
                   Waiter.matchAny "rebooting" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"dBInstances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"dBInstanceStatus" Core.. Lens._Just),
                   Waiter.matchAny "resetting-master-credentials" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"dBInstances" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"dBInstanceStatus" Core.. Lens._Just)]}

-- | Polls 'Network.AWS.RDS.DescribeDBClusterSnapshots' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkDBClusterSnapshotDeleted :: Waiter.Wait DescribeDBClusterSnapshots
mkDBClusterSnapshotDeleted
  = Waiter.Wait{Waiter._waitName = "DBClusterSnapshotDeleted",
                Waiter._waitAttempts = 60, Waiter._waitDelay = 30,
                Waiter._waitAcceptors =
                  [Waiter.matchEmpty Core.True Waiter.AcceptSuccess
                     (Lens.field @"dBClusterSnapshots" Core.. Lens._Just Core..
                        Core._isEmpty),
                   Waiter.matchError "DBClusterSnapshotNotFoundFault"
                     Waiter.AcceptSuccess,
                   Waiter.matchAny "creating" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"dBClusterSnapshots" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "modifying" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"dBClusterSnapshots" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "rebooting" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"dBClusterSnapshots" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "resetting-master-credentials" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"dBClusterSnapshots" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just)]}

-- | Polls 'Network.AWS.RDS.DescribeDBSnapshots' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkDBSnapshotAvailable :: Waiter.Wait DescribeDBSnapshots
mkDBSnapshotAvailable
  = Waiter.Wait{Waiter._waitName = "DBSnapshotAvailable",
                Waiter._waitAttempts = 60, Waiter._waitDelay = 30,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "available" Waiter.AcceptSuccess
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"dBSnapshots" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "deleted" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"dBSnapshots" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "deleting" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"dBSnapshots" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "failed" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"dBSnapshots" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "incompatible-restore" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"dBSnapshots" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "incompatible-parameters" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"dBSnapshots" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just)]}

-- | Polls 'Network.AWS.RDS.DescribeDBClusterSnapshots' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkDBClusterSnapshotAvailable :: Waiter.Wait DescribeDBClusterSnapshots
mkDBClusterSnapshotAvailable
  = Waiter.Wait{Waiter._waitName = "DBClusterSnapshotAvailable",
                Waiter._waitAttempts = 60, Waiter._waitDelay = 30,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "available" Waiter.AcceptSuccess
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"dBClusterSnapshots" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "deleted" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"dBClusterSnapshots" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "deleting" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"dBClusterSnapshots" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "failed" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"dBClusterSnapshots" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "incompatible-restore" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"dBClusterSnapshots" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAny "incompatible-parameters" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"dBClusterSnapshots" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"status" Core.. Lens._Just)]}

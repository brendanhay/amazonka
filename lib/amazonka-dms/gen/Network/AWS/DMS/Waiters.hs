{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Waiters
  ( -- * ReplicationInstanceAvailable
    mkReplicationInstanceAvailable,

    -- * ReplicationTaskDeleted
    mkReplicationTaskDeleted,

    -- * ReplicationTaskReady
    mkReplicationTaskReady,

    -- * ReplicationInstanceDeleted
    mkReplicationInstanceDeleted,

    -- * EndpointDeleted
    mkEndpointDeleted,

    -- * ReplicationTaskStopped
    mkReplicationTaskStopped,

    -- * ReplicationTaskRunning
    mkReplicationTaskRunning,

    -- * TestConnectionSucceeds
    mkTestConnectionSucceeds,
  )
where

import Network.AWS.DMS.DescribeConnections
import Network.AWS.DMS.DescribeEndpoints
import Network.AWS.DMS.DescribeReplicationInstances
import Network.AWS.DMS.DescribeReplicationTasks
import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.DMS.DescribeReplicationInstances' every 60 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkReplicationInstanceAvailable :: Waiter.Wait DescribeReplicationInstances
mkReplicationInstanceAvailable =
  Waiter.Wait
    { Waiter._waitName = "ReplicationInstanceAvailable",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 60,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "available"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationInstances" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"replicationInstanceStatus"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "deleting"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationInstances" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"replicationInstanceStatus"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "incompatible-credentials"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationInstances" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"replicationInstanceStatus"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "incompatible-network"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationInstances" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"replicationInstanceStatus"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "inaccessible-encryption-credentials"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationInstances" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"replicationInstanceStatus"
                Core.. Lens._Just
            )
        ]
    }

-- | Polls 'Network.AWS.DMS.DescribeReplicationTasks' every 15 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkReplicationTaskDeleted :: Waiter.Wait DescribeReplicationTasks
mkReplicationTaskDeleted =
  Waiter.Wait
    { Waiter._waitName = "ReplicationTaskDeleted",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAny
            "ready"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationTasks" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "creating"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationTasks" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "stopped"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationTasks" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "running"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationTasks" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "failed"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationTasks" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            ),
          Waiter.matchError "ResourceNotFoundFault" Waiter.AcceptSuccess
        ]
    }

-- | Polls 'Network.AWS.DMS.DescribeReplicationTasks' every 15 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkReplicationTaskReady :: Waiter.Wait DescribeReplicationTasks
mkReplicationTaskReady =
  Waiter.Wait
    { Waiter._waitName = "ReplicationTaskReady",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "ready"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationTasks" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "starting"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationTasks" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "running"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationTasks" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "stopping"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationTasks" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "stopped"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationTasks" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "failed"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationTasks" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "modifying"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationTasks" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "testing"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationTasks" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "deleting"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationTasks" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            )
        ]
    }

-- | Polls 'Network.AWS.DMS.DescribeReplicationInstances' every 15 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkReplicationInstanceDeleted :: Waiter.Wait DescribeReplicationInstances
mkReplicationInstanceDeleted =
  Waiter.Wait
    { Waiter._waitName = "ReplicationInstanceDeleted",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAny
            "available"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationInstances" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"replicationInstanceStatus"
                Core.. Lens._Just
            ),
          Waiter.matchError "ResourceNotFoundFault" Waiter.AcceptSuccess
        ]
    }

-- | Polls 'Network.AWS.DMS.DescribeEndpoints' every 5 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkEndpointDeleted :: Waiter.Wait DescribeEndpoints
mkEndpointDeleted =
  Waiter.Wait
    { Waiter._waitName = "EndpointDeleted",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 5,
      Waiter._waitAcceptors =
        [ Waiter.matchError "ResourceNotFoundFault" Waiter.AcceptSuccess,
          Waiter.matchAny
            "active"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"endpoints" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "creating"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"endpoints" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            )
        ]
    }

-- | Polls 'Network.AWS.DMS.DescribeReplicationTasks' every 15 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkReplicationTaskStopped :: Waiter.Wait DescribeReplicationTasks
mkReplicationTaskStopped =
  Waiter.Wait
    { Waiter._waitName = "ReplicationTaskStopped",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "stopped"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationTasks" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "ready"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationTasks" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "creating"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationTasks" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "starting"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationTasks" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "running"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationTasks" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "failed"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationTasks" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "modifying"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationTasks" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "testing"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationTasks" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "deleting"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationTasks" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            )
        ]
    }

-- | Polls 'Network.AWS.DMS.DescribeReplicationTasks' every 15 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkReplicationTaskRunning :: Waiter.Wait DescribeReplicationTasks
mkReplicationTaskRunning =
  Waiter.Wait
    { Waiter._waitName = "ReplicationTaskRunning",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "running"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationTasks" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "ready"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationTasks" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "creating"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationTasks" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "stopping"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationTasks" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "stopped"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationTasks" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "failed"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationTasks" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "modifying"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationTasks" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "testing"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationTasks" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "deleting"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"replicationTasks" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            )
        ]
    }

-- | Polls 'Network.AWS.DMS.DescribeConnections' every 5 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkTestConnectionSucceeds :: Waiter.Wait DescribeConnections
mkTestConnectionSucceeds =
  Waiter.Wait
    { Waiter._waitName = "TestConnectionSucceeds",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 5,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "successful"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"connections" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "failed"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"connections" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            )
        ]
    }

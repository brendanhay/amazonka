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
import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Waiter as Wait

-- | Polls 'Network.AWS.DMS.DescribeReplicationInstances' every 60 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkReplicationInstanceAvailable :: Wait.Wait DescribeReplicationInstances
mkReplicationInstanceAvailable =
  Wait.Wait
    { Wait._waitName = "ReplicationInstanceAvailable",
      Wait._waitAttempts = 60,
      Wait._waitDelay = 60,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "available"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( drisrsReplicationInstances Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. riReplicationInstanceStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "deleting"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( drisrsReplicationInstances Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. riReplicationInstanceStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "incompatible-credentials"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( drisrsReplicationInstances Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. riReplicationInstanceStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "incompatible-network"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( drisrsReplicationInstances Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. riReplicationInstanceStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "inaccessible-encryption-credentials"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( drisrsReplicationInstances Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. riReplicationInstanceStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.DMS.DescribeReplicationTasks' every 15 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkReplicationTaskDeleted :: Wait.Wait DescribeReplicationTasks
mkReplicationTaskDeleted =
  Wait.Wait
    { Wait._waitName = "ReplicationTaskDeleted",
      Wait._waitAttempts = 60,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAny
            "ready"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( drtsrsReplicationTasks Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. repStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "creating"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( drtsrsReplicationTasks Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. repStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "stopped"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( drtsrsReplicationTasks Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. repStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "running"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( drtsrsReplicationTasks Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. repStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "failed"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( drtsrsReplicationTasks Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. repStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchError "ResourceNotFoundFault" Wait.AcceptSuccess
        ]
    }

-- | Polls 'Network.AWS.DMS.DescribeReplicationTasks' every 15 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkReplicationTaskReady :: Wait.Wait DescribeReplicationTasks
mkReplicationTaskReady =
  Wait.Wait
    { Wait._waitName = "ReplicationTaskReady",
      Wait._waitAttempts = 60,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "ready"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( drtsrsReplicationTasks Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. repStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "starting"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( drtsrsReplicationTasks Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. repStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "running"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( drtsrsReplicationTasks Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. repStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "stopping"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( drtsrsReplicationTasks Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. repStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "stopped"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( drtsrsReplicationTasks Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. repStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "failed"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( drtsrsReplicationTasks Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. repStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "modifying"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( drtsrsReplicationTasks Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. repStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "testing"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( drtsrsReplicationTasks Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. repStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "deleting"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( drtsrsReplicationTasks Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. repStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.DMS.DescribeReplicationInstances' every 15 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkReplicationInstanceDeleted :: Wait.Wait DescribeReplicationInstances
mkReplicationInstanceDeleted =
  Wait.Wait
    { Wait._waitName = "ReplicationInstanceDeleted",
      Wait._waitAttempts = 60,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAny
            "available"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( drisrsReplicationInstances Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. riReplicationInstanceStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchError "ResourceNotFoundFault" Wait.AcceptSuccess
        ]
    }

-- | Polls 'Network.AWS.DMS.DescribeEndpoints' every 5 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkEndpointDeleted :: Wait.Wait DescribeEndpoints
mkEndpointDeleted =
  Wait.Wait
    { Wait._waitName = "EndpointDeleted",
      Wait._waitAttempts = 60,
      Wait._waitDelay = 5,
      Wait._waitAcceptors =
        [ Wait.matchError "ResourceNotFoundFault" Wait.AcceptSuccess,
          Wait.matchAny
            "active"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dersEndpoints Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. eStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "creating"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dersEndpoints Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. eStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.DMS.DescribeReplicationTasks' every 15 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkReplicationTaskStopped :: Wait.Wait DescribeReplicationTasks
mkReplicationTaskStopped =
  Wait.Wait
    { Wait._waitName = "ReplicationTaskStopped",
      Wait._waitAttempts = 60,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "stopped"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( drtsrsReplicationTasks Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. repStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "ready"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( drtsrsReplicationTasks Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. repStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "creating"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( drtsrsReplicationTasks Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. repStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "starting"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( drtsrsReplicationTasks Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. repStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "running"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( drtsrsReplicationTasks Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. repStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "failed"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( drtsrsReplicationTasks Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. repStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "modifying"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( drtsrsReplicationTasks Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. repStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "testing"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( drtsrsReplicationTasks Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. repStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "deleting"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( drtsrsReplicationTasks Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. repStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.DMS.DescribeReplicationTasks' every 15 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkReplicationTaskRunning :: Wait.Wait DescribeReplicationTasks
mkReplicationTaskRunning =
  Wait.Wait
    { Wait._waitName = "ReplicationTaskRunning",
      Wait._waitAttempts = 60,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "running"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( drtsrsReplicationTasks Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. repStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "ready"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( drtsrsReplicationTasks Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. repStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "creating"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( drtsrsReplicationTasks Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. repStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "stopping"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( drtsrsReplicationTasks Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. repStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "stopped"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( drtsrsReplicationTasks Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. repStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "failed"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( drtsrsReplicationTasks Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. repStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "modifying"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( drtsrsReplicationTasks Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. repStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "testing"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( drtsrsReplicationTasks Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. repStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "deleting"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( drtsrsReplicationTasks Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. repStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.DMS.DescribeConnections' every 5 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkTestConnectionSucceeds :: Wait.Wait DescribeConnections
mkTestConnectionSucceeds =
  Wait.Wait
    { Wait._waitName = "TestConnectionSucceeds",
      Wait._waitAttempts = 60,
      Wait._waitDelay = 5,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "successful"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (dcsrsConnections Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. cStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "failed"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dcsrsConnections Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. cStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

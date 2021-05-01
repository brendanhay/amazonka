{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Waiters where

import Network.AWS.DMS.DescribeConnections
import Network.AWS.DMS.DescribeEndpoints
import Network.AWS.DMS.DescribeReplicationInstances
import Network.AWS.DMS.DescribeReplicationTasks
import Network.AWS.DMS.Lens
import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.DMS.DescribeReplicationInstances' every 60 seconds until a successful state is reached. An error is returned after 60 failed checks.
newReplicationInstanceAvailable :: Waiter.Wait DescribeReplicationInstances
newReplicationInstanceAvailable =
  Waiter.Wait
    { Waiter._waitName =
        "ReplicationInstanceAvailable",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 60,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "available"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationInstancesResponse_replicationInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationInstance_replicationInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "deleting"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationInstancesResponse_replicationInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationInstance_replicationInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "incompatible-credentials"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationInstancesResponse_replicationInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationInstance_replicationInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "incompatible-network"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationInstancesResponse_replicationInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationInstance_replicationInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "inaccessible-encryption-credentials"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationInstancesResponse_replicationInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationInstance_replicationInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.DMS.DescribeReplicationTasks' every 15 seconds until a successful state is reached. An error is returned after 60 failed checks.
newReplicationTaskDeleted :: Waiter.Wait DescribeReplicationTasks
newReplicationTaskDeleted =
  Waiter.Wait
    { Waiter._waitName =
        "ReplicationTaskDeleted",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAny
            "ready"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "creating"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "stopped"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "running"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "failed"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchError
            "ResourceNotFoundFault"
            Waiter.AcceptSuccess
        ]
    }

-- | Polls 'Network.AWS.DMS.DescribeEndpoints' every 5 seconds until a successful state is reached. An error is returned after 60 failed checks.
newEndpointDeleted :: Waiter.Wait DescribeEndpoints
newEndpointDeleted =
  Waiter.Wait
    { Waiter._waitName = "EndpointDeleted",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 5,
      Waiter._waitAcceptors =
        [ Waiter.matchError
            "ResourceNotFoundFault"
            Waiter.AcceptSuccess,
          Waiter.matchAny
            "active"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeEndpointsResponse_endpoints
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. endpoint_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "creating"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeEndpointsResponse_endpoints
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. endpoint_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.DMS.DescribeReplicationTasks' every 15 seconds until a successful state is reached. An error is returned after 60 failed checks.
newReplicationTaskRunning :: Waiter.Wait DescribeReplicationTasks
newReplicationTaskRunning =
  Waiter.Wait
    { Waiter._waitName =
        "ReplicationTaskRunning",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "running"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "ready"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "creating"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "stopping"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "stopped"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "failed"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "modifying"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "testing"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "deleting"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.DMS.DescribeReplicationInstances' every 15 seconds until a successful state is reached. An error is returned after 60 failed checks.
newReplicationInstanceDeleted :: Waiter.Wait DescribeReplicationInstances
newReplicationInstanceDeleted =
  Waiter.Wait
    { Waiter._waitName =
        "ReplicationInstanceDeleted",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAny
            "available"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationInstancesResponse_replicationInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationInstance_replicationInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchError
            "ResourceNotFoundFault"
            Waiter.AcceptSuccess
        ]
    }

-- | Polls 'Network.AWS.DMS.DescribeReplicationTasks' every 15 seconds until a successful state is reached. An error is returned after 60 failed checks.
newReplicationTaskReady :: Waiter.Wait DescribeReplicationTasks
newReplicationTaskReady =
  Waiter.Wait
    { Waiter._waitName =
        "ReplicationTaskReady",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "ready"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "starting"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "running"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "stopping"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "stopped"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "failed"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "modifying"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "testing"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "deleting"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.DMS.DescribeConnections' every 5 seconds until a successful state is reached. An error is returned after 60 failed checks.
newTestConnectionSucceeds :: Waiter.Wait DescribeConnections
newTestConnectionSucceeds =
  Waiter.Wait
    { Waiter._waitName =
        "TestConnectionSucceeds",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 5,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "successful"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeConnectionsResponse_connections
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. connection_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "failed"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeConnectionsResponse_connections
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. connection_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.DMS.DescribeReplicationTasks' every 15 seconds until a successful state is reached. An error is returned after 60 failed checks.
newReplicationTaskStopped :: Waiter.Wait DescribeReplicationTasks
newReplicationTaskStopped =
  Waiter.Wait
    { Waiter._waitName =
        "ReplicationTaskStopped",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "stopped"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "ready"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "creating"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "starting"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "running"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "failed"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "modifying"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "testing"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "deleting"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

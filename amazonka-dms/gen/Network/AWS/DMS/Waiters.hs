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

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.DescribeConnections
import Network.AWS.DMS.DescribeEndpoints
import Network.AWS.DMS.DescribeReplicationInstances
import Network.AWS.DMS.DescribeReplicationTasks
import Network.AWS.DMS.Lens
import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens

-- | Polls 'Network.AWS.DMS.DescribeReplicationInstances' every 60 seconds until a successful state is reached. An error is returned after 60 failed checks.
newReplicationInstanceAvailable :: Core.Wait DescribeReplicationInstances
newReplicationInstanceAvailable =
  Core.Wait
    { Core._waitName =
        "ReplicationInstanceAvailable",
      Core._waitAttempts = 60,
      Core._waitDelay = 60,
      Core._waitAcceptors =
        [ Core.matchAll
            "available"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationInstancesResponse_replicationInstances
                        Core.. Lens._Just
                    )
                )
                Core.. replicationInstance_replicationInstanceStatus
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "deleting"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationInstancesResponse_replicationInstances
                        Core.. Lens._Just
                    )
                )
                Core.. replicationInstance_replicationInstanceStatus
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "incompatible-credentials"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationInstancesResponse_replicationInstances
                        Core.. Lens._Just
                    )
                )
                Core.. replicationInstance_replicationInstanceStatus
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "incompatible-network"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationInstancesResponse_replicationInstances
                        Core.. Lens._Just
                    )
                )
                Core.. replicationInstance_replicationInstanceStatus
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "inaccessible-encryption-credentials"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationInstancesResponse_replicationInstances
                        Core.. Lens._Just
                    )
                )
                Core.. replicationInstance_replicationInstanceStatus
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.DMS.DescribeReplicationTasks' every 15 seconds until a successful state is reached. An error is returned after 60 failed checks.
newReplicationTaskDeleted :: Core.Wait DescribeReplicationTasks
newReplicationTaskDeleted =
  Core.Wait
    { Core._waitName =
        "ReplicationTaskDeleted",
      Core._waitAttempts = 60,
      Core._waitDelay = 15,
      Core._waitAcceptors =
        [ Core.matchAny
            "ready"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Core.. Lens._Just
                    )
                )
                Core.. replicationTask_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "creating"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Core.. Lens._Just
                    )
                )
                Core.. replicationTask_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "stopped"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Core.. Lens._Just
                    )
                )
                Core.. replicationTask_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "running"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Core.. Lens._Just
                    )
                )
                Core.. replicationTask_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "failed"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Core.. Lens._Just
                    )
                )
                Core.. replicationTask_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchError
            "ResourceNotFoundFault"
            Core.AcceptSuccess
        ]
    }

-- | Polls 'Network.AWS.DMS.DescribeEndpoints' every 5 seconds until a successful state is reached. An error is returned after 60 failed checks.
newEndpointDeleted :: Core.Wait DescribeEndpoints
newEndpointDeleted =
  Core.Wait
    { Core._waitName = "EndpointDeleted",
      Core._waitAttempts = 60,
      Core._waitDelay = 5,
      Core._waitAcceptors =
        [ Core.matchError
            "ResourceNotFoundFault"
            Core.AcceptSuccess,
          Core.matchAny
            "active"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeEndpointsResponse_endpoints
                        Core.. Lens._Just
                    )
                )
                Core.. endpoint_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "creating"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeEndpointsResponse_endpoints
                        Core.. Lens._Just
                    )
                )
                Core.. endpoint_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.DMS.DescribeReplicationTasks' every 15 seconds until a successful state is reached. An error is returned after 60 failed checks.
newReplicationTaskRunning :: Core.Wait DescribeReplicationTasks
newReplicationTaskRunning =
  Core.Wait
    { Core._waitName =
        "ReplicationTaskRunning",
      Core._waitAttempts = 60,
      Core._waitDelay = 15,
      Core._waitAcceptors =
        [ Core.matchAll
            "running"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Core.. Lens._Just
                    )
                )
                Core.. replicationTask_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "ready"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Core.. Lens._Just
                    )
                )
                Core.. replicationTask_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "creating"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Core.. Lens._Just
                    )
                )
                Core.. replicationTask_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "stopping"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Core.. Lens._Just
                    )
                )
                Core.. replicationTask_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "stopped"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Core.. Lens._Just
                    )
                )
                Core.. replicationTask_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "failed"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Core.. Lens._Just
                    )
                )
                Core.. replicationTask_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "modifying"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Core.. Lens._Just
                    )
                )
                Core.. replicationTask_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "testing"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Core.. Lens._Just
                    )
                )
                Core.. replicationTask_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "deleting"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Core.. Lens._Just
                    )
                )
                Core.. replicationTask_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.DMS.DescribeReplicationInstances' every 15 seconds until a successful state is reached. An error is returned after 60 failed checks.
newReplicationInstanceDeleted :: Core.Wait DescribeReplicationInstances
newReplicationInstanceDeleted =
  Core.Wait
    { Core._waitName =
        "ReplicationInstanceDeleted",
      Core._waitAttempts = 60,
      Core._waitDelay = 15,
      Core._waitAcceptors =
        [ Core.matchAny
            "available"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationInstancesResponse_replicationInstances
                        Core.. Lens._Just
                    )
                )
                Core.. replicationInstance_replicationInstanceStatus
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchError
            "ResourceNotFoundFault"
            Core.AcceptSuccess
        ]
    }

-- | Polls 'Network.AWS.DMS.DescribeReplicationTasks' every 15 seconds until a successful state is reached. An error is returned after 60 failed checks.
newReplicationTaskReady :: Core.Wait DescribeReplicationTasks
newReplicationTaskReady =
  Core.Wait
    { Core._waitName = "ReplicationTaskReady",
      Core._waitAttempts = 60,
      Core._waitDelay = 15,
      Core._waitAcceptors =
        [ Core.matchAll
            "ready"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Core.. Lens._Just
                    )
                )
                Core.. replicationTask_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "starting"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Core.. Lens._Just
                    )
                )
                Core.. replicationTask_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "running"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Core.. Lens._Just
                    )
                )
                Core.. replicationTask_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "stopping"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Core.. Lens._Just
                    )
                )
                Core.. replicationTask_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "stopped"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Core.. Lens._Just
                    )
                )
                Core.. replicationTask_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "failed"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Core.. Lens._Just
                    )
                )
                Core.. replicationTask_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "modifying"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Core.. Lens._Just
                    )
                )
                Core.. replicationTask_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "testing"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Core.. Lens._Just
                    )
                )
                Core.. replicationTask_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "deleting"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Core.. Lens._Just
                    )
                )
                Core.. replicationTask_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.DMS.DescribeConnections' every 5 seconds until a successful state is reached. An error is returned after 60 failed checks.
newTestConnectionSucceeds :: Core.Wait DescribeConnections
newTestConnectionSucceeds =
  Core.Wait
    { Core._waitName =
        "TestConnectionSucceeds",
      Core._waitAttempts = 60,
      Core._waitDelay = 5,
      Core._waitAcceptors =
        [ Core.matchAll
            "successful"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeConnectionsResponse_connections
                        Core.. Lens._Just
                    )
                )
                Core.. connection_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "failed"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeConnectionsResponse_connections
                        Core.. Lens._Just
                    )
                )
                Core.. connection_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.DMS.DescribeReplicationTasks' every 15 seconds until a successful state is reached. An error is returned after 60 failed checks.
newReplicationTaskStopped :: Core.Wait DescribeReplicationTasks
newReplicationTaskStopped =
  Core.Wait
    { Core._waitName =
        "ReplicationTaskStopped",
      Core._waitAttempts = 60,
      Core._waitDelay = 15,
      Core._waitAcceptors =
        [ Core.matchAll
            "stopped"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Core.. Lens._Just
                    )
                )
                Core.. replicationTask_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "ready"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Core.. Lens._Just
                    )
                )
                Core.. replicationTask_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "creating"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Core.. Lens._Just
                    )
                )
                Core.. replicationTask_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "starting"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Core.. Lens._Just
                    )
                )
                Core.. replicationTask_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "running"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Core.. Lens._Just
                    )
                )
                Core.. replicationTask_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "failed"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Core.. Lens._Just
                    )
                )
                Core.. replicationTask_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "modifying"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Core.. Lens._Just
                    )
                )
                Core.. replicationTask_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "testing"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Core.. Lens._Just
                    )
                )
                Core.. replicationTask_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "deleting"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Core.. Lens._Just
                    )
                )
                Core.. replicationTask_status
                Core.. Lens._Just
                Core.. Lens.to Core.toTextCI
            )
        ]
    }

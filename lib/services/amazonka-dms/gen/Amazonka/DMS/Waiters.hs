{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DMS.Waiters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Waiters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.DescribeConnections
import Amazonka.DMS.DescribeEndpoints
import Amazonka.DMS.DescribeReplicationInstances
import Amazonka.DMS.DescribeReplicationTasks
import Amazonka.DMS.Lens
import Amazonka.DMS.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Polls 'Amazonka.DMS.DescribeEndpoints' every 5 seconds until a successful state is reached. An error is returned after 60 failed checks.
newEndpointDeleted :: Core.Wait DescribeEndpoints
newEndpointDeleted =
  Core.Wait
    { Core.name = "EndpointDeleted",
      Core.attempts = 60,
      Core.delay = 5,
      Core.acceptors =
        [ Core.matchError
            "ResourceNotFoundFault"
            Core.AcceptSuccess,
          Core.matchAny
            "active"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeEndpointsResponse_endpoints
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. endpoint_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "creating"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeEndpointsResponse_endpoints
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. endpoint_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.DMS.DescribeReplicationInstances' every 60 seconds until a successful state is reached. An error is returned after 60 failed checks.
newReplicationInstanceAvailable :: Core.Wait DescribeReplicationInstances
newReplicationInstanceAvailable =
  Core.Wait
    { Core.name =
        "ReplicationInstanceAvailable",
      Core.attempts = 60,
      Core.delay = 60,
      Core.acceptors =
        [ Core.matchAll
            "available"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationInstancesResponse_replicationInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationInstance_replicationInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "deleting"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationInstancesResponse_replicationInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationInstance_replicationInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "incompatible-credentials"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationInstancesResponse_replicationInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationInstance_replicationInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "incompatible-network"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationInstancesResponse_replicationInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationInstance_replicationInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "inaccessible-encryption-credentials"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationInstancesResponse_replicationInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationInstance_replicationInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.DMS.DescribeReplicationInstances' every 15 seconds until a successful state is reached. An error is returned after 60 failed checks.
newReplicationInstanceDeleted :: Core.Wait DescribeReplicationInstances
newReplicationInstanceDeleted =
  Core.Wait
    { Core.name = "ReplicationInstanceDeleted",
      Core.attempts = 60,
      Core.delay = 15,
      Core.acceptors =
        [ Core.matchAny
            "available"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationInstancesResponse_replicationInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationInstance_replicationInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchError
            "ResourceNotFoundFault"
            Core.AcceptSuccess
        ]
    }

-- | Polls 'Amazonka.DMS.DescribeReplicationTasks' every 15 seconds until a successful state is reached. An error is returned after 60 failed checks.
newReplicationTaskDeleted :: Core.Wait DescribeReplicationTasks
newReplicationTaskDeleted =
  Core.Wait
    { Core.name = "ReplicationTaskDeleted",
      Core.attempts = 60,
      Core.delay = 15,
      Core.acceptors =
        [ Core.matchAny
            "ready"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "creating"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "stopped"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "running"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "failed"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchError
            "ResourceNotFoundFault"
            Core.AcceptSuccess
        ]
    }

-- | Polls 'Amazonka.DMS.DescribeReplicationTasks' every 15 seconds until a successful state is reached. An error is returned after 60 failed checks.
newReplicationTaskReady :: Core.Wait DescribeReplicationTasks
newReplicationTaskReady =
  Core.Wait
    { Core.name = "ReplicationTaskReady",
      Core.attempts = 60,
      Core.delay = 15,
      Core.acceptors =
        [ Core.matchAll
            "ready"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "starting"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "running"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "stopping"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "stopped"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "failed"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "modifying"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "testing"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "deleting"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.DMS.DescribeReplicationTasks' every 15 seconds until a successful state is reached. An error is returned after 60 failed checks.
newReplicationTaskRunning :: Core.Wait DescribeReplicationTasks
newReplicationTaskRunning =
  Core.Wait
    { Core.name = "ReplicationTaskRunning",
      Core.attempts = 60,
      Core.delay = 15,
      Core.acceptors =
        [ Core.matchAll
            "running"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "ready"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "creating"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "stopping"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "stopped"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "failed"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "modifying"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "testing"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "deleting"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.DMS.DescribeReplicationTasks' every 15 seconds until a successful state is reached. An error is returned after 60 failed checks.
newReplicationTaskStopped :: Core.Wait DescribeReplicationTasks
newReplicationTaskStopped =
  Core.Wait
    { Core.name = "ReplicationTaskStopped",
      Core.attempts = 60,
      Core.delay = 15,
      Core.acceptors =
        [ Core.matchAll
            "stopped"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "ready"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "creating"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "starting"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "failed"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "modifying"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "testing"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "deleting"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeReplicationTasksResponse_replicationTasks
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. replicationTask_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.DMS.DescribeConnections' every 5 seconds until a successful state is reached. An error is returned after 60 failed checks.
newTestConnectionSucceeds :: Core.Wait DescribeConnections
newTestConnectionSucceeds =
  Core.Wait
    { Core.name = "TestConnectionSucceeds",
      Core.attempts = 60,
      Core.delay = 5,
      Core.acceptors =
        [ Core.matchAll
            "successful"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeConnectionsResponse_connections
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. connection_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "failed"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeConnectionsResponse_connections
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. connection_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

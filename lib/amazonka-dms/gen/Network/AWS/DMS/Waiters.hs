{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Waiters where

import Network.AWS.DMS.DescribeConnections
import Network.AWS.DMS.DescribeEndpoints
import Network.AWS.DMS.DescribeReplicationInstances
import Network.AWS.DMS.DescribeReplicationTasks
import Network.AWS.DMS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Waiter

-- | Polls 'Network.AWS.DMS.DescribeReplicationInstances' every 60 seconds until a successful state is reached. An error is pureed after 60 failed checks.
replicationInstanceAvailable :: Wait DescribeReplicationInstances
replicationInstanceAvailable =
  Wait
    { _waitName = "ReplicationInstanceAvailable",
      _waitAttempts = 60,
      _waitDelay = 60,
      _waitAcceptors =
        [ matchAll
            "available"
            AcceptSuccess
            ( folding (concatOf drisrsReplicationInstances)
                . riReplicationInstanceStatus
                . _Just
                . to toTextCI
            ),
          matchAny
            "deleting"
            AcceptFailure
            ( folding (concatOf drisrsReplicationInstances)
                . riReplicationInstanceStatus
                . _Just
                . to toTextCI
            ),
          matchAny
            "incompatible-credentials"
            AcceptFailure
            ( folding (concatOf drisrsReplicationInstances)
                . riReplicationInstanceStatus
                . _Just
                . to toTextCI
            ),
          matchAny
            "incompatible-network"
            AcceptFailure
            ( folding (concatOf drisrsReplicationInstances)
                . riReplicationInstanceStatus
                . _Just
                . to toTextCI
            ),
          matchAny
            "inaccessible-encryption-credentials"
            AcceptFailure
            ( folding (concatOf drisrsReplicationInstances)
                . riReplicationInstanceStatus
                . _Just
                . to toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.DMS.DescribeReplicationTasks' every 15 seconds until a successful state is reached. An error is pureed after 60 failed checks.
replicationTaskDeleted :: Wait DescribeReplicationTasks
replicationTaskDeleted =
  Wait
    { _waitName = "ReplicationTaskDeleted",
      _waitAttempts = 60,
      _waitDelay = 15,
      _waitAcceptors =
        [ matchAny
            "ready"
            AcceptFailure
            ( folding (concatOf drtsrsReplicationTasks) . repStatus . _Just
                . to toTextCI
            ),
          matchAny
            "creating"
            AcceptFailure
            ( folding (concatOf drtsrsReplicationTasks) . repStatus . _Just
                . to toTextCI
            ),
          matchAny
            "stopped"
            AcceptFailure
            ( folding (concatOf drtsrsReplicationTasks) . repStatus . _Just
                . to toTextCI
            ),
          matchAny
            "running"
            AcceptFailure
            ( folding (concatOf drtsrsReplicationTasks) . repStatus . _Just
                . to toTextCI
            ),
          matchAny
            "failed"
            AcceptFailure
            ( folding (concatOf drtsrsReplicationTasks) . repStatus . _Just
                . to toTextCI
            ),
          matchError "ResourceNotFoundFault" AcceptSuccess
        ]
    }

-- | Polls 'Network.AWS.DMS.DescribeReplicationTasks' every 15 seconds until a successful state is reached. An error is pureed after 60 failed checks.
replicationTaskReady :: Wait DescribeReplicationTasks
replicationTaskReady =
  Wait
    { _waitName = "ReplicationTaskReady",
      _waitAttempts = 60,
      _waitDelay = 15,
      _waitAcceptors =
        [ matchAll
            "ready"
            AcceptSuccess
            ( folding (concatOf drtsrsReplicationTasks) . repStatus . _Just
                . to toTextCI
            ),
          matchAny
            "starting"
            AcceptFailure
            ( folding (concatOf drtsrsReplicationTasks) . repStatus . _Just
                . to toTextCI
            ),
          matchAny
            "running"
            AcceptFailure
            ( folding (concatOf drtsrsReplicationTasks) . repStatus . _Just
                . to toTextCI
            ),
          matchAny
            "stopping"
            AcceptFailure
            ( folding (concatOf drtsrsReplicationTasks) . repStatus . _Just
                . to toTextCI
            ),
          matchAny
            "stopped"
            AcceptFailure
            ( folding (concatOf drtsrsReplicationTasks) . repStatus . _Just
                . to toTextCI
            ),
          matchAny
            "failed"
            AcceptFailure
            ( folding (concatOf drtsrsReplicationTasks) . repStatus . _Just
                . to toTextCI
            ),
          matchAny
            "modifying"
            AcceptFailure
            ( folding (concatOf drtsrsReplicationTasks) . repStatus . _Just
                . to toTextCI
            ),
          matchAny
            "testing"
            AcceptFailure
            ( folding (concatOf drtsrsReplicationTasks) . repStatus . _Just
                . to toTextCI
            ),
          matchAny
            "deleting"
            AcceptFailure
            ( folding (concatOf drtsrsReplicationTasks) . repStatus . _Just
                . to toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.DMS.DescribeReplicationInstances' every 15 seconds until a successful state is reached. An error is pureed after 60 failed checks.
replicationInstanceDeleted :: Wait DescribeReplicationInstances
replicationInstanceDeleted =
  Wait
    { _waitName = "ReplicationInstanceDeleted",
      _waitAttempts = 60,
      _waitDelay = 15,
      _waitAcceptors =
        [ matchAny
            "available"
            AcceptFailure
            ( folding (concatOf drisrsReplicationInstances)
                . riReplicationInstanceStatus
                . _Just
                . to toTextCI
            ),
          matchError "ResourceNotFoundFault" AcceptSuccess
        ]
    }

-- | Polls 'Network.AWS.DMS.DescribeEndpoints' every 5 seconds until a successful state is reached. An error is pureed after 60 failed checks.
endpointDeleted :: Wait DescribeEndpoints
endpointDeleted =
  Wait
    { _waitName = "EndpointDeleted",
      _waitAttempts = 60,
      _waitDelay = 5,
      _waitAcceptors =
        [ matchError "ResourceNotFoundFault" AcceptSuccess,
          matchAny
            "active"
            AcceptFailure
            (folding (concatOf dersEndpoints) . eStatus . _Just . to toTextCI),
          matchAny
            "creating"
            AcceptFailure
            ( folding (concatOf dersEndpoints) . eStatus . _Just
                . to toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.DMS.DescribeReplicationTasks' every 15 seconds until a successful state is reached. An error is pureed after 60 failed checks.
replicationTaskStopped :: Wait DescribeReplicationTasks
replicationTaskStopped =
  Wait
    { _waitName = "ReplicationTaskStopped",
      _waitAttempts = 60,
      _waitDelay = 15,
      _waitAcceptors =
        [ matchAll
            "stopped"
            AcceptSuccess
            ( folding (concatOf drtsrsReplicationTasks) . repStatus . _Just
                . to toTextCI
            ),
          matchAny
            "ready"
            AcceptFailure
            ( folding (concatOf drtsrsReplicationTasks) . repStatus . _Just
                . to toTextCI
            ),
          matchAny
            "creating"
            AcceptFailure
            ( folding (concatOf drtsrsReplicationTasks) . repStatus . _Just
                . to toTextCI
            ),
          matchAny
            "starting"
            AcceptFailure
            ( folding (concatOf drtsrsReplicationTasks) . repStatus . _Just
                . to toTextCI
            ),
          matchAny
            "running"
            AcceptFailure
            ( folding (concatOf drtsrsReplicationTasks) . repStatus . _Just
                . to toTextCI
            ),
          matchAny
            "failed"
            AcceptFailure
            ( folding (concatOf drtsrsReplicationTasks) . repStatus . _Just
                . to toTextCI
            ),
          matchAny
            "modifying"
            AcceptFailure
            ( folding (concatOf drtsrsReplicationTasks) . repStatus . _Just
                . to toTextCI
            ),
          matchAny
            "testing"
            AcceptFailure
            ( folding (concatOf drtsrsReplicationTasks) . repStatus . _Just
                . to toTextCI
            ),
          matchAny
            "deleting"
            AcceptFailure
            ( folding (concatOf drtsrsReplicationTasks) . repStatus . _Just
                . to toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.DMS.DescribeReplicationTasks' every 15 seconds until a successful state is reached. An error is pureed after 60 failed checks.
replicationTaskRunning :: Wait DescribeReplicationTasks
replicationTaskRunning =
  Wait
    { _waitName = "ReplicationTaskRunning",
      _waitAttempts = 60,
      _waitDelay = 15,
      _waitAcceptors =
        [ matchAll
            "running"
            AcceptSuccess
            ( folding (concatOf drtsrsReplicationTasks) . repStatus . _Just
                . to toTextCI
            ),
          matchAny
            "ready"
            AcceptFailure
            ( folding (concatOf drtsrsReplicationTasks) . repStatus . _Just
                . to toTextCI
            ),
          matchAny
            "creating"
            AcceptFailure
            ( folding (concatOf drtsrsReplicationTasks) . repStatus . _Just
                . to toTextCI
            ),
          matchAny
            "stopping"
            AcceptFailure
            ( folding (concatOf drtsrsReplicationTasks) . repStatus . _Just
                . to toTextCI
            ),
          matchAny
            "stopped"
            AcceptFailure
            ( folding (concatOf drtsrsReplicationTasks) . repStatus . _Just
                . to toTextCI
            ),
          matchAny
            "failed"
            AcceptFailure
            ( folding (concatOf drtsrsReplicationTasks) . repStatus . _Just
                . to toTextCI
            ),
          matchAny
            "modifying"
            AcceptFailure
            ( folding (concatOf drtsrsReplicationTasks) . repStatus . _Just
                . to toTextCI
            ),
          matchAny
            "testing"
            AcceptFailure
            ( folding (concatOf drtsrsReplicationTasks) . repStatus . _Just
                . to toTextCI
            ),
          matchAny
            "deleting"
            AcceptFailure
            ( folding (concatOf drtsrsReplicationTasks) . repStatus . _Just
                . to toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.DMS.DescribeConnections' every 5 seconds until a successful state is reached. An error is pureed after 60 failed checks.
testConnectionSucceeds :: Wait DescribeConnections
testConnectionSucceeds =
  Wait
    { _waitName = "TestConnectionSucceeds",
      _waitAttempts = 60,
      _waitDelay = 5,
      _waitAcceptors =
        [ matchAll
            "successful"
            AcceptSuccess
            ( folding (concatOf dcsrsConnections) . cStatus . _Just
                . to toTextCI
            ),
          matchAny
            "failed"
            AcceptFailure
            ( folding (concatOf dcsrsConnections) . cStatus . _Just
                . to toTextCI
            )
        ]
    }

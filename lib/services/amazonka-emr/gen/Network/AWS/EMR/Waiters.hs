{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Waiters where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.DescribeCluster
import Network.AWS.EMR.DescribeStep
import Network.AWS.EMR.Lens
import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Polls 'Network.AWS.EMR.DescribeStep' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newStepComplete :: Core.Wait DescribeStep
newStepComplete =
  Core.Wait
    { Core._waitName = "StepComplete",
      Core._waitAttempts = 60,
      Core._waitDelay = 30,
      Core._waitAcceptors =
        [ Core.matchAll
            "COMPLETED"
            Core.AcceptSuccess
            ( describeStepResponse_step Prelude.. Lens._Just
                Prelude.. step_status
                Prelude.. Lens._Just
                Prelude.. stepStatus_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( describeStepResponse_step Prelude.. Lens._Just
                Prelude.. step_status
                Prelude.. Lens._Just
                Prelude.. stepStatus_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "CANCELLED"
            Core.AcceptFailure
            ( describeStepResponse_step Prelude.. Lens._Just
                Prelude.. step_status
                Prelude.. Lens._Just
                Prelude.. stepStatus_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EMR.DescribeCluster' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newClusterTerminated :: Core.Wait DescribeCluster
newClusterTerminated =
  Core.Wait
    { Core._waitName = "ClusterTerminated",
      Core._waitAttempts = 60,
      Core._waitDelay = 30,
      Core._waitAcceptors =
        [ Core.matchAll
            "TERMINATED"
            Core.AcceptSuccess
            ( describeClusterResponse_cluster
                Prelude.. cluster_status
                Prelude.. clusterStatus_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "TERMINATED_WITH_ERRORS"
            Core.AcceptFailure
            ( describeClusterResponse_cluster
                Prelude.. cluster_status
                Prelude.. clusterStatus_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EMR.DescribeCluster' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newClusterRunning :: Core.Wait DescribeCluster
newClusterRunning =
  Core.Wait
    { Core._waitName = "ClusterRunning",
      Core._waitAttempts = 60,
      Core._waitDelay = 30,
      Core._waitAcceptors =
        [ Core.matchAll
            "RUNNING"
            Core.AcceptSuccess
            ( describeClusterResponse_cluster
                Prelude.. cluster_status
                Prelude.. clusterStatus_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "WAITING"
            Core.AcceptSuccess
            ( describeClusterResponse_cluster
                Prelude.. cluster_status
                Prelude.. clusterStatus_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "TERMINATING"
            Core.AcceptFailure
            ( describeClusterResponse_cluster
                Prelude.. cluster_status
                Prelude.. clusterStatus_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "TERMINATED"
            Core.AcceptFailure
            ( describeClusterResponse_cluster
                Prelude.. cluster_status
                Prelude.. clusterStatus_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "TERMINATED_WITH_ERRORS"
            Core.AcceptFailure
            ( describeClusterResponse_cluster
                Prelude.. cluster_status
                Prelude.. clusterStatus_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

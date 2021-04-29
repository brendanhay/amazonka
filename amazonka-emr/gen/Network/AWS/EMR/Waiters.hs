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

import Network.AWS.EMR.DescribeCluster
import Network.AWS.EMR.DescribeStep
import Network.AWS.EMR.Lens
import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.EMR.DescribeCluster' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newClusterTerminated :: Waiter.Wait DescribeCluster
newClusterTerminated =
  Waiter.Wait
    { Waiter._waitName = "ClusterTerminated",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 30,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "TERMINATED"
            Waiter.AcceptSuccess
            ( describeClusterResponse_cluster
                Prelude.. cluster_status
                Prelude.. clusterStatus_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "TERMINATED_WITH_ERRORS"
            Waiter.AcceptFailure
            ( describeClusterResponse_cluster
                Prelude.. cluster_status
                Prelude.. clusterStatus_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EMR.DescribeStep' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newStepComplete :: Waiter.Wait DescribeStep
newStepComplete =
  Waiter.Wait
    { Waiter._waitName = "StepComplete",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 30,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "COMPLETED"
            Waiter.AcceptSuccess
            ( describeStepResponse_step Prelude.. Lens._Just
                Prelude.. step_status
                Prelude.. Lens._Just
                Prelude.. stepStatus_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "FAILED"
            Waiter.AcceptFailure
            ( describeStepResponse_step Prelude.. Lens._Just
                Prelude.. step_status
                Prelude.. Lens._Just
                Prelude.. stepStatus_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "CANCELLED"
            Waiter.AcceptFailure
            ( describeStepResponse_step Prelude.. Lens._Just
                Prelude.. step_status
                Prelude.. Lens._Just
                Prelude.. stepStatus_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.EMR.DescribeCluster' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newClusterRunning :: Waiter.Wait DescribeCluster
newClusterRunning =
  Waiter.Wait
    { Waiter._waitName = "ClusterRunning",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 30,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "RUNNING"
            Waiter.AcceptSuccess
            ( describeClusterResponse_cluster
                Prelude.. cluster_status
                Prelude.. clusterStatus_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "WAITING"
            Waiter.AcceptSuccess
            ( describeClusterResponse_cluster
                Prelude.. cluster_status
                Prelude.. clusterStatus_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "TERMINATING"
            Waiter.AcceptFailure
            ( describeClusterResponse_cluster
                Prelude.. cluster_status
                Prelude.. clusterStatus_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "TERMINATED"
            Waiter.AcceptFailure
            ( describeClusterResponse_cluster
                Prelude.. cluster_status
                Prelude.. clusterStatus_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "TERMINATED_WITH_ERRORS"
            Waiter.AcceptFailure
            ( describeClusterResponse_cluster
                Prelude.. cluster_status
                Prelude.. clusterStatus_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

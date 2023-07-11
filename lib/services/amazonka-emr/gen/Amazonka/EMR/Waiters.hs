{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EMR.Waiters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Waiters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.DescribeCluster
import Amazonka.EMR.DescribeStep
import Amazonka.EMR.Lens
import Amazonka.EMR.Types
import qualified Amazonka.Prelude as Prelude

-- | Polls 'Amazonka.EMR.DescribeCluster' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newClusterRunning :: Core.Wait DescribeCluster
newClusterRunning =
  Core.Wait
    { Core.name = "ClusterRunning",
      Core.attempts = 60,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "RUNNING"
            Core.AcceptSuccess
            ( describeClusterResponse_cluster
                Prelude.. cluster_status
                Prelude.. clusterStatus_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "WAITING"
            Core.AcceptSuccess
            ( describeClusterResponse_cluster
                Prelude.. cluster_status
                Prelude.. clusterStatus_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "TERMINATING"
            Core.AcceptFailure
            ( describeClusterResponse_cluster
                Prelude.. cluster_status
                Prelude.. clusterStatus_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "TERMINATED"
            Core.AcceptFailure
            ( describeClusterResponse_cluster
                Prelude.. cluster_status
                Prelude.. clusterStatus_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "TERMINATED_WITH_ERRORS"
            Core.AcceptFailure
            ( describeClusterResponse_cluster
                Prelude.. cluster_status
                Prelude.. clusterStatus_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.EMR.DescribeCluster' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newClusterTerminated :: Core.Wait DescribeCluster
newClusterTerminated =
  Core.Wait
    { Core.name = "ClusterTerminated",
      Core.attempts = 60,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "TERMINATED"
            Core.AcceptSuccess
            ( describeClusterResponse_cluster
                Prelude.. cluster_status
                Prelude.. clusterStatus_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "TERMINATED_WITH_ERRORS"
            Core.AcceptFailure
            ( describeClusterResponse_cluster
                Prelude.. cluster_status
                Prelude.. clusterStatus_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.EMR.DescribeStep' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newStepComplete :: Core.Wait DescribeStep
newStepComplete =
  Core.Wait
    { Core.name = "StepComplete",
      Core.attempts = 60,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "COMPLETED"
            Core.AcceptSuccess
            ( describeStepResponse_step
                Prelude.. Lens._Just
                Prelude.. step_status
                Prelude.. Lens._Just
                Prelude.. stepStatus_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( describeStepResponse_step
                Prelude.. Lens._Just
                Prelude.. step_status
                Prelude.. Lens._Just
                Prelude.. stepStatus_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "CANCELLED"
            Core.AcceptFailure
            ( describeStepResponse_step
                Prelude.. Lens._Just
                Prelude.. step_status
                Prelude.. Lens._Just
                Prelude.. stepStatus_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

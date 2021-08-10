{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Waiters where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.DescribeApps
import Network.AWS.OpsWorks.DescribeDeployments
import Network.AWS.OpsWorks.DescribeInstances
import Network.AWS.OpsWorks.Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Prelude

-- | Polls 'Network.AWS.OpsWorks.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newInstanceTerminated :: Core.Wait DescribeInstances
newInstanceTerminated =
  Core.Wait
    { Core._waitName = "InstanceTerminated",
      Core._waitAttempts = 40,
      Core._waitDelay = 15,
      Core._waitAcceptors =
        [ Core.matchAll
            "terminated"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchError
            "ResourceNotFoundException"
            Core.AcceptSuccess,
          Core.matchAny
            "booting"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "online"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "pending"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "rebooting"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "requested"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "running_setup"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "setup_failed"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "start_failed"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.OpsWorks.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newInstanceRegistered :: Core.Wait DescribeInstances
newInstanceRegistered =
  Core.Wait
    { Core._waitName = "InstanceRegistered",
      Core._waitAttempts = 40,
      Core._waitDelay = 15,
      Core._waitAcceptors =
        [ Core.matchAll
            "registered"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "setup_failed"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "shutting_down"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "stopped"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "stopping"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "terminating"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "terminated"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "stop_failed"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.OpsWorks.DescribeApps' every 1 seconds until a successful state is reached. An error is returned after 40 failed checks.
newAppExists :: Core.Wait DescribeApps
newAppExists =
  Core.Wait
    { Core._waitName = "AppExists",
      Core._waitAttempts = 40,
      Core._waitDelay = 1,
      Core._waitAcceptors =
        [ Core.matchStatus 200 Core.AcceptSuccess,
          Core.matchStatus 400 Core.AcceptFailure
        ]
    }

-- | Polls 'Network.AWS.OpsWorks.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newInstanceOnline :: Core.Wait DescribeInstances
newInstanceOnline =
  Core.Wait
    { Core._waitName = "InstanceOnline",
      Core._waitAttempts = 40,
      Core._waitDelay = 15,
      Core._waitAcceptors =
        [ Core.matchAll
            "online"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "setup_failed"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "shutting_down"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "start_failed"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "stopped"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "stopping"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "terminating"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "terminated"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "stop_failed"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.OpsWorks.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newInstanceStopped :: Core.Wait DescribeInstances
newInstanceStopped =
  Core.Wait
    { Core._waitName = "InstanceStopped",
      Core._waitAttempts = 40,
      Core._waitDelay = 15,
      Core._waitAcceptors =
        [ Core.matchAll
            "stopped"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "booting"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "pending"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "rebooting"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "requested"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "running_setup"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "setup_failed"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "start_failed"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "stop_failed"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.OpsWorks.DescribeDeployments' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newDeploymentSuccessful :: Core.Wait DescribeDeployments
newDeploymentSuccessful =
  Core.Wait
    { Core._waitName = "DeploymentSuccessful",
      Core._waitAttempts = 40,
      Core._waitDelay = 15,
      Core._waitAcceptors =
        [ Core.matchAll
            "successful"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDeploymentsResponse_deployments
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. deployment_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "failed"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDeploymentsResponse_deployments
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. deployment_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

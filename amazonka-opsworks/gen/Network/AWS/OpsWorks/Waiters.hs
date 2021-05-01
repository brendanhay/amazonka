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

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.DescribeApps
import Network.AWS.OpsWorks.DescribeDeployments
import Network.AWS.OpsWorks.DescribeInstances
import Network.AWS.OpsWorks.Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.OpsWorks.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newInstanceTerminated :: Waiter.Wait DescribeInstances
newInstanceTerminated =
  Waiter.Wait
    { Waiter._waitName =
        "InstanceTerminated",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "terminated"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchError
            "ResourceNotFoundException"
            Waiter.AcceptSuccess,
          Waiter.matchAny
            "booting"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "online"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "pending"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "rebooting"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "requested"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "running_setup"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "setup_failed"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "start_failed"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.OpsWorks.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newInstanceRegistered :: Waiter.Wait DescribeInstances
newInstanceRegistered =
  Waiter.Wait
    { Waiter._waitName =
        "InstanceRegistered",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "registered"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "setup_failed"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "shutting_down"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "stopped"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "stopping"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "terminating"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "terminated"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "stop_failed"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.OpsWorks.DescribeApps' every 1 seconds until a successful state is reached. An error is returned after 40 failed checks.
newAppExists :: Waiter.Wait DescribeApps
newAppExists =
  Waiter.Wait
    { Waiter._waitName = "AppExists",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 1,
      Waiter._waitAcceptors =
        [ Waiter.matchStatus 200 Waiter.AcceptSuccess,
          Waiter.matchStatus 400 Waiter.AcceptFailure
        ]
    }

-- | Polls 'Network.AWS.OpsWorks.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newInstanceOnline :: Waiter.Wait DescribeInstances
newInstanceOnline =
  Waiter.Wait
    { Waiter._waitName = "InstanceOnline",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "online"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "setup_failed"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "shutting_down"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "start_failed"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "stopped"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "stopping"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "terminating"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "terminated"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "stop_failed"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.OpsWorks.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newInstanceStopped :: Waiter.Wait DescribeInstances
newInstanceStopped =
  Waiter.Wait
    { Waiter._waitName = "InstanceStopped",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "stopped"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "booting"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "pending"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "rebooting"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "requested"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "running_setup"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "setup_failed"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "start_failed"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "stop_failed"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstancesResponse_instances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instance_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.OpsWorks.DescribeDeployments' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newDeploymentSuccessful :: Waiter.Wait DescribeDeployments
newDeploymentSuccessful =
  Waiter.Wait
    { Waiter._waitName =
        "DeploymentSuccessful",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "successful"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDeploymentsResponse_deployments
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. deployment_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "failed"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDeploymentsResponse_deployments
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. deployment_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

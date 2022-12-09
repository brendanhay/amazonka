{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.OpsWorks.Waiters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Waiters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.DescribeApps
import Amazonka.OpsWorks.DescribeDeployments
import Amazonka.OpsWorks.DescribeInstances
import Amazonka.OpsWorks.Lens
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude

-- | Polls 'Amazonka.OpsWorks.DescribeApps' every 1 seconds until a successful state is reached. An error is returned after 40 failed checks.
newAppExists :: Core.Wait DescribeApps
newAppExists =
  Core.Wait
    { Core.name = "AppExists",
      Core.attempts = 40,
      Core.delay = 1,
      Core.acceptors =
        [ Core.matchStatus 200 Core.AcceptSuccess,
          Core.matchStatus 400 Core.AcceptFailure
        ]
    }

-- | Polls 'Amazonka.OpsWorks.DescribeDeployments' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newDeploymentSuccessful :: Core.Wait DescribeDeployments
newDeploymentSuccessful =
  Core.Wait
    { Core.name = "DeploymentSuccessful",
      Core.attempts = 40,
      Core.delay = 15,
      Core.acceptors =
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.OpsWorks.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newInstanceOnline :: Core.Wait DescribeInstances
newInstanceOnline =
  Core.Wait
    { Core.name = "InstanceOnline",
      Core.attempts = 40,
      Core.delay = 15,
      Core.acceptors =
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.OpsWorks.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newInstanceRegistered :: Core.Wait DescribeInstances
newInstanceRegistered =
  Core.Wait
    { Core.name = "InstanceRegistered",
      Core.attempts = 40,
      Core.delay = 15,
      Core.acceptors =
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.OpsWorks.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newInstanceStopped :: Core.Wait DescribeInstances
newInstanceStopped =
  Core.Wait
    { Core.name = "InstanceStopped",
      Core.attempts = 40,
      Core.delay = 15,
      Core.acceptors =
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.OpsWorks.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newInstanceTerminated :: Core.Wait DescribeInstances
newInstanceTerminated =
  Core.Wait
    { Core.name = "InstanceTerminated",
      Core.attempts = 40,
      Core.delay = 15,
      Core.acceptors =
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
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
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

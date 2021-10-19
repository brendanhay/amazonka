{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Waiters where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.DescribeServices
import Network.AWS.ECS.DescribeTasks
import Network.AWS.ECS.Lens
import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Polls 'Network.AWS.ECS.DescribeServices' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newServicesInactive :: Core.Wait DescribeServices
newServicesInactive =
  Core.Wait
    { Core._waitName = "ServicesInactive",
      Core._waitAttempts = 40,
      Core._waitDelay = 15,
      Core._waitAcceptors =
        [ Core.matchAny
            "MISSING"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeServicesResponse_failures
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. failure_reason
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "INACTIVE"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeServicesResponse_services
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. containerService_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.ECS.DescribeTasks' every 6 seconds until a successful state is reached. An error is returned after 100 failed checks.
newTasksRunning :: Core.Wait DescribeTasks
newTasksRunning =
  Core.Wait
    { Core._waitName = "TasksRunning",
      Core._waitAttempts = 100,
      Core._waitDelay = 6,
      Core._waitAcceptors =
        [ Core.matchAny
            "STOPPED"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeTasksResponse_tasks Prelude.. Lens._Just)
                )
                Prelude.. task_lastStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "MISSING"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeTasksResponse_failures
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. failure_reason
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "RUNNING"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (describeTasksResponse_tasks Prelude.. Lens._Just)
                )
                Prelude.. task_lastStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.ECS.DescribeTasks' every 6 seconds until a successful state is reached. An error is returned after 100 failed checks.
newTasksStopped :: Core.Wait DescribeTasks
newTasksStopped =
  Core.Wait
    { Core._waitName = "TasksStopped",
      Core._waitAttempts = 100,
      Core._waitDelay = 6,
      Core._waitAcceptors =
        [ Core.matchAll
            "STOPPED"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (describeTasksResponse_tasks Prelude.. Lens._Just)
                )
                Prelude.. task_lastStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

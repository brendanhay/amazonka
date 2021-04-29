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

import Network.AWS.ECS.DescribeServices
import Network.AWS.ECS.DescribeTasks
import Network.AWS.ECS.Lens
import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.ECS.DescribeTasks' every 6 seconds until a successful state is reached. An error is returned after 100 failed checks.
newTasksRunning :: Waiter.Wait DescribeTasks
newTasksRunning =
  Waiter.Wait
    { Waiter._waitName = "TasksRunning",
      Waiter._waitAttempts = 100,
      Waiter._waitDelay = 6,
      Waiter._waitAcceptors =
        [ Waiter.matchAny
            "STOPPED"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeTasksResponse_tasks Prelude.. Lens._Just)
                )
                Prelude.. task_lastStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "MISSING"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeTasksResponse_failures
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. failure_reason
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "RUNNING"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (describeTasksResponse_tasks Prelude.. Lens._Just)
                )
                Prelude.. task_lastStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.ECS.DescribeTasks' every 6 seconds until a successful state is reached. An error is returned after 100 failed checks.
newTasksStopped :: Waiter.Wait DescribeTasks
newTasksStopped =
  Waiter.Wait
    { Waiter._waitName = "TasksStopped",
      Waiter._waitAttempts = 100,
      Waiter._waitDelay = 6,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "STOPPED"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (describeTasksResponse_tasks Prelude.. Lens._Just)
                )
                Prelude.. task_lastStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.ECS.DescribeServices' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newServicesInactive :: Waiter.Wait DescribeServices
newServicesInactive =
  Waiter.Wait
    { Waiter._waitName = "ServicesInactive",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAny
            "MISSING"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeServicesResponse_failures
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. failure_reason
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "INACTIVE"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeServicesResponse_services
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. containerService_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

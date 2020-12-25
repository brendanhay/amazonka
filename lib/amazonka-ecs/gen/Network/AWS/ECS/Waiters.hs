{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Waiters
  ( -- * ServicesInactive
    mkServicesInactive,

    -- * TasksRunning
    mkTasksRunning,

    -- * TasksStopped
    mkTasksStopped,
  )
where

import Network.AWS.ECS.DescribeServices
import Network.AWS.ECS.DescribeTasks
import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.ECS.DescribeServices' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkServicesInactive :: Waiter.Wait DescribeServices
mkServicesInactive =
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
                    ( Lens.field @"failures" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"reason"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "INACTIVE"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"services" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"status"
                Core.. Lens._Just
            )
        ]
    }

-- | Polls 'Network.AWS.ECS.DescribeTasks' every 6 seconds until a successful state is reached. An error is returned after 100 failed checks.
mkTasksRunning :: Waiter.Wait DescribeTasks
mkTasksRunning =
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
                    (Lens.field @"tasks" Core.. Lens._Just Core.. Lens.to Core.toList)
                )
                Core.. Lens.field @"lastStatus"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "MISSING"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"failures" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"reason"
                Core.. Lens._Just
            ),
          Waiter.matchAll
            "RUNNING"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (Lens.field @"tasks" Core.. Lens._Just Core.. Lens.to Core.toList)
                )
                Core.. Lens.field @"lastStatus"
                Core.. Lens._Just
            )
        ]
    }

-- | Polls 'Network.AWS.ECS.DescribeTasks' every 6 seconds until a successful state is reached. An error is returned after 100 failed checks.
mkTasksStopped :: Waiter.Wait DescribeTasks
mkTasksStopped =
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
                    (Lens.field @"tasks" Core.. Lens._Just Core.. Lens.to Core.toList)
                )
                Core.. Lens.field @"lastStatus"
                Core.. Lens._Just
            )
        ]
    }

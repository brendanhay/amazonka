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
import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Waiter as Wait

-- | Polls 'Network.AWS.ECS.DescribeServices' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkServicesInactive :: Wait.Wait DescribeServices
mkServicesInactive =
  Wait.Wait
    { Wait._waitName = "ServicesInactive",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAny
            "MISSING"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dssrsFailures Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. fReason
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "INACTIVE"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (dssrsServices Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. csStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.ECS.DescribeTasks' every 6 seconds until a successful state is reached. An error is returned after 100 failed checks.
mkTasksRunning :: Wait.Wait DescribeTasks
mkTasksRunning =
  Wait.Wait
    { Wait._waitName = "TasksRunning",
      Wait._waitAttempts = 100,
      Wait._waitDelay = 6,
      Wait._waitAcceptors =
        [ Wait.matchAny
            "STOPPED"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dtrsTasks Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. tLastStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "MISSING"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dtrsFailures Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. fReason
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAll
            "RUNNING"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (dtrsTasks Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. tLastStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.ECS.DescribeTasks' every 6 seconds until a successful state is reached. An error is returned after 100 failed checks.
mkTasksStopped :: Wait.Wait DescribeTasks
mkTasksStopped =
  Wait.Wait
    { Wait._waitName = "TasksStopped",
      Wait._waitAttempts = 100,
      Wait._waitDelay = 6,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "STOPPED"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (dtrsTasks Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. tLastStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

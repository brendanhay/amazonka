{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Waiters
  ( -- * AnyInstanceInService
    mkAnyInstanceInService,

    -- * InstanceDeregistered
    mkInstanceDeregistered,

    -- * InstanceInService
    mkInstanceInService,
  )
where

import Network.AWS.ELB.DescribeInstanceHealth
import qualified Network.AWS.ELB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.ELB.DescribeInstanceHealth' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkAnyInstanceInService :: Waiter.Wait DescribeInstanceHealth
mkAnyInstanceInService =
  Waiter.Wait
    { Waiter._waitName = "AnyInstanceInService",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAny
            "InService"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"instanceStates" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"state"
                Core.. Lens._Just
            )
        ]
    }

-- | Polls 'Network.AWS.ELB.DescribeInstanceHealth' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkInstanceDeregistered :: Waiter.Wait DescribeInstanceHealth
mkInstanceDeregistered =
  Waiter.Wait
    { Waiter._waitName = "InstanceDeregistered",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "OutOfService"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"instanceStates" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"state"
                Core.. Lens._Just
            ),
          Waiter.matchError "InvalidInstance" Waiter.AcceptSuccess
        ]
    }

-- | Polls 'Network.AWS.ELB.DescribeInstanceHealth' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkInstanceInService :: Waiter.Wait DescribeInstanceHealth
mkInstanceInService =
  Waiter.Wait
    { Waiter._waitName = "InstanceInService",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "InService"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"instanceStates" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"state"
                Core.. Lens._Just
            ),
          Waiter.matchError "InvalidInstance" Waiter.AcceptRetry
        ]
    }

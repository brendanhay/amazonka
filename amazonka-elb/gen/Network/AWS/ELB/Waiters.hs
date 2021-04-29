{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Waiters where

import Network.AWS.ELB.DescribeInstanceHealth
import Network.AWS.ELB.Lens
import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.ELB.DescribeInstanceHealth' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newInstanceDeregistered :: Waiter.Wait DescribeInstanceHealth
newInstanceDeregistered =
  Waiter.Wait
    { Waiter._waitName =
        "InstanceDeregistered",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "OutOfService"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstanceHealthResponse_instanceStates
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instanceState_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchError
            "InvalidInstance"
            Waiter.AcceptSuccess
        ]
    }

-- | Polls 'Network.AWS.ELB.DescribeInstanceHealth' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newInstanceInService :: Waiter.Wait DescribeInstanceHealth
newInstanceInService =
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
                    ( describeInstanceHealthResponse_instanceStates
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instanceState_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchError
            "InvalidInstance"
            Waiter.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.ELB.DescribeInstanceHealth' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newAnyInstanceInService :: Waiter.Wait DescribeInstanceHealth
newAnyInstanceInService =
  Waiter.Wait
    { Waiter._waitName =
        "AnyInstanceInService",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAny
            "InService"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstanceHealthResponse_instanceStates
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instanceState_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

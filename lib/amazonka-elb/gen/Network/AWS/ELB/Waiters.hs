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
import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Waiter as Wait

-- | Polls 'Network.AWS.ELB.DescribeInstanceHealth' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkAnyInstanceInService :: Wait.Wait DescribeInstanceHealth
mkAnyInstanceInService =
  Wait.Wait
    { Wait._waitName = "AnyInstanceInService",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAny
            "InService"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (dihrsInstanceStates Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. isState
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.ELB.DescribeInstanceHealth' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkInstanceDeregistered :: Wait.Wait DescribeInstanceHealth
mkInstanceDeregistered =
  Wait.Wait
    { Wait._waitName = "InstanceDeregistered",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "OutOfService"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (dihrsInstanceStates Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. isState
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchError "InvalidInstance" Wait.AcceptSuccess
        ]
    }

-- | Polls 'Network.AWS.ELB.DescribeInstanceHealth' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkInstanceInService :: Wait.Wait DescribeInstanceHealth
mkInstanceInService =
  Wait.Wait
    { Wait._waitName = "InstanceInService",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "InService"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (dihrsInstanceStates Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. isState
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchError "InvalidInstance" Wait.AcceptRetry
        ]
    }

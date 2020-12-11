{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Waiters
  ( -- * LoadBalancersDeleted
    mkLoadBalancersDeleted,

    -- * TargetDeregistered
    mkTargetDeregistered,

    -- * LoadBalancerAvailable
    mkLoadBalancerAvailable,

    -- * TargetInService
    mkTargetInService,

    -- * LoadBalancerExists
    mkLoadBalancerExists,
  )
where

import Network.AWS.ELBv2.DescribeLoadBalancers
import Network.AWS.ELBv2.DescribeTargetHealth
import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Waiter as Wait

-- | Polls 'Network.AWS.ELBv2.DescribeLoadBalancers' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkLoadBalancersDeleted :: Wait.Wait DescribeLoadBalancers
mkLoadBalancersDeleted =
  Wait.Wait
    { Wait._waitName = "LoadBalancersDeleted",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "active"
            Wait.AcceptRetry
            ( Lens.folding
                ( Lens.concatOf
                    (dlbrsLoadBalancers Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. lbState
                Lude.. Lens._Just
                Lude.. lbsCode
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchError "LoadBalancerNotFound" Wait.AcceptSuccess
        ]
    }

-- | Polls 'Network.AWS.ELBv2.DescribeTargetHealth' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkTargetDeregistered :: Wait.Wait DescribeTargetHealth
mkTargetDeregistered =
  Wait.Wait
    { Wait._waitName = "TargetDeregistered",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchError "InvalidTarget" Wait.AcceptSuccess,
          Wait.matchAll
            "unused"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( dthrsTargetHealthDescriptions Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. thdTargetHealth
                Lude.. Lens._Just
                Lude.. thState
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.ELBv2.DescribeLoadBalancers' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkLoadBalancerAvailable :: Wait.Wait DescribeLoadBalancers
mkLoadBalancerAvailable =
  Wait.Wait
    { Wait._waitName = "LoadBalancerAvailable",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "active"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (dlbrsLoadBalancers Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. lbState
                Lude.. Lens._Just
                Lude.. lbsCode
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "provisioning"
            Wait.AcceptRetry
            ( Lens.folding
                ( Lens.concatOf
                    (dlbrsLoadBalancers Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. lbState
                Lude.. Lens._Just
                Lude.. lbsCode
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchError "LoadBalancerNotFound" Wait.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.ELBv2.DescribeTargetHealth' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkTargetInService :: Wait.Wait DescribeTargetHealth
mkTargetInService =
  Wait.Wait
    { Wait._waitName = "TargetInService",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "healthy"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( dthrsTargetHealthDescriptions Lude.. Lens._Just
                        Lude.. Lens.to Lude.toList
                    )
                )
                Lude.. thdTargetHealth
                Lude.. Lens._Just
                Lude.. thState
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchError "InvalidInstance" Wait.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.ELBv2.DescribeLoadBalancers' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkLoadBalancerExists :: Wait.Wait DescribeLoadBalancers
mkLoadBalancerExists =
  Wait.Wait
    { Wait._waitName = "LoadBalancerExists",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchStatus 200 Wait.AcceptSuccess,
          Wait.matchError "LoadBalancerNotFound" Wait.AcceptRetry
        ]
    }

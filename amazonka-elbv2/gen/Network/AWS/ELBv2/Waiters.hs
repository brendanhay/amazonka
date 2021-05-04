{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Waiters where

import Network.AWS.ELBv2.DescribeLoadBalancers
import Network.AWS.ELBv2.DescribeTargetHealth
import Network.AWS.ELBv2.Lens
import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.ELBv2.DescribeLoadBalancers' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newLoadBalancersDeleted :: Waiter.Wait DescribeLoadBalancers
newLoadBalancersDeleted =
  Waiter.Wait
    { Waiter._waitName =
        "LoadBalancersDeleted",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "active"
            Waiter.AcceptRetry
            ( Lens.folding
                ( Lens.concatOf
                    ( describeLoadBalancersResponse_loadBalancers
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. loadBalancer_state
                Prelude.. Lens._Just
                Prelude.. loadBalancerState_code
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchError
            "LoadBalancerNotFound"
            Waiter.AcceptSuccess
        ]
    }

-- | Polls 'Network.AWS.ELBv2.DescribeTargetHealth' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newTargetDeregistered :: Waiter.Wait DescribeTargetHealth
newTargetDeregistered =
  Waiter.Wait
    { Waiter._waitName =
        "TargetDeregistered",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchError
            "InvalidTarget"
            Waiter.AcceptSuccess,
          Waiter.matchAll
            "unused"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeTargetHealthResponse_targetHealthDescriptions
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. targetHealthDescription_targetHealth
                Prelude.. Lens._Just
                Prelude.. targetHealth_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.ELBv2.DescribeLoadBalancers' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newLoadBalancerAvailable :: Waiter.Wait DescribeLoadBalancers
newLoadBalancerAvailable =
  Waiter.Wait
    { Waiter._waitName =
        "LoadBalancerAvailable",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "active"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeLoadBalancersResponse_loadBalancers
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. loadBalancer_state
                Prelude.. Lens._Just
                Prelude.. loadBalancerState_code
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "provisioning"
            Waiter.AcceptRetry
            ( Lens.folding
                ( Lens.concatOf
                    ( describeLoadBalancersResponse_loadBalancers
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. loadBalancer_state
                Prelude.. Lens._Just
                Prelude.. loadBalancerState_code
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchError
            "LoadBalancerNotFound"
            Waiter.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.ELBv2.DescribeTargetHealth' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newTargetInService :: Waiter.Wait DescribeTargetHealth
newTargetInService =
  Waiter.Wait
    { Waiter._waitName = "TargetInService",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "healthy"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeTargetHealthResponse_targetHealthDescriptions
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. targetHealthDescription_targetHealth
                Prelude.. Lens._Just
                Prelude.. targetHealth_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchError
            "InvalidInstance"
            Waiter.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.ELBv2.DescribeLoadBalancers' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newLoadBalancerExists :: Waiter.Wait DescribeLoadBalancers
newLoadBalancerExists =
  Waiter.Wait
    { Waiter._waitName =
        "LoadBalancerExists",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchStatus 200 Waiter.AcceptSuccess,
          Waiter.matchError
            "LoadBalancerNotFound"
            Waiter.AcceptRetry
        ]
    }

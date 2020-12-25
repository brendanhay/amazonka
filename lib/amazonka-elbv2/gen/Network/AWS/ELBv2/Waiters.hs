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
import qualified Network.AWS.ELBv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.ELBv2.DescribeLoadBalancers' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkLoadBalancersDeleted :: Waiter.Wait DescribeLoadBalancers
mkLoadBalancersDeleted =
  Waiter.Wait
    { Waiter._waitName = "LoadBalancersDeleted",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "active"
            Waiter.AcceptRetry
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"loadBalancers" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"state"
                Core.. Lens._Just
                Core.. Lens.field @"code"
                Core.. Lens._Just
            ),
          Waiter.matchError "LoadBalancerNotFound" Waiter.AcceptSuccess
        ]
    }

-- | Polls 'Network.AWS.ELBv2.DescribeTargetHealth' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkTargetDeregistered :: Waiter.Wait DescribeTargetHealth
mkTargetDeregistered =
  Waiter.Wait
    { Waiter._waitName = "TargetDeregistered",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchError "InvalidTarget" Waiter.AcceptSuccess,
          Waiter.matchAll
            "unused"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"targetHealthDescriptions" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"targetHealth"
                Core.. Lens._Just
                Core.. Lens.field @"state"
                Core.. Lens._Just
            )
        ]
    }

-- | Polls 'Network.AWS.ELBv2.DescribeLoadBalancers' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkLoadBalancerAvailable :: Waiter.Wait DescribeLoadBalancers
mkLoadBalancerAvailable =
  Waiter.Wait
    { Waiter._waitName = "LoadBalancerAvailable",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "active"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"loadBalancers" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"state"
                Core.. Lens._Just
                Core.. Lens.field @"code"
                Core.. Lens._Just
            ),
          Waiter.matchAny
            "provisioning"
            Waiter.AcceptRetry
            ( Lens.folding
                ( Lens.concatOf
                    ( Lens.field @"loadBalancers" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"state"
                Core.. Lens._Just
                Core.. Lens.field @"code"
                Core.. Lens._Just
            ),
          Waiter.matchError "LoadBalancerNotFound" Waiter.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.ELBv2.DescribeTargetHealth' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkTargetInService :: Waiter.Wait DescribeTargetHealth
mkTargetInService =
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
                    ( Lens.field @"targetHealthDescriptions" Core.. Lens._Just
                        Core.. Lens.to Core.toList
                    )
                )
                Core.. Lens.field @"targetHealth"
                Core.. Lens._Just
                Core.. Lens.field @"state"
                Core.. Lens._Just
            ),
          Waiter.matchError "InvalidInstance" Waiter.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.ELBv2.DescribeLoadBalancers' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkLoadBalancerExists :: Waiter.Wait DescribeLoadBalancers
mkLoadBalancerExists =
  Waiter.Wait
    { Waiter._waitName = "LoadBalancerExists",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchStatus 200 Waiter.AcceptSuccess,
          Waiter.matchError "LoadBalancerNotFound" Waiter.AcceptRetry
        ]
    }

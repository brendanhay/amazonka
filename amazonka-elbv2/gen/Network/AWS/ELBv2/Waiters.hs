{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Waiters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELBv2.Waiters where

import Network.AWS.ELBv2.DescribeLoadBalancers
import Network.AWS.ELBv2.DescribeLoadBalancers
import Network.AWS.ELBv2.DescribeLoadBalancers
import Network.AWS.ELBv2.DescribeTargetHealth
import Network.AWS.ELBv2.DescribeTargetHealth
import Network.AWS.ELBv2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Waiter

-- | Polls 'Network.AWS.ELBv2.DescribeLoadBalancers' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
loadBalancersDeleted :: Wait DescribeLoadBalancers
loadBalancersDeleted =
  Wait
    { _waitName = "LoadBalancersDeleted"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors =
        [ matchAll
            "active"
            AcceptRetry
            (folding (concatOf dlbrsLoadBalancers) .
             lbState . _Just . lbsCode . _Just . to toTextCI)
        , matchError "LoadBalancerNotFound" AcceptSuccess
        ]
    }


-- | Polls 'Network.AWS.ELBv2.DescribeTargetHealth' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
targetDeregistered :: Wait DescribeTargetHealth
targetDeregistered =
  Wait
    { _waitName = "TargetDeregistered"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors =
        [ matchError "InvalidTarget" AcceptSuccess
        , matchAll
            "unused"
            AcceptSuccess
            (folding (concatOf dthrsTargetHealthDescriptions) .
             thdTargetHealth . _Just . thState . _Just . to toTextCI)
        ]
    }


-- | Polls 'Network.AWS.ELBv2.DescribeLoadBalancers' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
loadBalancerAvailable :: Wait DescribeLoadBalancers
loadBalancerAvailable =
  Wait
    { _waitName = "LoadBalancerAvailable"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors =
        [ matchAll
            "active"
            AcceptSuccess
            (folding (concatOf dlbrsLoadBalancers) .
             lbState . _Just . lbsCode . _Just . to toTextCI)
        , matchAny
            "provisioning"
            AcceptRetry
            (folding (concatOf dlbrsLoadBalancers) .
             lbState . _Just . lbsCode . _Just . to toTextCI)
        , matchError "LoadBalancerNotFound" AcceptRetry
        ]
    }


-- | Polls 'Network.AWS.ELBv2.DescribeTargetHealth' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
targetInService :: Wait DescribeTargetHealth
targetInService =
  Wait
    { _waitName = "TargetInService"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors =
        [ matchAll
            "healthy"
            AcceptSuccess
            (folding (concatOf dthrsTargetHealthDescriptions) .
             thdTargetHealth . _Just . thState . _Just . to toTextCI)
        , matchError "InvalidInstance" AcceptRetry
        ]
    }


-- | Polls 'Network.AWS.ELBv2.DescribeLoadBalancers' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
loadBalancerExists :: Wait DescribeLoadBalancers
loadBalancerExists =
  Wait
    { _waitName = "LoadBalancerExists"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors =
        [ matchStatus 200 AcceptSuccess
        , matchError "LoadBalancerNotFound" AcceptRetry
        ]
    }


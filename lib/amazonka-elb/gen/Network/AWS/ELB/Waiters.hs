{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Waiters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELB.Waiters where

import Network.AWS.ELB.DescribeInstanceHealth
import Network.AWS.ELB.DescribeInstanceHealth
import Network.AWS.ELB.DescribeInstanceHealth
import Network.AWS.ELB.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Waiter

-- | Polls 'Network.AWS.ELB.DescribeInstanceHealth' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
anyInstanceInService :: Wait DescribeInstanceHealth
anyInstanceInService =
  Wait
    { _waitName = "AnyInstanceInService"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors =
        [ matchAny
            "InService"
            AcceptSuccess
            (folding (concatOf dihrsInstanceStates) .
             isState . _Just . to toTextCI)
        ]
    }


-- | Polls 'Network.AWS.ELB.DescribeInstanceHealth' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
instanceDeregistered :: Wait DescribeInstanceHealth
instanceDeregistered =
  Wait
    { _waitName = "InstanceDeregistered"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors =
        [ matchAll
            "OutOfService"
            AcceptSuccess
            (folding (concatOf dihrsInstanceStates) .
             isState . _Just . to toTextCI)
        , matchError "InvalidInstance" AcceptSuccess
        ]
    }


-- | Polls 'Network.AWS.ELB.DescribeInstanceHealth' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
instanceInService :: Wait DescribeInstanceHealth
instanceInService =
  Wait
    { _waitName = "InstanceInService"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors =
        [ matchAll
            "InService"
            AcceptSuccess
            (folding (concatOf dihrsInstanceStates) .
             isState . _Just . to toTextCI)
        , matchError "InvalidInstance" AcceptRetry
        ]
    }


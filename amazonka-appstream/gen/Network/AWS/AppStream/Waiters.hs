{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Waiters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Waiters where

import Network.AWS.AppStream.DescribeFleets
import Network.AWS.AppStream.DescribeFleets
import Network.AWS.AppStream.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Waiter

-- | Polls 'Network.AWS.AppStream.DescribeFleets' every 30 seconds until a successful state is reached. An error is returned after 40 failed checks.
fleetStopped :: Wait DescribeFleets
fleetStopped =
  Wait
    { _waitName = "FleetStopped"
    , _waitAttempts = 40
    , _waitDelay = 30
    , _waitAcceptors =
        [ matchAll
            "INACTIVE"
            AcceptSuccess
            (folding (concatOf dfsrsFleets) . fState . to toTextCI)
        , matchAny
            "PENDING_ACTIVATE"
            AcceptFailure
            (folding (concatOf dfsrsFleets) . fState . to toTextCI)
        , matchAny
            "ACTIVE"
            AcceptFailure
            (folding (concatOf dfsrsFleets) . fState . to toTextCI)
        ]
    }


-- | Polls 'Network.AWS.AppStream.DescribeFleets' every 30 seconds until a successful state is reached. An error is returned after 40 failed checks.
fleetStarted :: Wait DescribeFleets
fleetStarted =
  Wait
    { _waitName = "FleetStarted"
    , _waitAttempts = 40
    , _waitDelay = 30
    , _waitAcceptors =
        [ matchAll
            "ACTIVE"
            AcceptSuccess
            (folding (concatOf dfsrsFleets) . fState . to toTextCI)
        , matchAny
            "PENDING_DEACTIVATE"
            AcceptFailure
            (folding (concatOf dfsrsFleets) . fState . to toTextCI)
        , matchAny
            "INACTIVE"
            AcceptFailure
            (folding (concatOf dfsrsFleets) . fState . to toTextCI)
        ]
    }


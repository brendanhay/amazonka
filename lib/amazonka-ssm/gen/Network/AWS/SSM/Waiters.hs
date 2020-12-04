{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Waiters where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.GetCommandInvocation
import Network.AWS.SSM.Types
import Network.AWS.Waiter

-- | Polls 'Network.AWS.SSM.GetCommandInvocation' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
commandExecuted :: Wait GetCommandInvocation
commandExecuted =
  Wait
    { _waitName = "CommandExecuted",
      _waitAttempts = 20,
      _waitDelay = 5,
      _waitAcceptors =
        [ matchAll "Pending" AcceptRetry (gcirsStatus . to toTextCI),
          matchAll "InProgress" AcceptRetry (gcirsStatus . to toTextCI),
          matchAll "Delayed" AcceptRetry (gcirsStatus . to toTextCI),
          matchAll "Success" AcceptSuccess (gcirsStatus . to toTextCI),
          matchAll "Cancelled" AcceptFailure (gcirsStatus . to toTextCI),
          matchAll "TimedOut" AcceptFailure (gcirsStatus . to toTextCI),
          matchAll "Failed" AcceptFailure (gcirsStatus . to toTextCI),
          matchAll "Cancelling" AcceptFailure (gcirsStatus . to toTextCI)
        ]
    }

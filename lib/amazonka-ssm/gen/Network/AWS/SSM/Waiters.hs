{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Waiters
  (
    -- * CommandExecuted
    mkCommandExecuted,
  ) where

import Network.AWS.SSM.GetCommandInvocation
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types as Types
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.SSM.GetCommandInvocation' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
mkCommandExecuted :: Waiter.Wait GetCommandInvocation
mkCommandExecuted
  = Waiter.Wait{Waiter._waitName = "CommandExecuted",
                Waiter._waitAttempts = 20, Waiter._waitDelay = 5,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "Pending" Waiter.AcceptRetry
                     (Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAll "InProgress" Waiter.AcceptRetry
                     (Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAll "Delayed" Waiter.AcceptRetry
                     (Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAll "Success" Waiter.AcceptSuccess
                     (Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAll "Cancelled" Waiter.AcceptFailure
                     (Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAll "TimedOut" Waiter.AcceptFailure
                     (Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAll "Failed" Waiter.AcceptFailure
                     (Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAll "Cancelling" Waiter.AcceptFailure
                     (Lens.field @"status" Core.. Lens._Just)]}

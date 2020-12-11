{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Waiters
  ( -- * CommandExecuted
    mkCommandExecuted,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.GetCommandInvocation
import Network.AWS.SSM.Types
import qualified Network.AWS.Waiter as Wait

-- | Polls 'Network.AWS.SSM.GetCommandInvocation' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
mkCommandExecuted :: Wait.Wait GetCommandInvocation
mkCommandExecuted =
  Wait.Wait
    { Wait._waitName = "CommandExecuted",
      Wait._waitAttempts = 20,
      Wait._waitDelay = 5,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "Pending"
            Wait.AcceptRetry
            (gcirsStatus Lude.. Lens._Just Lude.. Lens.to Lude.toText),
          Wait.matchAll
            "InProgress"
            Wait.AcceptRetry
            (gcirsStatus Lude.. Lens._Just Lude.. Lens.to Lude.toText),
          Wait.matchAll
            "Delayed"
            Wait.AcceptRetry
            (gcirsStatus Lude.. Lens._Just Lude.. Lens.to Lude.toText),
          Wait.matchAll
            "Success"
            Wait.AcceptSuccess
            (gcirsStatus Lude.. Lens._Just Lude.. Lens.to Lude.toText),
          Wait.matchAll
            "Cancelled"
            Wait.AcceptFailure
            (gcirsStatus Lude.. Lens._Just Lude.. Lens.to Lude.toText),
          Wait.matchAll
            "TimedOut"
            Wait.AcceptFailure
            (gcirsStatus Lude.. Lens._Just Lude.. Lens.to Lude.toText),
          Wait.matchAll
            "Failed"
            Wait.AcceptFailure
            (gcirsStatus Lude.. Lens._Just Lude.. Lens.to Lude.toText),
          Wait.matchAll
            "Cancelling"
            Wait.AcceptFailure
            (gcirsStatus Lude.. Lens._Just Lude.. Lens.to Lude.toText)
        ]
    }

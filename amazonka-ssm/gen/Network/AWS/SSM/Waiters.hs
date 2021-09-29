{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Waiters where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.GetCommandInvocation
import Network.AWS.SSM.Lens
import Network.AWS.SSM.Types

-- | Polls 'Network.AWS.SSM.GetCommandInvocation' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
newCommandExecuted :: Core.Wait GetCommandInvocation
newCommandExecuted =
  Core.Wait
    { Core._waitName = "CommandExecuted",
      Core._waitAttempts = 20,
      Core._waitDelay = 5,
      Core._waitAcceptors =
        [ Core.matchAll
            "Pending"
            Core.AcceptRetry
            ( getCommandInvocationResponse_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "InProgress"
            Core.AcceptRetry
            ( getCommandInvocationResponse_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "Delayed"
            Core.AcceptRetry
            ( getCommandInvocationResponse_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "Success"
            Core.AcceptSuccess
            ( getCommandInvocationResponse_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "Cancelled"
            Core.AcceptFailure
            ( getCommandInvocationResponse_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "TimedOut"
            Core.AcceptFailure
            ( getCommandInvocationResponse_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "Failed"
            Core.AcceptFailure
            ( getCommandInvocationResponse_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "Cancelling"
            Core.AcceptFailure
            ( getCommandInvocationResponse_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchError
            "InvocationDoesNotExist"
            Core.AcceptRetry
        ]
    }

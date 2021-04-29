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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.GetCommandInvocation
import Network.AWS.SSM.Lens
import Network.AWS.SSM.Types
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.SSM.GetCommandInvocation' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
newCommandExecuted :: Waiter.Wait GetCommandInvocation
newCommandExecuted =
  Waiter.Wait
    { Waiter._waitName = "CommandExecuted",
      Waiter._waitAttempts = 20,
      Waiter._waitDelay = 5,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "Pending"
            Waiter.AcceptRetry
            ( getCommandInvocationResponse_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "InProgress"
            Waiter.AcceptRetry
            ( getCommandInvocationResponse_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "Delayed"
            Waiter.AcceptRetry
            ( getCommandInvocationResponse_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "Success"
            Waiter.AcceptSuccess
            ( getCommandInvocationResponse_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "Cancelled"
            Waiter.AcceptFailure
            ( getCommandInvocationResponse_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "TimedOut"
            Waiter.AcceptFailure
            ( getCommandInvocationResponse_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "Failed"
            Waiter.AcceptFailure
            ( getCommandInvocationResponse_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "Cancelling"
            Waiter.AcceptFailure
            ( getCommandInvocationResponse_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

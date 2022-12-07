{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SSM.Waiters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Waiters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.GetCommandInvocation
import Amazonka.SSM.Lens
import Amazonka.SSM.Types

-- | Polls 'Amazonka.SSM.GetCommandInvocation' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
newCommandExecuted :: Core.Wait GetCommandInvocation
newCommandExecuted =
  Core.Wait
    { Core.name = "CommandExecuted",
      Core.attempts = 20,
      Core.delay = 5,
      Core.acceptors =
        [ Core.matchAll
            "Pending"
            Core.AcceptRetry
            ( getCommandInvocationResponse_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "InProgress"
            Core.AcceptRetry
            ( getCommandInvocationResponse_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "Delayed"
            Core.AcceptRetry
            ( getCommandInvocationResponse_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "Success"
            Core.AcceptSuccess
            ( getCommandInvocationResponse_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "Cancelled"
            Core.AcceptFailure
            ( getCommandInvocationResponse_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "TimedOut"
            Core.AcceptFailure
            ( getCommandInvocationResponse_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "Failed"
            Core.AcceptFailure
            ( getCommandInvocationResponse_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "Cancelling"
            Core.AcceptFailure
            ( getCommandInvocationResponse_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchError
            "InvocationDoesNotExist"
            Core.AcceptRetry
        ]
    }

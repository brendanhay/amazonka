{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Waiters where

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.GetFunction
import Network.AWS.Lambda.GetFunctionConfiguration
import Network.AWS.Lambda.Lens
import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Polls 'Network.AWS.Lambda.GetFunction' every 1 seconds until a successful state is reached. An error is returned after 20 failed checks.
newFunctionExists :: Core.Wait GetFunction
newFunctionExists =
  Core.Wait
    { Core._waitName = "FunctionExists",
      Core._waitAttempts = 20,
      Core._waitDelay = 1,
      Core._waitAcceptors =
        [ Core.matchStatus 200 Core.AcceptSuccess,
          Core.matchError
            "ResourceNotFoundException"
            Core.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.Lambda.GetFunctionConfiguration' every 5 seconds until a successful state is reached. An error is returned after 60 failed checks.
newFunctionActive :: Core.Wait GetFunctionConfiguration
newFunctionActive =
  Core.Wait
    { Core._waitName = "FunctionActive",
      Core._waitAttempts = 60,
      Core._waitDelay = 5,
      Core._waitAcceptors =
        [ Core.matchAll
            "Active"
            Core.AcceptSuccess
            ( functionConfiguration_state Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "Failed"
            Core.AcceptFailure
            ( functionConfiguration_state Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "Pending"
            Core.AcceptRetry
            ( functionConfiguration_state Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.Lambda.GetFunctionConfiguration' every 5 seconds until a successful state is reached. An error is returned after 60 failed checks.
newFunctionUpdated :: Core.Wait GetFunctionConfiguration
newFunctionUpdated =
  Core.Wait
    { Core._waitName = "FunctionUpdated",
      Core._waitAttempts = 60,
      Core._waitDelay = 5,
      Core._waitAcceptors =
        [ Core.matchAll
            "Successful"
            Core.AcceptSuccess
            ( functionConfiguration_lastUpdateStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "Failed"
            Core.AcceptFailure
            ( functionConfiguration_lastUpdateStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "InProgress"
            Core.AcceptRetry
            ( functionConfiguration_lastUpdateStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

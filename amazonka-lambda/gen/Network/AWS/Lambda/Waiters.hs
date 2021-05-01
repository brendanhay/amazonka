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

import Network.AWS.Lambda.GetFunction
import Network.AWS.Lambda.GetFunctionConfiguration
import Network.AWS.Lambda.Lens
import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.Lambda.GetFunctionConfiguration' every 5 seconds until a successful state is reached. An error is returned after 60 failed checks.
newFunctionUpdated :: Waiter.Wait GetFunctionConfiguration
newFunctionUpdated =
  Waiter.Wait
    { Waiter._waitName = "FunctionUpdated",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 5,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "Successful"
            Waiter.AcceptSuccess
            ( functionConfiguration_lastUpdateStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "Failed"
            Waiter.AcceptFailure
            ( functionConfiguration_lastUpdateStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "InProgress"
            Waiter.AcceptRetry
            ( functionConfiguration_lastUpdateStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.Lambda.GetFunction' every 1 seconds until a successful state is reached. An error is returned after 20 failed checks.
newFunctionExists :: Waiter.Wait GetFunction
newFunctionExists =
  Waiter.Wait
    { Waiter._waitName = "FunctionExists",
      Waiter._waitAttempts = 20,
      Waiter._waitDelay = 1,
      Waiter._waitAcceptors =
        [ Waiter.matchStatus 200 Waiter.AcceptSuccess,
          Waiter.matchError
            "ResourceNotFoundException"
            Waiter.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.Lambda.GetFunctionConfiguration' every 5 seconds until a successful state is reached. An error is returned after 60 failed checks.
newFunctionActive :: Waiter.Wait GetFunctionConfiguration
newFunctionActive =
  Waiter.Wait
    { Waiter._waitName = "FunctionActive",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 5,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "Active"
            Waiter.AcceptSuccess
            ( functionConfiguration_state Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "Failed"
            Waiter.AcceptFailure
            ( functionConfiguration_state Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "Pending"
            Waiter.AcceptRetry
            ( functionConfiguration_state Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

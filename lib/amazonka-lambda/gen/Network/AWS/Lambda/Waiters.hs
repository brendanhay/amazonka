{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Waiters
  ( -- * FunctionExists
    mkFunctionExists,

    -- * FunctionActive
    mkFunctionActive,

    -- * FunctionUpdated
    mkFunctionUpdated,
  )
where

import Network.AWS.Lambda.GetFunction
import Network.AWS.Lambda.GetFunctionConfiguration
import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.Lambda.GetFunction' every 1 seconds until a successful state is reached. An error is returned after 20 failed checks.
mkFunctionExists :: Waiter.Wait GetFunction
mkFunctionExists =
  Waiter.Wait
    { Waiter._waitName = "FunctionExists",
      Waiter._waitAttempts = 20,
      Waiter._waitDelay = 1,
      Waiter._waitAcceptors =
        [ Waiter.matchStatus 200 Waiter.AcceptSuccess,
          Waiter.matchError "ResourceNotFoundException" Waiter.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.Lambda.GetFunctionConfiguration' every 5 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkFunctionActive :: Waiter.Wait GetFunctionConfiguration
mkFunctionActive =
  Waiter.Wait
    { Waiter._waitName = "FunctionActive",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 5,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "Active"
            Waiter.AcceptSuccess
            (Lens.field @"state" Core.. Lens._Just),
          Waiter.matchAll
            "Failed"
            Waiter.AcceptFailure
            (Lens.field @"state" Core.. Lens._Just),
          Waiter.matchAll
            "Pending"
            Waiter.AcceptRetry
            (Lens.field @"state" Core.. Lens._Just)
        ]
    }

-- | Polls 'Network.AWS.Lambda.GetFunctionConfiguration' every 5 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkFunctionUpdated :: Waiter.Wait GetFunctionConfiguration
mkFunctionUpdated =
  Waiter.Wait
    { Waiter._waitName = "FunctionUpdated",
      Waiter._waitAttempts = 60,
      Waiter._waitDelay = 5,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "Successful"
            Waiter.AcceptSuccess
            (Lens.field @"lastUpdateStatus" Core.. Lens._Just),
          Waiter.matchAll
            "Failed"
            Waiter.AcceptFailure
            (Lens.field @"lastUpdateStatus" Core.. Lens._Just),
          Waiter.matchAll
            "InProgress"
            Waiter.AcceptRetry
            (Lens.field @"lastUpdateStatus" Core.. Lens._Just)
        ]
    }

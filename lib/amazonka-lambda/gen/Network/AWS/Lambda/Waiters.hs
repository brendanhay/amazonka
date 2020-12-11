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
import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Waiter as Wait

-- | Polls 'Network.AWS.Lambda.GetFunction' every 1 seconds until a successful state is reached. An error is returned after 20 failed checks.
mkFunctionExists :: Wait.Wait GetFunction
mkFunctionExists =
  Wait.Wait
    { Wait._waitName = "FunctionExists",
      Wait._waitAttempts = 20,
      Wait._waitDelay = 1,
      Wait._waitAcceptors =
        [ Wait.matchStatus 200 Wait.AcceptSuccess,
          Wait.matchError "ResourceNotFoundException" Wait.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.Lambda.GetFunctionConfiguration' every 5 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkFunctionActive :: Wait.Wait GetFunctionConfiguration
mkFunctionActive =
  Wait.Wait
    { Wait._waitName = "FunctionActive",
      Wait._waitAttempts = 60,
      Wait._waitDelay = 5,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "Active"
            Wait.AcceptSuccess
            (fcState Lude.. Lens._Just Lude.. Lens.to Lude.toText),
          Wait.matchAll
            "Failed"
            Wait.AcceptFailure
            (fcState Lude.. Lens._Just Lude.. Lens.to Lude.toText),
          Wait.matchAll
            "Pending"
            Wait.AcceptRetry
            (fcState Lude.. Lens._Just Lude.. Lens.to Lude.toText)
        ]
    }

-- | Polls 'Network.AWS.Lambda.GetFunctionConfiguration' every 5 seconds until a successful state is reached. An error is returned after 60 failed checks.
mkFunctionUpdated :: Wait.Wait GetFunctionConfiguration
mkFunctionUpdated =
  Wait.Wait
    { Wait._waitName = "FunctionUpdated",
      Wait._waitAttempts = 60,
      Wait._waitDelay = 5,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "Successful"
            Wait.AcceptSuccess
            (fcLastUpdateStatus Lude.. Lens._Just Lude.. Lens.to Lude.toText),
          Wait.matchAll
            "Failed"
            Wait.AcceptFailure
            (fcLastUpdateStatus Lude.. Lens._Just Lude.. Lens.to Lude.toText),
          Wait.matchAll
            "InProgress"
            Wait.AcceptRetry
            (fcLastUpdateStatus Lude.. Lens._Just Lude.. Lens.to Lude.toText)
        ]
    }

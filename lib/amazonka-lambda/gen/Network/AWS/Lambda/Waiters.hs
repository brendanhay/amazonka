{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Waiters where

import Network.AWS.Lambda.GetFunction
import Network.AWS.Lambda.GetFunctionConfiguration
import Network.AWS.Lambda.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Waiter

-- | Polls 'Network.AWS.Lambda.GetFunction' every 1 seconds until a successful state is reached. An error is returned after 20 failed checks.
functionExists :: Wait GetFunction
functionExists =
  Wait
    { _waitName = "FunctionExists",
      _waitAttempts = 20,
      _waitDelay = 1,
      _waitAcceptors =
        [ matchStatus 200 AcceptSuccess,
          matchError "ResourceNotFoundException" AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.Lambda.GetFunctionConfiguration' every 5 seconds until a successful state is reached. An error is returned after 60 failed checks.
functionActive :: Wait GetFunctionConfiguration
functionActive =
  Wait
    { _waitName = "FunctionActive",
      _waitAttempts = 60,
      _waitDelay = 5,
      _waitAcceptors =
        [ matchAll "Active" AcceptSuccess (fcState . to toTextCI),
          matchAll "Failed" AcceptFailure (fcState . to toTextCI),
          matchAll "Pending" AcceptRetry (fcState . to toTextCI)
        ]
    }

-- | Polls 'Network.AWS.Lambda.GetFunctionConfiguration' every 5 seconds until a successful state is reached. An error is returned after 60 failed checks.
functionUpdated :: Wait GetFunctionConfiguration
functionUpdated =
  Wait
    { _waitName = "FunctionUpdated",
      _waitAttempts = 60,
      _waitDelay = 5,
      _waitAcceptors =
        [ matchAll
            "Successful"
            AcceptSuccess
            (fcLastUpdateStatus . to toTextCI),
          matchAll "Failed" AcceptFailure (fcLastUpdateStatus . to toTextCI),
          matchAll
            "InProgress"
            AcceptRetry
            (fcLastUpdateStatus . to toTextCI)
        ]
    }

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Waiters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lambda.Waiters where

import Network.AWS.Lambda.GetFunction
import Network.AWS.Lambda.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Waiter

-- | Polls 'Network.AWS.Lambda.GetFunction' every 1 seconds until a successful state is reached. An error is returned after 20 failed checks.
functionExists :: Wait GetFunction
functionExists =
  Wait
    { _waitName = "FunctionExists"
    , _waitAttempts = 20
    , _waitDelay = 1
    , _waitAcceptors =
        [ matchStatus 200 AcceptSuccess
        , matchError "ResourceNotFoundException" AcceptRetry
        ]
    }


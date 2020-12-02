{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Waiters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Waiters where

import Network.AWS.IAM.GetInstanceProfile
import Network.AWS.IAM.GetUser
import Network.AWS.IAM.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Waiter

-- | Polls 'Network.AWS.IAM.GetInstanceProfile' every 1 seconds until a successful state is reached. An error is returned after 40 failed checks.
instanceProfileExists :: Wait GetInstanceProfile
instanceProfileExists =
  Wait
    { _waitName = "InstanceProfileExists"
    , _waitAttempts = 40
    , _waitDelay = 1
    , _waitAcceptors =
        [matchStatus 200 AcceptSuccess, matchStatus 404 AcceptRetry]
    }


-- | Polls 'Network.AWS.IAM.GetUser' every 1 seconds until a successful state is reached. An error is returned after 20 failed checks.
userExists :: Wait GetUser
userExists =
  Wait
    { _waitName = "UserExists"
    , _waitAttempts = 20
    , _waitDelay = 1
    , _waitAcceptors =
        [matchStatus 200 AcceptSuccess, matchError "NoSuchEntity" AcceptRetry]
    }


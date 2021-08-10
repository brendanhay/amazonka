{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Waiters where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.GetInstanceProfile
import Network.AWS.IAM.GetPolicy
import Network.AWS.IAM.GetRole
import Network.AWS.IAM.GetUser
import Network.AWS.IAM.Lens
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Polls 'Network.AWS.IAM.GetUser' every 1 seconds until a successful state is reached. An error is returned after 20 failed checks.
newUserExists :: Core.Wait GetUser
newUserExists =
  Core.Wait
    { Core._waitName = "UserExists",
      Core._waitAttempts = 20,
      Core._waitDelay = 1,
      Core._waitAcceptors =
        [ Core.matchStatus 200 Core.AcceptSuccess,
          Core.matchError "NoSuchEntity" Core.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.IAM.GetRole' every 1 seconds until a successful state is reached. An error is returned after 20 failed checks.
newRoleExists :: Core.Wait GetRole
newRoleExists =
  Core.Wait
    { Core._waitName = "RoleExists",
      Core._waitAttempts = 20,
      Core._waitDelay = 1,
      Core._waitAcceptors =
        [ Core.matchStatus 200 Core.AcceptSuccess,
          Core.matchError "NoSuchEntity" Core.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.IAM.GetPolicy' every 1 seconds until a successful state is reached. An error is returned after 20 failed checks.
newPolicyExists :: Core.Wait GetPolicy
newPolicyExists =
  Core.Wait
    { Core._waitName = "PolicyExists",
      Core._waitAttempts = 20,
      Core._waitDelay = 1,
      Core._waitAcceptors =
        [ Core.matchStatus 200 Core.AcceptSuccess,
          Core.matchError "NoSuchEntity" Core.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.IAM.GetInstanceProfile' every 1 seconds until a successful state is reached. An error is returned after 40 failed checks.
newInstanceProfileExists :: Core.Wait GetInstanceProfile
newInstanceProfileExists =
  Core.Wait
    { Core._waitName = "InstanceProfileExists",
      Core._waitAttempts = 40,
      Core._waitDelay = 1,
      Core._waitAcceptors =
        [ Core.matchStatus 200 Core.AcceptSuccess,
          Core.matchStatus 404 Core.AcceptRetry
        ]
    }

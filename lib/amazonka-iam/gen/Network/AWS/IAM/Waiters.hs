{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Waiters
  ( -- * InstanceProfileExists
    mkInstanceProfileExists,

    -- * UserExists
    mkUserExists,

    -- * RoleExists
    mkRoleExists,

    -- * PolicyExists
    mkPolicyExists,
  )
where

import Network.AWS.IAM.GetInstanceProfile
import Network.AWS.IAM.GetPolicy
import Network.AWS.IAM.GetRole
import Network.AWS.IAM.GetUser
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Waiter as Wait

-- | Polls 'Network.AWS.IAM.GetInstanceProfile' every 1 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkInstanceProfileExists :: Wait.Wait GetInstanceProfile
mkInstanceProfileExists =
  Wait.Wait
    { Wait._waitName = "InstanceProfileExists",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 1,
      Wait._waitAcceptors =
        [ Wait.matchStatus 200 Wait.AcceptSuccess,
          Wait.matchStatus 404 Wait.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.IAM.GetUser' every 1 seconds until a successful state is reached. An error is returned after 20 failed checks.
mkUserExists :: Wait.Wait GetUser
mkUserExists =
  Wait.Wait
    { Wait._waitName = "UserExists",
      Wait._waitAttempts = 20,
      Wait._waitDelay = 1,
      Wait._waitAcceptors =
        [ Wait.matchStatus 200 Wait.AcceptSuccess,
          Wait.matchError "NoSuchEntity" Wait.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.IAM.GetRole' every 1 seconds until a successful state is reached. An error is returned after 20 failed checks.
mkRoleExists :: Wait.Wait GetRole
mkRoleExists =
  Wait.Wait
    { Wait._waitName = "RoleExists",
      Wait._waitAttempts = 20,
      Wait._waitDelay = 1,
      Wait._waitAcceptors =
        [ Wait.matchStatus 200 Wait.AcceptSuccess,
          Wait.matchError "NoSuchEntity" Wait.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.IAM.GetPolicy' every 1 seconds until a successful state is reached. An error is returned after 20 failed checks.
mkPolicyExists :: Wait.Wait GetPolicy
mkPolicyExists =
  Wait.Wait
    { Wait._waitName = "PolicyExists",
      Wait._waitAttempts = 20,
      Wait._waitDelay = 1,
      Wait._waitAcceptors =
        [ Wait.matchStatus 200 Wait.AcceptSuccess,
          Wait.matchError "NoSuchEntity" Wait.AcceptRetry
        ]
    }

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Waiters
  (
    -- * InstanceProfileExists
    mkInstanceProfileExists,
    -- * UserExists
    mkUserExists,
    -- * RoleExists
    mkRoleExists,
    -- * PolicyExists
    mkPolicyExists,
  ) where

import Network.AWS.IAM.GetInstanceProfile
import Network.AWS.IAM.GetPolicy
import Network.AWS.IAM.GetRole
import Network.AWS.IAM.GetUser
import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.IAM.GetInstanceProfile' every 1 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkInstanceProfileExists :: Waiter.Wait GetInstanceProfile
mkInstanceProfileExists
  = Waiter.Wait{Waiter._waitName = "InstanceProfileExists",
                Waiter._waitAttempts = 40, Waiter._waitDelay = 1,
                Waiter._waitAcceptors =
                  [Waiter.matchStatus 200 Waiter.AcceptSuccess,
                   Waiter.matchStatus 404 Waiter.AcceptRetry]}

-- | Polls 'Network.AWS.IAM.GetUser' every 1 seconds until a successful state is reached. An error is returned after 20 failed checks.
mkUserExists :: Waiter.Wait GetUser
mkUserExists
  = Waiter.Wait{Waiter._waitName = "UserExists",
                Waiter._waitAttempts = 20, Waiter._waitDelay = 1,
                Waiter._waitAcceptors =
                  [Waiter.matchStatus 200 Waiter.AcceptSuccess,
                   Waiter.matchError "NoSuchEntity" Waiter.AcceptRetry]}

-- | Polls 'Network.AWS.IAM.GetRole' every 1 seconds until a successful state is reached. An error is returned after 20 failed checks.
mkRoleExists :: Waiter.Wait GetRole
mkRoleExists
  = Waiter.Wait{Waiter._waitName = "RoleExists",
                Waiter._waitAttempts = 20, Waiter._waitDelay = 1,
                Waiter._waitAcceptors =
                  [Waiter.matchStatus 200 Waiter.AcceptSuccess,
                   Waiter.matchError "NoSuchEntity" Waiter.AcceptRetry]}

-- | Polls 'Network.AWS.IAM.GetPolicy' every 1 seconds until a successful state is reached. An error is returned after 20 failed checks.
mkPolicyExists :: Waiter.Wait GetPolicy
mkPolicyExists
  = Waiter.Wait{Waiter._waitName = "PolicyExists",
                Waiter._waitAttempts = 20, Waiter._waitDelay = 1,
                Waiter._waitAcceptors =
                  [Waiter.matchStatus 200 Waiter.AcceptSuccess,
                   Waiter.matchError "NoSuchEntity" Waiter.AcceptRetry]}

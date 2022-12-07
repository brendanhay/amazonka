{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IAM.Waiters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Waiters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.GetInstanceProfile
import Amazonka.IAM.GetPolicy
import Amazonka.IAM.GetRole
import Amazonka.IAM.GetUser
import Amazonka.IAM.Lens
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude

-- | Polls 'Amazonka.IAM.GetRole' every 1 seconds until a successful state is reached. An error is returned after 20 failed checks.
newRoleExists :: Core.Wait GetRole
newRoleExists =
  Core.Wait
    { Core.name = "RoleExists",
      Core.attempts = 20,
      Core.delay = 1,
      Core.acceptors =
        [ Core.matchStatus 200 Core.AcceptSuccess,
          Core.matchError "NoSuchEntity" Core.AcceptRetry
        ]
    }

-- | Polls 'Amazonka.IAM.GetPolicy' every 1 seconds until a successful state is reached. An error is returned after 20 failed checks.
newPolicyExists :: Core.Wait GetPolicy
newPolicyExists =
  Core.Wait
    { Core.name = "PolicyExists",
      Core.attempts = 20,
      Core.delay = 1,
      Core.acceptors =
        [ Core.matchStatus 200 Core.AcceptSuccess,
          Core.matchError "NoSuchEntity" Core.AcceptRetry
        ]
    }

-- | Polls 'Amazonka.IAM.GetUser' every 1 seconds until a successful state is reached. An error is returned after 20 failed checks.
newUserExists :: Core.Wait GetUser
newUserExists =
  Core.Wait
    { Core.name = "UserExists",
      Core.attempts = 20,
      Core.delay = 1,
      Core.acceptors =
        [ Core.matchStatus 200 Core.AcceptSuccess,
          Core.matchError "NoSuchEntity" Core.AcceptRetry
        ]
    }

-- | Polls 'Amazonka.IAM.GetInstanceProfile' every 1 seconds until a successful state is reached. An error is returned after 40 failed checks.
newInstanceProfileExists :: Core.Wait GetInstanceProfile
newInstanceProfileExists =
  Core.Wait
    { Core.name = "InstanceProfileExists",
      Core.attempts = 40,
      Core.delay = 1,
      Core.acceptors =
        [ Core.matchStatus 200 Core.AcceptSuccess,
          Core.matchStatus 404 Core.AcceptRetry
        ]
    }

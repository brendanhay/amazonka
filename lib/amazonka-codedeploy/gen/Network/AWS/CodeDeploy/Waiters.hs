{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Waiters
  (
    -- * DeploymentSuccessful
    mkDeploymentSuccessful,
  ) where

import Network.AWS.CodeDeploy.GetDeployment
import qualified Network.AWS.CodeDeploy.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.CodeDeploy.GetDeployment' every 15 seconds until a successful state is reached. An error is returned after 120 failed checks.
mkDeploymentSuccessful :: Waiter.Wait GetDeployment
mkDeploymentSuccessful
  = Waiter.Wait{Waiter._waitName = "DeploymentSuccessful",
                Waiter._waitAttempts = 120, Waiter._waitDelay = 15,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "Succeeded" Waiter.AcceptSuccess
                     (Lens.field @"deploymentInfo" Core.. Lens._Just Core..
                        Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAll "Failed" Waiter.AcceptFailure
                     (Lens.field @"deploymentInfo" Core.. Lens._Just Core..
                        Lens.field @"status" Core.. Lens._Just),
                   Waiter.matchAll "Stopped" Waiter.AcceptFailure
                     (Lens.field @"deploymentInfo" Core.. Lens._Just Core..
                        Lens.field @"status" Core.. Lens._Just)]}

{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Waiters
  ( -- * DeploymentSuccessful
    mkDeploymentSuccessful,
  )
where

import Network.AWS.CodeDeploy.GetDeployment
import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Waiter as Wait

-- | Polls 'Network.AWS.CodeDeploy.GetDeployment' every 15 seconds until a successful state is reached. An error is returned after 120 failed checks.
mkDeploymentSuccessful :: Wait.Wait GetDeployment
mkDeploymentSuccessful =
  Wait.Wait
    { Wait._waitName = "DeploymentSuccessful",
      Wait._waitAttempts = 120,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "Succeeded"
            Wait.AcceptSuccess
            ( gdrsDeploymentInfo Lude.. Lens._Just
                Lude.. diStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAll
            "Failed"
            Wait.AcceptFailure
            ( gdrsDeploymentInfo Lude.. Lens._Just
                Lude.. diStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAll
            "Stopped"
            Wait.AcceptFailure
            ( gdrsDeploymentInfo Lude.. Lens._Just
                Lude.. diStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

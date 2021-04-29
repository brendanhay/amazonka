{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Waiters where

import Network.AWS.CodeDeploy.GetDeployment
import Network.AWS.CodeDeploy.Lens
import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.CodeDeploy.GetDeployment' every 15 seconds until a successful state is reached. An error is returned after 120 failed checks.
newDeploymentSuccessful :: Waiter.Wait GetDeployment
newDeploymentSuccessful =
  Waiter.Wait
    { Waiter._waitName =
        "DeploymentSuccessful",
      Waiter._waitAttempts = 120,
      Waiter._waitDelay = 15,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "Succeeded"
            Waiter.AcceptSuccess
            ( getDeploymentResponse_deploymentInfo
                Prelude.. Lens._Just
                Prelude.. deploymentInfo_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "Failed"
            Waiter.AcceptFailure
            ( getDeploymentResponse_deploymentInfo
                Prelude.. Lens._Just
                Prelude.. deploymentInfo_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "Stopped"
            Waiter.AcceptFailure
            ( getDeploymentResponse_deploymentInfo
                Prelude.. Lens._Just
                Prelude.. deploymentInfo_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Polls 'Network.AWS.CodeDeploy.GetDeployment' every 15 seconds until a successful state is reached. An error is returned after 120 failed checks.
newDeploymentSuccessful :: Core.Wait GetDeployment
newDeploymentSuccessful =
  Core.Wait
    { Core._waitName = "DeploymentSuccessful",
      Core._waitAttempts = 120,
      Core._waitDelay = 15,
      Core._waitAcceptors =
        [ Core.matchAll
            "Succeeded"
            Core.AcceptSuccess
            ( getDeploymentResponse_deploymentInfo
                Prelude.. Lens._Just
                Prelude.. deploymentInfo_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "Failed"
            Core.AcceptFailure
            ( getDeploymentResponse_deploymentInfo
                Prelude.. Lens._Just
                Prelude.. deploymentInfo_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "Stopped"
            Core.AcceptFailure
            ( getDeploymentResponse_deploymentInfo
                Prelude.. Lens._Just
                Prelude.. deploymentInfo_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

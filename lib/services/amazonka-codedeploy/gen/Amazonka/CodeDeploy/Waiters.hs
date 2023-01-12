{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodeDeploy.Waiters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Waiters where

import Amazonka.CodeDeploy.GetDeployment
import Amazonka.CodeDeploy.Lens
import Amazonka.CodeDeploy.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Polls 'Amazonka.CodeDeploy.GetDeployment' every 15 seconds until a successful state is reached. An error is returned after 120 failed checks.
newDeploymentSuccessful :: Core.Wait GetDeployment
newDeploymentSuccessful =
  Core.Wait
    { Core.name = "DeploymentSuccessful",
      Core.attempts = 120,
      Core.delay = 15,
      Core.acceptors =
        [ Core.matchAll
            "Succeeded"
            Core.AcceptSuccess
            ( getDeploymentResponse_deploymentInfo
                Prelude.. Lens._Just
                Prelude.. deploymentInfo_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "Failed"
            Core.AcceptFailure
            ( getDeploymentResponse_deploymentInfo
                Prelude.. Lens._Just
                Prelude.. deploymentInfo_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "Stopped"
            Core.AcceptFailure
            ( getDeploymentResponse_deploymentInfo
                Prelude.. Lens._Just
                Prelude.. deploymentInfo_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

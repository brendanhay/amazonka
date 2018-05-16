{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Waiters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Waiters where

import Network.AWS.CodeDeploy.GetDeployment
import Network.AWS.CodeDeploy.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Waiter

-- | Polls 'Network.AWS.CodeDeploy.GetDeployment' every 15 seconds until a successful state is reached. An error is returned after 120 failed checks.
deploymentSuccessful :: Wait GetDeployment
deploymentSuccessful =
  Wait
    { _waitName = "DeploymentSuccessful"
    , _waitAttempts = 120
    , _waitDelay = 15
    , _waitAcceptors =
        [ matchAll
            "Succeeded"
            AcceptSuccess
            (gdrsDeploymentInfo . _Just . diStatus . _Just . to toTextCI)
        , matchAll
            "Failed"
            AcceptFailure
            (gdrsDeploymentInfo . _Just . diStatus . _Just . to toTextCI)
        , matchAll
            "Stopped"
            AcceptFailure
            (gdrsDeploymentInfo . _Just . diStatus . _Just . to toTextCI)
        ]
    }


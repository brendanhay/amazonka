{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AMP.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AMP.Waiters where

import Network.AWS.AMP.DescribeWorkspace
import Network.AWS.AMP.Lens
import Network.AWS.AMP.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Polls 'Network.AWS.AMP.DescribeWorkspace' every 2 seconds until a successful state is reached. An error is returned after 60 failed checks.
newWorkspaceDeleted :: Core.Wait DescribeWorkspace
newWorkspaceDeleted =
  Core.Wait
    { Core._waitName = "WorkspaceDeleted",
      Core._waitAttempts = 60,
      Core._waitDelay = 2,
      Core._waitAcceptors =
        [ Core.matchError
            "ResourceNotFoundException"
            Core.AcceptSuccess,
          Core.matchAll
            "DELETING"
            Core.AcceptRetry
            ( describeWorkspaceResponse_workspace
                Prelude.. workspaceDescription_status
                Prelude.. workspaceStatus_statusCode
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.AMP.DescribeWorkspace' every 2 seconds until a successful state is reached. An error is returned after 60 failed checks.
newWorkspaceActive :: Core.Wait DescribeWorkspace
newWorkspaceActive =
  Core.Wait
    { Core._waitName = "WorkspaceActive",
      Core._waitAttempts = 60,
      Core._waitDelay = 2,
      Core._waitAcceptors =
        [ Core.matchAll
            "ACTIVE"
            Core.AcceptSuccess
            ( describeWorkspaceResponse_workspace
                Prelude.. workspaceDescription_status
                Prelude.. workspaceStatus_statusCode
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "UPDATING"
            Core.AcceptRetry
            ( describeWorkspaceResponse_workspace
                Prelude.. workspaceDescription_status
                Prelude.. workspaceStatus_statusCode
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "CREATING"
            Core.AcceptRetry
            ( describeWorkspaceResponse_workspace
                Prelude.. workspaceDescription_status
                Prelude.. workspaceStatus_statusCode
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

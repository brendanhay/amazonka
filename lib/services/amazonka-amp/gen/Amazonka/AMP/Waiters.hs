{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AMP.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AMP.Waiters where

import Amazonka.AMP.DescribeWorkspace
import Amazonka.AMP.Lens
import Amazonka.AMP.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Polls 'Amazonka.AMP.DescribeWorkspace' every 2 seconds until a successful state is reached. An error is returned after 60 failed checks.
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

-- | Polls 'Amazonka.AMP.DescribeWorkspace' every 2 seconds until a successful state is reached. An error is returned after 60 failed checks.
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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSMIncidents.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSMIncidents.Waiters where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSMIncidents.GetReplicationSet
import Network.AWS.SSMIncidents.Lens
import Network.AWS.SSMIncidents.Types

-- | Polls 'Network.AWS.SSMIncidents.GetReplicationSet' every 30 seconds until a successful state is reached. An error is returned after 5 failed checks.
newWaitForReplicationSetActive :: Core.Wait GetReplicationSet
newWaitForReplicationSetActive =
  Core.Wait
    { Core._waitName =
        "WaitForReplicationSetActive",
      Core._waitAttempts = 5,
      Core._waitDelay = 30,
      Core._waitAcceptors =
        [ Core.matchAll
            "ACTIVE"
            Core.AcceptSuccess
            ( getReplicationSetResponse_replicationSet
                Prelude.. replicationSet_status
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "CREATING"
            Core.AcceptRetry
            ( getReplicationSetResponse_replicationSet
                Prelude.. replicationSet_status
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "UPDATING"
            Core.AcceptRetry
            ( getReplicationSetResponse_replicationSet
                Prelude.. replicationSet_status
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( getReplicationSetResponse_replicationSet
                Prelude.. replicationSet_status
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.SSMIncidents.GetReplicationSet' every 30 seconds until a successful state is reached. An error is returned after 5 failed checks.
newWaitForReplicationSetDeleted :: Core.Wait GetReplicationSet
newWaitForReplicationSetDeleted =
  Core.Wait
    { Core._waitName =
        "WaitForReplicationSetDeleted",
      Core._waitAttempts = 5,
      Core._waitDelay = 30,
      Core._waitAcceptors =
        [ Core.matchError
            "ResourceNotFoundException"
            Core.AcceptSuccess,
          Core.matchAll
            "DELETING"
            Core.AcceptRetry
            ( getReplicationSetResponse_replicationSet
                Prelude.. replicationSet_status
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( getReplicationSetResponse_replicationSet
                Prelude.. replicationSet_status
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

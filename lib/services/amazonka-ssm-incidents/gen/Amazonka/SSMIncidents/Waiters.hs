{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SSMIncidents.Waiters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMIncidents.Waiters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMIncidents.GetReplicationSet
import Amazonka.SSMIncidents.Lens
import Amazonka.SSMIncidents.Types

-- | Polls 'Amazonka.SSMIncidents.GetReplicationSet' every 30 seconds until a successful state is reached. An error is returned after 5 failed checks.
newWaitForReplicationSetDeleted :: Core.Wait GetReplicationSet
newWaitForReplicationSetDeleted =
  Core.Wait
    { Core.name =
        "WaitForReplicationSetDeleted",
      Core.attempts = 5,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchError
            "ResourceNotFoundException"
            Core.AcceptSuccess,
          Core.matchAll
            "DELETING"
            Core.AcceptRetry
            ( getReplicationSetResponse_replicationSet
                Prelude.. replicationSet_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( getReplicationSetResponse_replicationSet
                Prelude.. replicationSet_status
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.SSMIncidents.GetReplicationSet' every 30 seconds until a successful state is reached. An error is returned after 5 failed checks.
newWaitForReplicationSetActive :: Core.Wait GetReplicationSet
newWaitForReplicationSetActive =
  Core.Wait
    { Core.name =
        "WaitForReplicationSetActive",
      Core.attempts = 5,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "ACTIVE"
            Core.AcceptSuccess
            ( getReplicationSetResponse_replicationSet
                Prelude.. replicationSet_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "CREATING"
            Core.AcceptRetry
            ( getReplicationSetResponse_replicationSet
                Prelude.. replicationSet_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "UPDATING"
            Core.AcceptRetry
            ( getReplicationSetResponse_replicationSet
                Prelude.. replicationSet_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( getReplicationSetResponse_replicationSet
                Prelude.. replicationSet_status
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

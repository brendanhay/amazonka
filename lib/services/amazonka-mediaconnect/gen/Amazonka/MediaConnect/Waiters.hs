{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaConnect.Waiters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Waiters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.DescribeFlow
import Amazonka.MediaConnect.Lens
import Amazonka.MediaConnect.Types
import qualified Amazonka.Prelude as Prelude

-- | Polls 'Amazonka.MediaConnect.DescribeFlow' every 3 seconds until a successful state is reached. An error is returned after 40 failed checks.
newFlowActive :: Core.Wait DescribeFlow
newFlowActive =
  Core.Wait
    { Core.name = "FlowActive",
      Core.attempts = 40,
      Core.delay = 3,
      Core.acceptors =
        [ Core.matchAll
            "ACTIVE"
            Core.AcceptSuccess
            ( describeFlowResponse_flow Prelude.. Lens._Just
                Prelude.. flow_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "STARTING"
            Core.AcceptRetry
            ( describeFlowResponse_flow Prelude.. Lens._Just
                Prelude.. flow_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "UPDATING"
            Core.AcceptRetry
            ( describeFlowResponse_flow Prelude.. Lens._Just
                Prelude.. flow_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchStatus 500 Core.AcceptRetry,
          Core.matchStatus 503 Core.AcceptRetry,
          Core.matchAll
            "ERROR"
            Core.AcceptFailure
            ( describeFlowResponse_flow Prelude.. Lens._Just
                Prelude.. flow_status
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.MediaConnect.DescribeFlow' every 3 seconds until a successful state is reached. An error is returned after 40 failed checks.
newFlowDeleted :: Core.Wait DescribeFlow
newFlowDeleted =
  Core.Wait
    { Core.name = "FlowDeleted",
      Core.attempts = 40,
      Core.delay = 3,
      Core.acceptors =
        [ Core.matchStatus 404 Core.AcceptSuccess,
          Core.matchAll
            "DELETING"
            Core.AcceptRetry
            ( describeFlowResponse_flow Prelude.. Lens._Just
                Prelude.. flow_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchStatus 500 Core.AcceptRetry,
          Core.matchStatus 503 Core.AcceptRetry,
          Core.matchAll
            "ERROR"
            Core.AcceptFailure
            ( describeFlowResponse_flow Prelude.. Lens._Just
                Prelude.. flow_status
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.MediaConnect.DescribeFlow' every 3 seconds until a successful state is reached. An error is returned after 40 failed checks.
newFlowStandby :: Core.Wait DescribeFlow
newFlowStandby =
  Core.Wait
    { Core.name = "FlowStandby",
      Core.attempts = 40,
      Core.delay = 3,
      Core.acceptors =
        [ Core.matchAll
            "STANDBY"
            Core.AcceptSuccess
            ( describeFlowResponse_flow Prelude.. Lens._Just
                Prelude.. flow_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "STOPPING"
            Core.AcceptRetry
            ( describeFlowResponse_flow Prelude.. Lens._Just
                Prelude.. flow_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchStatus 500 Core.AcceptRetry,
          Core.matchStatus 503 Core.AcceptRetry,
          Core.matchAll
            "ERROR"
            Core.AcceptFailure
            ( describeFlowResponse_flow Prelude.. Lens._Just
                Prelude.. flow_status
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

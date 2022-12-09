{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ELB.Waiters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELB.Waiters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ELB.DescribeInstanceHealth
import Amazonka.ELB.Lens
import Amazonka.ELB.Types
import qualified Amazonka.Prelude as Prelude

-- | Polls 'Amazonka.ELB.DescribeInstanceHealth' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newAnyInstanceInService :: Core.Wait DescribeInstanceHealth
newAnyInstanceInService =
  Core.Wait
    { Core.name = "AnyInstanceInService",
      Core.attempts = 40,
      Core.delay = 15,
      Core.acceptors =
        [ Core.matchAny
            "InService"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstanceHealthResponse_instanceStates
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instanceState_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.ELB.DescribeInstanceHealth' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newInstanceDeregistered :: Core.Wait DescribeInstanceHealth
newInstanceDeregistered =
  Core.Wait
    { Core.name = "InstanceDeregistered",
      Core.attempts = 40,
      Core.delay = 15,
      Core.acceptors =
        [ Core.matchAll
            "OutOfService"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstanceHealthResponse_instanceStates
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instanceState_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchError
            "InvalidInstance"
            Core.AcceptSuccess
        ]
    }

-- | Polls 'Amazonka.ELB.DescribeInstanceHealth' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newInstanceInService :: Core.Wait DescribeInstanceHealth
newInstanceInService =
  Core.Wait
    { Core.name = "InstanceInService",
      Core.attempts = 40,
      Core.delay = 15,
      Core.acceptors =
        [ Core.matchAll
            "InService"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeInstanceHealthResponse_instanceStates
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. instanceState_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchError "InvalidInstance" Core.AcceptRetry
        ]
    }

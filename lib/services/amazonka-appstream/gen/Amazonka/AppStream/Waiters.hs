{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AppStream.Waiters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Waiters where

import Amazonka.AppStream.DescribeFleets
import Amazonka.AppStream.Lens
import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Polls 'Amazonka.AppStream.DescribeFleets' every 30 seconds until a successful state is reached. An error is returned after 40 failed checks.
newFleetStopped :: Core.Wait DescribeFleets
newFleetStopped =
  Core.Wait
    { Core.name = "FleetStopped",
      Core.attempts = 40,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "INACTIVE"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (describeFleetsResponse_fleets Prelude.. Lens._Just)
                )
                Prelude.. fleet_state
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "PENDING_ACTIVATE"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeFleetsResponse_fleets Prelude.. Lens._Just)
                )
                Prelude.. fleet_state
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "ACTIVE"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeFleetsResponse_fleets Prelude.. Lens._Just)
                )
                Prelude.. fleet_state
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.AppStream.DescribeFleets' every 30 seconds until a successful state is reached. An error is returned after 40 failed checks.
newFleetStarted :: Core.Wait DescribeFleets
newFleetStarted =
  Core.Wait
    { Core.name = "FleetStarted",
      Core.attempts = 40,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "ACTIVE"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (describeFleetsResponse_fleets Prelude.. Lens._Just)
                )
                Prelude.. fleet_state
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "PENDING_DEACTIVATE"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeFleetsResponse_fleets Prelude.. Lens._Just)
                )
                Prelude.. fleet_state
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "INACTIVE"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeFleetsResponse_fleets Prelude.. Lens._Just)
                )
                Prelude.. fleet_state
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

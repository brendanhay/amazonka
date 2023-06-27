{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AppStream.Waiters
-- Copyright   : (c) 2013-2023 Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Polls 'Amazonka.AppStream.DescribeFleets' every 30 seconds until a successful state is reached. An error is returned after 40 failed checks.
newFleetStarted :: Core.Wait DescribeFleets
newFleetStarted =
  Core.Wait
    { Core.name = "FleetStarted",
      Core.attempts = 40,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "RUNNING"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (describeFleetsResponse_fleets Prelude.. Lens._Just)
                )
                Prelude.. fleet_state
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "STOPPING"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeFleetsResponse_fleets Prelude.. Lens._Just)
                )
                Prelude.. fleet_state
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "STOPPED"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeFleetsResponse_fleets Prelude.. Lens._Just)
                )
                Prelude.. fleet_state
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.AppStream.DescribeFleets' every 30 seconds until a successful state is reached. An error is returned after 40 failed checks.
newFleetStopped :: Core.Wait DescribeFleets
newFleetStopped =
  Core.Wait
    { Core.name = "FleetStopped",
      Core.attempts = 40,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "STOPPED"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (describeFleetsResponse_fleets Prelude.. Lens._Just)
                )
                Prelude.. fleet_state
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "STARTING"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeFleetsResponse_fleets Prelude.. Lens._Just)
                )
                Prelude.. fleet_state
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "RUNNING"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeFleetsResponse_fleets Prelude.. Lens._Just)
                )
                Prelude.. fleet_state
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

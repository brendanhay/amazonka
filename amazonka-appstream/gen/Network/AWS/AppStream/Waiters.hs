{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Waiters where

import Network.AWS.AppStream.DescribeFleets
import Network.AWS.AppStream.Lens
import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Polls 'Network.AWS.AppStream.DescribeFleets' every 30 seconds until a successful state is reached. An error is returned after 40 failed checks.
newFleetStopped :: Core.Wait DescribeFleets
newFleetStopped =
  Core.Wait
    { Core._waitName = "FleetStopped",
      Core._waitAttempts = 40,
      Core._waitDelay = 30,
      Core._waitAcceptors =
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

-- | Polls 'Network.AWS.AppStream.DescribeFleets' every 30 seconds until a successful state is reached. An error is returned after 40 failed checks.
newFleetStarted :: Core.Wait DescribeFleets
newFleetStarted =
  Core.Wait
    { Core._waitName = "FleetStarted",
      Core._waitAttempts = 40,
      Core._waitDelay = 30,
      Core._waitAcceptors =
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

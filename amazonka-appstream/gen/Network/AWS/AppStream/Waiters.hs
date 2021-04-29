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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.AppStream.DescribeFleets' every 30 seconds until a successful state is reached. An error is returned after 40 failed checks.
newFleetStopped :: Waiter.Wait DescribeFleets
newFleetStopped =
  Waiter.Wait
    { Waiter._waitName = "FleetStopped",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 30,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "INACTIVE"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (describeFleetsResponse_fleets Prelude.. Lens._Just)
                )
                Prelude.. fleet_state
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "PENDING_ACTIVATE"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeFleetsResponse_fleets Prelude.. Lens._Just)
                )
                Prelude.. fleet_state
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "ACTIVE"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeFleetsResponse_fleets Prelude.. Lens._Just)
                )
                Prelude.. fleet_state
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.AppStream.DescribeFleets' every 30 seconds until a successful state is reached. An error is returned after 40 failed checks.
newFleetStarted :: Waiter.Wait DescribeFleets
newFleetStarted =
  Waiter.Wait
    { Waiter._waitName = "FleetStarted",
      Waiter._waitAttempts = 40,
      Waiter._waitDelay = 30,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "ACTIVE"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (describeFleetsResponse_fleets Prelude.. Lens._Just)
                )
                Prelude.. fleet_state
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "PENDING_DEACTIVATE"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeFleetsResponse_fleets Prelude.. Lens._Just)
                )
                Prelude.. fleet_state
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "INACTIVE"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeFleetsResponse_fleets Prelude.. Lens._Just)
                )
                Prelude.. fleet_state
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

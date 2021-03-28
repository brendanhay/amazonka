{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Waiters
  (
    -- * FleetStopped
    mkFleetStopped,
    -- * FleetStarted
    mkFleetStarted,
  ) where

import Network.AWS.AppStream.DescribeFleets
import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.AppStream.DescribeFleets' every 30 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkFleetStopped :: Waiter.Wait DescribeFleets
mkFleetStopped
  = Waiter.Wait{Waiter._waitName = "FleetStopped",
                Waiter._waitAttempts = 40, Waiter._waitDelay = 30,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "INACTIVE" Waiter.AcceptSuccess
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"fleets" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"state"),
                   Waiter.matchAny "PENDING_ACTIVATE" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"fleets" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"state"),
                   Waiter.matchAny "ACTIVE" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"fleets" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"state")]}

-- | Polls 'Network.AWS.AppStream.DescribeFleets' every 30 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkFleetStarted :: Waiter.Wait DescribeFleets
mkFleetStarted
  = Waiter.Wait{Waiter._waitName = "FleetStarted",
                Waiter._waitAttempts = 40, Waiter._waitDelay = 30,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "ACTIVE" Waiter.AcceptSuccess
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"fleets" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"state"),
                   Waiter.matchAny "PENDING_DEACTIVATE" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"fleets" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"state"),
                   Waiter.matchAny "INACTIVE" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"fleets" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"state")]}

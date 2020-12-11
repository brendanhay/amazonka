{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Waiters
  ( -- * FleetStopped
    mkFleetStopped,

    -- * FleetStarted
    mkFleetStarted,
  )
where

import Network.AWS.AppStream.DescribeFleets
import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Waiter as Wait

-- | Polls 'Network.AWS.AppStream.DescribeFleets' every 30 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkFleetStopped :: Wait.Wait DescribeFleets
mkFleetStopped =
  Wait.Wait
    { Wait._waitName = "FleetStopped",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 30,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "INACTIVE"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (dfsrsFleets Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. fState
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "PENDING_ACTIVATE"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dfsrsFleets Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. fState
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "ACTIVE"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dfsrsFleets Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. fState
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.AppStream.DescribeFleets' every 30 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkFleetStarted :: Wait.Wait DescribeFleets
mkFleetStarted =
  Wait.Wait
    { Wait._waitName = "FleetStarted",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 30,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "ACTIVE"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (dfsrsFleets Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. fState
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "PENDING_DEACTIVATE"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dfsrsFleets Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. fState
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "INACTIVE"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dfsrsFleets Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. fState
                Lude.. Lens.to Lude.toText
            )
        ]
    }

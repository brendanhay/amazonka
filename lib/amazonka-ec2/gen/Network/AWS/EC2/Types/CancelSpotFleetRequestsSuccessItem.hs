{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CancelSpotFleetRequestsSuccessItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CancelSpotFleetRequestsSuccessItem
  ( CancelSpotFleetRequestsSuccessItem (..),

    -- * Smart constructor
    mkCancelSpotFleetRequestsSuccessItem,

    -- * Lenses
    csfrsiCurrentSpotFleetRequestState,
    csfrsiPreviousSpotFleetRequestState,
    csfrsiSpotFleetRequestId,
  )
where

import qualified Network.AWS.EC2.Types.BatchState as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a Spot Fleet request that was successfully canceled.
--
-- /See:/ 'mkCancelSpotFleetRequestsSuccessItem' smart constructor.
data CancelSpotFleetRequestsSuccessItem = CancelSpotFleetRequestsSuccessItem'
  { -- | The current state of the Spot Fleet request.
    currentSpotFleetRequestState :: Core.Maybe Types.BatchState,
    -- | The previous state of the Spot Fleet request.
    previousSpotFleetRequestState :: Core.Maybe Types.BatchState,
    -- | The ID of the Spot Fleet request.
    spotFleetRequestId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelSpotFleetRequestsSuccessItem' value with any optional fields omitted.
mkCancelSpotFleetRequestsSuccessItem ::
  CancelSpotFleetRequestsSuccessItem
mkCancelSpotFleetRequestsSuccessItem =
  CancelSpotFleetRequestsSuccessItem'
    { currentSpotFleetRequestState =
        Core.Nothing,
      previousSpotFleetRequestState = Core.Nothing,
      spotFleetRequestId = Core.Nothing
    }

-- | The current state of the Spot Fleet request.
--
-- /Note:/ Consider using 'currentSpotFleetRequestState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfrsiCurrentSpotFleetRequestState :: Lens.Lens' CancelSpotFleetRequestsSuccessItem (Core.Maybe Types.BatchState)
csfrsiCurrentSpotFleetRequestState = Lens.field @"currentSpotFleetRequestState"
{-# DEPRECATED csfrsiCurrentSpotFleetRequestState "Use generic-lens or generic-optics with 'currentSpotFleetRequestState' instead." #-}

-- | The previous state of the Spot Fleet request.
--
-- /Note:/ Consider using 'previousSpotFleetRequestState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfrsiPreviousSpotFleetRequestState :: Lens.Lens' CancelSpotFleetRequestsSuccessItem (Core.Maybe Types.BatchState)
csfrsiPreviousSpotFleetRequestState = Lens.field @"previousSpotFleetRequestState"
{-# DEPRECATED csfrsiPreviousSpotFleetRequestState "Use generic-lens or generic-optics with 'previousSpotFleetRequestState' instead." #-}

-- | The ID of the Spot Fleet request.
--
-- /Note:/ Consider using 'spotFleetRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfrsiSpotFleetRequestId :: Lens.Lens' CancelSpotFleetRequestsSuccessItem (Core.Maybe Types.String)
csfrsiSpotFleetRequestId = Lens.field @"spotFleetRequestId"
{-# DEPRECATED csfrsiSpotFleetRequestId "Use generic-lens or generic-optics with 'spotFleetRequestId' instead." #-}

instance Core.FromXML CancelSpotFleetRequestsSuccessItem where
  parseXML x =
    CancelSpotFleetRequestsSuccessItem'
      Core.<$> (x Core..@? "currentSpotFleetRequestState")
      Core.<*> (x Core..@? "previousSpotFleetRequestState")
      Core.<*> (x Core..@? "spotFleetRequestId")

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CancelSpotFleetRequestsErrorItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CancelSpotFleetRequestsErrorItem
  ( CancelSpotFleetRequestsErrorItem (..),

    -- * Smart constructor
    mkCancelSpotFleetRequestsErrorItem,

    -- * Lenses
    csfreiError,
    csfreiSpotFleetRequestId,
  )
where

import qualified Network.AWS.EC2.Types.CancelSpotFleetRequestsError as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a Spot Fleet request that was not successfully canceled.
--
-- /See:/ 'mkCancelSpotFleetRequestsErrorItem' smart constructor.
data CancelSpotFleetRequestsErrorItem = CancelSpotFleetRequestsErrorItem'
  { -- | The error.
    error :: Core.Maybe Types.CancelSpotFleetRequestsError,
    -- | The ID of the Spot Fleet request.
    spotFleetRequestId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelSpotFleetRequestsErrorItem' value with any optional fields omitted.
mkCancelSpotFleetRequestsErrorItem ::
  CancelSpotFleetRequestsErrorItem
mkCancelSpotFleetRequestsErrorItem =
  CancelSpotFleetRequestsErrorItem'
    { error = Core.Nothing,
      spotFleetRequestId = Core.Nothing
    }

-- | The error.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfreiError :: Lens.Lens' CancelSpotFleetRequestsErrorItem (Core.Maybe Types.CancelSpotFleetRequestsError)
csfreiError = Lens.field @"error"
{-# DEPRECATED csfreiError "Use generic-lens or generic-optics with 'error' instead." #-}

-- | The ID of the Spot Fleet request.
--
-- /Note:/ Consider using 'spotFleetRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfreiSpotFleetRequestId :: Lens.Lens' CancelSpotFleetRequestsErrorItem (Core.Maybe Types.String)
csfreiSpotFleetRequestId = Lens.field @"spotFleetRequestId"
{-# DEPRECATED csfreiSpotFleetRequestId "Use generic-lens or generic-optics with 'spotFleetRequestId' instead." #-}

instance Core.FromXML CancelSpotFleetRequestsErrorItem where
  parseXML x =
    CancelSpotFleetRequestsErrorItem'
      Core.<$> (x Core..@? "error") Core.<*> (x Core..@? "spotFleetRequestId")

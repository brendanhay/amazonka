{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CancelledSpotInstanceRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CancelledSpotInstanceRequest
  ( CancelledSpotInstanceRequest (..),

    -- * Smart constructor
    mkCancelledSpotInstanceRequest,

    -- * Lenses
    csirSpotInstanceRequestId,
    csirState,
  )
where

import qualified Network.AWS.EC2.Types.CancelSpotInstanceRequestState as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a request to cancel a Spot Instance.
--
-- /See:/ 'mkCancelledSpotInstanceRequest' smart constructor.
data CancelledSpotInstanceRequest = CancelledSpotInstanceRequest'
  { -- | The ID of the Spot Instance request.
    spotInstanceRequestId :: Core.Maybe Types.String,
    -- | The state of the Spot Instance request.
    state :: Core.Maybe Types.CancelSpotInstanceRequestState
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelledSpotInstanceRequest' value with any optional fields omitted.
mkCancelledSpotInstanceRequest ::
  CancelledSpotInstanceRequest
mkCancelledSpotInstanceRequest =
  CancelledSpotInstanceRequest'
    { spotInstanceRequestId =
        Core.Nothing,
      state = Core.Nothing
    }

-- | The ID of the Spot Instance request.
--
-- /Note:/ Consider using 'spotInstanceRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csirSpotInstanceRequestId :: Lens.Lens' CancelledSpotInstanceRequest (Core.Maybe Types.String)
csirSpotInstanceRequestId = Lens.field @"spotInstanceRequestId"
{-# DEPRECATED csirSpotInstanceRequestId "Use generic-lens or generic-optics with 'spotInstanceRequestId' instead." #-}

-- | The state of the Spot Instance request.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csirState :: Lens.Lens' CancelledSpotInstanceRequest (Core.Maybe Types.CancelSpotInstanceRequestState)
csirState = Lens.field @"state"
{-# DEPRECATED csirState "Use generic-lens or generic-optics with 'state' instead." #-}

instance Core.FromXML CancelledSpotInstanceRequest where
  parseXML x =
    CancelledSpotInstanceRequest'
      Core.<$> (x Core..@? "spotInstanceRequestId") Core.<*> (x Core..@? "state")

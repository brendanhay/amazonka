{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.PendingAggregationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.PendingAggregationRequest
  ( PendingAggregationRequest (..),

    -- * Smart constructor
    mkPendingAggregationRequest,

    -- * Lenses
    parRequesterAccountId,
    parRequesterAwsRegion,
  )
where

import qualified Network.AWS.Config.Types.RequesterAccountId as Types
import qualified Network.AWS.Config.Types.RequesterAwsRegion as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object that represents the account ID and region of an aggregator account that is requesting authorization but is not yet authorized.
--
-- /See:/ 'mkPendingAggregationRequest' smart constructor.
data PendingAggregationRequest = PendingAggregationRequest'
  { -- | The 12-digit account ID of the account requesting to aggregate data.
    requesterAccountId :: Core.Maybe Types.RequesterAccountId,
    -- | The region requesting to aggregate data.
    requesterAwsRegion :: Core.Maybe Types.RequesterAwsRegion
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PendingAggregationRequest' value with any optional fields omitted.
mkPendingAggregationRequest ::
  PendingAggregationRequest
mkPendingAggregationRequest =
  PendingAggregationRequest'
    { requesterAccountId = Core.Nothing,
      requesterAwsRegion = Core.Nothing
    }

-- | The 12-digit account ID of the account requesting to aggregate data.
--
-- /Note:/ Consider using 'requesterAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parRequesterAccountId :: Lens.Lens' PendingAggregationRequest (Core.Maybe Types.RequesterAccountId)
parRequesterAccountId = Lens.field @"requesterAccountId"
{-# DEPRECATED parRequesterAccountId "Use generic-lens or generic-optics with 'requesterAccountId' instead." #-}

-- | The region requesting to aggregate data.
--
-- /Note:/ Consider using 'requesterAwsRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
parRequesterAwsRegion :: Lens.Lens' PendingAggregationRequest (Core.Maybe Types.RequesterAwsRegion)
parRequesterAwsRegion = Lens.field @"requesterAwsRegion"
{-# DEPRECATED parRequesterAwsRegion "Use generic-lens or generic-optics with 'requesterAwsRegion' instead." #-}

instance Core.FromJSON PendingAggregationRequest where
  parseJSON =
    Core.withObject "PendingAggregationRequest" Core.$
      \x ->
        PendingAggregationRequest'
          Core.<$> (x Core..:? "RequesterAccountId")
          Core.<*> (x Core..:? "RequesterAwsRegion")

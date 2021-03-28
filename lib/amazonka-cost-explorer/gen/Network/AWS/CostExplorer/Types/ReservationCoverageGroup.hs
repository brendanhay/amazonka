{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.ReservationCoverageGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.ReservationCoverageGroup
  ( ReservationCoverageGroup (..)
  -- * Smart constructor
  , mkReservationCoverageGroup
  -- * Lenses
  , rcgAttributes
  , rcgCoverage
  ) where

import qualified Network.AWS.CostExplorer.Types.AttributeType as Types
import qualified Network.AWS.CostExplorer.Types.AttributeValue as Types
import qualified Network.AWS.CostExplorer.Types.Coverage as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A group of reservations that share a set of attributes.
--
-- /See:/ 'mkReservationCoverageGroup' smart constructor.
data ReservationCoverageGroup = ReservationCoverageGroup'
  { attributes :: Core.Maybe (Core.HashMap Types.AttributeType Types.AttributeValue)
    -- ^ The attributes for this group of reservations.
  , coverage :: Core.Maybe Types.Coverage
    -- ^ How much instance usage this group of reservations covered.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReservationCoverageGroup' value with any optional fields omitted.
mkReservationCoverageGroup
    :: ReservationCoverageGroup
mkReservationCoverageGroup
  = ReservationCoverageGroup'{attributes = Core.Nothing,
                              coverage = Core.Nothing}

-- | The attributes for this group of reservations.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcgAttributes :: Lens.Lens' ReservationCoverageGroup (Core.Maybe (Core.HashMap Types.AttributeType Types.AttributeValue))
rcgAttributes = Lens.field @"attributes"
{-# INLINEABLE rcgAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | How much instance usage this group of reservations covered.
--
-- /Note:/ Consider using 'coverage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcgCoverage :: Lens.Lens' ReservationCoverageGroup (Core.Maybe Types.Coverage)
rcgCoverage = Lens.field @"coverage"
{-# INLINEABLE rcgCoverage #-}
{-# DEPRECATED coverage "Use generic-lens or generic-optics with 'coverage' instead"  #-}

instance Core.FromJSON ReservationCoverageGroup where
        parseJSON
          = Core.withObject "ReservationCoverageGroup" Core.$
              \ x ->
                ReservationCoverageGroup' Core.<$>
                  (x Core..:? "Attributes") Core.<*> x Core..:? "Coverage"

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AssociationOverview
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.AssociationOverview
  ( AssociationOverview (..)
  -- * Smart constructor
  , mkAssociationOverview
  -- * Lenses
  , aoAssociationStatusAggregatedCount
  , aoDetailedStatus
  , aoStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.DetailedStatus as Types
import qualified Network.AWS.SSM.Types.Status as Types
import qualified Network.AWS.SSM.Types.StatusName as Types

-- | Information about the association.
--
-- /See:/ 'mkAssociationOverview' smart constructor.
data AssociationOverview = AssociationOverview'
  { associationStatusAggregatedCount :: Core.Maybe (Core.HashMap Types.StatusName Core.Int)
    -- ^ Returns the number of targets for the association status. For example, if you created an association with two instances, and one of them was successful, this would return the count of instances by status.
  , detailedStatus :: Core.Maybe Types.DetailedStatus
    -- ^ A detailed status of the association.
  , status :: Core.Maybe Types.Status
    -- ^ The status of the association. Status can be: Pending, Success, or Failed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociationOverview' value with any optional fields omitted.
mkAssociationOverview
    :: AssociationOverview
mkAssociationOverview
  = AssociationOverview'{associationStatusAggregatedCount =
                           Core.Nothing,
                         detailedStatus = Core.Nothing, status = Core.Nothing}

-- | Returns the number of targets for the association status. For example, if you created an association with two instances, and one of them was successful, this would return the count of instances by status.
--
-- /Note:/ Consider using 'associationStatusAggregatedCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoAssociationStatusAggregatedCount :: Lens.Lens' AssociationOverview (Core.Maybe (Core.HashMap Types.StatusName Core.Int))
aoAssociationStatusAggregatedCount = Lens.field @"associationStatusAggregatedCount"
{-# INLINEABLE aoAssociationStatusAggregatedCount #-}
{-# DEPRECATED associationStatusAggregatedCount "Use generic-lens or generic-optics with 'associationStatusAggregatedCount' instead"  #-}

-- | A detailed status of the association.
--
-- /Note:/ Consider using 'detailedStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoDetailedStatus :: Lens.Lens' AssociationOverview (Core.Maybe Types.DetailedStatus)
aoDetailedStatus = Lens.field @"detailedStatus"
{-# INLINEABLE aoDetailedStatus #-}
{-# DEPRECATED detailedStatus "Use generic-lens or generic-optics with 'detailedStatus' instead"  #-}

-- | The status of the association. Status can be: Pending, Success, or Failed.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoStatus :: Lens.Lens' AssociationOverview (Core.Maybe Types.Status)
aoStatus = Lens.field @"status"
{-# INLINEABLE aoStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON AssociationOverview where
        parseJSON
          = Core.withObject "AssociationOverview" Core.$
              \ x ->
                AssociationOverview' Core.<$>
                  (x Core..:? "AssociationStatusAggregatedCount") Core.<*>
                    x Core..:? "DetailedStatus"
                    Core.<*> x Core..:? "Status"

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InstanceAggregatedAssociationOverview
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InstanceAggregatedAssociationOverview
  ( InstanceAggregatedAssociationOverview (..),

    -- * Smart constructor
    mkInstanceAggregatedAssociationOverview,

    -- * Lenses
    iaaoDetailedStatus,
    iaaoInstanceAssociationStatusAggregatedCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.StatusName as Types

-- | Status information about the aggregated associations.
--
-- /See:/ 'mkInstanceAggregatedAssociationOverview' smart constructor.
data InstanceAggregatedAssociationOverview = InstanceAggregatedAssociationOverview'
  { -- | Detailed status information about the aggregated associations.
    detailedStatus :: Core.Maybe Types.StatusName,
    -- | The number of associations for the instance(s).
    instanceAssociationStatusAggregatedCount :: Core.Maybe (Core.HashMap Types.StatusName Core.Int)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceAggregatedAssociationOverview' value with any optional fields omitted.
mkInstanceAggregatedAssociationOverview ::
  InstanceAggregatedAssociationOverview
mkInstanceAggregatedAssociationOverview =
  InstanceAggregatedAssociationOverview'
    { detailedStatus =
        Core.Nothing,
      instanceAssociationStatusAggregatedCount = Core.Nothing
    }

-- | Detailed status information about the aggregated associations.
--
-- /Note:/ Consider using 'detailedStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaaoDetailedStatus :: Lens.Lens' InstanceAggregatedAssociationOverview (Core.Maybe Types.StatusName)
iaaoDetailedStatus = Lens.field @"detailedStatus"
{-# DEPRECATED iaaoDetailedStatus "Use generic-lens or generic-optics with 'detailedStatus' instead." #-}

-- | The number of associations for the instance(s).
--
-- /Note:/ Consider using 'instanceAssociationStatusAggregatedCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaaoInstanceAssociationStatusAggregatedCount :: Lens.Lens' InstanceAggregatedAssociationOverview (Core.Maybe (Core.HashMap Types.StatusName Core.Int))
iaaoInstanceAssociationStatusAggregatedCount = Lens.field @"instanceAssociationStatusAggregatedCount"
{-# DEPRECATED iaaoInstanceAssociationStatusAggregatedCount "Use generic-lens or generic-optics with 'instanceAssociationStatusAggregatedCount' instead." #-}

instance Core.FromJSON InstanceAggregatedAssociationOverview where
  parseJSON =
    Core.withObject "InstanceAggregatedAssociationOverview" Core.$
      \x ->
        InstanceAggregatedAssociationOverview'
          Core.<$> (x Core..:? "DetailedStatus")
          Core.<*> (x Core..:? "InstanceAssociationStatusAggregatedCount")

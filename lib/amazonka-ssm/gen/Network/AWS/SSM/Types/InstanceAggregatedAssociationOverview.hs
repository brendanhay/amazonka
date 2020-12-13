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
import qualified Network.AWS.Prelude as Lude

-- | Status information about the aggregated associations.
--
-- /See:/ 'mkInstanceAggregatedAssociationOverview' smart constructor.
data InstanceAggregatedAssociationOverview = InstanceAggregatedAssociationOverview'
  { -- | Detailed status information about the aggregated associations.
    detailedStatus :: Lude.Maybe Lude.Text,
    -- | The number of associations for the instance(s).
    instanceAssociationStatusAggregatedCount :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Int))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceAggregatedAssociationOverview' with the minimum fields required to make a request.
--
-- * 'detailedStatus' - Detailed status information about the aggregated associations.
-- * 'instanceAssociationStatusAggregatedCount' - The number of associations for the instance(s).
mkInstanceAggregatedAssociationOverview ::
  InstanceAggregatedAssociationOverview
mkInstanceAggregatedAssociationOverview =
  InstanceAggregatedAssociationOverview'
    { detailedStatus =
        Lude.Nothing,
      instanceAssociationStatusAggregatedCount = Lude.Nothing
    }

-- | Detailed status information about the aggregated associations.
--
-- /Note:/ Consider using 'detailedStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaaoDetailedStatus :: Lens.Lens' InstanceAggregatedAssociationOverview (Lude.Maybe Lude.Text)
iaaoDetailedStatus = Lens.lens (detailedStatus :: InstanceAggregatedAssociationOverview -> Lude.Maybe Lude.Text) (\s a -> s {detailedStatus = a} :: InstanceAggregatedAssociationOverview)
{-# DEPRECATED iaaoDetailedStatus "Use generic-lens or generic-optics with 'detailedStatus' instead." #-}

-- | The number of associations for the instance(s).
--
-- /Note:/ Consider using 'instanceAssociationStatusAggregatedCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaaoInstanceAssociationStatusAggregatedCount :: Lens.Lens' InstanceAggregatedAssociationOverview (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Int)))
iaaoInstanceAssociationStatusAggregatedCount = Lens.lens (instanceAssociationStatusAggregatedCount :: InstanceAggregatedAssociationOverview -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Int))) (\s a -> s {instanceAssociationStatusAggregatedCount = a} :: InstanceAggregatedAssociationOverview)
{-# DEPRECATED iaaoInstanceAssociationStatusAggregatedCount "Use generic-lens or generic-optics with 'instanceAssociationStatusAggregatedCount' instead." #-}

instance Lude.FromJSON InstanceAggregatedAssociationOverview where
  parseJSON =
    Lude.withObject
      "InstanceAggregatedAssociationOverview"
      ( \x ->
          InstanceAggregatedAssociationOverview'
            Lude.<$> (x Lude..:? "DetailedStatus")
            Lude.<*> ( x Lude..:? "InstanceAssociationStatusAggregatedCount"
                         Lude..!= Lude.mempty
                     )
      )

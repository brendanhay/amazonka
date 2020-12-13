{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AssociationOverview
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationOverview
  ( AssociationOverview (..),

    -- * Smart constructor
    mkAssociationOverview,

    -- * Lenses
    aoDetailedStatus,
    aoStatus,
    aoAssociationStatusAggregatedCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the association.
--
-- /See:/ 'mkAssociationOverview' smart constructor.
data AssociationOverview = AssociationOverview'
  { -- | A detailed status of the association.
    detailedStatus :: Lude.Maybe Lude.Text,
    -- | The status of the association. Status can be: Pending, Success, or Failed.
    status :: Lude.Maybe Lude.Text,
    -- | Returns the number of targets for the association status. For example, if you created an association with two instances, and one of them was successful, this would return the count of instances by status.
    associationStatusAggregatedCount :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Int))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociationOverview' with the minimum fields required to make a request.
--
-- * 'detailedStatus' - A detailed status of the association.
-- * 'status' - The status of the association. Status can be: Pending, Success, or Failed.
-- * 'associationStatusAggregatedCount' - Returns the number of targets for the association status. For example, if you created an association with two instances, and one of them was successful, this would return the count of instances by status.
mkAssociationOverview ::
  AssociationOverview
mkAssociationOverview =
  AssociationOverview'
    { detailedStatus = Lude.Nothing,
      status = Lude.Nothing,
      associationStatusAggregatedCount = Lude.Nothing
    }

-- | A detailed status of the association.
--
-- /Note:/ Consider using 'detailedStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoDetailedStatus :: Lens.Lens' AssociationOverview (Lude.Maybe Lude.Text)
aoDetailedStatus = Lens.lens (detailedStatus :: AssociationOverview -> Lude.Maybe Lude.Text) (\s a -> s {detailedStatus = a} :: AssociationOverview)
{-# DEPRECATED aoDetailedStatus "Use generic-lens or generic-optics with 'detailedStatus' instead." #-}

-- | The status of the association. Status can be: Pending, Success, or Failed.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoStatus :: Lens.Lens' AssociationOverview (Lude.Maybe Lude.Text)
aoStatus = Lens.lens (status :: AssociationOverview -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: AssociationOverview)
{-# DEPRECATED aoStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Returns the number of targets for the association status. For example, if you created an association with two instances, and one of them was successful, this would return the count of instances by status.
--
-- /Note:/ Consider using 'associationStatusAggregatedCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoAssociationStatusAggregatedCount :: Lens.Lens' AssociationOverview (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Int)))
aoAssociationStatusAggregatedCount = Lens.lens (associationStatusAggregatedCount :: AssociationOverview -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Int))) (\s a -> s {associationStatusAggregatedCount = a} :: AssociationOverview)
{-# DEPRECATED aoAssociationStatusAggregatedCount "Use generic-lens or generic-optics with 'associationStatusAggregatedCount' instead." #-}

instance Lude.FromJSON AssociationOverview where
  parseJSON =
    Lude.withObject
      "AssociationOverview"
      ( \x ->
          AssociationOverview'
            Lude.<$> (x Lude..:? "DetailedStatus")
            Lude.<*> (x Lude..:? "Status")
            Lude.<*> ( x Lude..:? "AssociationStatusAggregatedCount"
                         Lude..!= Lude.mempty
                     )
      )

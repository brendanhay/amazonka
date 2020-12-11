-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SavingsPlansDetails
  ( SavingsPlansDetails (..),

    -- * Smart constructor
    mkSavingsPlansDetails,

    -- * Lenses
    spdInstanceFamily,
    spdOfferingId,
    spdRegion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Attribute details on a specific Savings Plan.
--
-- /See:/ 'mkSavingsPlansDetails' smart constructor.
data SavingsPlansDetails = SavingsPlansDetails'
  { instanceFamily ::
      Lude.Maybe Lude.Text,
    offeringId :: Lude.Maybe Lude.Text,
    region :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SavingsPlansDetails' with the minimum fields required to make a request.
--
-- * 'instanceFamily' - A group of instance types that Savings Plans applies to.
-- * 'offeringId' - The unique ID used to distinguish Savings Plans from one another.
-- * 'region' - A collection of AWS resources in a geographic area. Each AWS Region is isolated and independent of the other Regions.
mkSavingsPlansDetails ::
  SavingsPlansDetails
mkSavingsPlansDetails =
  SavingsPlansDetails'
    { instanceFamily = Lude.Nothing,
      offeringId = Lude.Nothing,
      region = Lude.Nothing
    }

-- | A group of instance types that Savings Plans applies to.
--
-- /Note:/ Consider using 'instanceFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spdInstanceFamily :: Lens.Lens' SavingsPlansDetails (Lude.Maybe Lude.Text)
spdInstanceFamily = Lens.lens (instanceFamily :: SavingsPlansDetails -> Lude.Maybe Lude.Text) (\s a -> s {instanceFamily = a} :: SavingsPlansDetails)
{-# DEPRECATED spdInstanceFamily "Use generic-lens or generic-optics with 'instanceFamily' instead." #-}

-- | The unique ID used to distinguish Savings Plans from one another.
--
-- /Note:/ Consider using 'offeringId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spdOfferingId :: Lens.Lens' SavingsPlansDetails (Lude.Maybe Lude.Text)
spdOfferingId = Lens.lens (offeringId :: SavingsPlansDetails -> Lude.Maybe Lude.Text) (\s a -> s {offeringId = a} :: SavingsPlansDetails)
{-# DEPRECATED spdOfferingId "Use generic-lens or generic-optics with 'offeringId' instead." #-}

-- | A collection of AWS resources in a geographic area. Each AWS Region is isolated and independent of the other Regions.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spdRegion :: Lens.Lens' SavingsPlansDetails (Lude.Maybe Lude.Text)
spdRegion = Lens.lens (region :: SavingsPlansDetails -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: SavingsPlansDetails)
{-# DEPRECATED spdRegion "Use generic-lens or generic-optics with 'region' instead." #-}

instance Lude.FromJSON SavingsPlansDetails where
  parseJSON =
    Lude.withObject
      "SavingsPlansDetails"
      ( \x ->
          SavingsPlansDetails'
            Lude.<$> (x Lude..:? "InstanceFamily")
            Lude.<*> (x Lude..:? "OfferingId")
            Lude.<*> (x Lude..:? "Region")
      )

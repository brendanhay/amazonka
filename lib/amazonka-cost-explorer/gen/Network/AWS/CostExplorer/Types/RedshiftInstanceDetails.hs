-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.RedshiftInstanceDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.RedshiftInstanceDetails
  ( RedshiftInstanceDetails (..),

    -- * Smart constructor
    mkRedshiftInstanceDetails,

    -- * Lenses
    rCurrentGeneration,
    rFamily,
    rSizeFlexEligible,
    rRegion,
    rNodeType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details about the Amazon Redshift instances that AWS recommends that you purchase.
--
-- /See:/ 'mkRedshiftInstanceDetails' smart constructor.
data RedshiftInstanceDetails = RedshiftInstanceDetails'
  { currentGeneration ::
      Lude.Maybe Lude.Bool,
    family :: Lude.Maybe Lude.Text,
    sizeFlexEligible :: Lude.Maybe Lude.Bool,
    region :: Lude.Maybe Lude.Text,
    nodeType :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RedshiftInstanceDetails' with the minimum fields required to make a request.
--
-- * 'currentGeneration' - Whether the recommendation is for a current-generation instance.
-- * 'family' - The instance family of the recommended reservation.
-- * 'nodeType' - The type of node that AWS recommends.
-- * 'region' - The AWS Region of the recommended reservation.
-- * 'sizeFlexEligible' - Whether the recommended reservation is size flexible.
mkRedshiftInstanceDetails ::
  RedshiftInstanceDetails
mkRedshiftInstanceDetails =
  RedshiftInstanceDetails'
    { currentGeneration = Lude.Nothing,
      family = Lude.Nothing,
      sizeFlexEligible = Lude.Nothing,
      region = Lude.Nothing,
      nodeType = Lude.Nothing
    }

-- | Whether the recommendation is for a current-generation instance.
--
-- /Note:/ Consider using 'currentGeneration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rCurrentGeneration :: Lens.Lens' RedshiftInstanceDetails (Lude.Maybe Lude.Bool)
rCurrentGeneration = Lens.lens (currentGeneration :: RedshiftInstanceDetails -> Lude.Maybe Lude.Bool) (\s a -> s {currentGeneration = a} :: RedshiftInstanceDetails)
{-# DEPRECATED rCurrentGeneration "Use generic-lens or generic-optics with 'currentGeneration' instead." #-}

-- | The instance family of the recommended reservation.
--
-- /Note:/ Consider using 'family' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rFamily :: Lens.Lens' RedshiftInstanceDetails (Lude.Maybe Lude.Text)
rFamily = Lens.lens (family :: RedshiftInstanceDetails -> Lude.Maybe Lude.Text) (\s a -> s {family = a} :: RedshiftInstanceDetails)
{-# DEPRECATED rFamily "Use generic-lens or generic-optics with 'family' instead." #-}

-- | Whether the recommended reservation is size flexible.
--
-- /Note:/ Consider using 'sizeFlexEligible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rSizeFlexEligible :: Lens.Lens' RedshiftInstanceDetails (Lude.Maybe Lude.Bool)
rSizeFlexEligible = Lens.lens (sizeFlexEligible :: RedshiftInstanceDetails -> Lude.Maybe Lude.Bool) (\s a -> s {sizeFlexEligible = a} :: RedshiftInstanceDetails)
{-# DEPRECATED rSizeFlexEligible "Use generic-lens or generic-optics with 'sizeFlexEligible' instead." #-}

-- | The AWS Region of the recommended reservation.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRegion :: Lens.Lens' RedshiftInstanceDetails (Lude.Maybe Lude.Text)
rRegion = Lens.lens (region :: RedshiftInstanceDetails -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: RedshiftInstanceDetails)
{-# DEPRECATED rRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The type of node that AWS recommends.
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rNodeType :: Lens.Lens' RedshiftInstanceDetails (Lude.Maybe Lude.Text)
rNodeType = Lens.lens (nodeType :: RedshiftInstanceDetails -> Lude.Maybe Lude.Text) (\s a -> s {nodeType = a} :: RedshiftInstanceDetails)
{-# DEPRECATED rNodeType "Use generic-lens or generic-optics with 'nodeType' instead." #-}

instance Lude.FromJSON RedshiftInstanceDetails where
  parseJSON =
    Lude.withObject
      "RedshiftInstanceDetails"
      ( \x ->
          RedshiftInstanceDetails'
            Lude.<$> (x Lude..:? "CurrentGeneration")
            Lude.<*> (x Lude..:? "Family")
            Lude.<*> (x Lude..:? "SizeFlexEligible")
            Lude.<*> (x Lude..:? "Region")
            Lude.<*> (x Lude..:? "NodeType")
      )

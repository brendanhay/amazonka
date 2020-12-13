{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.ElastiCacheInstanceDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ElastiCacheInstanceDetails
  ( ElastiCacheInstanceDetails (..),

    -- * Smart constructor
    mkElastiCacheInstanceDetails,

    -- * Lenses
    ecidCurrentGeneration,
    ecidProductDescription,
    ecidFamily,
    ecidSizeFlexEligible,
    ecidRegion,
    ecidNodeType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details about the Amazon ElastiCache instances that AWS recommends that you purchase.
--
-- /See:/ 'mkElastiCacheInstanceDetails' smart constructor.
data ElastiCacheInstanceDetails = ElastiCacheInstanceDetails'
  { -- | Whether the recommendation is for a current generation instance.
    currentGeneration :: Lude.Maybe Lude.Bool,
    -- | The description of the recommended reservation.
    productDescription :: Lude.Maybe Lude.Text,
    -- | The instance family of the recommended reservation.
    family :: Lude.Maybe Lude.Text,
    -- | Whether the recommended reservation is size flexible.
    sizeFlexEligible :: Lude.Maybe Lude.Bool,
    -- | The AWS Region of the recommended reservation.
    region :: Lude.Maybe Lude.Text,
    -- | The type of node that AWS recommends.
    nodeType :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ElastiCacheInstanceDetails' with the minimum fields required to make a request.
--
-- * 'currentGeneration' - Whether the recommendation is for a current generation instance.
-- * 'productDescription' - The description of the recommended reservation.
-- * 'family' - The instance family of the recommended reservation.
-- * 'sizeFlexEligible' - Whether the recommended reservation is size flexible.
-- * 'region' - The AWS Region of the recommended reservation.
-- * 'nodeType' - The type of node that AWS recommends.
mkElastiCacheInstanceDetails ::
  ElastiCacheInstanceDetails
mkElastiCacheInstanceDetails =
  ElastiCacheInstanceDetails'
    { currentGeneration = Lude.Nothing,
      productDescription = Lude.Nothing,
      family = Lude.Nothing,
      sizeFlexEligible = Lude.Nothing,
      region = Lude.Nothing,
      nodeType = Lude.Nothing
    }

-- | Whether the recommendation is for a current generation instance.
--
-- /Note:/ Consider using 'currentGeneration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecidCurrentGeneration :: Lens.Lens' ElastiCacheInstanceDetails (Lude.Maybe Lude.Bool)
ecidCurrentGeneration = Lens.lens (currentGeneration :: ElastiCacheInstanceDetails -> Lude.Maybe Lude.Bool) (\s a -> s {currentGeneration = a} :: ElastiCacheInstanceDetails)
{-# DEPRECATED ecidCurrentGeneration "Use generic-lens or generic-optics with 'currentGeneration' instead." #-}

-- | The description of the recommended reservation.
--
-- /Note:/ Consider using 'productDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecidProductDescription :: Lens.Lens' ElastiCacheInstanceDetails (Lude.Maybe Lude.Text)
ecidProductDescription = Lens.lens (productDescription :: ElastiCacheInstanceDetails -> Lude.Maybe Lude.Text) (\s a -> s {productDescription = a} :: ElastiCacheInstanceDetails)
{-# DEPRECATED ecidProductDescription "Use generic-lens or generic-optics with 'productDescription' instead." #-}

-- | The instance family of the recommended reservation.
--
-- /Note:/ Consider using 'family' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecidFamily :: Lens.Lens' ElastiCacheInstanceDetails (Lude.Maybe Lude.Text)
ecidFamily = Lens.lens (family :: ElastiCacheInstanceDetails -> Lude.Maybe Lude.Text) (\s a -> s {family = a} :: ElastiCacheInstanceDetails)
{-# DEPRECATED ecidFamily "Use generic-lens or generic-optics with 'family' instead." #-}

-- | Whether the recommended reservation is size flexible.
--
-- /Note:/ Consider using 'sizeFlexEligible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecidSizeFlexEligible :: Lens.Lens' ElastiCacheInstanceDetails (Lude.Maybe Lude.Bool)
ecidSizeFlexEligible = Lens.lens (sizeFlexEligible :: ElastiCacheInstanceDetails -> Lude.Maybe Lude.Bool) (\s a -> s {sizeFlexEligible = a} :: ElastiCacheInstanceDetails)
{-# DEPRECATED ecidSizeFlexEligible "Use generic-lens or generic-optics with 'sizeFlexEligible' instead." #-}

-- | The AWS Region of the recommended reservation.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecidRegion :: Lens.Lens' ElastiCacheInstanceDetails (Lude.Maybe Lude.Text)
ecidRegion = Lens.lens (region :: ElastiCacheInstanceDetails -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: ElastiCacheInstanceDetails)
{-# DEPRECATED ecidRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The type of node that AWS recommends.
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecidNodeType :: Lens.Lens' ElastiCacheInstanceDetails (Lude.Maybe Lude.Text)
ecidNodeType = Lens.lens (nodeType :: ElastiCacheInstanceDetails -> Lude.Maybe Lude.Text) (\s a -> s {nodeType = a} :: ElastiCacheInstanceDetails)
{-# DEPRECATED ecidNodeType "Use generic-lens or generic-optics with 'nodeType' instead." #-}

instance Lude.FromJSON ElastiCacheInstanceDetails where
  parseJSON =
    Lude.withObject
      "ElastiCacheInstanceDetails"
      ( \x ->
          ElastiCacheInstanceDetails'
            Lude.<$> (x Lude..:? "CurrentGeneration")
            Lude.<*> (x Lude..:? "ProductDescription")
            Lude.<*> (x Lude..:? "Family")
            Lude.<*> (x Lude..:? "SizeFlexEligible")
            Lude.<*> (x Lude..:? "Region")
            Lude.<*> (x Lude..:? "NodeType")
      )

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
    eCurrentGeneration,
    eFamily,
    eNodeType,
    eProductDescription,
    eRegion,
    eSizeFlexEligible,
  )
where

import qualified Network.AWS.CostExplorer.Types.Family as Types
import qualified Network.AWS.CostExplorer.Types.NodeType as Types
import qualified Network.AWS.CostExplorer.Types.ProductDescription as Types
import qualified Network.AWS.CostExplorer.Types.Region as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details about the Amazon ElastiCache instances that AWS recommends that you purchase.
--
-- /See:/ 'mkElastiCacheInstanceDetails' smart constructor.
data ElastiCacheInstanceDetails = ElastiCacheInstanceDetails'
  { -- | Whether the recommendation is for a current generation instance.
    currentGeneration :: Core.Maybe Core.Bool,
    -- | The instance family of the recommended reservation.
    family :: Core.Maybe Types.Family,
    -- | The type of node that AWS recommends.
    nodeType :: Core.Maybe Types.NodeType,
    -- | The description of the recommended reservation.
    productDescription :: Core.Maybe Types.ProductDescription,
    -- | The AWS Region of the recommended reservation.
    region :: Core.Maybe Types.Region,
    -- | Whether the recommended reservation is size flexible.
    sizeFlexEligible :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ElastiCacheInstanceDetails' value with any optional fields omitted.
mkElastiCacheInstanceDetails ::
  ElastiCacheInstanceDetails
mkElastiCacheInstanceDetails =
  ElastiCacheInstanceDetails'
    { currentGeneration = Core.Nothing,
      family = Core.Nothing,
      nodeType = Core.Nothing,
      productDescription = Core.Nothing,
      region = Core.Nothing,
      sizeFlexEligible = Core.Nothing
    }

-- | Whether the recommendation is for a current generation instance.
--
-- /Note:/ Consider using 'currentGeneration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eCurrentGeneration :: Lens.Lens' ElastiCacheInstanceDetails (Core.Maybe Core.Bool)
eCurrentGeneration = Lens.field @"currentGeneration"
{-# DEPRECATED eCurrentGeneration "Use generic-lens or generic-optics with 'currentGeneration' instead." #-}

-- | The instance family of the recommended reservation.
--
-- /Note:/ Consider using 'family' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eFamily :: Lens.Lens' ElastiCacheInstanceDetails (Core.Maybe Types.Family)
eFamily = Lens.field @"family"
{-# DEPRECATED eFamily "Use generic-lens or generic-optics with 'family' instead." #-}

-- | The type of node that AWS recommends.
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eNodeType :: Lens.Lens' ElastiCacheInstanceDetails (Core.Maybe Types.NodeType)
eNodeType = Lens.field @"nodeType"
{-# DEPRECATED eNodeType "Use generic-lens or generic-optics with 'nodeType' instead." #-}

-- | The description of the recommended reservation.
--
-- /Note:/ Consider using 'productDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eProductDescription :: Lens.Lens' ElastiCacheInstanceDetails (Core.Maybe Types.ProductDescription)
eProductDescription = Lens.field @"productDescription"
{-# DEPRECATED eProductDescription "Use generic-lens or generic-optics with 'productDescription' instead." #-}

-- | The AWS Region of the recommended reservation.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eRegion :: Lens.Lens' ElastiCacheInstanceDetails (Core.Maybe Types.Region)
eRegion = Lens.field @"region"
{-# DEPRECATED eRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | Whether the recommended reservation is size flexible.
--
-- /Note:/ Consider using 'sizeFlexEligible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eSizeFlexEligible :: Lens.Lens' ElastiCacheInstanceDetails (Core.Maybe Core.Bool)
eSizeFlexEligible = Lens.field @"sizeFlexEligible"
{-# DEPRECATED eSizeFlexEligible "Use generic-lens or generic-optics with 'sizeFlexEligible' instead." #-}

instance Core.FromJSON ElastiCacheInstanceDetails where
  parseJSON =
    Core.withObject "ElastiCacheInstanceDetails" Core.$
      \x ->
        ElastiCacheInstanceDetails'
          Core.<$> (x Core..:? "CurrentGeneration")
          Core.<*> (x Core..:? "Family")
          Core.<*> (x Core..:? "NodeType")
          Core.<*> (x Core..:? "ProductDescription")
          Core.<*> (x Core..:? "Region")
          Core.<*> (x Core..:? "SizeFlexEligible")

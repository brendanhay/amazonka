{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    ridCurrentGeneration,
    ridFamily,
    ridNodeType,
    ridRegion,
    ridSizeFlexEligible,
  )
where

import qualified Network.AWS.CostExplorer.Types.GenericString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details about the Amazon Redshift instances that AWS recommends that you purchase.
--
-- /See:/ 'mkRedshiftInstanceDetails' smart constructor.
data RedshiftInstanceDetails = RedshiftInstanceDetails'
  { -- | Whether the recommendation is for a current-generation instance.
    currentGeneration :: Core.Maybe Core.Bool,
    -- | The instance family of the recommended reservation.
    family :: Core.Maybe Types.GenericString,
    -- | The type of node that AWS recommends.
    nodeType :: Core.Maybe Types.GenericString,
    -- | The AWS Region of the recommended reservation.
    region :: Core.Maybe Types.GenericString,
    -- | Whether the recommended reservation is size flexible.
    sizeFlexEligible :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RedshiftInstanceDetails' value with any optional fields omitted.
mkRedshiftInstanceDetails ::
  RedshiftInstanceDetails
mkRedshiftInstanceDetails =
  RedshiftInstanceDetails'
    { currentGeneration = Core.Nothing,
      family = Core.Nothing,
      nodeType = Core.Nothing,
      region = Core.Nothing,
      sizeFlexEligible = Core.Nothing
    }

-- | Whether the recommendation is for a current-generation instance.
--
-- /Note:/ Consider using 'currentGeneration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ridCurrentGeneration :: Lens.Lens' RedshiftInstanceDetails (Core.Maybe Core.Bool)
ridCurrentGeneration = Lens.field @"currentGeneration"
{-# DEPRECATED ridCurrentGeneration "Use generic-lens or generic-optics with 'currentGeneration' instead." #-}

-- | The instance family of the recommended reservation.
--
-- /Note:/ Consider using 'family' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ridFamily :: Lens.Lens' RedshiftInstanceDetails (Core.Maybe Types.GenericString)
ridFamily = Lens.field @"family"
{-# DEPRECATED ridFamily "Use generic-lens or generic-optics with 'family' instead." #-}

-- | The type of node that AWS recommends.
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ridNodeType :: Lens.Lens' RedshiftInstanceDetails (Core.Maybe Types.GenericString)
ridNodeType = Lens.field @"nodeType"
{-# DEPRECATED ridNodeType "Use generic-lens or generic-optics with 'nodeType' instead." #-}

-- | The AWS Region of the recommended reservation.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ridRegion :: Lens.Lens' RedshiftInstanceDetails (Core.Maybe Types.GenericString)
ridRegion = Lens.field @"region"
{-# DEPRECATED ridRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | Whether the recommended reservation is size flexible.
--
-- /Note:/ Consider using 'sizeFlexEligible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ridSizeFlexEligible :: Lens.Lens' RedshiftInstanceDetails (Core.Maybe Core.Bool)
ridSizeFlexEligible = Lens.field @"sizeFlexEligible"
{-# DEPRECATED ridSizeFlexEligible "Use generic-lens or generic-optics with 'sizeFlexEligible' instead." #-}

instance Core.FromJSON RedshiftInstanceDetails where
  parseJSON =
    Core.withObject "RedshiftInstanceDetails" Core.$
      \x ->
        RedshiftInstanceDetails'
          Core.<$> (x Core..:? "CurrentGeneration")
          Core.<*> (x Core..:? "Family")
          Core.<*> (x Core..:? "NodeType")
          Core.<*> (x Core..:? "Region")
          Core.<*> (x Core..:? "SizeFlexEligible")

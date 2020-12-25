{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.CostCategoryReference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.CostCategoryReference
  ( CostCategoryReference (..),

    -- * Smart constructor
    mkCostCategoryReference,

    -- * Lenses
    ccrCostCategoryArn,
    ccrEffectiveEnd,
    ccrEffectiveStart,
    ccrName,
    ccrNumberOfRules,
    ccrProcessingStatus,
    ccrValues,
  )
where

import qualified Network.AWS.CostExplorer.Types.Arn as Types
import qualified Network.AWS.CostExplorer.Types.CostCategoryName as Types
import qualified Network.AWS.CostExplorer.Types.CostCategoryProcessingStatus as Types
import qualified Network.AWS.CostExplorer.Types.CostCategoryValue as Types
import qualified Network.AWS.CostExplorer.Types.EffectiveEnd as Types
import qualified Network.AWS.CostExplorer.Types.EffectiveStart as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A reference to a Cost Category containing only enough information to identify the Cost Category.
--
-- You can use this information to retrieve the full Cost Category information using @DescribeCostCategory@ .
--
-- /See:/ 'mkCostCategoryReference' smart constructor.
data CostCategoryReference = CostCategoryReference'
  { -- | The unique identifier for your Cost Category.
    costCategoryArn :: Core.Maybe Types.Arn,
    -- | The Cost Category's effective end date.
    effectiveEnd :: Core.Maybe Types.EffectiveEnd,
    -- | The Cost Category's effective start date.
    effectiveStart :: Core.Maybe Types.EffectiveStart,
    name :: Core.Maybe Types.CostCategoryName,
    -- | The number of rules associated with a specific Cost Category.
    numberOfRules :: Core.Maybe Core.Natural,
    -- | The list of processing statuses for Cost Management products for a specific cost category.
    processingStatus :: Core.Maybe [Types.CostCategoryProcessingStatus],
    -- | A list of unique cost category values in a specific cost category.
    values :: Core.Maybe [Types.CostCategoryValue]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CostCategoryReference' value with any optional fields omitted.
mkCostCategoryReference ::
  CostCategoryReference
mkCostCategoryReference =
  CostCategoryReference'
    { costCategoryArn = Core.Nothing,
      effectiveEnd = Core.Nothing,
      effectiveStart = Core.Nothing,
      name = Core.Nothing,
      numberOfRules = Core.Nothing,
      processingStatus = Core.Nothing,
      values = Core.Nothing
    }

-- | The unique identifier for your Cost Category.
--
-- /Note:/ Consider using 'costCategoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrCostCategoryArn :: Lens.Lens' CostCategoryReference (Core.Maybe Types.Arn)
ccrCostCategoryArn = Lens.field @"costCategoryArn"
{-# DEPRECATED ccrCostCategoryArn "Use generic-lens or generic-optics with 'costCategoryArn' instead." #-}

-- | The Cost Category's effective end date.
--
-- /Note:/ Consider using 'effectiveEnd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrEffectiveEnd :: Lens.Lens' CostCategoryReference (Core.Maybe Types.EffectiveEnd)
ccrEffectiveEnd = Lens.field @"effectiveEnd"
{-# DEPRECATED ccrEffectiveEnd "Use generic-lens or generic-optics with 'effectiveEnd' instead." #-}

-- | The Cost Category's effective start date.
--
-- /Note:/ Consider using 'effectiveStart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrEffectiveStart :: Lens.Lens' CostCategoryReference (Core.Maybe Types.EffectiveStart)
ccrEffectiveStart = Lens.field @"effectiveStart"
{-# DEPRECATED ccrEffectiveStart "Use generic-lens or generic-optics with 'effectiveStart' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrName :: Lens.Lens' CostCategoryReference (Core.Maybe Types.CostCategoryName)
ccrName = Lens.field @"name"
{-# DEPRECATED ccrName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The number of rules associated with a specific Cost Category.
--
-- /Note:/ Consider using 'numberOfRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrNumberOfRules :: Lens.Lens' CostCategoryReference (Core.Maybe Core.Natural)
ccrNumberOfRules = Lens.field @"numberOfRules"
{-# DEPRECATED ccrNumberOfRules "Use generic-lens or generic-optics with 'numberOfRules' instead." #-}

-- | The list of processing statuses for Cost Management products for a specific cost category.
--
-- /Note:/ Consider using 'processingStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrProcessingStatus :: Lens.Lens' CostCategoryReference (Core.Maybe [Types.CostCategoryProcessingStatus])
ccrProcessingStatus = Lens.field @"processingStatus"
{-# DEPRECATED ccrProcessingStatus "Use generic-lens or generic-optics with 'processingStatus' instead." #-}

-- | A list of unique cost category values in a specific cost category.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrValues :: Lens.Lens' CostCategoryReference (Core.Maybe [Types.CostCategoryValue])
ccrValues = Lens.field @"values"
{-# DEPRECATED ccrValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Core.FromJSON CostCategoryReference where
  parseJSON =
    Core.withObject "CostCategoryReference" Core.$
      \x ->
        CostCategoryReference'
          Core.<$> (x Core..:? "CostCategoryArn")
          Core.<*> (x Core..:? "EffectiveEnd")
          Core.<*> (x Core..:? "EffectiveStart")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "NumberOfRules")
          Core.<*> (x Core..:? "ProcessingStatus")
          Core.<*> (x Core..:? "Values")

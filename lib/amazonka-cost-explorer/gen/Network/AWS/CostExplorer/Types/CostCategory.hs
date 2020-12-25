{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.CostCategory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.CostCategory
  ( CostCategory (..),

    -- * Smart constructor
    mkCostCategory,

    -- * Lenses
    ccCostCategoryArn,
    ccEffectiveStart,
    ccName,
    ccRuleVersion,
    ccRules,
    ccEffectiveEnd,
    ccProcessingStatus,
  )
where

import qualified Network.AWS.CostExplorer.Types.Arn as Types
import qualified Network.AWS.CostExplorer.Types.CostCategoryProcessingStatus as Types
import qualified Network.AWS.CostExplorer.Types.CostCategoryRule as Types
import qualified Network.AWS.CostExplorer.Types.CostCategoryRuleVersion as Types
import qualified Network.AWS.CostExplorer.Types.EffectiveEnd as Types
import qualified Network.AWS.CostExplorer.Types.EffectiveStart as Types
import qualified Network.AWS.CostExplorer.Types.Name as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The structure of Cost Categories. This includes detailed metadata and the set of rules for the @CostCategory@ object.
--
-- /See:/ 'mkCostCategory' smart constructor.
data CostCategory = CostCategory'
  { -- | The unique identifier for your Cost Category.
    costCategoryArn :: Types.Arn,
    -- | The Cost Category's effective start date.
    effectiveStart :: Types.EffectiveStart,
    name :: Types.Name,
    ruleVersion :: Types.CostCategoryRuleVersion,
    -- | Rules are processed in order. If there are multiple rules that match the line item, then the first rule to match is used to determine that Cost Category value.
    rules :: Core.NonEmpty Types.CostCategoryRule,
    -- | The Cost Category's effective end date.
    effectiveEnd :: Core.Maybe Types.EffectiveEnd,
    -- | The list of processing statuses for Cost Management products for a specific cost category.
    processingStatus :: Core.Maybe [Types.CostCategoryProcessingStatus]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CostCategory' value with any optional fields omitted.
mkCostCategory ::
  -- | 'costCategoryArn'
  Types.Arn ->
  -- | 'effectiveStart'
  Types.EffectiveStart ->
  -- | 'name'
  Types.Name ->
  -- | 'ruleVersion'
  Types.CostCategoryRuleVersion ->
  -- | 'rules'
  Core.NonEmpty Types.CostCategoryRule ->
  CostCategory
mkCostCategory
  costCategoryArn
  effectiveStart
  name
  ruleVersion
  rules =
    CostCategory'
      { costCategoryArn,
        effectiveStart,
        name,
        ruleVersion,
        rules,
        effectiveEnd = Core.Nothing,
        processingStatus = Core.Nothing
      }

-- | The unique identifier for your Cost Category.
--
-- /Note:/ Consider using 'costCategoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCostCategoryArn :: Lens.Lens' CostCategory Types.Arn
ccCostCategoryArn = Lens.field @"costCategoryArn"
{-# DEPRECATED ccCostCategoryArn "Use generic-lens or generic-optics with 'costCategoryArn' instead." #-}

-- | The Cost Category's effective start date.
--
-- /Note:/ Consider using 'effectiveStart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccEffectiveStart :: Lens.Lens' CostCategory Types.EffectiveStart
ccEffectiveStart = Lens.field @"effectiveStart"
{-# DEPRECATED ccEffectiveStart "Use generic-lens or generic-optics with 'effectiveStart' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccName :: Lens.Lens' CostCategory Types.Name
ccName = Lens.field @"name"
{-# DEPRECATED ccName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'ruleVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccRuleVersion :: Lens.Lens' CostCategory Types.CostCategoryRuleVersion
ccRuleVersion = Lens.field @"ruleVersion"
{-# DEPRECATED ccRuleVersion "Use generic-lens or generic-optics with 'ruleVersion' instead." #-}

-- | Rules are processed in order. If there are multiple rules that match the line item, then the first rule to match is used to determine that Cost Category value.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccRules :: Lens.Lens' CostCategory (Core.NonEmpty Types.CostCategoryRule)
ccRules = Lens.field @"rules"
{-# DEPRECATED ccRules "Use generic-lens or generic-optics with 'rules' instead." #-}

-- | The Cost Category's effective end date.
--
-- /Note:/ Consider using 'effectiveEnd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccEffectiveEnd :: Lens.Lens' CostCategory (Core.Maybe Types.EffectiveEnd)
ccEffectiveEnd = Lens.field @"effectiveEnd"
{-# DEPRECATED ccEffectiveEnd "Use generic-lens or generic-optics with 'effectiveEnd' instead." #-}

-- | The list of processing statuses for Cost Management products for a specific cost category.
--
-- /Note:/ Consider using 'processingStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccProcessingStatus :: Lens.Lens' CostCategory (Core.Maybe [Types.CostCategoryProcessingStatus])
ccProcessingStatus = Lens.field @"processingStatus"
{-# DEPRECATED ccProcessingStatus "Use generic-lens or generic-optics with 'processingStatus' instead." #-}

instance Core.FromJSON CostCategory where
  parseJSON =
    Core.withObject "CostCategory" Core.$
      \x ->
        CostCategory'
          Core.<$> (x Core..: "CostCategoryArn")
          Core.<*> (x Core..: "EffectiveStart")
          Core.<*> (x Core..: "Name")
          Core.<*> (x Core..: "RuleVersion")
          Core.<*> (x Core..: "Rules")
          Core.<*> (x Core..:? "EffectiveEnd")
          Core.<*> (x Core..:? "ProcessingStatus")

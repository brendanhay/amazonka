{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.CostCategoryRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.CostCategoryRule
  ( CostCategoryRule (..),

    -- * Smart constructor
    mkCostCategoryRule,

    -- * Lenses
    ccrValue,
    ccrRule,
  )
where

import qualified Network.AWS.CostExplorer.Types.Expression as Types
import qualified Network.AWS.CostExplorer.Types.Value as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Rules are processed in order. If there are multiple rules that match the line item, then the first rule to match is used to determine that Cost Category value.
--
-- /See:/ 'mkCostCategoryRule' smart constructor.
data CostCategoryRule = CostCategoryRule'
  { value :: Types.Value,
    -- | An <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> object used to categorize costs. This supports dimensions, tags, and nested expressions. Currently the only dimensions supported are @LINKED_ACCOUNT@ , @SERVICE_CODE@ , @RECORD_TYPE@ , and @LINKED_ACCOUNT_NAME@ .
    --
    -- Root level @OR@ is not supported. We recommend that you create a separate rule instead.
    -- @RECORD_TYPE@ is a dimension used for Cost Explorer APIs, and is also supported for Cost Category expressions. This dimension uses different terms, depending on whether you're using the console or API/JSON editor. For a detailed comparison, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/manage-cost-categories.html#cost-categories-terms Term Comparisons> in the /AWS Billing and Cost Management User Guide/ .
    rule :: Types.Expression
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CostCategoryRule' value with any optional fields omitted.
mkCostCategoryRule ::
  -- | 'value'
  Types.Value ->
  -- | 'rule'
  Types.Expression ->
  CostCategoryRule
mkCostCategoryRule value rule = CostCategoryRule' {value, rule}

-- | Undocumented field.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrValue :: Lens.Lens' CostCategoryRule Types.Value
ccrValue = Lens.field @"value"
{-# DEPRECATED ccrValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | An <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> object used to categorize costs. This supports dimensions, tags, and nested expressions. Currently the only dimensions supported are @LINKED_ACCOUNT@ , @SERVICE_CODE@ , @RECORD_TYPE@ , and @LINKED_ACCOUNT_NAME@ .
--
-- Root level @OR@ is not supported. We recommend that you create a separate rule instead.
-- @RECORD_TYPE@ is a dimension used for Cost Explorer APIs, and is also supported for Cost Category expressions. This dimension uses different terms, depending on whether you're using the console or API/JSON editor. For a detailed comparison, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/manage-cost-categories.html#cost-categories-terms Term Comparisons> in the /AWS Billing and Cost Management User Guide/ .
--
-- /Note:/ Consider using 'rule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrRule :: Lens.Lens' CostCategoryRule Types.Expression
ccrRule = Lens.field @"rule"
{-# DEPRECATED ccrRule "Use generic-lens or generic-optics with 'rule' instead." #-}

instance Core.FromJSON CostCategoryRule where
  toJSON CostCategoryRule {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Value" Core..= value),
            Core.Just ("Rule" Core..= rule)
          ]
      )

instance Core.FromJSON CostCategoryRule where
  parseJSON =
    Core.withObject "CostCategoryRule" Core.$
      \x ->
        CostCategoryRule'
          Core.<$> (x Core..: "Value") Core.<*> (x Core..: "Rule")

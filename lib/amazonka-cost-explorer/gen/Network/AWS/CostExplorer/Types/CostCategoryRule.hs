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

import Network.AWS.CostExplorer.Types.Expression
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Rules are processed in order. If there are multiple rules that match the line item, then the first rule to match is used to determine that Cost Category value.
--
-- /See:/ 'mkCostCategoryRule' smart constructor.
data CostCategoryRule = CostCategoryRule'
  { value :: Lude.Text,
    rule :: Expression
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CostCategoryRule' with the minimum fields required to make a request.
--
-- * 'rule' - An <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> object used to categorize costs. This supports dimensions, tags, and nested expressions. Currently the only dimensions supported are @LINKED_ACCOUNT@ , @SERVICE_CODE@ , @RECORD_TYPE@ , and @LINKED_ACCOUNT_NAME@ .
--
-- Root level @OR@ is not supported. We recommend that you create a separate rule instead.
-- @RECORD_TYPE@ is a dimension used for Cost Explorer APIs, and is also supported for Cost Category expressions. This dimension uses different terms, depending on whether you're using the console or API/JSON editor. For a detailed comparison, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/manage-cost-categories.html#cost-categories-terms Term Comparisons> in the /AWS Billing and Cost Management User Guide/ .
-- * 'value' - Undocumented field.
mkCostCategoryRule ::
  -- | 'value'
  Lude.Text ->
  -- | 'rule'
  Expression ->
  CostCategoryRule
mkCostCategoryRule pValue_ pRule_ =
  CostCategoryRule' {value = pValue_, rule = pRule_}

-- | Undocumented field.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrValue :: Lens.Lens' CostCategoryRule Lude.Text
ccrValue = Lens.lens (value :: CostCategoryRule -> Lude.Text) (\s a -> s {value = a} :: CostCategoryRule)
{-# DEPRECATED ccrValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | An <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> object used to categorize costs. This supports dimensions, tags, and nested expressions. Currently the only dimensions supported are @LINKED_ACCOUNT@ , @SERVICE_CODE@ , @RECORD_TYPE@ , and @LINKED_ACCOUNT_NAME@ .
--
-- Root level @OR@ is not supported. We recommend that you create a separate rule instead.
-- @RECORD_TYPE@ is a dimension used for Cost Explorer APIs, and is also supported for Cost Category expressions. This dimension uses different terms, depending on whether you're using the console or API/JSON editor. For a detailed comparison, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/manage-cost-categories.html#cost-categories-terms Term Comparisons> in the /AWS Billing and Cost Management User Guide/ .
--
-- /Note:/ Consider using 'rule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrRule :: Lens.Lens' CostCategoryRule Expression
ccrRule = Lens.lens (rule :: CostCategoryRule -> Expression) (\s a -> s {rule = a} :: CostCategoryRule)
{-# DEPRECATED ccrRule "Use generic-lens or generic-optics with 'rule' instead." #-}

instance Lude.FromJSON CostCategoryRule where
  parseJSON =
    Lude.withObject
      "CostCategoryRule"
      ( \x ->
          CostCategoryRule'
            Lude.<$> (x Lude..: "Value") Lude.<*> (x Lude..: "Rule")
      )

instance Lude.ToJSON CostCategoryRule where
  toJSON CostCategoryRule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Value" Lude..= value),
            Lude.Just ("Rule" Lude..= rule)
          ]
      )

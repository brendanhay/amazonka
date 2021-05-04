{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.CostCategoryRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.CostCategoryRule where

import Network.AWS.CostExplorer.Types.Expression
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Rules are processed in order. If there are multiple rules that match the
-- line item, then the first rule to match is used to determine that Cost
-- Category value.
--
-- /See:/ 'newCostCategoryRule' smart constructor.
data CostCategoryRule = CostCategoryRule'
  { value :: Prelude.Text,
    -- | An
    -- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression>
    -- object used to categorize costs. This supports dimensions, tags, and
    -- nested expressions. Currently the only dimensions supported are
    -- @LINKED_ACCOUNT@, @SERVICE_CODE@, @RECORD_TYPE@, and
    -- @LINKED_ACCOUNT_NAME@.
    --
    -- Root level @OR@ is not supported. We recommend that you create a
    -- separate rule instead.
    --
    -- @RECORD_TYPE@ is a dimension used for Cost Explorer APIs, and is also
    -- supported for Cost Category expressions. This dimension uses different
    -- terms, depending on whether you\'re using the console or API\/JSON
    -- editor. For a detailed comparison, see
    -- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/manage-cost-categories.html#cost-categories-terms Term Comparisons>
    -- in the /AWS Billing and Cost Management User Guide/.
    rule :: Expression
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CostCategoryRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'costCategoryRule_value' - Undocumented member.
--
-- 'rule', 'costCategoryRule_rule' - An
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression>
-- object used to categorize costs. This supports dimensions, tags, and
-- nested expressions. Currently the only dimensions supported are
-- @LINKED_ACCOUNT@, @SERVICE_CODE@, @RECORD_TYPE@, and
-- @LINKED_ACCOUNT_NAME@.
--
-- Root level @OR@ is not supported. We recommend that you create a
-- separate rule instead.
--
-- @RECORD_TYPE@ is a dimension used for Cost Explorer APIs, and is also
-- supported for Cost Category expressions. This dimension uses different
-- terms, depending on whether you\'re using the console or API\/JSON
-- editor. For a detailed comparison, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/manage-cost-categories.html#cost-categories-terms Term Comparisons>
-- in the /AWS Billing and Cost Management User Guide/.
newCostCategoryRule ::
  -- | 'value'
  Prelude.Text ->
  -- | 'rule'
  Expression ->
  CostCategoryRule
newCostCategoryRule pValue_ pRule_ =
  CostCategoryRule' {value = pValue_, rule = pRule_}

-- | Undocumented member.
costCategoryRule_value :: Lens.Lens' CostCategoryRule Prelude.Text
costCategoryRule_value = Lens.lens (\CostCategoryRule' {value} -> value) (\s@CostCategoryRule' {} a -> s {value = a} :: CostCategoryRule)

-- | An
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression>
-- object used to categorize costs. This supports dimensions, tags, and
-- nested expressions. Currently the only dimensions supported are
-- @LINKED_ACCOUNT@, @SERVICE_CODE@, @RECORD_TYPE@, and
-- @LINKED_ACCOUNT_NAME@.
--
-- Root level @OR@ is not supported. We recommend that you create a
-- separate rule instead.
--
-- @RECORD_TYPE@ is a dimension used for Cost Explorer APIs, and is also
-- supported for Cost Category expressions. This dimension uses different
-- terms, depending on whether you\'re using the console or API\/JSON
-- editor. For a detailed comparison, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/manage-cost-categories.html#cost-categories-terms Term Comparisons>
-- in the /AWS Billing and Cost Management User Guide/.
costCategoryRule_rule :: Lens.Lens' CostCategoryRule Expression
costCategoryRule_rule = Lens.lens (\CostCategoryRule' {rule} -> rule) (\s@CostCategoryRule' {} a -> s {rule = a} :: CostCategoryRule)

instance Prelude.FromJSON CostCategoryRule where
  parseJSON =
    Prelude.withObject
      "CostCategoryRule"
      ( \x ->
          CostCategoryRule'
            Prelude.<$> (x Prelude..: "Value")
            Prelude.<*> (x Prelude..: "Rule")
      )

instance Prelude.Hashable CostCategoryRule

instance Prelude.NFData CostCategoryRule

instance Prelude.ToJSON CostCategoryRule where
  toJSON CostCategoryRule' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Value" Prelude..= value),
            Prelude.Just ("Rule" Prelude..= rule)
          ]
      )

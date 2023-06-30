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
-- Module      : Amazonka.CostExplorer.Types.CostCategoryRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.CostCategoryRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.CostCategoryInheritedValueDimension
import Amazonka.CostExplorer.Types.CostCategoryRuleType
import Amazonka.CostExplorer.Types.Expression
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Rules are processed in order. If there are multiple rules that match the
-- line item, then the first rule to match is used to determine that Cost
-- Category value.
--
-- /See:/ 'newCostCategoryRule' smart constructor.
data CostCategoryRule = CostCategoryRule'
  { -- | The value the line item is categorized as if the line item contains the
    -- matched dimension.
    inheritedValue :: Prelude.Maybe CostCategoryInheritedValueDimension,
    -- | An
    -- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression>
    -- object used to categorize costs. This supports dimensions, tags, and
    -- nested expressions. Currently the only dimensions supported are
    -- @LINKED_ACCOUNT@, @SERVICE_CODE@, @RECORD_TYPE@, and
    -- @LINKED_ACCOUNT_NAME@.
    --
    -- Root level @OR@ isn\'t supported. We recommend that you create a
    -- separate rule instead.
    --
    -- @RECORD_TYPE@ is a dimension used for Cost Explorer APIs, and is also
    -- supported for Cost Category expressions. This dimension uses different
    -- terms, depending on whether you\'re using the console or API\/JSON
    -- editor. For a detailed comparison, see
    -- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/manage-cost-categories.html#cost-categories-terms Term Comparisons>
    -- in the /Billing and Cost Management User Guide/.
    rule :: Prelude.Maybe Expression,
    -- | You can define the @CostCategoryRule@ rule type as either @REGULAR@ or
    -- @INHERITED_VALUE@. The @INHERITED_VALUE@ rule type adds the flexibility
    -- to define a rule that dynamically inherits the cost category value. This
    -- value is from the dimension value that\'s defined by
    -- @CostCategoryInheritedValueDimension@. For example, suppose that you
    -- want to costs to be dynamically grouped based on the value of a specific
    -- tag key. First, choose an inherited value rule type, and then choose the
    -- tag dimension and specify the tag key to use.
    type' :: Prelude.Maybe CostCategoryRuleType,
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CostCategoryRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inheritedValue', 'costCategoryRule_inheritedValue' - The value the line item is categorized as if the line item contains the
-- matched dimension.
--
-- 'rule', 'costCategoryRule_rule' - An
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression>
-- object used to categorize costs. This supports dimensions, tags, and
-- nested expressions. Currently the only dimensions supported are
-- @LINKED_ACCOUNT@, @SERVICE_CODE@, @RECORD_TYPE@, and
-- @LINKED_ACCOUNT_NAME@.
--
-- Root level @OR@ isn\'t supported. We recommend that you create a
-- separate rule instead.
--
-- @RECORD_TYPE@ is a dimension used for Cost Explorer APIs, and is also
-- supported for Cost Category expressions. This dimension uses different
-- terms, depending on whether you\'re using the console or API\/JSON
-- editor. For a detailed comparison, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/manage-cost-categories.html#cost-categories-terms Term Comparisons>
-- in the /Billing and Cost Management User Guide/.
--
-- 'type'', 'costCategoryRule_type' - You can define the @CostCategoryRule@ rule type as either @REGULAR@ or
-- @INHERITED_VALUE@. The @INHERITED_VALUE@ rule type adds the flexibility
-- to define a rule that dynamically inherits the cost category value. This
-- value is from the dimension value that\'s defined by
-- @CostCategoryInheritedValueDimension@. For example, suppose that you
-- want to costs to be dynamically grouped based on the value of a specific
-- tag key. First, choose an inherited value rule type, and then choose the
-- tag dimension and specify the tag key to use.
--
-- 'value', 'costCategoryRule_value' - Undocumented member.
newCostCategoryRule ::
  CostCategoryRule
newCostCategoryRule =
  CostCategoryRule'
    { inheritedValue = Prelude.Nothing,
      rule = Prelude.Nothing,
      type' = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The value the line item is categorized as if the line item contains the
-- matched dimension.
costCategoryRule_inheritedValue :: Lens.Lens' CostCategoryRule (Prelude.Maybe CostCategoryInheritedValueDimension)
costCategoryRule_inheritedValue = Lens.lens (\CostCategoryRule' {inheritedValue} -> inheritedValue) (\s@CostCategoryRule' {} a -> s {inheritedValue = a} :: CostCategoryRule)

-- | An
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression>
-- object used to categorize costs. This supports dimensions, tags, and
-- nested expressions. Currently the only dimensions supported are
-- @LINKED_ACCOUNT@, @SERVICE_CODE@, @RECORD_TYPE@, and
-- @LINKED_ACCOUNT_NAME@.
--
-- Root level @OR@ isn\'t supported. We recommend that you create a
-- separate rule instead.
--
-- @RECORD_TYPE@ is a dimension used for Cost Explorer APIs, and is also
-- supported for Cost Category expressions. This dimension uses different
-- terms, depending on whether you\'re using the console or API\/JSON
-- editor. For a detailed comparison, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/manage-cost-categories.html#cost-categories-terms Term Comparisons>
-- in the /Billing and Cost Management User Guide/.
costCategoryRule_rule :: Lens.Lens' CostCategoryRule (Prelude.Maybe Expression)
costCategoryRule_rule = Lens.lens (\CostCategoryRule' {rule} -> rule) (\s@CostCategoryRule' {} a -> s {rule = a} :: CostCategoryRule)

-- | You can define the @CostCategoryRule@ rule type as either @REGULAR@ or
-- @INHERITED_VALUE@. The @INHERITED_VALUE@ rule type adds the flexibility
-- to define a rule that dynamically inherits the cost category value. This
-- value is from the dimension value that\'s defined by
-- @CostCategoryInheritedValueDimension@. For example, suppose that you
-- want to costs to be dynamically grouped based on the value of a specific
-- tag key. First, choose an inherited value rule type, and then choose the
-- tag dimension and specify the tag key to use.
costCategoryRule_type :: Lens.Lens' CostCategoryRule (Prelude.Maybe CostCategoryRuleType)
costCategoryRule_type = Lens.lens (\CostCategoryRule' {type'} -> type') (\s@CostCategoryRule' {} a -> s {type' = a} :: CostCategoryRule)

-- | Undocumented member.
costCategoryRule_value :: Lens.Lens' CostCategoryRule (Prelude.Maybe Prelude.Text)
costCategoryRule_value = Lens.lens (\CostCategoryRule' {value} -> value) (\s@CostCategoryRule' {} a -> s {value = a} :: CostCategoryRule)

instance Data.FromJSON CostCategoryRule where
  parseJSON =
    Data.withObject
      "CostCategoryRule"
      ( \x ->
          CostCategoryRule'
            Prelude.<$> (x Data..:? "InheritedValue")
            Prelude.<*> (x Data..:? "Rule")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable CostCategoryRule where
  hashWithSalt _salt CostCategoryRule' {..} =
    _salt
      `Prelude.hashWithSalt` inheritedValue
      `Prelude.hashWithSalt` rule
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` value

instance Prelude.NFData CostCategoryRule where
  rnf CostCategoryRule' {..} =
    Prelude.rnf inheritedValue
      `Prelude.seq` Prelude.rnf rule
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON CostCategoryRule where
  toJSON CostCategoryRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InheritedValue" Data..=)
              Prelude.<$> inheritedValue,
            ("Rule" Data..=) Prelude.<$> rule,
            ("Type" Data..=) Prelude.<$> type',
            ("Value" Data..=) Prelude.<$> value
          ]
      )

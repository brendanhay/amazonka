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
-- Module      : Amazonka.DataBrew.Types.Rule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.Rule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataBrew.Types.ColumnSelector
import Amazonka.DataBrew.Types.Threshold
import qualified Amazonka.Prelude as Prelude

-- | Represents a single data quality requirement that should be validated in
-- the scope of this dataset.
--
-- /See:/ 'newRule' smart constructor.
data Rule = Rule'
  { -- | The map of substitution variable names to their values used in a check
    -- expression. Variable names should start with a \':\' (colon). Variable
    -- values can either be actual values or column names. To differentiate
    -- between the two, column names should be enclosed in backticks, for
    -- example, @\":col1\": \"\`Column A\`\".@
    substitutionMap :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | List of column selectors. Selectors can be used to select columns using
    -- a name or regular expression from the dataset. Rule will be applied to
    -- selected columns.
    columnSelectors :: Prelude.Maybe (Prelude.NonEmpty ColumnSelector),
    -- | A value that specifies whether the rule is disabled. Once a rule is
    -- disabled, a profile job will not validate it during a job run. Default
    -- value is false.
    disabled :: Prelude.Maybe Prelude.Bool,
    -- | The threshold used with a non-aggregate check expression. Non-aggregate
    -- check expressions will be applied to each row in a specific column, and
    -- the threshold will be used to determine whether the validation succeeds.
    threshold :: Prelude.Maybe Threshold,
    -- | The name of the rule.
    name :: Prelude.Text,
    -- | The expression which includes column references, condition names
    -- followed by variable references, possibly grouped and combined with
    -- other conditions. For example,
    -- @(:col1 starts_with :prefix1 or :col1 starts_with :prefix2) and (:col1 ends_with :suffix1 or :col1 ends_with :suffix2)@.
    -- Column and value references are substitution variables that should start
    -- with the \':\' symbol. Depending on the context, substitution
    -- variables\' values can be either an actual value or a column name. These
    -- values are defined in the SubstitutionMap. If a CheckExpression starts
    -- with a column reference, then ColumnSelectors in the rule should be
    -- null. If ColumnSelectors has been defined, then there should be no
    -- column reference in the left side of a condition, for example,
    -- @is_between :val1 and :val2@.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/databrew/latest/dg/profile.data-quality-available-checks.html Available checks>
    checkExpression :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Rule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'substitutionMap', 'rule_substitutionMap' - The map of substitution variable names to their values used in a check
-- expression. Variable names should start with a \':\' (colon). Variable
-- values can either be actual values or column names. To differentiate
-- between the two, column names should be enclosed in backticks, for
-- example, @\":col1\": \"\`Column A\`\".@
--
-- 'columnSelectors', 'rule_columnSelectors' - List of column selectors. Selectors can be used to select columns using
-- a name or regular expression from the dataset. Rule will be applied to
-- selected columns.
--
-- 'disabled', 'rule_disabled' - A value that specifies whether the rule is disabled. Once a rule is
-- disabled, a profile job will not validate it during a job run. Default
-- value is false.
--
-- 'threshold', 'rule_threshold' - The threshold used with a non-aggregate check expression. Non-aggregate
-- check expressions will be applied to each row in a specific column, and
-- the threshold will be used to determine whether the validation succeeds.
--
-- 'name', 'rule_name' - The name of the rule.
--
-- 'checkExpression', 'rule_checkExpression' - The expression which includes column references, condition names
-- followed by variable references, possibly grouped and combined with
-- other conditions. For example,
-- @(:col1 starts_with :prefix1 or :col1 starts_with :prefix2) and (:col1 ends_with :suffix1 or :col1 ends_with :suffix2)@.
-- Column and value references are substitution variables that should start
-- with the \':\' symbol. Depending on the context, substitution
-- variables\' values can be either an actual value or a column name. These
-- values are defined in the SubstitutionMap. If a CheckExpression starts
-- with a column reference, then ColumnSelectors in the rule should be
-- null. If ColumnSelectors has been defined, then there should be no
-- column reference in the left side of a condition, for example,
-- @is_between :val1 and :val2@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/databrew/latest/dg/profile.data-quality-available-checks.html Available checks>
newRule ::
  -- | 'name'
  Prelude.Text ->
  -- | 'checkExpression'
  Prelude.Text ->
  Rule
newRule pName_ pCheckExpression_ =
  Rule'
    { substitutionMap = Prelude.Nothing,
      columnSelectors = Prelude.Nothing,
      disabled = Prelude.Nothing,
      threshold = Prelude.Nothing,
      name = pName_,
      checkExpression = pCheckExpression_
    }

-- | The map of substitution variable names to their values used in a check
-- expression. Variable names should start with a \':\' (colon). Variable
-- values can either be actual values or column names. To differentiate
-- between the two, column names should be enclosed in backticks, for
-- example, @\":col1\": \"\`Column A\`\".@
rule_substitutionMap :: Lens.Lens' Rule (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
rule_substitutionMap = Lens.lens (\Rule' {substitutionMap} -> substitutionMap) (\s@Rule' {} a -> s {substitutionMap = a} :: Rule) Prelude.. Lens.mapping Lens.coerced

-- | List of column selectors. Selectors can be used to select columns using
-- a name or regular expression from the dataset. Rule will be applied to
-- selected columns.
rule_columnSelectors :: Lens.Lens' Rule (Prelude.Maybe (Prelude.NonEmpty ColumnSelector))
rule_columnSelectors = Lens.lens (\Rule' {columnSelectors} -> columnSelectors) (\s@Rule' {} a -> s {columnSelectors = a} :: Rule) Prelude.. Lens.mapping Lens.coerced

-- | A value that specifies whether the rule is disabled. Once a rule is
-- disabled, a profile job will not validate it during a job run. Default
-- value is false.
rule_disabled :: Lens.Lens' Rule (Prelude.Maybe Prelude.Bool)
rule_disabled = Lens.lens (\Rule' {disabled} -> disabled) (\s@Rule' {} a -> s {disabled = a} :: Rule)

-- | The threshold used with a non-aggregate check expression. Non-aggregate
-- check expressions will be applied to each row in a specific column, and
-- the threshold will be used to determine whether the validation succeeds.
rule_threshold :: Lens.Lens' Rule (Prelude.Maybe Threshold)
rule_threshold = Lens.lens (\Rule' {threshold} -> threshold) (\s@Rule' {} a -> s {threshold = a} :: Rule)

-- | The name of the rule.
rule_name :: Lens.Lens' Rule Prelude.Text
rule_name = Lens.lens (\Rule' {name} -> name) (\s@Rule' {} a -> s {name = a} :: Rule)

-- | The expression which includes column references, condition names
-- followed by variable references, possibly grouped and combined with
-- other conditions. For example,
-- @(:col1 starts_with :prefix1 or :col1 starts_with :prefix2) and (:col1 ends_with :suffix1 or :col1 ends_with :suffix2)@.
-- Column and value references are substitution variables that should start
-- with the \':\' symbol. Depending on the context, substitution
-- variables\' values can be either an actual value or a column name. These
-- values are defined in the SubstitutionMap. If a CheckExpression starts
-- with a column reference, then ColumnSelectors in the rule should be
-- null. If ColumnSelectors has been defined, then there should be no
-- column reference in the left side of a condition, for example,
-- @is_between :val1 and :val2@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/databrew/latest/dg/profile.data-quality-available-checks.html Available checks>
rule_checkExpression :: Lens.Lens' Rule Prelude.Text
rule_checkExpression = Lens.lens (\Rule' {checkExpression} -> checkExpression) (\s@Rule' {} a -> s {checkExpression = a} :: Rule)

instance Data.FromJSON Rule where
  parseJSON =
    Data.withObject
      "Rule"
      ( \x ->
          Rule'
            Prelude.<$> ( x Data..:? "SubstitutionMap"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ColumnSelectors")
            Prelude.<*> (x Data..:? "Disabled")
            Prelude.<*> (x Data..:? "Threshold")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "CheckExpression")
      )

instance Prelude.Hashable Rule where
  hashWithSalt _salt Rule' {..} =
    _salt `Prelude.hashWithSalt` substitutionMap
      `Prelude.hashWithSalt` columnSelectors
      `Prelude.hashWithSalt` disabled
      `Prelude.hashWithSalt` threshold
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` checkExpression

instance Prelude.NFData Rule where
  rnf Rule' {..} =
    Prelude.rnf substitutionMap
      `Prelude.seq` Prelude.rnf columnSelectors
      `Prelude.seq` Prelude.rnf disabled
      `Prelude.seq` Prelude.rnf threshold
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf checkExpression

instance Data.ToJSON Rule where
  toJSON Rule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SubstitutionMap" Data..=)
              Prelude.<$> substitutionMap,
            ("ColumnSelectors" Data..=)
              Prelude.<$> columnSelectors,
            ("Disabled" Data..=) Prelude.<$> disabled,
            ("Threshold" Data..=) Prelude.<$> threshold,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("CheckExpression" Data..= checkExpression)
          ]
      )

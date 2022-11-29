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
-- Module      : Amazonka.DataBrew.Types.ConditionExpression
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.ConditionExpression where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents an individual condition that evaluates to true or false.
--
-- Conditions are used with recipe actions. The action is only performed
-- for column values where the condition evaluates to true.
--
-- If a recipe requires more than one condition, then the recipe must
-- specify multiple @ConditionExpression@ elements. Each condition is
-- applied to the rows in a dataset first, before the recipe action is
-- performed.
--
-- /See:/ 'newConditionExpression' smart constructor.
data ConditionExpression = ConditionExpression'
  { -- | A value that the condition must evaluate to for the condition to
    -- succeed.
    value :: Prelude.Maybe Prelude.Text,
    -- | A specific condition to apply to a recipe action. For more information,
    -- see
    -- <https://docs.aws.amazon.com/databrew/latest/dg/recipes.html#recipes.structure Recipe structure>
    -- in the /Glue DataBrew Developer Guide/.
    condition :: Prelude.Text,
    -- | A column to apply this condition to.
    targetColumn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConditionExpression' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'conditionExpression_value' - A value that the condition must evaluate to for the condition to
-- succeed.
--
-- 'condition', 'conditionExpression_condition' - A specific condition to apply to a recipe action. For more information,
-- see
-- <https://docs.aws.amazon.com/databrew/latest/dg/recipes.html#recipes.structure Recipe structure>
-- in the /Glue DataBrew Developer Guide/.
--
-- 'targetColumn', 'conditionExpression_targetColumn' - A column to apply this condition to.
newConditionExpression ::
  -- | 'condition'
  Prelude.Text ->
  -- | 'targetColumn'
  Prelude.Text ->
  ConditionExpression
newConditionExpression pCondition_ pTargetColumn_ =
  ConditionExpression'
    { value = Prelude.Nothing,
      condition = pCondition_,
      targetColumn = pTargetColumn_
    }

-- | A value that the condition must evaluate to for the condition to
-- succeed.
conditionExpression_value :: Lens.Lens' ConditionExpression (Prelude.Maybe Prelude.Text)
conditionExpression_value = Lens.lens (\ConditionExpression' {value} -> value) (\s@ConditionExpression' {} a -> s {value = a} :: ConditionExpression)

-- | A specific condition to apply to a recipe action. For more information,
-- see
-- <https://docs.aws.amazon.com/databrew/latest/dg/recipes.html#recipes.structure Recipe structure>
-- in the /Glue DataBrew Developer Guide/.
conditionExpression_condition :: Lens.Lens' ConditionExpression Prelude.Text
conditionExpression_condition = Lens.lens (\ConditionExpression' {condition} -> condition) (\s@ConditionExpression' {} a -> s {condition = a} :: ConditionExpression)

-- | A column to apply this condition to.
conditionExpression_targetColumn :: Lens.Lens' ConditionExpression Prelude.Text
conditionExpression_targetColumn = Lens.lens (\ConditionExpression' {targetColumn} -> targetColumn) (\s@ConditionExpression' {} a -> s {targetColumn = a} :: ConditionExpression)

instance Core.FromJSON ConditionExpression where
  parseJSON =
    Core.withObject
      "ConditionExpression"
      ( \x ->
          ConditionExpression'
            Prelude.<$> (x Core..:? "Value")
            Prelude.<*> (x Core..: "Condition")
            Prelude.<*> (x Core..: "TargetColumn")
      )

instance Prelude.Hashable ConditionExpression where
  hashWithSalt _salt ConditionExpression' {..} =
    _salt `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` condition
      `Prelude.hashWithSalt` targetColumn

instance Prelude.NFData ConditionExpression where
  rnf ConditionExpression' {..} =
    Prelude.rnf value
      `Prelude.seq` Prelude.rnf condition
      `Prelude.seq` Prelude.rnf targetColumn

instance Core.ToJSON ConditionExpression where
  toJSON ConditionExpression' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Value" Core..=) Prelude.<$> value,
            Prelude.Just ("Condition" Core..= condition),
            Prelude.Just ("TargetColumn" Core..= targetColumn)
          ]
      )

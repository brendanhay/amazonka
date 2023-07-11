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
-- Module      : Amazonka.MigrationHubStrategy.Types.AssessmentTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.AssessmentTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types.Condition
import qualified Amazonka.Prelude as Prelude

-- | Defines the criteria of assessment.
--
-- /See:/ 'newAssessmentTarget' smart constructor.
data AssessmentTarget = AssessmentTarget'
  { -- | Condition of an assessment.
    condition :: Condition,
    -- | Name of an assessment.
    name :: Prelude.Text,
    -- | Values of an assessment.
    values :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssessmentTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'condition', 'assessmentTarget_condition' - Condition of an assessment.
--
-- 'name', 'assessmentTarget_name' - Name of an assessment.
--
-- 'values', 'assessmentTarget_values' - Values of an assessment.
newAssessmentTarget ::
  -- | 'condition'
  Condition ->
  -- | 'name'
  Prelude.Text ->
  AssessmentTarget
newAssessmentTarget pCondition_ pName_ =
  AssessmentTarget'
    { condition = pCondition_,
      name = pName_,
      values = Prelude.mempty
    }

-- | Condition of an assessment.
assessmentTarget_condition :: Lens.Lens' AssessmentTarget Condition
assessmentTarget_condition = Lens.lens (\AssessmentTarget' {condition} -> condition) (\s@AssessmentTarget' {} a -> s {condition = a} :: AssessmentTarget)

-- | Name of an assessment.
assessmentTarget_name :: Lens.Lens' AssessmentTarget Prelude.Text
assessmentTarget_name = Lens.lens (\AssessmentTarget' {name} -> name) (\s@AssessmentTarget' {} a -> s {name = a} :: AssessmentTarget)

-- | Values of an assessment.
assessmentTarget_values :: Lens.Lens' AssessmentTarget [Prelude.Text]
assessmentTarget_values = Lens.lens (\AssessmentTarget' {values} -> values) (\s@AssessmentTarget' {} a -> s {values = a} :: AssessmentTarget) Prelude.. Lens.coerced

instance Data.FromJSON AssessmentTarget where
  parseJSON =
    Data.withObject
      "AssessmentTarget"
      ( \x ->
          AssessmentTarget'
            Prelude.<$> (x Data..: "condition")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..:? "values" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AssessmentTarget where
  hashWithSalt _salt AssessmentTarget' {..} =
    _salt
      `Prelude.hashWithSalt` condition
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values

instance Prelude.NFData AssessmentTarget where
  rnf AssessmentTarget' {..} =
    Prelude.rnf condition
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf values

instance Data.ToJSON AssessmentTarget where
  toJSON AssessmentTarget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("condition" Data..= condition),
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("values" Data..= values)
          ]
      )

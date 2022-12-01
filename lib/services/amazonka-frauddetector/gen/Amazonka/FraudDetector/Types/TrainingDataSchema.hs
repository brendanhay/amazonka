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
-- Module      : Amazonka.FraudDetector.Types.TrainingDataSchema
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.TrainingDataSchema where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FraudDetector.Types.LabelSchema
import qualified Amazonka.Prelude as Prelude

-- | The training data schema.
--
-- /See:/ 'newTrainingDataSchema' smart constructor.
data TrainingDataSchema = TrainingDataSchema'
  { labelSchema :: Prelude.Maybe LabelSchema,
    -- | The training data schema variables.
    modelVariables :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrainingDataSchema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'labelSchema', 'trainingDataSchema_labelSchema' - Undocumented member.
--
-- 'modelVariables', 'trainingDataSchema_modelVariables' - The training data schema variables.
newTrainingDataSchema ::
  TrainingDataSchema
newTrainingDataSchema =
  TrainingDataSchema'
    { labelSchema = Prelude.Nothing,
      modelVariables = Prelude.mempty
    }

-- | Undocumented member.
trainingDataSchema_labelSchema :: Lens.Lens' TrainingDataSchema (Prelude.Maybe LabelSchema)
trainingDataSchema_labelSchema = Lens.lens (\TrainingDataSchema' {labelSchema} -> labelSchema) (\s@TrainingDataSchema' {} a -> s {labelSchema = a} :: TrainingDataSchema)

-- | The training data schema variables.
trainingDataSchema_modelVariables :: Lens.Lens' TrainingDataSchema [Prelude.Text]
trainingDataSchema_modelVariables = Lens.lens (\TrainingDataSchema' {modelVariables} -> modelVariables) (\s@TrainingDataSchema' {} a -> s {modelVariables = a} :: TrainingDataSchema) Prelude.. Lens.coerced

instance Core.FromJSON TrainingDataSchema where
  parseJSON =
    Core.withObject
      "TrainingDataSchema"
      ( \x ->
          TrainingDataSchema'
            Prelude.<$> (x Core..:? "labelSchema")
            Prelude.<*> ( x Core..:? "modelVariables"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable TrainingDataSchema where
  hashWithSalt _salt TrainingDataSchema' {..} =
    _salt `Prelude.hashWithSalt` labelSchema
      `Prelude.hashWithSalt` modelVariables

instance Prelude.NFData TrainingDataSchema where
  rnf TrainingDataSchema' {..} =
    Prelude.rnf labelSchema
      `Prelude.seq` Prelude.rnf modelVariables

instance Core.ToJSON TrainingDataSchema where
  toJSON TrainingDataSchema' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("labelSchema" Core..=) Prelude.<$> labelSchema,
            Prelude.Just
              ("modelVariables" Core..= modelVariables)
          ]
      )

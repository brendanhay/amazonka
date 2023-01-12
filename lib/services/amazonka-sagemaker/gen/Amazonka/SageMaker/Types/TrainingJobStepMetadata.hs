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
-- Module      : Amazonka.SageMaker.Types.TrainingJobStepMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TrainingJobStepMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Metadata for a training job step.
--
-- /See:/ 'newTrainingJobStepMetadata' smart constructor.
data TrainingJobStepMetadata = TrainingJobStepMetadata'
  { -- | The Amazon Resource Name (ARN) of the training job that was run by this
    -- step execution.
    arn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrainingJobStepMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'trainingJobStepMetadata_arn' - The Amazon Resource Name (ARN) of the training job that was run by this
-- step execution.
newTrainingJobStepMetadata ::
  TrainingJobStepMetadata
newTrainingJobStepMetadata =
  TrainingJobStepMetadata' {arn = Prelude.Nothing}

-- | The Amazon Resource Name (ARN) of the training job that was run by this
-- step execution.
trainingJobStepMetadata_arn :: Lens.Lens' TrainingJobStepMetadata (Prelude.Maybe Prelude.Text)
trainingJobStepMetadata_arn = Lens.lens (\TrainingJobStepMetadata' {arn} -> arn) (\s@TrainingJobStepMetadata' {} a -> s {arn = a} :: TrainingJobStepMetadata)

instance Data.FromJSON TrainingJobStepMetadata where
  parseJSON =
    Data.withObject
      "TrainingJobStepMetadata"
      ( \x ->
          TrainingJobStepMetadata'
            Prelude.<$> (x Data..:? "Arn")
      )

instance Prelude.Hashable TrainingJobStepMetadata where
  hashWithSalt _salt TrainingJobStepMetadata' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData TrainingJobStepMetadata where
  rnf TrainingJobStepMetadata' {..} = Prelude.rnf arn

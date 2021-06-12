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
-- Module      : Network.AWS.SageMaker.Types.TrainingJobStepMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrainingJobStepMetadata where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Metadata for a training job step.
--
-- /See:/ 'newTrainingJobStepMetadata' smart constructor.
data TrainingJobStepMetadata = TrainingJobStepMetadata'
  { -- | The Amazon Resource Name (ARN) of the training job that was run by this
    -- step execution.
    arn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  TrainingJobStepMetadata' {arn = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the training job that was run by this
-- step execution.
trainingJobStepMetadata_arn :: Lens.Lens' TrainingJobStepMetadata (Core.Maybe Core.Text)
trainingJobStepMetadata_arn = Lens.lens (\TrainingJobStepMetadata' {arn} -> arn) (\s@TrainingJobStepMetadata' {} a -> s {arn = a} :: TrainingJobStepMetadata)

instance Core.FromJSON TrainingJobStepMetadata where
  parseJSON =
    Core.withObject
      "TrainingJobStepMetadata"
      ( \x ->
          TrainingJobStepMetadata' Core.<$> (x Core..:? "Arn")
      )

instance Core.Hashable TrainingJobStepMetadata

instance Core.NFData TrainingJobStepMetadata

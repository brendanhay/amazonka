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
-- Module      : Amazonka.SageMaker.Types.ModelQualityJobInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelQualityJobInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.BatchTransformInput
import Amazonka.SageMaker.Types.EndpointInput
import Amazonka.SageMaker.Types.MonitoringGroundTruthS3Input

-- | The input for the model quality monitoring job. Currently endponts are
-- supported for input for model quality monitoring jobs.
--
-- /See:/ 'newModelQualityJobInput' smart constructor.
data ModelQualityJobInput = ModelQualityJobInput'
  { -- | Input object for the batch transform job.
    batchTransformInput :: Prelude.Maybe BatchTransformInput,
    endpointInput :: Prelude.Maybe EndpointInput,
    -- | The ground truth label provided for the model.
    groundTruthS3Input :: MonitoringGroundTruthS3Input
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelQualityJobInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchTransformInput', 'modelQualityJobInput_batchTransformInput' - Input object for the batch transform job.
--
-- 'endpointInput', 'modelQualityJobInput_endpointInput' - Undocumented member.
--
-- 'groundTruthS3Input', 'modelQualityJobInput_groundTruthS3Input' - The ground truth label provided for the model.
newModelQualityJobInput ::
  -- | 'groundTruthS3Input'
  MonitoringGroundTruthS3Input ->
  ModelQualityJobInput
newModelQualityJobInput pGroundTruthS3Input_ =
  ModelQualityJobInput'
    { batchTransformInput =
        Prelude.Nothing,
      endpointInput = Prelude.Nothing,
      groundTruthS3Input = pGroundTruthS3Input_
    }

-- | Input object for the batch transform job.
modelQualityJobInput_batchTransformInput :: Lens.Lens' ModelQualityJobInput (Prelude.Maybe BatchTransformInput)
modelQualityJobInput_batchTransformInput = Lens.lens (\ModelQualityJobInput' {batchTransformInput} -> batchTransformInput) (\s@ModelQualityJobInput' {} a -> s {batchTransformInput = a} :: ModelQualityJobInput)

-- | Undocumented member.
modelQualityJobInput_endpointInput :: Lens.Lens' ModelQualityJobInput (Prelude.Maybe EndpointInput)
modelQualityJobInput_endpointInput = Lens.lens (\ModelQualityJobInput' {endpointInput} -> endpointInput) (\s@ModelQualityJobInput' {} a -> s {endpointInput = a} :: ModelQualityJobInput)

-- | The ground truth label provided for the model.
modelQualityJobInput_groundTruthS3Input :: Lens.Lens' ModelQualityJobInput MonitoringGroundTruthS3Input
modelQualityJobInput_groundTruthS3Input = Lens.lens (\ModelQualityJobInput' {groundTruthS3Input} -> groundTruthS3Input) (\s@ModelQualityJobInput' {} a -> s {groundTruthS3Input = a} :: ModelQualityJobInput)

instance Data.FromJSON ModelQualityJobInput where
  parseJSON =
    Data.withObject
      "ModelQualityJobInput"
      ( \x ->
          ModelQualityJobInput'
            Prelude.<$> (x Data..:? "BatchTransformInput")
            Prelude.<*> (x Data..:? "EndpointInput")
            Prelude.<*> (x Data..: "GroundTruthS3Input")
      )

instance Prelude.Hashable ModelQualityJobInput where
  hashWithSalt _salt ModelQualityJobInput' {..} =
    _salt
      `Prelude.hashWithSalt` batchTransformInput
      `Prelude.hashWithSalt` endpointInput
      `Prelude.hashWithSalt` groundTruthS3Input

instance Prelude.NFData ModelQualityJobInput where
  rnf ModelQualityJobInput' {..} =
    Prelude.rnf batchTransformInput
      `Prelude.seq` Prelude.rnf endpointInput
      `Prelude.seq` Prelude.rnf groundTruthS3Input

instance Data.ToJSON ModelQualityJobInput where
  toJSON ModelQualityJobInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BatchTransformInput" Data..=)
              Prelude.<$> batchTransformInput,
            ("EndpointInput" Data..=) Prelude.<$> endpointInput,
            Prelude.Just
              ("GroundTruthS3Input" Data..= groundTruthS3Input)
          ]
      )

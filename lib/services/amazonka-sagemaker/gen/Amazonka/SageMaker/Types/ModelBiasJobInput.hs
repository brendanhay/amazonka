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
-- Module      : Amazonka.SageMaker.Types.ModelBiasJobInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelBiasJobInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.BatchTransformInput
import Amazonka.SageMaker.Types.EndpointInput
import Amazonka.SageMaker.Types.MonitoringGroundTruthS3Input

-- | Inputs for the model bias job.
--
-- /See:/ 'newModelBiasJobInput' smart constructor.
data ModelBiasJobInput = ModelBiasJobInput'
  { endpointInput :: Prelude.Maybe EndpointInput,
    -- | Input object for the batch transform job.
    batchTransformInput :: Prelude.Maybe BatchTransformInput,
    -- | Location of ground truth labels to use in model bias job.
    groundTruthS3Input :: MonitoringGroundTruthS3Input
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelBiasJobInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointInput', 'modelBiasJobInput_endpointInput' - Undocumented member.
--
-- 'batchTransformInput', 'modelBiasJobInput_batchTransformInput' - Input object for the batch transform job.
--
-- 'groundTruthS3Input', 'modelBiasJobInput_groundTruthS3Input' - Location of ground truth labels to use in model bias job.
newModelBiasJobInput ::
  -- | 'groundTruthS3Input'
  MonitoringGroundTruthS3Input ->
  ModelBiasJobInput
newModelBiasJobInput pGroundTruthS3Input_ =
  ModelBiasJobInput'
    { endpointInput = Prelude.Nothing,
      batchTransformInput = Prelude.Nothing,
      groundTruthS3Input = pGroundTruthS3Input_
    }

-- | Undocumented member.
modelBiasJobInput_endpointInput :: Lens.Lens' ModelBiasJobInput (Prelude.Maybe EndpointInput)
modelBiasJobInput_endpointInput = Lens.lens (\ModelBiasJobInput' {endpointInput} -> endpointInput) (\s@ModelBiasJobInput' {} a -> s {endpointInput = a} :: ModelBiasJobInput)

-- | Input object for the batch transform job.
modelBiasJobInput_batchTransformInput :: Lens.Lens' ModelBiasJobInput (Prelude.Maybe BatchTransformInput)
modelBiasJobInput_batchTransformInput = Lens.lens (\ModelBiasJobInput' {batchTransformInput} -> batchTransformInput) (\s@ModelBiasJobInput' {} a -> s {batchTransformInput = a} :: ModelBiasJobInput)

-- | Location of ground truth labels to use in model bias job.
modelBiasJobInput_groundTruthS3Input :: Lens.Lens' ModelBiasJobInput MonitoringGroundTruthS3Input
modelBiasJobInput_groundTruthS3Input = Lens.lens (\ModelBiasJobInput' {groundTruthS3Input} -> groundTruthS3Input) (\s@ModelBiasJobInput' {} a -> s {groundTruthS3Input = a} :: ModelBiasJobInput)

instance Data.FromJSON ModelBiasJobInput where
  parseJSON =
    Data.withObject
      "ModelBiasJobInput"
      ( \x ->
          ModelBiasJobInput'
            Prelude.<$> (x Data..:? "EndpointInput")
            Prelude.<*> (x Data..:? "BatchTransformInput")
            Prelude.<*> (x Data..: "GroundTruthS3Input")
      )

instance Prelude.Hashable ModelBiasJobInput where
  hashWithSalt _salt ModelBiasJobInput' {..} =
    _salt `Prelude.hashWithSalt` endpointInput
      `Prelude.hashWithSalt` batchTransformInput
      `Prelude.hashWithSalt` groundTruthS3Input

instance Prelude.NFData ModelBiasJobInput where
  rnf ModelBiasJobInput' {..} =
    Prelude.rnf endpointInput
      `Prelude.seq` Prelude.rnf batchTransformInput
      `Prelude.seq` Prelude.rnf groundTruthS3Input

instance Data.ToJSON ModelBiasJobInput where
  toJSON ModelBiasJobInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EndpointInput" Data..=) Prelude.<$> endpointInput,
            ("BatchTransformInput" Data..=)
              Prelude.<$> batchTransformInput,
            Prelude.Just
              ("GroundTruthS3Input" Data..= groundTruthS3Input)
          ]
      )

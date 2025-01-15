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
-- Module      : Amazonka.SageMaker.Types.ModelExplainabilityJobInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelExplainabilityJobInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.BatchTransformInput
import Amazonka.SageMaker.Types.EndpointInput

-- | Inputs for the model explainability job.
--
-- /See:/ 'newModelExplainabilityJobInput' smart constructor.
data ModelExplainabilityJobInput = ModelExplainabilityJobInput'
  { -- | Input object for the batch transform job.
    batchTransformInput :: Prelude.Maybe BatchTransformInput,
    endpointInput :: Prelude.Maybe EndpointInput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelExplainabilityJobInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchTransformInput', 'modelExplainabilityJobInput_batchTransformInput' - Input object for the batch transform job.
--
-- 'endpointInput', 'modelExplainabilityJobInput_endpointInput' - Undocumented member.
newModelExplainabilityJobInput ::
  ModelExplainabilityJobInput
newModelExplainabilityJobInput =
  ModelExplainabilityJobInput'
    { batchTransformInput =
        Prelude.Nothing,
      endpointInput = Prelude.Nothing
    }

-- | Input object for the batch transform job.
modelExplainabilityJobInput_batchTransformInput :: Lens.Lens' ModelExplainabilityJobInput (Prelude.Maybe BatchTransformInput)
modelExplainabilityJobInput_batchTransformInput = Lens.lens (\ModelExplainabilityJobInput' {batchTransformInput} -> batchTransformInput) (\s@ModelExplainabilityJobInput' {} a -> s {batchTransformInput = a} :: ModelExplainabilityJobInput)

-- | Undocumented member.
modelExplainabilityJobInput_endpointInput :: Lens.Lens' ModelExplainabilityJobInput (Prelude.Maybe EndpointInput)
modelExplainabilityJobInput_endpointInput = Lens.lens (\ModelExplainabilityJobInput' {endpointInput} -> endpointInput) (\s@ModelExplainabilityJobInput' {} a -> s {endpointInput = a} :: ModelExplainabilityJobInput)

instance Data.FromJSON ModelExplainabilityJobInput where
  parseJSON =
    Data.withObject
      "ModelExplainabilityJobInput"
      ( \x ->
          ModelExplainabilityJobInput'
            Prelude.<$> (x Data..:? "BatchTransformInput")
            Prelude.<*> (x Data..:? "EndpointInput")
      )

instance Prelude.Hashable ModelExplainabilityJobInput where
  hashWithSalt _salt ModelExplainabilityJobInput' {..} =
    _salt
      `Prelude.hashWithSalt` batchTransformInput
      `Prelude.hashWithSalt` endpointInput

instance Prelude.NFData ModelExplainabilityJobInput where
  rnf ModelExplainabilityJobInput' {..} =
    Prelude.rnf batchTransformInput `Prelude.seq`
      Prelude.rnf endpointInput

instance Data.ToJSON ModelExplainabilityJobInput where
  toJSON ModelExplainabilityJobInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BatchTransformInput" Data..=)
              Prelude.<$> batchTransformInput,
            ("EndpointInput" Data..=) Prelude.<$> endpointInput
          ]
      )

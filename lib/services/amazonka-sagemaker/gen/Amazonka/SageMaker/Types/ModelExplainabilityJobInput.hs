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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelExplainabilityJobInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.EndpointInput

-- | Inputs for the model explainability job.
--
-- /See:/ 'newModelExplainabilityJobInput' smart constructor.
data ModelExplainabilityJobInput = ModelExplainabilityJobInput'
  { endpointInput :: EndpointInput
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
-- 'endpointInput', 'modelExplainabilityJobInput_endpointInput' - Undocumented member.
newModelExplainabilityJobInput ::
  -- | 'endpointInput'
  EndpointInput ->
  ModelExplainabilityJobInput
newModelExplainabilityJobInput pEndpointInput_ =
  ModelExplainabilityJobInput'
    { endpointInput =
        pEndpointInput_
    }

-- | Undocumented member.
modelExplainabilityJobInput_endpointInput :: Lens.Lens' ModelExplainabilityJobInput EndpointInput
modelExplainabilityJobInput_endpointInput = Lens.lens (\ModelExplainabilityJobInput' {endpointInput} -> endpointInput) (\s@ModelExplainabilityJobInput' {} a -> s {endpointInput = a} :: ModelExplainabilityJobInput)

instance Core.FromJSON ModelExplainabilityJobInput where
  parseJSON =
    Core.withObject
      "ModelExplainabilityJobInput"
      ( \x ->
          ModelExplainabilityJobInput'
            Prelude.<$> (x Core..: "EndpointInput")
      )

instance Prelude.Hashable ModelExplainabilityJobInput where
  hashWithSalt salt' ModelExplainabilityJobInput' {..} =
    salt' `Prelude.hashWithSalt` endpointInput

instance Prelude.NFData ModelExplainabilityJobInput where
  rnf ModelExplainabilityJobInput' {..} =
    Prelude.rnf endpointInput

instance Core.ToJSON ModelExplainabilityJobInput where
  toJSON ModelExplainabilityJobInput' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("EndpointInput" Core..= endpointInput)
          ]
      )

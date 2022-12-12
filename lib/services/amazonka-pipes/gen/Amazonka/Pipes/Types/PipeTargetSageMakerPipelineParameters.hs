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
-- Module      : Amazonka.Pipes.Types.PipeTargetSageMakerPipelineParameters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.PipeTargetSageMakerPipelineParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pipes.Types.SageMakerPipelineParameter
import qualified Amazonka.Prelude as Prelude

-- | The parameters for using a SageMaker pipeline as a target.
--
-- /See:/ 'newPipeTargetSageMakerPipelineParameters' smart constructor.
data PipeTargetSageMakerPipelineParameters = PipeTargetSageMakerPipelineParameters'
  { -- | List of Parameter names and values for SageMaker Model Building Pipeline
    -- execution.
    pipelineParameterList :: Prelude.Maybe [SageMakerPipelineParameter]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipeTargetSageMakerPipelineParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineParameterList', 'pipeTargetSageMakerPipelineParameters_pipelineParameterList' - List of Parameter names and values for SageMaker Model Building Pipeline
-- execution.
newPipeTargetSageMakerPipelineParameters ::
  PipeTargetSageMakerPipelineParameters
newPipeTargetSageMakerPipelineParameters =
  PipeTargetSageMakerPipelineParameters'
    { pipelineParameterList =
        Prelude.Nothing
    }

-- | List of Parameter names and values for SageMaker Model Building Pipeline
-- execution.
pipeTargetSageMakerPipelineParameters_pipelineParameterList :: Lens.Lens' PipeTargetSageMakerPipelineParameters (Prelude.Maybe [SageMakerPipelineParameter])
pipeTargetSageMakerPipelineParameters_pipelineParameterList = Lens.lens (\PipeTargetSageMakerPipelineParameters' {pipelineParameterList} -> pipelineParameterList) (\s@PipeTargetSageMakerPipelineParameters' {} a -> s {pipelineParameterList = a} :: PipeTargetSageMakerPipelineParameters) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    PipeTargetSageMakerPipelineParameters
  where
  parseJSON =
    Data.withObject
      "PipeTargetSageMakerPipelineParameters"
      ( \x ->
          PipeTargetSageMakerPipelineParameters'
            Prelude.<$> ( x Data..:? "PipelineParameterList"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    PipeTargetSageMakerPipelineParameters
  where
  hashWithSalt
    _salt
    PipeTargetSageMakerPipelineParameters' {..} =
      _salt `Prelude.hashWithSalt` pipelineParameterList

instance
  Prelude.NFData
    PipeTargetSageMakerPipelineParameters
  where
  rnf PipeTargetSageMakerPipelineParameters' {..} =
    Prelude.rnf pipelineParameterList

instance
  Data.ToJSON
    PipeTargetSageMakerPipelineParameters
  where
  toJSON PipeTargetSageMakerPipelineParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PipelineParameterList" Data..=)
              Prelude.<$> pipelineParameterList
          ]
      )

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
-- Module      : Amazonka.Scheduler.Types.SageMakerPipelineParameters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Scheduler.Types.SageMakerPipelineParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Scheduler.Types.SageMakerPipelineParameter

-- | The templated target type for the Amazon SageMaker
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_StartPipelineExecution.html StartPipelineExecution>
-- API operation.
--
-- /See:/ 'newSageMakerPipelineParameters' smart constructor.
data SageMakerPipelineParameters = SageMakerPipelineParameters'
  { -- | List of parameter names and values to use when executing the SageMaker
    -- Model Building Pipeline.
    pipelineParameterList :: Prelude.Maybe [SageMakerPipelineParameter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SageMakerPipelineParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineParameterList', 'sageMakerPipelineParameters_pipelineParameterList' - List of parameter names and values to use when executing the SageMaker
-- Model Building Pipeline.
newSageMakerPipelineParameters ::
  SageMakerPipelineParameters
newSageMakerPipelineParameters =
  SageMakerPipelineParameters'
    { pipelineParameterList =
        Prelude.Nothing
    }

-- | List of parameter names and values to use when executing the SageMaker
-- Model Building Pipeline.
sageMakerPipelineParameters_pipelineParameterList :: Lens.Lens' SageMakerPipelineParameters (Prelude.Maybe [SageMakerPipelineParameter])
sageMakerPipelineParameters_pipelineParameterList = Lens.lens (\SageMakerPipelineParameters' {pipelineParameterList} -> pipelineParameterList) (\s@SageMakerPipelineParameters' {} a -> s {pipelineParameterList = a} :: SageMakerPipelineParameters) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON SageMakerPipelineParameters where
  parseJSON =
    Core.withObject
      "SageMakerPipelineParameters"
      ( \x ->
          SageMakerPipelineParameters'
            Prelude.<$> ( x Core..:? "PipelineParameterList"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable SageMakerPipelineParameters where
  hashWithSalt _salt SageMakerPipelineParameters' {..} =
    _salt `Prelude.hashWithSalt` pipelineParameterList

instance Prelude.NFData SageMakerPipelineParameters where
  rnf SageMakerPipelineParameters' {..} =
    Prelude.rnf pipelineParameterList

instance Core.ToJSON SageMakerPipelineParameters where
  toJSON SageMakerPipelineParameters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PipelineParameterList" Core..=)
              Prelude.<$> pipelineParameterList
          ]
      )

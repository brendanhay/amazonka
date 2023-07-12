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
-- Module      : Amazonka.CloudWatchEvents.Types.SageMakerPipelineParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.SageMakerPipelineParameters where

import Amazonka.CloudWatchEvents.Types.SageMakerPipelineParameter
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | These are custom parameters to use when the target is a SageMaker Model
-- Building Pipeline that starts based on EventBridge events.
--
-- /See:/ 'newSageMakerPipelineParameters' smart constructor.
data SageMakerPipelineParameters = SageMakerPipelineParameters'
  { -- | List of Parameter names and values for SageMaker Model Building Pipeline
    -- execution.
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
-- 'pipelineParameterList', 'sageMakerPipelineParameters_pipelineParameterList' - List of Parameter names and values for SageMaker Model Building Pipeline
-- execution.
newSageMakerPipelineParameters ::
  SageMakerPipelineParameters
newSageMakerPipelineParameters =
  SageMakerPipelineParameters'
    { pipelineParameterList =
        Prelude.Nothing
    }

-- | List of Parameter names and values for SageMaker Model Building Pipeline
-- execution.
sageMakerPipelineParameters_pipelineParameterList :: Lens.Lens' SageMakerPipelineParameters (Prelude.Maybe [SageMakerPipelineParameter])
sageMakerPipelineParameters_pipelineParameterList = Lens.lens (\SageMakerPipelineParameters' {pipelineParameterList} -> pipelineParameterList) (\s@SageMakerPipelineParameters' {} a -> s {pipelineParameterList = a} :: SageMakerPipelineParameters) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON SageMakerPipelineParameters where
  parseJSON =
    Data.withObject
      "SageMakerPipelineParameters"
      ( \x ->
          SageMakerPipelineParameters'
            Prelude.<$> ( x
                            Data..:? "PipelineParameterList"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable SageMakerPipelineParameters where
  hashWithSalt _salt SageMakerPipelineParameters' {..} =
    _salt `Prelude.hashWithSalt` pipelineParameterList

instance Prelude.NFData SageMakerPipelineParameters where
  rnf SageMakerPipelineParameters' {..} =
    Prelude.rnf pipelineParameterList

instance Data.ToJSON SageMakerPipelineParameters where
  toJSON SageMakerPipelineParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PipelineParameterList" Data..=)
              Prelude.<$> pipelineParameterList
          ]
      )

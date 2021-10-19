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
-- Module      : Network.AWS.CloudWatchEvents.Types.SageMakerPipelineParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.SageMakerPipelineParameters where

import Network.AWS.CloudWatchEvents.Types.SageMakerPipelineParameter
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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

instance Prelude.Hashable SageMakerPipelineParameters

instance Prelude.NFData SageMakerPipelineParameters

instance Core.ToJSON SageMakerPipelineParameters where
  toJSON SageMakerPipelineParameters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PipelineParameterList" Core..=)
              Prelude.<$> pipelineParameterList
          ]
      )

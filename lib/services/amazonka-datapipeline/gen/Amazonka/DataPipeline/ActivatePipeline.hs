{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DataPipeline.ActivatePipeline
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Validates the specified pipeline and starts processing pipeline tasks.
-- If the pipeline does not pass validation, activation fails.
--
-- If you need to pause the pipeline to investigate an issue with a
-- component, such as a data source or script, call DeactivatePipeline.
--
-- To activate a finished pipeline, modify the end date for the pipeline
-- and then activate it.
module Amazonka.DataPipeline.ActivatePipeline
  ( -- * Creating a Request
    ActivatePipeline (..),
    newActivatePipeline,

    -- * Request Lenses
    activatePipeline_startTimestamp,
    activatePipeline_parameterValues,
    activatePipeline_pipelineId,

    -- * Destructuring the Response
    ActivatePipelineResponse (..),
    newActivatePipelineResponse,

    -- * Response Lenses
    activatePipelineResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DataPipeline.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for ActivatePipeline.
--
-- /See:/ 'newActivatePipeline' smart constructor.
data ActivatePipeline = ActivatePipeline'
  { -- | The date and time to resume the pipeline. By default, the pipeline
    -- resumes from the last completed execution.
    startTimestamp :: Prelude.Maybe Core.POSIX,
    -- | A list of parameter values to pass to the pipeline at activation.
    parameterValues :: Prelude.Maybe [ParameterValue],
    -- | The ID of the pipeline.
    pipelineId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActivatePipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startTimestamp', 'activatePipeline_startTimestamp' - The date and time to resume the pipeline. By default, the pipeline
-- resumes from the last completed execution.
--
-- 'parameterValues', 'activatePipeline_parameterValues' - A list of parameter values to pass to the pipeline at activation.
--
-- 'pipelineId', 'activatePipeline_pipelineId' - The ID of the pipeline.
newActivatePipeline ::
  -- | 'pipelineId'
  Prelude.Text ->
  ActivatePipeline
newActivatePipeline pPipelineId_ =
  ActivatePipeline'
    { startTimestamp = Prelude.Nothing,
      parameterValues = Prelude.Nothing,
      pipelineId = pPipelineId_
    }

-- | The date and time to resume the pipeline. By default, the pipeline
-- resumes from the last completed execution.
activatePipeline_startTimestamp :: Lens.Lens' ActivatePipeline (Prelude.Maybe Prelude.UTCTime)
activatePipeline_startTimestamp = Lens.lens (\ActivatePipeline' {startTimestamp} -> startTimestamp) (\s@ActivatePipeline' {} a -> s {startTimestamp = a} :: ActivatePipeline) Prelude.. Lens.mapping Core._Time

-- | A list of parameter values to pass to the pipeline at activation.
activatePipeline_parameterValues :: Lens.Lens' ActivatePipeline (Prelude.Maybe [ParameterValue])
activatePipeline_parameterValues = Lens.lens (\ActivatePipeline' {parameterValues} -> parameterValues) (\s@ActivatePipeline' {} a -> s {parameterValues = a} :: ActivatePipeline) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the pipeline.
activatePipeline_pipelineId :: Lens.Lens' ActivatePipeline Prelude.Text
activatePipeline_pipelineId = Lens.lens (\ActivatePipeline' {pipelineId} -> pipelineId) (\s@ActivatePipeline' {} a -> s {pipelineId = a} :: ActivatePipeline)

instance Core.AWSRequest ActivatePipeline where
  type
    AWSResponse ActivatePipeline =
      ActivatePipelineResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          ActivatePipelineResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ActivatePipeline where
  hashWithSalt _salt ActivatePipeline' {..} =
    _salt `Prelude.hashWithSalt` startTimestamp
      `Prelude.hashWithSalt` parameterValues
      `Prelude.hashWithSalt` pipelineId

instance Prelude.NFData ActivatePipeline where
  rnf ActivatePipeline' {..} =
    Prelude.rnf startTimestamp
      `Prelude.seq` Prelude.rnf parameterValues
      `Prelude.seq` Prelude.rnf pipelineId

instance Core.ToHeaders ActivatePipeline where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DataPipeline.ActivatePipeline" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ActivatePipeline where
  toJSON ActivatePipeline' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("startTimestamp" Core..=)
              Prelude.<$> startTimestamp,
            ("parameterValues" Core..=)
              Prelude.<$> parameterValues,
            Prelude.Just ("pipelineId" Core..= pipelineId)
          ]
      )

instance Core.ToPath ActivatePipeline where
  toPath = Prelude.const "/"

instance Core.ToQuery ActivatePipeline where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the output of ActivatePipeline.
--
-- /See:/ 'newActivatePipelineResponse' smart constructor.
data ActivatePipelineResponse = ActivatePipelineResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActivatePipelineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'activatePipelineResponse_httpStatus' - The response's http status code.
newActivatePipelineResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ActivatePipelineResponse
newActivatePipelineResponse pHttpStatus_ =
  ActivatePipelineResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
activatePipelineResponse_httpStatus :: Lens.Lens' ActivatePipelineResponse Prelude.Int
activatePipelineResponse_httpStatus = Lens.lens (\ActivatePipelineResponse' {httpStatus} -> httpStatus) (\s@ActivatePipelineResponse' {} a -> s {httpStatus = a} :: ActivatePipelineResponse)

instance Prelude.NFData ActivatePipelineResponse where
  rnf ActivatePipelineResponse' {..} =
    Prelude.rnf httpStatus

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
-- Module      : Amazonka.CodePipeline.StopPipelineExecution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the specified pipeline execution. You choose to either stop the
-- pipeline execution by completing in-progress actions without starting
-- subsequent actions, or by abandoning in-progress actions. While
-- completing or abandoning in-progress actions, the pipeline execution is
-- in a @Stopping@ state. After all in-progress actions are completed or
-- abandoned, the pipeline execution is in a @Stopped@ state.
module Amazonka.CodePipeline.StopPipelineExecution
  ( -- * Creating a Request
    StopPipelineExecution (..),
    newStopPipelineExecution,

    -- * Request Lenses
    stopPipelineExecution_abandon,
    stopPipelineExecution_reason,
    stopPipelineExecution_pipelineName,
    stopPipelineExecution_pipelineExecutionId,

    -- * Destructuring the Response
    StopPipelineExecutionResponse (..),
    newStopPipelineExecutionResponse,

    -- * Response Lenses
    stopPipelineExecutionResponse_pipelineExecutionId,
    stopPipelineExecutionResponse_httpStatus,
  )
where

import Amazonka.CodePipeline.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopPipelineExecution' smart constructor.
data StopPipelineExecution = StopPipelineExecution'
  { -- | Use this option to stop the pipeline execution by abandoning, rather
    -- than finishing, in-progress actions.
    --
    -- This option can lead to failed or out-of-sequence tasks.
    abandon :: Prelude.Maybe Prelude.Bool,
    -- | Use this option to enter comments, such as the reason the pipeline was
    -- stopped.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The name of the pipeline to stop.
    pipelineName :: Prelude.Text,
    -- | The ID of the pipeline execution to be stopped in the current stage. Use
    -- the @GetPipelineState@ action to retrieve the current
    -- pipelineExecutionId.
    pipelineExecutionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopPipelineExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'abandon', 'stopPipelineExecution_abandon' - Use this option to stop the pipeline execution by abandoning, rather
-- than finishing, in-progress actions.
--
-- This option can lead to failed or out-of-sequence tasks.
--
-- 'reason', 'stopPipelineExecution_reason' - Use this option to enter comments, such as the reason the pipeline was
-- stopped.
--
-- 'pipelineName', 'stopPipelineExecution_pipelineName' - The name of the pipeline to stop.
--
-- 'pipelineExecutionId', 'stopPipelineExecution_pipelineExecutionId' - The ID of the pipeline execution to be stopped in the current stage. Use
-- the @GetPipelineState@ action to retrieve the current
-- pipelineExecutionId.
newStopPipelineExecution ::
  -- | 'pipelineName'
  Prelude.Text ->
  -- | 'pipelineExecutionId'
  Prelude.Text ->
  StopPipelineExecution
newStopPipelineExecution
  pPipelineName_
  pPipelineExecutionId_ =
    StopPipelineExecution'
      { abandon = Prelude.Nothing,
        reason = Prelude.Nothing,
        pipelineName = pPipelineName_,
        pipelineExecutionId = pPipelineExecutionId_
      }

-- | Use this option to stop the pipeline execution by abandoning, rather
-- than finishing, in-progress actions.
--
-- This option can lead to failed or out-of-sequence tasks.
stopPipelineExecution_abandon :: Lens.Lens' StopPipelineExecution (Prelude.Maybe Prelude.Bool)
stopPipelineExecution_abandon = Lens.lens (\StopPipelineExecution' {abandon} -> abandon) (\s@StopPipelineExecution' {} a -> s {abandon = a} :: StopPipelineExecution)

-- | Use this option to enter comments, such as the reason the pipeline was
-- stopped.
stopPipelineExecution_reason :: Lens.Lens' StopPipelineExecution (Prelude.Maybe Prelude.Text)
stopPipelineExecution_reason = Lens.lens (\StopPipelineExecution' {reason} -> reason) (\s@StopPipelineExecution' {} a -> s {reason = a} :: StopPipelineExecution)

-- | The name of the pipeline to stop.
stopPipelineExecution_pipelineName :: Lens.Lens' StopPipelineExecution Prelude.Text
stopPipelineExecution_pipelineName = Lens.lens (\StopPipelineExecution' {pipelineName} -> pipelineName) (\s@StopPipelineExecution' {} a -> s {pipelineName = a} :: StopPipelineExecution)

-- | The ID of the pipeline execution to be stopped in the current stage. Use
-- the @GetPipelineState@ action to retrieve the current
-- pipelineExecutionId.
stopPipelineExecution_pipelineExecutionId :: Lens.Lens' StopPipelineExecution Prelude.Text
stopPipelineExecution_pipelineExecutionId = Lens.lens (\StopPipelineExecution' {pipelineExecutionId} -> pipelineExecutionId) (\s@StopPipelineExecution' {} a -> s {pipelineExecutionId = a} :: StopPipelineExecution)

instance Core.AWSRequest StopPipelineExecution where
  type
    AWSResponse StopPipelineExecution =
      StopPipelineExecutionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopPipelineExecutionResponse'
            Prelude.<$> (x Data..?> "pipelineExecutionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopPipelineExecution where
  hashWithSalt _salt StopPipelineExecution' {..} =
    _salt
      `Prelude.hashWithSalt` abandon
      `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` pipelineName
      `Prelude.hashWithSalt` pipelineExecutionId

instance Prelude.NFData StopPipelineExecution where
  rnf StopPipelineExecution' {..} =
    Prelude.rnf abandon
      `Prelude.seq` Prelude.rnf reason
      `Prelude.seq` Prelude.rnf pipelineName
      `Prelude.seq` Prelude.rnf pipelineExecutionId

instance Data.ToHeaders StopPipelineExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodePipeline_20150709.StopPipelineExecution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopPipelineExecution where
  toJSON StopPipelineExecution' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("abandon" Data..=) Prelude.<$> abandon,
            ("reason" Data..=) Prelude.<$> reason,
            Prelude.Just ("pipelineName" Data..= pipelineName),
            Prelude.Just
              ("pipelineExecutionId" Data..= pipelineExecutionId)
          ]
      )

instance Data.ToPath StopPipelineExecution where
  toPath = Prelude.const "/"

instance Data.ToQuery StopPipelineExecution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopPipelineExecutionResponse' smart constructor.
data StopPipelineExecutionResponse = StopPipelineExecutionResponse'
  { -- | The unique system-generated ID of the pipeline execution that was
    -- stopped.
    pipelineExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopPipelineExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineExecutionId', 'stopPipelineExecutionResponse_pipelineExecutionId' - The unique system-generated ID of the pipeline execution that was
-- stopped.
--
-- 'httpStatus', 'stopPipelineExecutionResponse_httpStatus' - The response's http status code.
newStopPipelineExecutionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopPipelineExecutionResponse
newStopPipelineExecutionResponse pHttpStatus_ =
  StopPipelineExecutionResponse'
    { pipelineExecutionId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique system-generated ID of the pipeline execution that was
-- stopped.
stopPipelineExecutionResponse_pipelineExecutionId :: Lens.Lens' StopPipelineExecutionResponse (Prelude.Maybe Prelude.Text)
stopPipelineExecutionResponse_pipelineExecutionId = Lens.lens (\StopPipelineExecutionResponse' {pipelineExecutionId} -> pipelineExecutionId) (\s@StopPipelineExecutionResponse' {} a -> s {pipelineExecutionId = a} :: StopPipelineExecutionResponse)

-- | The response's http status code.
stopPipelineExecutionResponse_httpStatus :: Lens.Lens' StopPipelineExecutionResponse Prelude.Int
stopPipelineExecutionResponse_httpStatus = Lens.lens (\StopPipelineExecutionResponse' {httpStatus} -> httpStatus) (\s@StopPipelineExecutionResponse' {} a -> s {httpStatus = a} :: StopPipelineExecutionResponse)

instance Prelude.NFData StopPipelineExecutionResponse where
  rnf StopPipelineExecutionResponse' {..} =
    Prelude.rnf pipelineExecutionId
      `Prelude.seq` Prelude.rnf httpStatus

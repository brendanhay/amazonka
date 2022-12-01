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
-- Module      : Amazonka.Polly.GetSpeechSynthesisTask
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a specific SpeechSynthesisTask object based on its TaskID.
-- This object contains information about the given speech synthesis task,
-- including the status of the task, and a link to the S3 bucket containing
-- the output of the task.
module Amazonka.Polly.GetSpeechSynthesisTask
  ( -- * Creating a Request
    GetSpeechSynthesisTask (..),
    newGetSpeechSynthesisTask,

    -- * Request Lenses
    getSpeechSynthesisTask_taskId,

    -- * Destructuring the Response
    GetSpeechSynthesisTaskResponse (..),
    newGetSpeechSynthesisTaskResponse,

    -- * Response Lenses
    getSpeechSynthesisTaskResponse_synthesisTask,
    getSpeechSynthesisTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Polly.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSpeechSynthesisTask' smart constructor.
data GetSpeechSynthesisTask = GetSpeechSynthesisTask'
  { -- | The Amazon Polly generated identifier for a speech synthesis task.
    taskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSpeechSynthesisTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskId', 'getSpeechSynthesisTask_taskId' - The Amazon Polly generated identifier for a speech synthesis task.
newGetSpeechSynthesisTask ::
  -- | 'taskId'
  Prelude.Text ->
  GetSpeechSynthesisTask
newGetSpeechSynthesisTask pTaskId_ =
  GetSpeechSynthesisTask' {taskId = pTaskId_}

-- | The Amazon Polly generated identifier for a speech synthesis task.
getSpeechSynthesisTask_taskId :: Lens.Lens' GetSpeechSynthesisTask Prelude.Text
getSpeechSynthesisTask_taskId = Lens.lens (\GetSpeechSynthesisTask' {taskId} -> taskId) (\s@GetSpeechSynthesisTask' {} a -> s {taskId = a} :: GetSpeechSynthesisTask)

instance Core.AWSRequest GetSpeechSynthesisTask where
  type
    AWSResponse GetSpeechSynthesisTask =
      GetSpeechSynthesisTaskResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSpeechSynthesisTaskResponse'
            Prelude.<$> (x Core..?> "SynthesisTask")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSpeechSynthesisTask where
  hashWithSalt _salt GetSpeechSynthesisTask' {..} =
    _salt `Prelude.hashWithSalt` taskId

instance Prelude.NFData GetSpeechSynthesisTask where
  rnf GetSpeechSynthesisTask' {..} = Prelude.rnf taskId

instance Core.ToHeaders GetSpeechSynthesisTask where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetSpeechSynthesisTask where
  toPath GetSpeechSynthesisTask' {..} =
    Prelude.mconcat
      ["/v1/synthesisTasks/", Core.toBS taskId]

instance Core.ToQuery GetSpeechSynthesisTask where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSpeechSynthesisTaskResponse' smart constructor.
data GetSpeechSynthesisTaskResponse = GetSpeechSynthesisTaskResponse'
  { -- | SynthesisTask object that provides information from the requested task,
    -- including output format, creation time, task status, and so on.
    synthesisTask :: Prelude.Maybe SynthesisTask,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSpeechSynthesisTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'synthesisTask', 'getSpeechSynthesisTaskResponse_synthesisTask' - SynthesisTask object that provides information from the requested task,
-- including output format, creation time, task status, and so on.
--
-- 'httpStatus', 'getSpeechSynthesisTaskResponse_httpStatus' - The response's http status code.
newGetSpeechSynthesisTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSpeechSynthesisTaskResponse
newGetSpeechSynthesisTaskResponse pHttpStatus_ =
  GetSpeechSynthesisTaskResponse'
    { synthesisTask =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | SynthesisTask object that provides information from the requested task,
-- including output format, creation time, task status, and so on.
getSpeechSynthesisTaskResponse_synthesisTask :: Lens.Lens' GetSpeechSynthesisTaskResponse (Prelude.Maybe SynthesisTask)
getSpeechSynthesisTaskResponse_synthesisTask = Lens.lens (\GetSpeechSynthesisTaskResponse' {synthesisTask} -> synthesisTask) (\s@GetSpeechSynthesisTaskResponse' {} a -> s {synthesisTask = a} :: GetSpeechSynthesisTaskResponse)

-- | The response's http status code.
getSpeechSynthesisTaskResponse_httpStatus :: Lens.Lens' GetSpeechSynthesisTaskResponse Prelude.Int
getSpeechSynthesisTaskResponse_httpStatus = Lens.lens (\GetSpeechSynthesisTaskResponse' {httpStatus} -> httpStatus) (\s@GetSpeechSynthesisTaskResponse' {} a -> s {httpStatus = a} :: GetSpeechSynthesisTaskResponse)

instance
  Prelude.NFData
    GetSpeechSynthesisTaskResponse
  where
  rnf GetSpeechSynthesisTaskResponse' {..} =
    Prelude.rnf synthesisTask
      `Prelude.seq` Prelude.rnf httpStatus

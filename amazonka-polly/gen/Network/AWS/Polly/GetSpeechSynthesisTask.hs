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
-- Module      : Network.AWS.Polly.GetSpeechSynthesisTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a specific SpeechSynthesisTask object based on its TaskID.
-- This object contains information about the given speech synthesis task,
-- including the status of the task, and a link to the S3 bucket containing
-- the output of the task.
module Network.AWS.Polly.GetSpeechSynthesisTask
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Polly.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetSpeechSynthesisTask' smart constructor.
data GetSpeechSynthesisTask = GetSpeechSynthesisTask'
  { -- | The Amazon Polly generated identifier for a speech synthesis task.
    taskId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  GetSpeechSynthesisTask
newGetSpeechSynthesisTask pTaskId_ =
  GetSpeechSynthesisTask' {taskId = pTaskId_}

-- | The Amazon Polly generated identifier for a speech synthesis task.
getSpeechSynthesisTask_taskId :: Lens.Lens' GetSpeechSynthesisTask Core.Text
getSpeechSynthesisTask_taskId = Lens.lens (\GetSpeechSynthesisTask' {taskId} -> taskId) (\s@GetSpeechSynthesisTask' {} a -> s {taskId = a} :: GetSpeechSynthesisTask)

instance Core.AWSRequest GetSpeechSynthesisTask where
  type
    AWSResponse GetSpeechSynthesisTask =
      GetSpeechSynthesisTaskResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSpeechSynthesisTaskResponse'
            Core.<$> (x Core..?> "SynthesisTask")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetSpeechSynthesisTask

instance Core.NFData GetSpeechSynthesisTask

instance Core.ToHeaders GetSpeechSynthesisTask where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetSpeechSynthesisTask where
  toPath GetSpeechSynthesisTask' {..} =
    Core.mconcat
      ["/v1/synthesisTasks/", Core.toBS taskId]

instance Core.ToQuery GetSpeechSynthesisTask where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetSpeechSynthesisTaskResponse' smart constructor.
data GetSpeechSynthesisTaskResponse = GetSpeechSynthesisTaskResponse'
  { -- | SynthesisTask object that provides information from the requested task,
    -- including output format, creation time, task status, and so on.
    synthesisTask :: Core.Maybe SynthesisTask,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetSpeechSynthesisTaskResponse
newGetSpeechSynthesisTaskResponse pHttpStatus_ =
  GetSpeechSynthesisTaskResponse'
    { synthesisTask =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | SynthesisTask object that provides information from the requested task,
-- including output format, creation time, task status, and so on.
getSpeechSynthesisTaskResponse_synthesisTask :: Lens.Lens' GetSpeechSynthesisTaskResponse (Core.Maybe SynthesisTask)
getSpeechSynthesisTaskResponse_synthesisTask = Lens.lens (\GetSpeechSynthesisTaskResponse' {synthesisTask} -> synthesisTask) (\s@GetSpeechSynthesisTaskResponse' {} a -> s {synthesisTask = a} :: GetSpeechSynthesisTaskResponse)

-- | The response's http status code.
getSpeechSynthesisTaskResponse_httpStatus :: Lens.Lens' GetSpeechSynthesisTaskResponse Core.Int
getSpeechSynthesisTaskResponse_httpStatus = Lens.lens (\GetSpeechSynthesisTaskResponse' {httpStatus} -> httpStatus) (\s@GetSpeechSynthesisTaskResponse' {} a -> s {httpStatus = a} :: GetSpeechSynthesisTaskResponse)

instance Core.NFData GetSpeechSynthesisTaskResponse

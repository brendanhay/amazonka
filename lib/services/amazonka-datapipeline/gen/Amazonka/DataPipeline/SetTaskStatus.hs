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
-- Module      : Amazonka.DataPipeline.SetTaskStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Task runners call @SetTaskStatus@ to notify AWS Data Pipeline that a
-- task is completed and provide information about the final status. A task
-- runner makes this call regardless of whether the task was sucessful. A
-- task runner does not need to call @SetTaskStatus@ for tasks that are
-- canceled by the web service during a call to ReportTaskProgress.
module Amazonka.DataPipeline.SetTaskStatus
  ( -- * Creating a Request
    SetTaskStatus (..),
    newSetTaskStatus,

    -- * Request Lenses
    setTaskStatus_errorId,
    setTaskStatus_errorMessage,
    setTaskStatus_errorStackTrace,
    setTaskStatus_taskId,
    setTaskStatus_taskStatus,

    -- * Destructuring the Response
    SetTaskStatusResponse (..),
    newSetTaskStatusResponse,

    -- * Response Lenses
    setTaskStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataPipeline.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for SetTaskStatus.
--
-- /See:/ 'newSetTaskStatus' smart constructor.
data SetTaskStatus = SetTaskStatus'
  { -- | If an error occurred during the task, this value specifies the error
    -- code. This value is set on the physical attempt object. It is used to
    -- display error information to the user. It should not start with string
    -- \"Service_\" which is reserved by the system.
    errorId :: Prelude.Maybe Prelude.Text,
    -- | If an error occurred during the task, this value specifies a text
    -- description of the error. This value is set on the physical attempt
    -- object. It is used to display error information to the user. The web
    -- service does not parse this value.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | If an error occurred during the task, this value specifies the stack
    -- trace associated with the error. This value is set on the physical
    -- attempt object. It is used to display error information to the user. The
    -- web service does not parse this value.
    errorStackTrace :: Prelude.Maybe Prelude.Text,
    -- | The ID of the task assigned to the task runner. This value is provided
    -- in the response for PollForTask.
    taskId :: Prelude.Text,
    -- | If @FINISHED@, the task successfully completed. If @FAILED@, the task
    -- ended unsuccessfully. Preconditions use false.
    taskStatus :: TaskStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetTaskStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorId', 'setTaskStatus_errorId' - If an error occurred during the task, this value specifies the error
-- code. This value is set on the physical attempt object. It is used to
-- display error information to the user. It should not start with string
-- \"Service_\" which is reserved by the system.
--
-- 'errorMessage', 'setTaskStatus_errorMessage' - If an error occurred during the task, this value specifies a text
-- description of the error. This value is set on the physical attempt
-- object. It is used to display error information to the user. The web
-- service does not parse this value.
--
-- 'errorStackTrace', 'setTaskStatus_errorStackTrace' - If an error occurred during the task, this value specifies the stack
-- trace associated with the error. This value is set on the physical
-- attempt object. It is used to display error information to the user. The
-- web service does not parse this value.
--
-- 'taskId', 'setTaskStatus_taskId' - The ID of the task assigned to the task runner. This value is provided
-- in the response for PollForTask.
--
-- 'taskStatus', 'setTaskStatus_taskStatus' - If @FINISHED@, the task successfully completed. If @FAILED@, the task
-- ended unsuccessfully. Preconditions use false.
newSetTaskStatus ::
  -- | 'taskId'
  Prelude.Text ->
  -- | 'taskStatus'
  TaskStatus ->
  SetTaskStatus
newSetTaskStatus pTaskId_ pTaskStatus_ =
  SetTaskStatus'
    { errorId = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      errorStackTrace = Prelude.Nothing,
      taskId = pTaskId_,
      taskStatus = pTaskStatus_
    }

-- | If an error occurred during the task, this value specifies the error
-- code. This value is set on the physical attempt object. It is used to
-- display error information to the user. It should not start with string
-- \"Service_\" which is reserved by the system.
setTaskStatus_errorId :: Lens.Lens' SetTaskStatus (Prelude.Maybe Prelude.Text)
setTaskStatus_errorId = Lens.lens (\SetTaskStatus' {errorId} -> errorId) (\s@SetTaskStatus' {} a -> s {errorId = a} :: SetTaskStatus)

-- | If an error occurred during the task, this value specifies a text
-- description of the error. This value is set on the physical attempt
-- object. It is used to display error information to the user. The web
-- service does not parse this value.
setTaskStatus_errorMessage :: Lens.Lens' SetTaskStatus (Prelude.Maybe Prelude.Text)
setTaskStatus_errorMessage = Lens.lens (\SetTaskStatus' {errorMessage} -> errorMessage) (\s@SetTaskStatus' {} a -> s {errorMessage = a} :: SetTaskStatus)

-- | If an error occurred during the task, this value specifies the stack
-- trace associated with the error. This value is set on the physical
-- attempt object. It is used to display error information to the user. The
-- web service does not parse this value.
setTaskStatus_errorStackTrace :: Lens.Lens' SetTaskStatus (Prelude.Maybe Prelude.Text)
setTaskStatus_errorStackTrace = Lens.lens (\SetTaskStatus' {errorStackTrace} -> errorStackTrace) (\s@SetTaskStatus' {} a -> s {errorStackTrace = a} :: SetTaskStatus)

-- | The ID of the task assigned to the task runner. This value is provided
-- in the response for PollForTask.
setTaskStatus_taskId :: Lens.Lens' SetTaskStatus Prelude.Text
setTaskStatus_taskId = Lens.lens (\SetTaskStatus' {taskId} -> taskId) (\s@SetTaskStatus' {} a -> s {taskId = a} :: SetTaskStatus)

-- | If @FINISHED@, the task successfully completed. If @FAILED@, the task
-- ended unsuccessfully. Preconditions use false.
setTaskStatus_taskStatus :: Lens.Lens' SetTaskStatus TaskStatus
setTaskStatus_taskStatus = Lens.lens (\SetTaskStatus' {taskStatus} -> taskStatus) (\s@SetTaskStatus' {} a -> s {taskStatus = a} :: SetTaskStatus)

instance Core.AWSRequest SetTaskStatus where
  type
    AWSResponse SetTaskStatus =
      SetTaskStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          SetTaskStatusResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SetTaskStatus where
  hashWithSalt _salt SetTaskStatus' {..} =
    _salt
      `Prelude.hashWithSalt` errorId
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` errorStackTrace
      `Prelude.hashWithSalt` taskId
      `Prelude.hashWithSalt` taskStatus

instance Prelude.NFData SetTaskStatus where
  rnf SetTaskStatus' {..} =
    Prelude.rnf errorId `Prelude.seq`
      Prelude.rnf errorMessage `Prelude.seq`
        Prelude.rnf errorStackTrace `Prelude.seq`
          Prelude.rnf taskId `Prelude.seq`
            Prelude.rnf taskStatus

instance Data.ToHeaders SetTaskStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("DataPipeline.SetTaskStatus" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SetTaskStatus where
  toJSON SetTaskStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("errorId" Data..=) Prelude.<$> errorId,
            ("errorMessage" Data..=) Prelude.<$> errorMessage,
            ("errorStackTrace" Data..=)
              Prelude.<$> errorStackTrace,
            Prelude.Just ("taskId" Data..= taskId),
            Prelude.Just ("taskStatus" Data..= taskStatus)
          ]
      )

instance Data.ToPath SetTaskStatus where
  toPath = Prelude.const "/"

instance Data.ToQuery SetTaskStatus where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the output of SetTaskStatus.
--
-- /See:/ 'newSetTaskStatusResponse' smart constructor.
data SetTaskStatusResponse = SetTaskStatusResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetTaskStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'setTaskStatusResponse_httpStatus' - The response's http status code.
newSetTaskStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SetTaskStatusResponse
newSetTaskStatusResponse pHttpStatus_ =
  SetTaskStatusResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
setTaskStatusResponse_httpStatus :: Lens.Lens' SetTaskStatusResponse Prelude.Int
setTaskStatusResponse_httpStatus = Lens.lens (\SetTaskStatusResponse' {httpStatus} -> httpStatus) (\s@SetTaskStatusResponse' {} a -> s {httpStatus = a} :: SetTaskStatusResponse)

instance Prelude.NFData SetTaskStatusResponse where
  rnf SetTaskStatusResponse' {..} =
    Prelude.rnf httpStatus

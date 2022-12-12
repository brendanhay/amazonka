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
-- Module      : Amazonka.DataSync.StartTaskExecution
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a specific invocation of a task. A @TaskExecution@ value
-- represents an individual run of a task. Each task can have at most one
-- @TaskExecution@ at a time.
--
-- @TaskExecution@ has the following transition phases: INITIALIZING |
-- PREPARING | TRANSFERRING | VERIFYING | SUCCESS\/FAILURE.
--
-- For detailed information, see the Task Execution section in the
-- Components and Terminology topic in the /DataSync User Guide/.
module Amazonka.DataSync.StartTaskExecution
  ( -- * Creating a Request
    StartTaskExecution (..),
    newStartTaskExecution,

    -- * Request Lenses
    startTaskExecution_excludes,
    startTaskExecution_includes,
    startTaskExecution_overrideOptions,
    startTaskExecution_taskArn,

    -- * Destructuring the Response
    StartTaskExecutionResponse (..),
    newStartTaskExecutionResponse,

    -- * Response Lenses
    startTaskExecutionResponse_taskExecutionArn,
    startTaskExecutionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | StartTaskExecutionRequest
--
-- /See:/ 'newStartTaskExecution' smart constructor.
data StartTaskExecution = StartTaskExecution'
  { -- | A list of filter rules that determines which files to exclude from a
    -- task. The list contains a single filter string that consists of the
    -- patterns to exclude. The patterns are delimited by \"|\" (that is, a
    -- pipe), for example, @\"\/folder1|\/folder2\"@.
    excludes :: Prelude.Maybe [FilterRule],
    -- | A list of filter rules that determines which files to include when
    -- running a task. The pattern should contain a single filter string that
    -- consists of the patterns to include. The patterns are delimited by \"|\"
    -- (that is, a pipe), for example, @\"\/folder1|\/folder2\"@.
    includes :: Prelude.Maybe [FilterRule],
    overrideOptions :: Prelude.Maybe Options,
    -- | The Amazon Resource Name (ARN) of the task to start.
    taskArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartTaskExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'excludes', 'startTaskExecution_excludes' - A list of filter rules that determines which files to exclude from a
-- task. The list contains a single filter string that consists of the
-- patterns to exclude. The patterns are delimited by \"|\" (that is, a
-- pipe), for example, @\"\/folder1|\/folder2\"@.
--
-- 'includes', 'startTaskExecution_includes' - A list of filter rules that determines which files to include when
-- running a task. The pattern should contain a single filter string that
-- consists of the patterns to include. The patterns are delimited by \"|\"
-- (that is, a pipe), for example, @\"\/folder1|\/folder2\"@.
--
-- 'overrideOptions', 'startTaskExecution_overrideOptions' - Undocumented member.
--
-- 'taskArn', 'startTaskExecution_taskArn' - The Amazon Resource Name (ARN) of the task to start.
newStartTaskExecution ::
  -- | 'taskArn'
  Prelude.Text ->
  StartTaskExecution
newStartTaskExecution pTaskArn_ =
  StartTaskExecution'
    { excludes = Prelude.Nothing,
      includes = Prelude.Nothing,
      overrideOptions = Prelude.Nothing,
      taskArn = pTaskArn_
    }

-- | A list of filter rules that determines which files to exclude from a
-- task. The list contains a single filter string that consists of the
-- patterns to exclude. The patterns are delimited by \"|\" (that is, a
-- pipe), for example, @\"\/folder1|\/folder2\"@.
startTaskExecution_excludes :: Lens.Lens' StartTaskExecution (Prelude.Maybe [FilterRule])
startTaskExecution_excludes = Lens.lens (\StartTaskExecution' {excludes} -> excludes) (\s@StartTaskExecution' {} a -> s {excludes = a} :: StartTaskExecution) Prelude.. Lens.mapping Lens.coerced

-- | A list of filter rules that determines which files to include when
-- running a task. The pattern should contain a single filter string that
-- consists of the patterns to include. The patterns are delimited by \"|\"
-- (that is, a pipe), for example, @\"\/folder1|\/folder2\"@.
startTaskExecution_includes :: Lens.Lens' StartTaskExecution (Prelude.Maybe [FilterRule])
startTaskExecution_includes = Lens.lens (\StartTaskExecution' {includes} -> includes) (\s@StartTaskExecution' {} a -> s {includes = a} :: StartTaskExecution) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
startTaskExecution_overrideOptions :: Lens.Lens' StartTaskExecution (Prelude.Maybe Options)
startTaskExecution_overrideOptions = Lens.lens (\StartTaskExecution' {overrideOptions} -> overrideOptions) (\s@StartTaskExecution' {} a -> s {overrideOptions = a} :: StartTaskExecution)

-- | The Amazon Resource Name (ARN) of the task to start.
startTaskExecution_taskArn :: Lens.Lens' StartTaskExecution Prelude.Text
startTaskExecution_taskArn = Lens.lens (\StartTaskExecution' {taskArn} -> taskArn) (\s@StartTaskExecution' {} a -> s {taskArn = a} :: StartTaskExecution)

instance Core.AWSRequest StartTaskExecution where
  type
    AWSResponse StartTaskExecution =
      StartTaskExecutionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartTaskExecutionResponse'
            Prelude.<$> (x Data..?> "TaskExecutionArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartTaskExecution where
  hashWithSalt _salt StartTaskExecution' {..} =
    _salt `Prelude.hashWithSalt` excludes
      `Prelude.hashWithSalt` includes
      `Prelude.hashWithSalt` overrideOptions
      `Prelude.hashWithSalt` taskArn

instance Prelude.NFData StartTaskExecution where
  rnf StartTaskExecution' {..} =
    Prelude.rnf excludes
      `Prelude.seq` Prelude.rnf includes
      `Prelude.seq` Prelude.rnf overrideOptions
      `Prelude.seq` Prelude.rnf taskArn

instance Data.ToHeaders StartTaskExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "FmrsService.StartTaskExecution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartTaskExecution where
  toJSON StartTaskExecution' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Excludes" Data..=) Prelude.<$> excludes,
            ("Includes" Data..=) Prelude.<$> includes,
            ("OverrideOptions" Data..=)
              Prelude.<$> overrideOptions,
            Prelude.Just ("TaskArn" Data..= taskArn)
          ]
      )

instance Data.ToPath StartTaskExecution where
  toPath = Prelude.const "/"

instance Data.ToQuery StartTaskExecution where
  toQuery = Prelude.const Prelude.mempty

-- | StartTaskExecutionResponse
--
-- /See:/ 'newStartTaskExecutionResponse' smart constructor.
data StartTaskExecutionResponse = StartTaskExecutionResponse'
  { -- | The Amazon Resource Name (ARN) of the specific task execution that was
    -- started.
    taskExecutionArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartTaskExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskExecutionArn', 'startTaskExecutionResponse_taskExecutionArn' - The Amazon Resource Name (ARN) of the specific task execution that was
-- started.
--
-- 'httpStatus', 'startTaskExecutionResponse_httpStatus' - The response's http status code.
newStartTaskExecutionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartTaskExecutionResponse
newStartTaskExecutionResponse pHttpStatus_ =
  StartTaskExecutionResponse'
    { taskExecutionArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the specific task execution that was
-- started.
startTaskExecutionResponse_taskExecutionArn :: Lens.Lens' StartTaskExecutionResponse (Prelude.Maybe Prelude.Text)
startTaskExecutionResponse_taskExecutionArn = Lens.lens (\StartTaskExecutionResponse' {taskExecutionArn} -> taskExecutionArn) (\s@StartTaskExecutionResponse' {} a -> s {taskExecutionArn = a} :: StartTaskExecutionResponse)

-- | The response's http status code.
startTaskExecutionResponse_httpStatus :: Lens.Lens' StartTaskExecutionResponse Prelude.Int
startTaskExecutionResponse_httpStatus = Lens.lens (\StartTaskExecutionResponse' {httpStatus} -> httpStatus) (\s@StartTaskExecutionResponse' {} a -> s {httpStatus = a} :: StartTaskExecutionResponse)

instance Prelude.NFData StartTaskExecutionResponse where
  rnf StartTaskExecutionResponse' {..} =
    Prelude.rnf taskExecutionArn
      `Prelude.seq` Prelude.rnf httpStatus

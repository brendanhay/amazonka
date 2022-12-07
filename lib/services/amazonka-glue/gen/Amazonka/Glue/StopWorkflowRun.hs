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
-- Module      : Amazonka.Glue.StopWorkflowRun
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the execution of the specified workflow run.
module Amazonka.Glue.StopWorkflowRun
  ( -- * Creating a Request
    StopWorkflowRun (..),
    newStopWorkflowRun,

    -- * Request Lenses
    stopWorkflowRun_name,
    stopWorkflowRun_runId,

    -- * Destructuring the Response
    StopWorkflowRunResponse (..),
    newStopWorkflowRunResponse,

    -- * Response Lenses
    stopWorkflowRunResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopWorkflowRun' smart constructor.
data StopWorkflowRun = StopWorkflowRun'
  { -- | The name of the workflow to stop.
    name :: Prelude.Text,
    -- | The ID of the workflow run to stop.
    runId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopWorkflowRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'stopWorkflowRun_name' - The name of the workflow to stop.
--
-- 'runId', 'stopWorkflowRun_runId' - The ID of the workflow run to stop.
newStopWorkflowRun ::
  -- | 'name'
  Prelude.Text ->
  -- | 'runId'
  Prelude.Text ->
  StopWorkflowRun
newStopWorkflowRun pName_ pRunId_ =
  StopWorkflowRun' {name = pName_, runId = pRunId_}

-- | The name of the workflow to stop.
stopWorkflowRun_name :: Lens.Lens' StopWorkflowRun Prelude.Text
stopWorkflowRun_name = Lens.lens (\StopWorkflowRun' {name} -> name) (\s@StopWorkflowRun' {} a -> s {name = a} :: StopWorkflowRun)

-- | The ID of the workflow run to stop.
stopWorkflowRun_runId :: Lens.Lens' StopWorkflowRun Prelude.Text
stopWorkflowRun_runId = Lens.lens (\StopWorkflowRun' {runId} -> runId) (\s@StopWorkflowRun' {} a -> s {runId = a} :: StopWorkflowRun)

instance Core.AWSRequest StopWorkflowRun where
  type
    AWSResponse StopWorkflowRun =
      StopWorkflowRunResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopWorkflowRunResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopWorkflowRun where
  hashWithSalt _salt StopWorkflowRun' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` runId

instance Prelude.NFData StopWorkflowRun where
  rnf StopWorkflowRun' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf runId

instance Data.ToHeaders StopWorkflowRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.StopWorkflowRun" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopWorkflowRun where
  toJSON StopWorkflowRun' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("RunId" Data..= runId)
          ]
      )

instance Data.ToPath StopWorkflowRun where
  toPath = Prelude.const "/"

instance Data.ToQuery StopWorkflowRun where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopWorkflowRunResponse' smart constructor.
data StopWorkflowRunResponse = StopWorkflowRunResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopWorkflowRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopWorkflowRunResponse_httpStatus' - The response's http status code.
newStopWorkflowRunResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopWorkflowRunResponse
newStopWorkflowRunResponse pHttpStatus_ =
  StopWorkflowRunResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
stopWorkflowRunResponse_httpStatus :: Lens.Lens' StopWorkflowRunResponse Prelude.Int
stopWorkflowRunResponse_httpStatus = Lens.lens (\StopWorkflowRunResponse' {httpStatus} -> httpStatus) (\s@StopWorkflowRunResponse' {} a -> s {httpStatus = a} :: StopWorkflowRunResponse)

instance Prelude.NFData StopWorkflowRunResponse where
  rnf StopWorkflowRunResponse' {..} =
    Prelude.rnf httpStatus

{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Glue.StopWorkflowRun
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the execution of the specified workflow run.
module Network.AWS.Glue.StopWorkflowRun
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

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopWorkflowRun' smart constructor.
data StopWorkflowRun = StopWorkflowRun'
  { -- | The name of the workflow to stop.
    name :: Prelude.Text,
    -- | The ID of the workflow run to stop.
    runId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest StopWorkflowRun where
  type Rs StopWorkflowRun = StopWorkflowRunResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopWorkflowRunResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopWorkflowRun

instance Prelude.NFData StopWorkflowRun

instance Prelude.ToHeaders StopWorkflowRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSGlue.StopWorkflowRun" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StopWorkflowRun where
  toJSON StopWorkflowRun' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Prelude..= name),
            Prelude.Just ("RunId" Prelude..= runId)
          ]
      )

instance Prelude.ToPath StopWorkflowRun where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StopWorkflowRun where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopWorkflowRunResponse' smart constructor.
data StopWorkflowRunResponse = StopWorkflowRunResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData StopWorkflowRunResponse

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
-- Module      : Amazonka.Glue.GetWorkflowRun
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the metadata for a given workflow run.
module Amazonka.Glue.GetWorkflowRun
  ( -- * Creating a Request
    GetWorkflowRun (..),
    newGetWorkflowRun,

    -- * Request Lenses
    getWorkflowRun_includeGraph,
    getWorkflowRun_name,
    getWorkflowRun_runId,

    -- * Destructuring the Response
    GetWorkflowRunResponse (..),
    newGetWorkflowRunResponse,

    -- * Response Lenses
    getWorkflowRunResponse_run,
    getWorkflowRunResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetWorkflowRun' smart constructor.
data GetWorkflowRun = GetWorkflowRun'
  { -- | Specifies whether to include the workflow graph in response or not.
    includeGraph :: Prelude.Maybe Prelude.Bool,
    -- | Name of the workflow being run.
    name :: Prelude.Text,
    -- | The ID of the workflow run.
    runId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWorkflowRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeGraph', 'getWorkflowRun_includeGraph' - Specifies whether to include the workflow graph in response or not.
--
-- 'name', 'getWorkflowRun_name' - Name of the workflow being run.
--
-- 'runId', 'getWorkflowRun_runId' - The ID of the workflow run.
newGetWorkflowRun ::
  -- | 'name'
  Prelude.Text ->
  -- | 'runId'
  Prelude.Text ->
  GetWorkflowRun
newGetWorkflowRun pName_ pRunId_ =
  GetWorkflowRun'
    { includeGraph = Prelude.Nothing,
      name = pName_,
      runId = pRunId_
    }

-- | Specifies whether to include the workflow graph in response or not.
getWorkflowRun_includeGraph :: Lens.Lens' GetWorkflowRun (Prelude.Maybe Prelude.Bool)
getWorkflowRun_includeGraph = Lens.lens (\GetWorkflowRun' {includeGraph} -> includeGraph) (\s@GetWorkflowRun' {} a -> s {includeGraph = a} :: GetWorkflowRun)

-- | Name of the workflow being run.
getWorkflowRun_name :: Lens.Lens' GetWorkflowRun Prelude.Text
getWorkflowRun_name = Lens.lens (\GetWorkflowRun' {name} -> name) (\s@GetWorkflowRun' {} a -> s {name = a} :: GetWorkflowRun)

-- | The ID of the workflow run.
getWorkflowRun_runId :: Lens.Lens' GetWorkflowRun Prelude.Text
getWorkflowRun_runId = Lens.lens (\GetWorkflowRun' {runId} -> runId) (\s@GetWorkflowRun' {} a -> s {runId = a} :: GetWorkflowRun)

instance Core.AWSRequest GetWorkflowRun where
  type
    AWSResponse GetWorkflowRun =
      GetWorkflowRunResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWorkflowRunResponse'
            Prelude.<$> (x Data..?> "Run")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetWorkflowRun where
  hashWithSalt _salt GetWorkflowRun' {..} =
    _salt `Prelude.hashWithSalt` includeGraph
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` runId

instance Prelude.NFData GetWorkflowRun where
  rnf GetWorkflowRun' {..} =
    Prelude.rnf includeGraph
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf runId

instance Data.ToHeaders GetWorkflowRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.GetWorkflowRun" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetWorkflowRun where
  toJSON GetWorkflowRun' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IncludeGraph" Data..=) Prelude.<$> includeGraph,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("RunId" Data..= runId)
          ]
      )

instance Data.ToPath GetWorkflowRun where
  toPath = Prelude.const "/"

instance Data.ToQuery GetWorkflowRun where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetWorkflowRunResponse' smart constructor.
data GetWorkflowRunResponse = GetWorkflowRunResponse'
  { -- | The requested workflow run metadata.
    run :: Prelude.Maybe WorkflowRun,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWorkflowRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'run', 'getWorkflowRunResponse_run' - The requested workflow run metadata.
--
-- 'httpStatus', 'getWorkflowRunResponse_httpStatus' - The response's http status code.
newGetWorkflowRunResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetWorkflowRunResponse
newGetWorkflowRunResponse pHttpStatus_ =
  GetWorkflowRunResponse'
    { run = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The requested workflow run metadata.
getWorkflowRunResponse_run :: Lens.Lens' GetWorkflowRunResponse (Prelude.Maybe WorkflowRun)
getWorkflowRunResponse_run = Lens.lens (\GetWorkflowRunResponse' {run} -> run) (\s@GetWorkflowRunResponse' {} a -> s {run = a} :: GetWorkflowRunResponse)

-- | The response's http status code.
getWorkflowRunResponse_httpStatus :: Lens.Lens' GetWorkflowRunResponse Prelude.Int
getWorkflowRunResponse_httpStatus = Lens.lens (\GetWorkflowRunResponse' {httpStatus} -> httpStatus) (\s@GetWorkflowRunResponse' {} a -> s {httpStatus = a} :: GetWorkflowRunResponse)

instance Prelude.NFData GetWorkflowRunResponse where
  rnf GetWorkflowRunResponse' {..} =
    Prelude.rnf run
      `Prelude.seq` Prelude.rnf httpStatus

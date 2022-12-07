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
-- Module      : Amazonka.Glue.ResumeWorkflowRun
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restarts selected nodes of a previous partially completed workflow run
-- and resumes the workflow run. The selected nodes and all nodes that are
-- downstream from the selected nodes are run.
module Amazonka.Glue.ResumeWorkflowRun
  ( -- * Creating a Request
    ResumeWorkflowRun (..),
    newResumeWorkflowRun,

    -- * Request Lenses
    resumeWorkflowRun_name,
    resumeWorkflowRun_runId,
    resumeWorkflowRun_nodeIds,

    -- * Destructuring the Response
    ResumeWorkflowRunResponse (..),
    newResumeWorkflowRunResponse,

    -- * Response Lenses
    resumeWorkflowRunResponse_nodeIds,
    resumeWorkflowRunResponse_runId,
    resumeWorkflowRunResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newResumeWorkflowRun' smart constructor.
data ResumeWorkflowRun = ResumeWorkflowRun'
  { -- | The name of the workflow to resume.
    name :: Prelude.Text,
    -- | The ID of the workflow run to resume.
    runId :: Prelude.Text,
    -- | A list of the node IDs for the nodes you want to restart. The nodes that
    -- are to be restarted must have a run attempt in the original run.
    nodeIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResumeWorkflowRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'resumeWorkflowRun_name' - The name of the workflow to resume.
--
-- 'runId', 'resumeWorkflowRun_runId' - The ID of the workflow run to resume.
--
-- 'nodeIds', 'resumeWorkflowRun_nodeIds' - A list of the node IDs for the nodes you want to restart. The nodes that
-- are to be restarted must have a run attempt in the original run.
newResumeWorkflowRun ::
  -- | 'name'
  Prelude.Text ->
  -- | 'runId'
  Prelude.Text ->
  ResumeWorkflowRun
newResumeWorkflowRun pName_ pRunId_ =
  ResumeWorkflowRun'
    { name = pName_,
      runId = pRunId_,
      nodeIds = Prelude.mempty
    }

-- | The name of the workflow to resume.
resumeWorkflowRun_name :: Lens.Lens' ResumeWorkflowRun Prelude.Text
resumeWorkflowRun_name = Lens.lens (\ResumeWorkflowRun' {name} -> name) (\s@ResumeWorkflowRun' {} a -> s {name = a} :: ResumeWorkflowRun)

-- | The ID of the workflow run to resume.
resumeWorkflowRun_runId :: Lens.Lens' ResumeWorkflowRun Prelude.Text
resumeWorkflowRun_runId = Lens.lens (\ResumeWorkflowRun' {runId} -> runId) (\s@ResumeWorkflowRun' {} a -> s {runId = a} :: ResumeWorkflowRun)

-- | A list of the node IDs for the nodes you want to restart. The nodes that
-- are to be restarted must have a run attempt in the original run.
resumeWorkflowRun_nodeIds :: Lens.Lens' ResumeWorkflowRun [Prelude.Text]
resumeWorkflowRun_nodeIds = Lens.lens (\ResumeWorkflowRun' {nodeIds} -> nodeIds) (\s@ResumeWorkflowRun' {} a -> s {nodeIds = a} :: ResumeWorkflowRun) Prelude.. Lens.coerced

instance Core.AWSRequest ResumeWorkflowRun where
  type
    AWSResponse ResumeWorkflowRun =
      ResumeWorkflowRunResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ResumeWorkflowRunResponse'
            Prelude.<$> (x Data..?> "NodeIds" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "RunId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ResumeWorkflowRun where
  hashWithSalt _salt ResumeWorkflowRun' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` runId
      `Prelude.hashWithSalt` nodeIds

instance Prelude.NFData ResumeWorkflowRun where
  rnf ResumeWorkflowRun' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf runId
      `Prelude.seq` Prelude.rnf nodeIds

instance Data.ToHeaders ResumeWorkflowRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.ResumeWorkflowRun" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ResumeWorkflowRun where
  toJSON ResumeWorkflowRun' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("RunId" Data..= runId),
            Prelude.Just ("NodeIds" Data..= nodeIds)
          ]
      )

instance Data.ToPath ResumeWorkflowRun where
  toPath = Prelude.const "/"

instance Data.ToQuery ResumeWorkflowRun where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newResumeWorkflowRunResponse' smart constructor.
data ResumeWorkflowRunResponse = ResumeWorkflowRunResponse'
  { -- | A list of the node IDs for the nodes that were actually restarted.
    nodeIds :: Prelude.Maybe [Prelude.Text],
    -- | The new ID assigned to the resumed workflow run. Each resume of a
    -- workflow run will have a new run ID.
    runId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResumeWorkflowRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nodeIds', 'resumeWorkflowRunResponse_nodeIds' - A list of the node IDs for the nodes that were actually restarted.
--
-- 'runId', 'resumeWorkflowRunResponse_runId' - The new ID assigned to the resumed workflow run. Each resume of a
-- workflow run will have a new run ID.
--
-- 'httpStatus', 'resumeWorkflowRunResponse_httpStatus' - The response's http status code.
newResumeWorkflowRunResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ResumeWorkflowRunResponse
newResumeWorkflowRunResponse pHttpStatus_ =
  ResumeWorkflowRunResponse'
    { nodeIds =
        Prelude.Nothing,
      runId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of the node IDs for the nodes that were actually restarted.
resumeWorkflowRunResponse_nodeIds :: Lens.Lens' ResumeWorkflowRunResponse (Prelude.Maybe [Prelude.Text])
resumeWorkflowRunResponse_nodeIds = Lens.lens (\ResumeWorkflowRunResponse' {nodeIds} -> nodeIds) (\s@ResumeWorkflowRunResponse' {} a -> s {nodeIds = a} :: ResumeWorkflowRunResponse) Prelude.. Lens.mapping Lens.coerced

-- | The new ID assigned to the resumed workflow run. Each resume of a
-- workflow run will have a new run ID.
resumeWorkflowRunResponse_runId :: Lens.Lens' ResumeWorkflowRunResponse (Prelude.Maybe Prelude.Text)
resumeWorkflowRunResponse_runId = Lens.lens (\ResumeWorkflowRunResponse' {runId} -> runId) (\s@ResumeWorkflowRunResponse' {} a -> s {runId = a} :: ResumeWorkflowRunResponse)

-- | The response's http status code.
resumeWorkflowRunResponse_httpStatus :: Lens.Lens' ResumeWorkflowRunResponse Prelude.Int
resumeWorkflowRunResponse_httpStatus = Lens.lens (\ResumeWorkflowRunResponse' {httpStatus} -> httpStatus) (\s@ResumeWorkflowRunResponse' {} a -> s {httpStatus = a} :: ResumeWorkflowRunResponse)

instance Prelude.NFData ResumeWorkflowRunResponse where
  rnf ResumeWorkflowRunResponse' {..} =
    Prelude.rnf nodeIds
      `Prelude.seq` Prelude.rnf runId
      `Prelude.seq` Prelude.rnf httpStatus

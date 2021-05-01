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
-- Module      : Network.AWS.Glue.ResumeWorkflowRun
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restarts selected nodes of a previous partially completed workflow run
-- and resumes the workflow run. The selected nodes and all nodes that are
-- downstream from the selected nodes are run.
module Network.AWS.Glue.ResumeWorkflowRun
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
    resumeWorkflowRunResponse_runId,
    resumeWorkflowRunResponse_nodeIds,
    resumeWorkflowRunResponse_httpStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
resumeWorkflowRun_nodeIds = Lens.lens (\ResumeWorkflowRun' {nodeIds} -> nodeIds) (\s@ResumeWorkflowRun' {} a -> s {nodeIds = a} :: ResumeWorkflowRun) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest ResumeWorkflowRun where
  type Rs ResumeWorkflowRun = ResumeWorkflowRunResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ResumeWorkflowRunResponse'
            Prelude.<$> (x Prelude..?> "RunId")
            Prelude.<*> (x Prelude..?> "NodeIds" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ResumeWorkflowRun

instance Prelude.NFData ResumeWorkflowRun

instance Prelude.ToHeaders ResumeWorkflowRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSGlue.ResumeWorkflowRun" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ResumeWorkflowRun where
  toJSON ResumeWorkflowRun' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Prelude..= name),
            Prelude.Just ("RunId" Prelude..= runId),
            Prelude.Just ("NodeIds" Prelude..= nodeIds)
          ]
      )

instance Prelude.ToPath ResumeWorkflowRun where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ResumeWorkflowRun where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newResumeWorkflowRunResponse' smart constructor.
data ResumeWorkflowRunResponse = ResumeWorkflowRunResponse'
  { -- | The new ID assigned to the resumed workflow run. Each resume of a
    -- workflow run will have a new run ID.
    runId :: Prelude.Maybe Prelude.Text,
    -- | A list of the node IDs for the nodes that were actually restarted.
    nodeIds :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResumeWorkflowRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'runId', 'resumeWorkflowRunResponse_runId' - The new ID assigned to the resumed workflow run. Each resume of a
-- workflow run will have a new run ID.
--
-- 'nodeIds', 'resumeWorkflowRunResponse_nodeIds' - A list of the node IDs for the nodes that were actually restarted.
--
-- 'httpStatus', 'resumeWorkflowRunResponse_httpStatus' - The response's http status code.
newResumeWorkflowRunResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ResumeWorkflowRunResponse
newResumeWorkflowRunResponse pHttpStatus_ =
  ResumeWorkflowRunResponse'
    { runId = Prelude.Nothing,
      nodeIds = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The new ID assigned to the resumed workflow run. Each resume of a
-- workflow run will have a new run ID.
resumeWorkflowRunResponse_runId :: Lens.Lens' ResumeWorkflowRunResponse (Prelude.Maybe Prelude.Text)
resumeWorkflowRunResponse_runId = Lens.lens (\ResumeWorkflowRunResponse' {runId} -> runId) (\s@ResumeWorkflowRunResponse' {} a -> s {runId = a} :: ResumeWorkflowRunResponse)

-- | A list of the node IDs for the nodes that were actually restarted.
resumeWorkflowRunResponse_nodeIds :: Lens.Lens' ResumeWorkflowRunResponse (Prelude.Maybe [Prelude.Text])
resumeWorkflowRunResponse_nodeIds = Lens.lens (\ResumeWorkflowRunResponse' {nodeIds} -> nodeIds) (\s@ResumeWorkflowRunResponse' {} a -> s {nodeIds = a} :: ResumeWorkflowRunResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
resumeWorkflowRunResponse_httpStatus :: Lens.Lens' ResumeWorkflowRunResponse Prelude.Int
resumeWorkflowRunResponse_httpStatus = Lens.lens (\ResumeWorkflowRunResponse' {httpStatus} -> httpStatus) (\s@ResumeWorkflowRunResponse' {} a -> s {httpStatus = a} :: ResumeWorkflowRunResponse)

instance Prelude.NFData ResumeWorkflowRunResponse

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
-- Module      : Network.AWS.Glue.BatchGetWorkflows
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of resource metadata for a given list of workflow names.
-- After calling the @ListWorkflows@ operation, you can call this operation
-- to access the data to which you have been granted permissions. This
-- operation supports all IAM permissions, including permission conditions
-- that uses tags.
module Network.AWS.Glue.BatchGetWorkflows
  ( -- * Creating a Request
    BatchGetWorkflows (..),
    newBatchGetWorkflows,

    -- * Request Lenses
    batchGetWorkflows_includeGraph,
    batchGetWorkflows_names,

    -- * Destructuring the Response
    BatchGetWorkflowsResponse (..),
    newBatchGetWorkflowsResponse,

    -- * Response Lenses
    batchGetWorkflowsResponse_missingWorkflows,
    batchGetWorkflowsResponse_workflows,
    batchGetWorkflowsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchGetWorkflows' smart constructor.
data BatchGetWorkflows = BatchGetWorkflows'
  { -- | Specifies whether to include a graph when returning the workflow
    -- resource metadata.
    includeGraph :: Core.Maybe Core.Bool,
    -- | A list of workflow names, which may be the names returned from the
    -- @ListWorkflows@ operation.
    names :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchGetWorkflows' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeGraph', 'batchGetWorkflows_includeGraph' - Specifies whether to include a graph when returning the workflow
-- resource metadata.
--
-- 'names', 'batchGetWorkflows_names' - A list of workflow names, which may be the names returned from the
-- @ListWorkflows@ operation.
newBatchGetWorkflows ::
  -- | 'names'
  Core.NonEmpty Core.Text ->
  BatchGetWorkflows
newBatchGetWorkflows pNames_ =
  BatchGetWorkflows'
    { includeGraph = Core.Nothing,
      names = Lens._Coerce Lens.# pNames_
    }

-- | Specifies whether to include a graph when returning the workflow
-- resource metadata.
batchGetWorkflows_includeGraph :: Lens.Lens' BatchGetWorkflows (Core.Maybe Core.Bool)
batchGetWorkflows_includeGraph = Lens.lens (\BatchGetWorkflows' {includeGraph} -> includeGraph) (\s@BatchGetWorkflows' {} a -> s {includeGraph = a} :: BatchGetWorkflows)

-- | A list of workflow names, which may be the names returned from the
-- @ListWorkflows@ operation.
batchGetWorkflows_names :: Lens.Lens' BatchGetWorkflows (Core.NonEmpty Core.Text)
batchGetWorkflows_names = Lens.lens (\BatchGetWorkflows' {names} -> names) (\s@BatchGetWorkflows' {} a -> s {names = a} :: BatchGetWorkflows) Core.. Lens._Coerce

instance Core.AWSRequest BatchGetWorkflows where
  type
    AWSResponse BatchGetWorkflows =
      BatchGetWorkflowsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetWorkflowsResponse'
            Core.<$> (x Core..?> "MissingWorkflows")
            Core.<*> (x Core..?> "Workflows")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable BatchGetWorkflows

instance Core.NFData BatchGetWorkflows

instance Core.ToHeaders BatchGetWorkflows where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.BatchGetWorkflows" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON BatchGetWorkflows where
  toJSON BatchGetWorkflows' {..} =
    Core.object
      ( Core.catMaybes
          [ ("IncludeGraph" Core..=) Core.<$> includeGraph,
            Core.Just ("Names" Core..= names)
          ]
      )

instance Core.ToPath BatchGetWorkflows where
  toPath = Core.const "/"

instance Core.ToQuery BatchGetWorkflows where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newBatchGetWorkflowsResponse' smart constructor.
data BatchGetWorkflowsResponse = BatchGetWorkflowsResponse'
  { -- | A list of names of workflows not found.
    missingWorkflows :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | A list of workflow resource metadata.
    workflows :: Core.Maybe (Core.NonEmpty Workflow),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchGetWorkflowsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'missingWorkflows', 'batchGetWorkflowsResponse_missingWorkflows' - A list of names of workflows not found.
--
-- 'workflows', 'batchGetWorkflowsResponse_workflows' - A list of workflow resource metadata.
--
-- 'httpStatus', 'batchGetWorkflowsResponse_httpStatus' - The response's http status code.
newBatchGetWorkflowsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  BatchGetWorkflowsResponse
newBatchGetWorkflowsResponse pHttpStatus_ =
  BatchGetWorkflowsResponse'
    { missingWorkflows =
        Core.Nothing,
      workflows = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of names of workflows not found.
batchGetWorkflowsResponse_missingWorkflows :: Lens.Lens' BatchGetWorkflowsResponse (Core.Maybe (Core.NonEmpty Core.Text))
batchGetWorkflowsResponse_missingWorkflows = Lens.lens (\BatchGetWorkflowsResponse' {missingWorkflows} -> missingWorkflows) (\s@BatchGetWorkflowsResponse' {} a -> s {missingWorkflows = a} :: BatchGetWorkflowsResponse) Core.. Lens.mapping Lens._Coerce

-- | A list of workflow resource metadata.
batchGetWorkflowsResponse_workflows :: Lens.Lens' BatchGetWorkflowsResponse (Core.Maybe (Core.NonEmpty Workflow))
batchGetWorkflowsResponse_workflows = Lens.lens (\BatchGetWorkflowsResponse' {workflows} -> workflows) (\s@BatchGetWorkflowsResponse' {} a -> s {workflows = a} :: BatchGetWorkflowsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchGetWorkflowsResponse_httpStatus :: Lens.Lens' BatchGetWorkflowsResponse Core.Int
batchGetWorkflowsResponse_httpStatus = Lens.lens (\BatchGetWorkflowsResponse' {httpStatus} -> httpStatus) (\s@BatchGetWorkflowsResponse' {} a -> s {httpStatus = a} :: BatchGetWorkflowsResponse)

instance Core.NFData BatchGetWorkflowsResponse

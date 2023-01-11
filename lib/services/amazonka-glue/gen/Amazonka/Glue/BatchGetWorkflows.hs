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
-- Module      : Amazonka.Glue.BatchGetWorkflows
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.Glue.BatchGetWorkflows
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchGetWorkflows' smart constructor.
data BatchGetWorkflows = BatchGetWorkflows'
  { -- | Specifies whether to include a graph when returning the workflow
    -- resource metadata.
    includeGraph :: Prelude.Maybe Prelude.Bool,
    -- | A list of workflow names, which may be the names returned from the
    -- @ListWorkflows@ operation.
    names :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.NonEmpty Prelude.Text ->
  BatchGetWorkflows
newBatchGetWorkflows pNames_ =
  BatchGetWorkflows'
    { includeGraph = Prelude.Nothing,
      names = Lens.coerced Lens.# pNames_
    }

-- | Specifies whether to include a graph when returning the workflow
-- resource metadata.
batchGetWorkflows_includeGraph :: Lens.Lens' BatchGetWorkflows (Prelude.Maybe Prelude.Bool)
batchGetWorkflows_includeGraph = Lens.lens (\BatchGetWorkflows' {includeGraph} -> includeGraph) (\s@BatchGetWorkflows' {} a -> s {includeGraph = a} :: BatchGetWorkflows)

-- | A list of workflow names, which may be the names returned from the
-- @ListWorkflows@ operation.
batchGetWorkflows_names :: Lens.Lens' BatchGetWorkflows (Prelude.NonEmpty Prelude.Text)
batchGetWorkflows_names = Lens.lens (\BatchGetWorkflows' {names} -> names) (\s@BatchGetWorkflows' {} a -> s {names = a} :: BatchGetWorkflows) Prelude.. Lens.coerced

instance Core.AWSRequest BatchGetWorkflows where
  type
    AWSResponse BatchGetWorkflows =
      BatchGetWorkflowsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetWorkflowsResponse'
            Prelude.<$> (x Data..?> "MissingWorkflows")
            Prelude.<*> (x Data..?> "Workflows")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetWorkflows where
  hashWithSalt _salt BatchGetWorkflows' {..} =
    _salt `Prelude.hashWithSalt` includeGraph
      `Prelude.hashWithSalt` names

instance Prelude.NFData BatchGetWorkflows where
  rnf BatchGetWorkflows' {..} =
    Prelude.rnf includeGraph
      `Prelude.seq` Prelude.rnf names

instance Data.ToHeaders BatchGetWorkflows where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.BatchGetWorkflows" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchGetWorkflows where
  toJSON BatchGetWorkflows' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IncludeGraph" Data..=) Prelude.<$> includeGraph,
            Prelude.Just ("Names" Data..= names)
          ]
      )

instance Data.ToPath BatchGetWorkflows where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchGetWorkflows where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetWorkflowsResponse' smart constructor.
data BatchGetWorkflowsResponse = BatchGetWorkflowsResponse'
  { -- | A list of names of workflows not found.
    missingWorkflows :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of workflow resource metadata.
    workflows :: Prelude.Maybe (Prelude.NonEmpty Workflow),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  BatchGetWorkflowsResponse
newBatchGetWorkflowsResponse pHttpStatus_ =
  BatchGetWorkflowsResponse'
    { missingWorkflows =
        Prelude.Nothing,
      workflows = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of names of workflows not found.
batchGetWorkflowsResponse_missingWorkflows :: Lens.Lens' BatchGetWorkflowsResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
batchGetWorkflowsResponse_missingWorkflows = Lens.lens (\BatchGetWorkflowsResponse' {missingWorkflows} -> missingWorkflows) (\s@BatchGetWorkflowsResponse' {} a -> s {missingWorkflows = a} :: BatchGetWorkflowsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of workflow resource metadata.
batchGetWorkflowsResponse_workflows :: Lens.Lens' BatchGetWorkflowsResponse (Prelude.Maybe (Prelude.NonEmpty Workflow))
batchGetWorkflowsResponse_workflows = Lens.lens (\BatchGetWorkflowsResponse' {workflows} -> workflows) (\s@BatchGetWorkflowsResponse' {} a -> s {workflows = a} :: BatchGetWorkflowsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchGetWorkflowsResponse_httpStatus :: Lens.Lens' BatchGetWorkflowsResponse Prelude.Int
batchGetWorkflowsResponse_httpStatus = Lens.lens (\BatchGetWorkflowsResponse' {httpStatus} -> httpStatus) (\s@BatchGetWorkflowsResponse' {} a -> s {httpStatus = a} :: BatchGetWorkflowsResponse)

instance Prelude.NFData BatchGetWorkflowsResponse where
  rnf BatchGetWorkflowsResponse' {..} =
    Prelude.rnf missingWorkflows
      `Prelude.seq` Prelude.rnf workflows
      `Prelude.seq` Prelude.rnf httpStatus

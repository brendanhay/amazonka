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
-- Module      : Network.AWS.Glue.DeleteWorkflow
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a workflow.
module Network.AWS.Glue.DeleteWorkflow
  ( -- * Creating a Request
    DeleteWorkflow (..),
    newDeleteWorkflow,

    -- * Request Lenses
    deleteWorkflow_name,

    -- * Destructuring the Response
    DeleteWorkflowResponse (..),
    newDeleteWorkflowResponse,

    -- * Response Lenses
    deleteWorkflowResponse_name,
    deleteWorkflowResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteWorkflow' smart constructor.
data DeleteWorkflow = DeleteWorkflow'
  { -- | Name of the workflow to be deleted.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteWorkflow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteWorkflow_name' - Name of the workflow to be deleted.
newDeleteWorkflow ::
  -- | 'name'
  Core.Text ->
  DeleteWorkflow
newDeleteWorkflow pName_ =
  DeleteWorkflow' {name = pName_}

-- | Name of the workflow to be deleted.
deleteWorkflow_name :: Lens.Lens' DeleteWorkflow Core.Text
deleteWorkflow_name = Lens.lens (\DeleteWorkflow' {name} -> name) (\s@DeleteWorkflow' {} a -> s {name = a} :: DeleteWorkflow)

instance Core.AWSRequest DeleteWorkflow where
  type
    AWSResponse DeleteWorkflow =
      DeleteWorkflowResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteWorkflowResponse'
            Core.<$> (x Core..?> "Name")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteWorkflow

instance Core.NFData DeleteWorkflow

instance Core.ToHeaders DeleteWorkflow where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.DeleteWorkflow" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteWorkflow where
  toJSON DeleteWorkflow' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.ToPath DeleteWorkflow where
  toPath = Core.const "/"

instance Core.ToQuery DeleteWorkflow where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteWorkflowResponse' smart constructor.
data DeleteWorkflowResponse = DeleteWorkflowResponse'
  { -- | Name of the workflow specified in input.
    name :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteWorkflowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteWorkflowResponse_name' - Name of the workflow specified in input.
--
-- 'httpStatus', 'deleteWorkflowResponse_httpStatus' - The response's http status code.
newDeleteWorkflowResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteWorkflowResponse
newDeleteWorkflowResponse pHttpStatus_ =
  DeleteWorkflowResponse'
    { name = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Name of the workflow specified in input.
deleteWorkflowResponse_name :: Lens.Lens' DeleteWorkflowResponse (Core.Maybe Core.Text)
deleteWorkflowResponse_name = Lens.lens (\DeleteWorkflowResponse' {name} -> name) (\s@DeleteWorkflowResponse' {} a -> s {name = a} :: DeleteWorkflowResponse)

-- | The response's http status code.
deleteWorkflowResponse_httpStatus :: Lens.Lens' DeleteWorkflowResponse Core.Int
deleteWorkflowResponse_httpStatus = Lens.lens (\DeleteWorkflowResponse' {httpStatus} -> httpStatus) (\s@DeleteWorkflowResponse' {} a -> s {httpStatus = a} :: DeleteWorkflowResponse)

instance Core.NFData DeleteWorkflowResponse

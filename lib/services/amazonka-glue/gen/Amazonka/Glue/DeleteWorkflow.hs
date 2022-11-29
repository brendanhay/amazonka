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
-- Module      : Amazonka.Glue.DeleteWorkflow
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a workflow.
module Amazonka.Glue.DeleteWorkflow
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteWorkflow' smart constructor.
data DeleteWorkflow = DeleteWorkflow'
  { -- | Name of the workflow to be deleted.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteWorkflow
newDeleteWorkflow pName_ =
  DeleteWorkflow' {name = pName_}

-- | Name of the workflow to be deleted.
deleteWorkflow_name :: Lens.Lens' DeleteWorkflow Prelude.Text
deleteWorkflow_name = Lens.lens (\DeleteWorkflow' {name} -> name) (\s@DeleteWorkflow' {} a -> s {name = a} :: DeleteWorkflow)

instance Core.AWSRequest DeleteWorkflow where
  type
    AWSResponse DeleteWorkflow =
      DeleteWorkflowResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteWorkflowResponse'
            Prelude.<$> (x Core..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteWorkflow where
  hashWithSalt _salt DeleteWorkflow' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteWorkflow where
  rnf DeleteWorkflow' {..} = Prelude.rnf name

instance Core.ToHeaders DeleteWorkflow where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.DeleteWorkflow" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteWorkflow where
  toJSON DeleteWorkflow' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Core..= name)]
      )

instance Core.ToPath DeleteWorkflow where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteWorkflow where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteWorkflowResponse' smart constructor.
data DeleteWorkflowResponse = DeleteWorkflowResponse'
  { -- | Name of the workflow specified in input.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteWorkflowResponse
newDeleteWorkflowResponse pHttpStatus_ =
  DeleteWorkflowResponse'
    { name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Name of the workflow specified in input.
deleteWorkflowResponse_name :: Lens.Lens' DeleteWorkflowResponse (Prelude.Maybe Prelude.Text)
deleteWorkflowResponse_name = Lens.lens (\DeleteWorkflowResponse' {name} -> name) (\s@DeleteWorkflowResponse' {} a -> s {name = a} :: DeleteWorkflowResponse)

-- | The response's http status code.
deleteWorkflowResponse_httpStatus :: Lens.Lens' DeleteWorkflowResponse Prelude.Int
deleteWorkflowResponse_httpStatus = Lens.lens (\DeleteWorkflowResponse' {httpStatus} -> httpStatus) (\s@DeleteWorkflowResponse' {} a -> s {httpStatus = a} :: DeleteWorkflowResponse)

instance Prelude.NFData DeleteWorkflowResponse where
  rnf DeleteWorkflowResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf httpStatus

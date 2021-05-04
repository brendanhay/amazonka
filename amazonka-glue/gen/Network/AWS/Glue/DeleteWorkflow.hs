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

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteWorkflow' smart constructor.
data DeleteWorkflow = DeleteWorkflow'
  { -- | Name of the workflow to be deleted.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteWorkflow where
  type Rs DeleteWorkflow = DeleteWorkflowResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteWorkflowResponse'
            Prelude.<$> (x Prelude..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteWorkflow

instance Prelude.NFData DeleteWorkflow

instance Prelude.ToHeaders DeleteWorkflow where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSGlue.DeleteWorkflow" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteWorkflow where
  toJSON DeleteWorkflow' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Prelude..= name)]
      )

instance Prelude.ToPath DeleteWorkflow where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteWorkflow where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteWorkflowResponse' smart constructor.
data DeleteWorkflowResponse = DeleteWorkflowResponse'
  { -- | Name of the workflow specified in input.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DeleteWorkflowResponse

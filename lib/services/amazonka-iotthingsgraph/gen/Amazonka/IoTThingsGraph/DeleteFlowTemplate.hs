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
-- Module      : Amazonka.IoTThingsGraph.DeleteFlowTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a workflow. Any new system or deployment that contains this
-- workflow will fail to update or deploy. Existing deployments that
-- contain the workflow will continue to run (since they use a snapshot of
-- the workflow taken at the time of deployment).
module Amazonka.IoTThingsGraph.DeleteFlowTemplate
  ( -- * Creating a Request
    DeleteFlowTemplate (..),
    newDeleteFlowTemplate,

    -- * Request Lenses
    deleteFlowTemplate_id,

    -- * Destructuring the Response
    DeleteFlowTemplateResponse (..),
    newDeleteFlowTemplateResponse,

    -- * Response Lenses
    deleteFlowTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTThingsGraph.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteFlowTemplate' smart constructor.
data DeleteFlowTemplate = DeleteFlowTemplate'
  { -- | The ID of the workflow to be deleted.
    --
    -- The ID should be in the following format.
    --
    -- @urn:tdm:REGION\/ACCOUNT ID\/default:workflow:WORKFLOWNAME@
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFlowTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteFlowTemplate_id' - The ID of the workflow to be deleted.
--
-- The ID should be in the following format.
--
-- @urn:tdm:REGION\/ACCOUNT ID\/default:workflow:WORKFLOWNAME@
newDeleteFlowTemplate ::
  -- | 'id'
  Prelude.Text ->
  DeleteFlowTemplate
newDeleteFlowTemplate pId_ =
  DeleteFlowTemplate' {id = pId_}

-- | The ID of the workflow to be deleted.
--
-- The ID should be in the following format.
--
-- @urn:tdm:REGION\/ACCOUNT ID\/default:workflow:WORKFLOWNAME@
deleteFlowTemplate_id :: Lens.Lens' DeleteFlowTemplate Prelude.Text
deleteFlowTemplate_id = Lens.lens (\DeleteFlowTemplate' {id} -> id) (\s@DeleteFlowTemplate' {} a -> s {id = a} :: DeleteFlowTemplate)

instance Core.AWSRequest DeleteFlowTemplate where
  type
    AWSResponse DeleteFlowTemplate =
      DeleteFlowTemplateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteFlowTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteFlowTemplate where
  hashWithSalt _salt DeleteFlowTemplate' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteFlowTemplate where
  rnf DeleteFlowTemplate' {..} = Prelude.rnf id

instance Core.ToHeaders DeleteFlowTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IotThingsGraphFrontEndService.DeleteFlowTemplate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteFlowTemplate where
  toJSON DeleteFlowTemplate' {..} =
    Core.object
      (Prelude.catMaybes [Prelude.Just ("id" Core..= id)])

instance Core.ToPath DeleteFlowTemplate where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteFlowTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFlowTemplateResponse' smart constructor.
data DeleteFlowTemplateResponse = DeleteFlowTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFlowTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteFlowTemplateResponse_httpStatus' - The response's http status code.
newDeleteFlowTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteFlowTemplateResponse
newDeleteFlowTemplateResponse pHttpStatus_ =
  DeleteFlowTemplateResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteFlowTemplateResponse_httpStatus :: Lens.Lens' DeleteFlowTemplateResponse Prelude.Int
deleteFlowTemplateResponse_httpStatus = Lens.lens (\DeleteFlowTemplateResponse' {httpStatus} -> httpStatus) (\s@DeleteFlowTemplateResponse' {} a -> s {httpStatus = a} :: DeleteFlowTemplateResponse)

instance Prelude.NFData DeleteFlowTemplateResponse where
  rnf DeleteFlowTemplateResponse' {..} =
    Prelude.rnf httpStatus

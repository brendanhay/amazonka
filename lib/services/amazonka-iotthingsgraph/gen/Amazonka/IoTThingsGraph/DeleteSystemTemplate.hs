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
-- Module      : Amazonka.IoTThingsGraph.DeleteSystemTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a system. New deployments can\'t contain the system after its
-- deletion. Existing deployments that contain the system will continue to
-- work because they use a snapshot of the system that is taken when it is
-- deployed.
module Amazonka.IoTThingsGraph.DeleteSystemTemplate
  ( -- * Creating a Request
    DeleteSystemTemplate (..),
    newDeleteSystemTemplate,

    -- * Request Lenses
    deleteSystemTemplate_id,

    -- * Destructuring the Response
    DeleteSystemTemplateResponse (..),
    newDeleteSystemTemplateResponse,

    -- * Response Lenses
    deleteSystemTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTThingsGraph.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteSystemTemplate' smart constructor.
data DeleteSystemTemplate = DeleteSystemTemplate'
  { -- | The ID of the system to be deleted.
    --
    -- The ID should be in the following format.
    --
    -- @urn:tdm:REGION\/ACCOUNT ID\/default:system:SYSTEMNAME@
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSystemTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteSystemTemplate_id' - The ID of the system to be deleted.
--
-- The ID should be in the following format.
--
-- @urn:tdm:REGION\/ACCOUNT ID\/default:system:SYSTEMNAME@
newDeleteSystemTemplate ::
  -- | 'id'
  Prelude.Text ->
  DeleteSystemTemplate
newDeleteSystemTemplate pId_ =
  DeleteSystemTemplate' {id = pId_}

-- | The ID of the system to be deleted.
--
-- The ID should be in the following format.
--
-- @urn:tdm:REGION\/ACCOUNT ID\/default:system:SYSTEMNAME@
deleteSystemTemplate_id :: Lens.Lens' DeleteSystemTemplate Prelude.Text
deleteSystemTemplate_id = Lens.lens (\DeleteSystemTemplate' {id} -> id) (\s@DeleteSystemTemplate' {} a -> s {id = a} :: DeleteSystemTemplate)

instance Core.AWSRequest DeleteSystemTemplate where
  type
    AWSResponse DeleteSystemTemplate =
      DeleteSystemTemplateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteSystemTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteSystemTemplate where
  hashWithSalt salt' DeleteSystemTemplate' {..} =
    salt' `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteSystemTemplate where
  rnf DeleteSystemTemplate' {..} = Prelude.rnf id

instance Core.ToHeaders DeleteSystemTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IotThingsGraphFrontEndService.DeleteSystemTemplate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteSystemTemplate where
  toJSON DeleteSystemTemplate' {..} =
    Core.object
      (Prelude.catMaybes [Prelude.Just ("id" Core..= id)])

instance Core.ToPath DeleteSystemTemplate where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteSystemTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSystemTemplateResponse' smart constructor.
data DeleteSystemTemplateResponse = DeleteSystemTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSystemTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteSystemTemplateResponse_httpStatus' - The response's http status code.
newDeleteSystemTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSystemTemplateResponse
newDeleteSystemTemplateResponse pHttpStatus_ =
  DeleteSystemTemplateResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteSystemTemplateResponse_httpStatus :: Lens.Lens' DeleteSystemTemplateResponse Prelude.Int
deleteSystemTemplateResponse_httpStatus = Lens.lens (\DeleteSystemTemplateResponse' {httpStatus} -> httpStatus) (\s@DeleteSystemTemplateResponse' {} a -> s {httpStatus = a} :: DeleteSystemTemplateResponse)

instance Prelude.NFData DeleteSystemTemplateResponse where
  rnf DeleteSystemTemplateResponse' {..} =
    Prelude.rnf httpStatus

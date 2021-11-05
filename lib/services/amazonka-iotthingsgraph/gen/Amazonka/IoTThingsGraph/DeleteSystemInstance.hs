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
-- Module      : Amazonka.IoTThingsGraph.DeleteSystemInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a system instance. Only system instances that have never been
-- deployed, or that have been undeployed can be deleted.
--
-- Users can create a new system instance that has the same ID as a deleted
-- system instance.
module Amazonka.IoTThingsGraph.DeleteSystemInstance
  ( -- * Creating a Request
    DeleteSystemInstance (..),
    newDeleteSystemInstance,

    -- * Request Lenses
    deleteSystemInstance_id,

    -- * Destructuring the Response
    DeleteSystemInstanceResponse (..),
    newDeleteSystemInstanceResponse,

    -- * Response Lenses
    deleteSystemInstanceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTThingsGraph.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteSystemInstance' smart constructor.
data DeleteSystemInstance = DeleteSystemInstance'
  { -- | The ID of the system instance to be deleted.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSystemInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteSystemInstance_id' - The ID of the system instance to be deleted.
newDeleteSystemInstance ::
  DeleteSystemInstance
newDeleteSystemInstance =
  DeleteSystemInstance' {id = Prelude.Nothing}

-- | The ID of the system instance to be deleted.
deleteSystemInstance_id :: Lens.Lens' DeleteSystemInstance (Prelude.Maybe Prelude.Text)
deleteSystemInstance_id = Lens.lens (\DeleteSystemInstance' {id} -> id) (\s@DeleteSystemInstance' {} a -> s {id = a} :: DeleteSystemInstance)

instance Core.AWSRequest DeleteSystemInstance where
  type
    AWSResponse DeleteSystemInstance =
      DeleteSystemInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteSystemInstanceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteSystemInstance

instance Prelude.NFData DeleteSystemInstance

instance Core.ToHeaders DeleteSystemInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IotThingsGraphFrontEndService.DeleteSystemInstance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteSystemInstance where
  toJSON DeleteSystemInstance' {..} =
    Core.object
      (Prelude.catMaybes [("id" Core..=) Prelude.<$> id])

instance Core.ToPath DeleteSystemInstance where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteSystemInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSystemInstanceResponse' smart constructor.
data DeleteSystemInstanceResponse = DeleteSystemInstanceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSystemInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteSystemInstanceResponse_httpStatus' - The response's http status code.
newDeleteSystemInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSystemInstanceResponse
newDeleteSystemInstanceResponse pHttpStatus_ =
  DeleteSystemInstanceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteSystemInstanceResponse_httpStatus :: Lens.Lens' DeleteSystemInstanceResponse Prelude.Int
deleteSystemInstanceResponse_httpStatus = Lens.lens (\DeleteSystemInstanceResponse' {httpStatus} -> httpStatus) (\s@DeleteSystemInstanceResponse' {} a -> s {httpStatus = a} :: DeleteSystemInstanceResponse)

instance Prelude.NFData DeleteSystemInstanceResponse

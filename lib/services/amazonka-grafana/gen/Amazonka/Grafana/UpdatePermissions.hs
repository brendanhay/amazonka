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
-- Module      : Amazonka.Grafana.UpdatePermissions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates which users in a workspace have the Grafana @Admin@ or @Editor@
-- roles.
module Amazonka.Grafana.UpdatePermissions
  ( -- * Creating a Request
    UpdatePermissions (..),
    newUpdatePermissions,

    -- * Request Lenses
    updatePermissions_updateInstructionBatch,
    updatePermissions_workspaceId,

    -- * Destructuring the Response
    UpdatePermissionsResponse (..),
    newUpdatePermissionsResponse,

    -- * Response Lenses
    updatePermissionsResponse_httpStatus,
    updatePermissionsResponse_errors,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Grafana.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdatePermissions' smart constructor.
data UpdatePermissions = UpdatePermissions'
  { -- | An array of structures that contain the permission updates to make.
    updateInstructionBatch :: [UpdateInstruction],
    -- | The ID of the workspace to update.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'updateInstructionBatch', 'updatePermissions_updateInstructionBatch' - An array of structures that contain the permission updates to make.
--
-- 'workspaceId', 'updatePermissions_workspaceId' - The ID of the workspace to update.
newUpdatePermissions ::
  -- | 'workspaceId'
  Prelude.Text ->
  UpdatePermissions
newUpdatePermissions pWorkspaceId_ =
  UpdatePermissions'
    { updateInstructionBatch =
        Prelude.mempty,
      workspaceId = pWorkspaceId_
    }

-- | An array of structures that contain the permission updates to make.
updatePermissions_updateInstructionBatch :: Lens.Lens' UpdatePermissions [UpdateInstruction]
updatePermissions_updateInstructionBatch = Lens.lens (\UpdatePermissions' {updateInstructionBatch} -> updateInstructionBatch) (\s@UpdatePermissions' {} a -> s {updateInstructionBatch = a} :: UpdatePermissions) Prelude.. Lens.coerced

-- | The ID of the workspace to update.
updatePermissions_workspaceId :: Lens.Lens' UpdatePermissions Prelude.Text
updatePermissions_workspaceId = Lens.lens (\UpdatePermissions' {workspaceId} -> workspaceId) (\s@UpdatePermissions' {} a -> s {workspaceId = a} :: UpdatePermissions)

instance Core.AWSRequest UpdatePermissions where
  type
    AWSResponse UpdatePermissions =
      UpdatePermissionsResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePermissionsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "errors" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable UpdatePermissions where
  hashWithSalt _salt UpdatePermissions' {..} =
    _salt `Prelude.hashWithSalt` updateInstructionBatch
      `Prelude.hashWithSalt` workspaceId

instance Prelude.NFData UpdatePermissions where
  rnf UpdatePermissions' {..} =
    Prelude.rnf updateInstructionBatch
      `Prelude.seq` Prelude.rnf workspaceId

instance Data.ToHeaders UpdatePermissions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdatePermissions where
  toJSON UpdatePermissions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "updateInstructionBatch"
                  Data..= updateInstructionBatch
              )
          ]
      )

instance Data.ToPath UpdatePermissions where
  toPath UpdatePermissions' {..} =
    Prelude.mconcat
      [ "/workspaces/",
        Data.toBS workspaceId,
        "/permissions"
      ]

instance Data.ToQuery UpdatePermissions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePermissionsResponse' smart constructor.
data UpdatePermissionsResponse = UpdatePermissionsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of structures that contain the errors from the operation, if
    -- any.
    errors :: [UpdateError]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updatePermissionsResponse_httpStatus' - The response's http status code.
--
-- 'errors', 'updatePermissionsResponse_errors' - An array of structures that contain the errors from the operation, if
-- any.
newUpdatePermissionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdatePermissionsResponse
newUpdatePermissionsResponse pHttpStatus_ =
  UpdatePermissionsResponse'
    { httpStatus =
        pHttpStatus_,
      errors = Prelude.mempty
    }

-- | The response's http status code.
updatePermissionsResponse_httpStatus :: Lens.Lens' UpdatePermissionsResponse Prelude.Int
updatePermissionsResponse_httpStatus = Lens.lens (\UpdatePermissionsResponse' {httpStatus} -> httpStatus) (\s@UpdatePermissionsResponse' {} a -> s {httpStatus = a} :: UpdatePermissionsResponse)

-- | An array of structures that contain the errors from the operation, if
-- any.
updatePermissionsResponse_errors :: Lens.Lens' UpdatePermissionsResponse [UpdateError]
updatePermissionsResponse_errors = Lens.lens (\UpdatePermissionsResponse' {errors} -> errors) (\s@UpdatePermissionsResponse' {} a -> s {errors = a} :: UpdatePermissionsResponse) Prelude.. Lens.coerced

instance Prelude.NFData UpdatePermissionsResponse where
  rnf UpdatePermissionsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf errors

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
-- Module      : Amazonka.IotTwinMaker.UpdateWorkspace
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a workspace.
module Amazonka.IotTwinMaker.UpdateWorkspace
  ( -- * Creating a Request
    UpdateWorkspace (..),
    newUpdateWorkspace,

    -- * Request Lenses
    updateWorkspace_description,
    updateWorkspace_role,
    updateWorkspace_workspaceId,

    -- * Destructuring the Response
    UpdateWorkspaceResponse (..),
    newUpdateWorkspaceResponse,

    -- * Response Lenses
    updateWorkspaceResponse_httpStatus,
    updateWorkspaceResponse_updateDateTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateWorkspace' smart constructor.
data UpdateWorkspace = UpdateWorkspace'
  { -- | The description of the workspace.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the execution role associated with the workspace.
    role' :: Prelude.Maybe Prelude.Text,
    -- | The ID of the workspace.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWorkspace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateWorkspace_description' - The description of the workspace.
--
-- 'role'', 'updateWorkspace_role' - The ARN of the execution role associated with the workspace.
--
-- 'workspaceId', 'updateWorkspace_workspaceId' - The ID of the workspace.
newUpdateWorkspace ::
  -- | 'workspaceId'
  Prelude.Text ->
  UpdateWorkspace
newUpdateWorkspace pWorkspaceId_ =
  UpdateWorkspace'
    { description = Prelude.Nothing,
      role' = Prelude.Nothing,
      workspaceId = pWorkspaceId_
    }

-- | The description of the workspace.
updateWorkspace_description :: Lens.Lens' UpdateWorkspace (Prelude.Maybe Prelude.Text)
updateWorkspace_description = Lens.lens (\UpdateWorkspace' {description} -> description) (\s@UpdateWorkspace' {} a -> s {description = a} :: UpdateWorkspace)

-- | The ARN of the execution role associated with the workspace.
updateWorkspace_role :: Lens.Lens' UpdateWorkspace (Prelude.Maybe Prelude.Text)
updateWorkspace_role = Lens.lens (\UpdateWorkspace' {role'} -> role') (\s@UpdateWorkspace' {} a -> s {role' = a} :: UpdateWorkspace)

-- | The ID of the workspace.
updateWorkspace_workspaceId :: Lens.Lens' UpdateWorkspace Prelude.Text
updateWorkspace_workspaceId = Lens.lens (\UpdateWorkspace' {workspaceId} -> workspaceId) (\s@UpdateWorkspace' {} a -> s {workspaceId = a} :: UpdateWorkspace)

instance Core.AWSRequest UpdateWorkspace where
  type
    AWSResponse UpdateWorkspace =
      UpdateWorkspaceResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateWorkspaceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "updateDateTime")
      )

instance Prelude.Hashable UpdateWorkspace where
  hashWithSalt _salt UpdateWorkspace' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` workspaceId

instance Prelude.NFData UpdateWorkspace where
  rnf UpdateWorkspace' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf role' `Prelude.seq`
        Prelude.rnf workspaceId

instance Data.ToHeaders UpdateWorkspace where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateWorkspace where
  toJSON UpdateWorkspace' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("role" Data..=) Prelude.<$> role'
          ]
      )

instance Data.ToPath UpdateWorkspace where
  toPath UpdateWorkspace' {..} =
    Prelude.mconcat
      ["/workspaces/", Data.toBS workspaceId]

instance Data.ToQuery UpdateWorkspace where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateWorkspaceResponse' smart constructor.
data UpdateWorkspaceResponse = UpdateWorkspaceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The date and time of the current update.
    updateDateTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWorkspaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateWorkspaceResponse_httpStatus' - The response's http status code.
--
-- 'updateDateTime', 'updateWorkspaceResponse_updateDateTime' - The date and time of the current update.
newUpdateWorkspaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'updateDateTime'
  Prelude.UTCTime ->
  UpdateWorkspaceResponse
newUpdateWorkspaceResponse
  pHttpStatus_
  pUpdateDateTime_ =
    UpdateWorkspaceResponse'
      { httpStatus = pHttpStatus_,
        updateDateTime =
          Data._Time Lens.# pUpdateDateTime_
      }

-- | The response's http status code.
updateWorkspaceResponse_httpStatus :: Lens.Lens' UpdateWorkspaceResponse Prelude.Int
updateWorkspaceResponse_httpStatus = Lens.lens (\UpdateWorkspaceResponse' {httpStatus} -> httpStatus) (\s@UpdateWorkspaceResponse' {} a -> s {httpStatus = a} :: UpdateWorkspaceResponse)

-- | The date and time of the current update.
updateWorkspaceResponse_updateDateTime :: Lens.Lens' UpdateWorkspaceResponse Prelude.UTCTime
updateWorkspaceResponse_updateDateTime = Lens.lens (\UpdateWorkspaceResponse' {updateDateTime} -> updateDateTime) (\s@UpdateWorkspaceResponse' {} a -> s {updateDateTime = a} :: UpdateWorkspaceResponse) Prelude.. Data._Time

instance Prelude.NFData UpdateWorkspaceResponse where
  rnf UpdateWorkspaceResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf updateDateTime

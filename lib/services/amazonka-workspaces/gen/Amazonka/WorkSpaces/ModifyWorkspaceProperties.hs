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
-- Module      : Amazonka.WorkSpaces.ModifyWorkspaceProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified WorkSpace properties. For important information
-- about how to modify the size of the root and user volumes, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/modify-workspaces.html Modify a WorkSpace>.
--
-- The @MANUAL@ running mode value is only supported by Amazon WorkSpaces
-- Core. Contact your account team to be allow-listed to use this value.
-- For more information, see
-- <http://aws.amazon.com/workspaces/core/ Amazon WorkSpaces Core>.
module Amazonka.WorkSpaces.ModifyWorkspaceProperties
  ( -- * Creating a Request
    ModifyWorkspaceProperties (..),
    newModifyWorkspaceProperties,

    -- * Request Lenses
    modifyWorkspaceProperties_workspaceId,
    modifyWorkspaceProperties_workspaceProperties,

    -- * Destructuring the Response
    ModifyWorkspacePropertiesResponse (..),
    newModifyWorkspacePropertiesResponse,

    -- * Response Lenses
    modifyWorkspacePropertiesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

-- | /See:/ 'newModifyWorkspaceProperties' smart constructor.
data ModifyWorkspaceProperties = ModifyWorkspaceProperties'
  { -- | The identifier of the WorkSpace.
    workspaceId :: Prelude.Text,
    -- | The properties of the WorkSpace.
    workspaceProperties :: WorkspaceProperties
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyWorkspaceProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workspaceId', 'modifyWorkspaceProperties_workspaceId' - The identifier of the WorkSpace.
--
-- 'workspaceProperties', 'modifyWorkspaceProperties_workspaceProperties' - The properties of the WorkSpace.
newModifyWorkspaceProperties ::
  -- | 'workspaceId'
  Prelude.Text ->
  -- | 'workspaceProperties'
  WorkspaceProperties ->
  ModifyWorkspaceProperties
newModifyWorkspaceProperties
  pWorkspaceId_
  pWorkspaceProperties_ =
    ModifyWorkspaceProperties'
      { workspaceId =
          pWorkspaceId_,
        workspaceProperties = pWorkspaceProperties_
      }

-- | The identifier of the WorkSpace.
modifyWorkspaceProperties_workspaceId :: Lens.Lens' ModifyWorkspaceProperties Prelude.Text
modifyWorkspaceProperties_workspaceId = Lens.lens (\ModifyWorkspaceProperties' {workspaceId} -> workspaceId) (\s@ModifyWorkspaceProperties' {} a -> s {workspaceId = a} :: ModifyWorkspaceProperties)

-- | The properties of the WorkSpace.
modifyWorkspaceProperties_workspaceProperties :: Lens.Lens' ModifyWorkspaceProperties WorkspaceProperties
modifyWorkspaceProperties_workspaceProperties = Lens.lens (\ModifyWorkspaceProperties' {workspaceProperties} -> workspaceProperties) (\s@ModifyWorkspaceProperties' {} a -> s {workspaceProperties = a} :: ModifyWorkspaceProperties)

instance Core.AWSRequest ModifyWorkspaceProperties where
  type
    AWSResponse ModifyWorkspaceProperties =
      ModifyWorkspacePropertiesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          ModifyWorkspacePropertiesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyWorkspaceProperties where
  hashWithSalt _salt ModifyWorkspaceProperties' {..} =
    _salt
      `Prelude.hashWithSalt` workspaceId
      `Prelude.hashWithSalt` workspaceProperties

instance Prelude.NFData ModifyWorkspaceProperties where
  rnf ModifyWorkspaceProperties' {..} =
    Prelude.rnf workspaceId
      `Prelude.seq` Prelude.rnf workspaceProperties

instance Data.ToHeaders ModifyWorkspaceProperties where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkspacesService.ModifyWorkspaceProperties" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ModifyWorkspaceProperties where
  toJSON ModifyWorkspaceProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("WorkspaceId" Data..= workspaceId),
            Prelude.Just
              ("WorkspaceProperties" Data..= workspaceProperties)
          ]
      )

instance Data.ToPath ModifyWorkspaceProperties where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyWorkspaceProperties where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newModifyWorkspacePropertiesResponse' smart constructor.
data ModifyWorkspacePropertiesResponse = ModifyWorkspacePropertiesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyWorkspacePropertiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'modifyWorkspacePropertiesResponse_httpStatus' - The response's http status code.
newModifyWorkspacePropertiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyWorkspacePropertiesResponse
newModifyWorkspacePropertiesResponse pHttpStatus_ =
  ModifyWorkspacePropertiesResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
modifyWorkspacePropertiesResponse_httpStatus :: Lens.Lens' ModifyWorkspacePropertiesResponse Prelude.Int
modifyWorkspacePropertiesResponse_httpStatus = Lens.lens (\ModifyWorkspacePropertiesResponse' {httpStatus} -> httpStatus) (\s@ModifyWorkspacePropertiesResponse' {} a -> s {httpStatus = a} :: ModifyWorkspacePropertiesResponse)

instance
  Prelude.NFData
    ModifyWorkspacePropertiesResponse
  where
  rnf ModifyWorkspacePropertiesResponse' {..} =
    Prelude.rnf httpStatus

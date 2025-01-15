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
-- Module      : Amazonka.WorkSpaces.ModifyWorkspaceAccessProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specifies which devices and operating systems users can use to access
-- their WorkSpaces. For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/update-directory-details.html#control-device-access Control Device Access>.
module Amazonka.WorkSpaces.ModifyWorkspaceAccessProperties
  ( -- * Creating a Request
    ModifyWorkspaceAccessProperties (..),
    newModifyWorkspaceAccessProperties,

    -- * Request Lenses
    modifyWorkspaceAccessProperties_resourceId,
    modifyWorkspaceAccessProperties_workspaceAccessProperties,

    -- * Destructuring the Response
    ModifyWorkspaceAccessPropertiesResponse (..),
    newModifyWorkspaceAccessPropertiesResponse,

    -- * Response Lenses
    modifyWorkspaceAccessPropertiesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

-- | /See:/ 'newModifyWorkspaceAccessProperties' smart constructor.
data ModifyWorkspaceAccessProperties = ModifyWorkspaceAccessProperties'
  { -- | The identifier of the directory.
    resourceId :: Prelude.Text,
    -- | The device types and operating systems to enable or disable for access.
    workspaceAccessProperties :: WorkspaceAccessProperties
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyWorkspaceAccessProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'modifyWorkspaceAccessProperties_resourceId' - The identifier of the directory.
--
-- 'workspaceAccessProperties', 'modifyWorkspaceAccessProperties_workspaceAccessProperties' - The device types and operating systems to enable or disable for access.
newModifyWorkspaceAccessProperties ::
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'workspaceAccessProperties'
  WorkspaceAccessProperties ->
  ModifyWorkspaceAccessProperties
newModifyWorkspaceAccessProperties
  pResourceId_
  pWorkspaceAccessProperties_ =
    ModifyWorkspaceAccessProperties'
      { resourceId =
          pResourceId_,
        workspaceAccessProperties =
          pWorkspaceAccessProperties_
      }

-- | The identifier of the directory.
modifyWorkspaceAccessProperties_resourceId :: Lens.Lens' ModifyWorkspaceAccessProperties Prelude.Text
modifyWorkspaceAccessProperties_resourceId = Lens.lens (\ModifyWorkspaceAccessProperties' {resourceId} -> resourceId) (\s@ModifyWorkspaceAccessProperties' {} a -> s {resourceId = a} :: ModifyWorkspaceAccessProperties)

-- | The device types and operating systems to enable or disable for access.
modifyWorkspaceAccessProperties_workspaceAccessProperties :: Lens.Lens' ModifyWorkspaceAccessProperties WorkspaceAccessProperties
modifyWorkspaceAccessProperties_workspaceAccessProperties = Lens.lens (\ModifyWorkspaceAccessProperties' {workspaceAccessProperties} -> workspaceAccessProperties) (\s@ModifyWorkspaceAccessProperties' {} a -> s {workspaceAccessProperties = a} :: ModifyWorkspaceAccessProperties)

instance
  Core.AWSRequest
    ModifyWorkspaceAccessProperties
  where
  type
    AWSResponse ModifyWorkspaceAccessProperties =
      ModifyWorkspaceAccessPropertiesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          ModifyWorkspaceAccessPropertiesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyWorkspaceAccessProperties
  where
  hashWithSalt
    _salt
    ModifyWorkspaceAccessProperties' {..} =
      _salt
        `Prelude.hashWithSalt` resourceId
        `Prelude.hashWithSalt` workspaceAccessProperties

instance
  Prelude.NFData
    ModifyWorkspaceAccessProperties
  where
  rnf ModifyWorkspaceAccessProperties' {..} =
    Prelude.rnf resourceId `Prelude.seq`
      Prelude.rnf workspaceAccessProperties

instance
  Data.ToHeaders
    ModifyWorkspaceAccessProperties
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkspacesService.ModifyWorkspaceAccessProperties" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ModifyWorkspaceAccessProperties where
  toJSON ModifyWorkspaceAccessProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceId" Data..= resourceId),
            Prelude.Just
              ( "WorkspaceAccessProperties"
                  Data..= workspaceAccessProperties
              )
          ]
      )

instance Data.ToPath ModifyWorkspaceAccessProperties where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyWorkspaceAccessProperties where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newModifyWorkspaceAccessPropertiesResponse' smart constructor.
data ModifyWorkspaceAccessPropertiesResponse = ModifyWorkspaceAccessPropertiesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyWorkspaceAccessPropertiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'modifyWorkspaceAccessPropertiesResponse_httpStatus' - The response's http status code.
newModifyWorkspaceAccessPropertiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyWorkspaceAccessPropertiesResponse
newModifyWorkspaceAccessPropertiesResponse
  pHttpStatus_ =
    ModifyWorkspaceAccessPropertiesResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
modifyWorkspaceAccessPropertiesResponse_httpStatus :: Lens.Lens' ModifyWorkspaceAccessPropertiesResponse Prelude.Int
modifyWorkspaceAccessPropertiesResponse_httpStatus = Lens.lens (\ModifyWorkspaceAccessPropertiesResponse' {httpStatus} -> httpStatus) (\s@ModifyWorkspaceAccessPropertiesResponse' {} a -> s {httpStatus = a} :: ModifyWorkspaceAccessPropertiesResponse)

instance
  Prelude.NFData
    ModifyWorkspaceAccessPropertiesResponse
  where
  rnf ModifyWorkspaceAccessPropertiesResponse' {..} =
    Prelude.rnf httpStatus

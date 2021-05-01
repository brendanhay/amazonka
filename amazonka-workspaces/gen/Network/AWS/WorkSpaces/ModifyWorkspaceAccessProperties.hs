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
-- Module      : Network.AWS.WorkSpaces.ModifyWorkspaceAccessProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specifies which devices and operating systems users can use to access
-- their WorkSpaces. For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/update-directory-details.html#control-device-access Control Device Access>.
module Network.AWS.WorkSpaces.ModifyWorkspaceAccessProperties
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newModifyWorkspaceAccessProperties' smart constructor.
data ModifyWorkspaceAccessProperties = ModifyWorkspaceAccessProperties'
  { -- | The identifier of the directory.
    resourceId :: Prelude.Text,
    -- | The device types and operating systems to enable or disable for access.
    workspaceAccessProperties :: WorkspaceAccessProperties
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.AWSRequest
    ModifyWorkspaceAccessProperties
  where
  type
    Rs ModifyWorkspaceAccessProperties =
      ModifyWorkspaceAccessPropertiesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          ModifyWorkspaceAccessPropertiesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyWorkspaceAccessProperties

instance
  Prelude.NFData
    ModifyWorkspaceAccessProperties

instance
  Prelude.ToHeaders
    ModifyWorkspaceAccessProperties
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "WorkspacesService.ModifyWorkspaceAccessProperties" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    ModifyWorkspaceAccessProperties
  where
  toJSON ModifyWorkspaceAccessProperties' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceId" Prelude..= resourceId),
            Prelude.Just
              ( "WorkspaceAccessProperties"
                  Prelude..= workspaceAccessProperties
              )
          ]
      )

instance
  Prelude.ToPath
    ModifyWorkspaceAccessProperties
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    ModifyWorkspaceAccessProperties
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newModifyWorkspaceAccessPropertiesResponse' smart constructor.
data ModifyWorkspaceAccessPropertiesResponse = ModifyWorkspaceAccessPropertiesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

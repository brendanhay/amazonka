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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newModifyWorkspaceAccessProperties' smart constructor.
data ModifyWorkspaceAccessProperties = ModifyWorkspaceAccessProperties'
  { -- | The identifier of the directory.
    resourceId :: Core.Text,
    -- | The device types and operating systems to enable or disable for access.
    workspaceAccessProperties :: WorkspaceAccessProperties
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
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
modifyWorkspaceAccessProperties_resourceId :: Lens.Lens' ModifyWorkspaceAccessProperties Core.Text
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
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          ModifyWorkspaceAccessPropertiesResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    ModifyWorkspaceAccessProperties

instance Core.NFData ModifyWorkspaceAccessProperties

instance
  Core.ToHeaders
    ModifyWorkspaceAccessProperties
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.ModifyWorkspaceAccessProperties" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ModifyWorkspaceAccessProperties where
  toJSON ModifyWorkspaceAccessProperties' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ResourceId" Core..= resourceId),
            Core.Just
              ( "WorkspaceAccessProperties"
                  Core..= workspaceAccessProperties
              )
          ]
      )

instance Core.ToPath ModifyWorkspaceAccessProperties where
  toPath = Core.const "/"

instance Core.ToQuery ModifyWorkspaceAccessProperties where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newModifyWorkspaceAccessPropertiesResponse' smart constructor.
data ModifyWorkspaceAccessPropertiesResponse = ModifyWorkspaceAccessPropertiesResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ModifyWorkspaceAccessPropertiesResponse
newModifyWorkspaceAccessPropertiesResponse
  pHttpStatus_ =
    ModifyWorkspaceAccessPropertiesResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
modifyWorkspaceAccessPropertiesResponse_httpStatus :: Lens.Lens' ModifyWorkspaceAccessPropertiesResponse Core.Int
modifyWorkspaceAccessPropertiesResponse_httpStatus = Lens.lens (\ModifyWorkspaceAccessPropertiesResponse' {httpStatus} -> httpStatus) (\s@ModifyWorkspaceAccessPropertiesResponse' {} a -> s {httpStatus = a} :: ModifyWorkspaceAccessPropertiesResponse)

instance
  Core.NFData
    ModifyWorkspaceAccessPropertiesResponse

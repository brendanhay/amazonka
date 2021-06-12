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
-- Module      : Network.AWS.WorkSpaces.ModifyWorkspaceProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified WorkSpace properties. For important information
-- about how to modify the size of the root and user volumes, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/modify-workspaces.html Modify a WorkSpace>.
module Network.AWS.WorkSpaces.ModifyWorkspaceProperties
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newModifyWorkspaceProperties' smart constructor.
data ModifyWorkspaceProperties = ModifyWorkspaceProperties'
  { -- | The identifier of the WorkSpace.
    workspaceId :: Core.Text,
    -- | The properties of the WorkSpace.
    workspaceProperties :: WorkspaceProperties
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
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
modifyWorkspaceProperties_workspaceId :: Lens.Lens' ModifyWorkspaceProperties Core.Text
modifyWorkspaceProperties_workspaceId = Lens.lens (\ModifyWorkspaceProperties' {workspaceId} -> workspaceId) (\s@ModifyWorkspaceProperties' {} a -> s {workspaceId = a} :: ModifyWorkspaceProperties)

-- | The properties of the WorkSpace.
modifyWorkspaceProperties_workspaceProperties :: Lens.Lens' ModifyWorkspaceProperties WorkspaceProperties
modifyWorkspaceProperties_workspaceProperties = Lens.lens (\ModifyWorkspaceProperties' {workspaceProperties} -> workspaceProperties) (\s@ModifyWorkspaceProperties' {} a -> s {workspaceProperties = a} :: ModifyWorkspaceProperties)

instance Core.AWSRequest ModifyWorkspaceProperties where
  type
    AWSResponse ModifyWorkspaceProperties =
      ModifyWorkspacePropertiesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          ModifyWorkspacePropertiesResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ModifyWorkspaceProperties

instance Core.NFData ModifyWorkspaceProperties

instance Core.ToHeaders ModifyWorkspaceProperties where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.ModifyWorkspaceProperties" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ModifyWorkspaceProperties where
  toJSON ModifyWorkspaceProperties' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("WorkspaceId" Core..= workspaceId),
            Core.Just
              ("WorkspaceProperties" Core..= workspaceProperties)
          ]
      )

instance Core.ToPath ModifyWorkspaceProperties where
  toPath = Core.const "/"

instance Core.ToQuery ModifyWorkspaceProperties where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newModifyWorkspacePropertiesResponse' smart constructor.
data ModifyWorkspacePropertiesResponse = ModifyWorkspacePropertiesResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ModifyWorkspacePropertiesResponse
newModifyWorkspacePropertiesResponse pHttpStatus_ =
  ModifyWorkspacePropertiesResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
modifyWorkspacePropertiesResponse_httpStatus :: Lens.Lens' ModifyWorkspacePropertiesResponse Core.Int
modifyWorkspacePropertiesResponse_httpStatus = Lens.lens (\ModifyWorkspacePropertiesResponse' {httpStatus} -> httpStatus) (\s@ModifyWorkspacePropertiesResponse' {} a -> s {httpStatus = a} :: ModifyWorkspacePropertiesResponse)

instance
  Core.NFData
    ModifyWorkspacePropertiesResponse

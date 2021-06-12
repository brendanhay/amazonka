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
-- Module      : Network.AWS.WorkSpaces.ModifyWorkspaceCreationProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify the default properties used to create WorkSpaces.
module Network.AWS.WorkSpaces.ModifyWorkspaceCreationProperties
  ( -- * Creating a Request
    ModifyWorkspaceCreationProperties (..),
    newModifyWorkspaceCreationProperties,

    -- * Request Lenses
    modifyWorkspaceCreationProperties_resourceId,
    modifyWorkspaceCreationProperties_workspaceCreationProperties,

    -- * Destructuring the Response
    ModifyWorkspaceCreationPropertiesResponse (..),
    newModifyWorkspaceCreationPropertiesResponse,

    -- * Response Lenses
    modifyWorkspaceCreationPropertiesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newModifyWorkspaceCreationProperties' smart constructor.
data ModifyWorkspaceCreationProperties = ModifyWorkspaceCreationProperties'
  { -- | The identifier of the directory.
    resourceId :: Core.Text,
    -- | The default properties for creating WorkSpaces.
    workspaceCreationProperties :: WorkspaceCreationProperties
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyWorkspaceCreationProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'modifyWorkspaceCreationProperties_resourceId' - The identifier of the directory.
--
-- 'workspaceCreationProperties', 'modifyWorkspaceCreationProperties_workspaceCreationProperties' - The default properties for creating WorkSpaces.
newModifyWorkspaceCreationProperties ::
  -- | 'resourceId'
  Core.Text ->
  -- | 'workspaceCreationProperties'
  WorkspaceCreationProperties ->
  ModifyWorkspaceCreationProperties
newModifyWorkspaceCreationProperties
  pResourceId_
  pWorkspaceCreationProperties_ =
    ModifyWorkspaceCreationProperties'
      { resourceId =
          pResourceId_,
        workspaceCreationProperties =
          pWorkspaceCreationProperties_
      }

-- | The identifier of the directory.
modifyWorkspaceCreationProperties_resourceId :: Lens.Lens' ModifyWorkspaceCreationProperties Core.Text
modifyWorkspaceCreationProperties_resourceId = Lens.lens (\ModifyWorkspaceCreationProperties' {resourceId} -> resourceId) (\s@ModifyWorkspaceCreationProperties' {} a -> s {resourceId = a} :: ModifyWorkspaceCreationProperties)

-- | The default properties for creating WorkSpaces.
modifyWorkspaceCreationProperties_workspaceCreationProperties :: Lens.Lens' ModifyWorkspaceCreationProperties WorkspaceCreationProperties
modifyWorkspaceCreationProperties_workspaceCreationProperties = Lens.lens (\ModifyWorkspaceCreationProperties' {workspaceCreationProperties} -> workspaceCreationProperties) (\s@ModifyWorkspaceCreationProperties' {} a -> s {workspaceCreationProperties = a} :: ModifyWorkspaceCreationProperties)

instance
  Core.AWSRequest
    ModifyWorkspaceCreationProperties
  where
  type
    AWSResponse ModifyWorkspaceCreationProperties =
      ModifyWorkspaceCreationPropertiesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          ModifyWorkspaceCreationPropertiesResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    ModifyWorkspaceCreationProperties

instance
  Core.NFData
    ModifyWorkspaceCreationProperties

instance
  Core.ToHeaders
    ModifyWorkspaceCreationProperties
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.ModifyWorkspaceCreationProperties" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    ModifyWorkspaceCreationProperties
  where
  toJSON ModifyWorkspaceCreationProperties' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ResourceId" Core..= resourceId),
            Core.Just
              ( "WorkspaceCreationProperties"
                  Core..= workspaceCreationProperties
              )
          ]
      )

instance
  Core.ToPath
    ModifyWorkspaceCreationProperties
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    ModifyWorkspaceCreationProperties
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newModifyWorkspaceCreationPropertiesResponse' smart constructor.
data ModifyWorkspaceCreationPropertiesResponse = ModifyWorkspaceCreationPropertiesResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyWorkspaceCreationPropertiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'modifyWorkspaceCreationPropertiesResponse_httpStatus' - The response's http status code.
newModifyWorkspaceCreationPropertiesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ModifyWorkspaceCreationPropertiesResponse
newModifyWorkspaceCreationPropertiesResponse
  pHttpStatus_ =
    ModifyWorkspaceCreationPropertiesResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
modifyWorkspaceCreationPropertiesResponse_httpStatus :: Lens.Lens' ModifyWorkspaceCreationPropertiesResponse Core.Int
modifyWorkspaceCreationPropertiesResponse_httpStatus = Lens.lens (\ModifyWorkspaceCreationPropertiesResponse' {httpStatus} -> httpStatus) (\s@ModifyWorkspaceCreationPropertiesResponse' {} a -> s {httpStatus = a} :: ModifyWorkspaceCreationPropertiesResponse)

instance
  Core.NFData
    ModifyWorkspaceCreationPropertiesResponse

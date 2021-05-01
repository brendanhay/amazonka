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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newModifyWorkspaceCreationProperties' smart constructor.
data ModifyWorkspaceCreationProperties = ModifyWorkspaceCreationProperties'
  { -- | The identifier of the directory.
    resourceId :: Prelude.Text,
    -- | The default properties for creating WorkSpaces.
    workspaceCreationProperties :: WorkspaceCreationProperties
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
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
modifyWorkspaceCreationProperties_resourceId :: Lens.Lens' ModifyWorkspaceCreationProperties Prelude.Text
modifyWorkspaceCreationProperties_resourceId = Lens.lens (\ModifyWorkspaceCreationProperties' {resourceId} -> resourceId) (\s@ModifyWorkspaceCreationProperties' {} a -> s {resourceId = a} :: ModifyWorkspaceCreationProperties)

-- | The default properties for creating WorkSpaces.
modifyWorkspaceCreationProperties_workspaceCreationProperties :: Lens.Lens' ModifyWorkspaceCreationProperties WorkspaceCreationProperties
modifyWorkspaceCreationProperties_workspaceCreationProperties = Lens.lens (\ModifyWorkspaceCreationProperties' {workspaceCreationProperties} -> workspaceCreationProperties) (\s@ModifyWorkspaceCreationProperties' {} a -> s {workspaceCreationProperties = a} :: ModifyWorkspaceCreationProperties)

instance
  Prelude.AWSRequest
    ModifyWorkspaceCreationProperties
  where
  type
    Rs ModifyWorkspaceCreationProperties =
      ModifyWorkspaceCreationPropertiesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          ModifyWorkspaceCreationPropertiesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyWorkspaceCreationProperties

instance
  Prelude.NFData
    ModifyWorkspaceCreationProperties

instance
  Prelude.ToHeaders
    ModifyWorkspaceCreationProperties
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "WorkspacesService.ModifyWorkspaceCreationProperties" ::
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
    ModifyWorkspaceCreationProperties
  where
  toJSON ModifyWorkspaceCreationProperties' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceId" Prelude..= resourceId),
            Prelude.Just
              ( "WorkspaceCreationProperties"
                  Prelude..= workspaceCreationProperties
              )
          ]
      )

instance
  Prelude.ToPath
    ModifyWorkspaceCreationProperties
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    ModifyWorkspaceCreationProperties
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newModifyWorkspaceCreationPropertiesResponse' smart constructor.
data ModifyWorkspaceCreationPropertiesResponse = ModifyWorkspaceCreationPropertiesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ModifyWorkspaceCreationPropertiesResponse
newModifyWorkspaceCreationPropertiesResponse
  pHttpStatus_ =
    ModifyWorkspaceCreationPropertiesResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
modifyWorkspaceCreationPropertiesResponse_httpStatus :: Lens.Lens' ModifyWorkspaceCreationPropertiesResponse Prelude.Int
modifyWorkspaceCreationPropertiesResponse_httpStatus = Lens.lens (\ModifyWorkspaceCreationPropertiesResponse' {httpStatus} -> httpStatus) (\s@ModifyWorkspaceCreationPropertiesResponse' {} a -> s {httpStatus = a} :: ModifyWorkspaceCreationPropertiesResponse)

instance
  Prelude.NFData
    ModifyWorkspaceCreationPropertiesResponse

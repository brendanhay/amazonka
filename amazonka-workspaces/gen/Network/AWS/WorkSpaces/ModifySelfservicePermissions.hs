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
-- Module      : Network.AWS.WorkSpaces.ModifySelfservicePermissions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the self-service WorkSpace management capabilities for your
-- users. For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/enable-user-self-service-workspace-management.html Enable Self-Service WorkSpace Management Capabilities for Your Users>.
module Network.AWS.WorkSpaces.ModifySelfservicePermissions
  ( -- * Creating a Request
    ModifySelfservicePermissions (..),
    newModifySelfservicePermissions,

    -- * Request Lenses
    modifySelfservicePermissions_resourceId,
    modifySelfservicePermissions_selfservicePermissions,

    -- * Destructuring the Response
    ModifySelfservicePermissionsResponse (..),
    newModifySelfservicePermissionsResponse,

    -- * Response Lenses
    modifySelfservicePermissionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newModifySelfservicePermissions' smart constructor.
data ModifySelfservicePermissions = ModifySelfservicePermissions'
  { -- | The identifier of the directory.
    resourceId :: Core.Text,
    -- | The permissions to enable or disable self-service capabilities.
    selfservicePermissions :: SelfservicePermissions
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifySelfservicePermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'modifySelfservicePermissions_resourceId' - The identifier of the directory.
--
-- 'selfservicePermissions', 'modifySelfservicePermissions_selfservicePermissions' - The permissions to enable or disable self-service capabilities.
newModifySelfservicePermissions ::
  -- | 'resourceId'
  Core.Text ->
  -- | 'selfservicePermissions'
  SelfservicePermissions ->
  ModifySelfservicePermissions
newModifySelfservicePermissions
  pResourceId_
  pSelfservicePermissions_ =
    ModifySelfservicePermissions'
      { resourceId =
          pResourceId_,
        selfservicePermissions =
          pSelfservicePermissions_
      }

-- | The identifier of the directory.
modifySelfservicePermissions_resourceId :: Lens.Lens' ModifySelfservicePermissions Core.Text
modifySelfservicePermissions_resourceId = Lens.lens (\ModifySelfservicePermissions' {resourceId} -> resourceId) (\s@ModifySelfservicePermissions' {} a -> s {resourceId = a} :: ModifySelfservicePermissions)

-- | The permissions to enable or disable self-service capabilities.
modifySelfservicePermissions_selfservicePermissions :: Lens.Lens' ModifySelfservicePermissions SelfservicePermissions
modifySelfservicePermissions_selfservicePermissions = Lens.lens (\ModifySelfservicePermissions' {selfservicePermissions} -> selfservicePermissions) (\s@ModifySelfservicePermissions' {} a -> s {selfservicePermissions = a} :: ModifySelfservicePermissions)

instance Core.AWSRequest ModifySelfservicePermissions where
  type
    AWSResponse ModifySelfservicePermissions =
      ModifySelfservicePermissionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          ModifySelfservicePermissionsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ModifySelfservicePermissions

instance Core.NFData ModifySelfservicePermissions

instance Core.ToHeaders ModifySelfservicePermissions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.ModifySelfservicePermissions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ModifySelfservicePermissions where
  toJSON ModifySelfservicePermissions' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ResourceId" Core..= resourceId),
            Core.Just
              ( "SelfservicePermissions"
                  Core..= selfservicePermissions
              )
          ]
      )

instance Core.ToPath ModifySelfservicePermissions where
  toPath = Core.const "/"

instance Core.ToQuery ModifySelfservicePermissions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newModifySelfservicePermissionsResponse' smart constructor.
data ModifySelfservicePermissionsResponse = ModifySelfservicePermissionsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifySelfservicePermissionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'modifySelfservicePermissionsResponse_httpStatus' - The response's http status code.
newModifySelfservicePermissionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ModifySelfservicePermissionsResponse
newModifySelfservicePermissionsResponse pHttpStatus_ =
  ModifySelfservicePermissionsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
modifySelfservicePermissionsResponse_httpStatus :: Lens.Lens' ModifySelfservicePermissionsResponse Core.Int
modifySelfservicePermissionsResponse_httpStatus = Lens.lens (\ModifySelfservicePermissionsResponse' {httpStatus} -> httpStatus) (\s@ModifySelfservicePermissionsResponse' {} a -> s {httpStatus = a} :: ModifySelfservicePermissionsResponse)

instance
  Core.NFData
    ModifySelfservicePermissionsResponse

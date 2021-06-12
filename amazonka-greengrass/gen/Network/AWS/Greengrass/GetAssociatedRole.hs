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
-- Module      : Network.AWS.Greengrass.GetAssociatedRole
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the role associated with a particular group.
module Network.AWS.Greengrass.GetAssociatedRole
  ( -- * Creating a Request
    GetAssociatedRole (..),
    newGetAssociatedRole,

    -- * Request Lenses
    getAssociatedRole_groupId,

    -- * Destructuring the Response
    GetAssociatedRoleResponse (..),
    newGetAssociatedRoleResponse,

    -- * Response Lenses
    getAssociatedRoleResponse_roleArn,
    getAssociatedRoleResponse_associatedAt,
    getAssociatedRoleResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetAssociatedRole' smart constructor.
data GetAssociatedRole = GetAssociatedRole'
  { -- | The ID of the Greengrass group.
    groupId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAssociatedRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupId', 'getAssociatedRole_groupId' - The ID of the Greengrass group.
newGetAssociatedRole ::
  -- | 'groupId'
  Core.Text ->
  GetAssociatedRole
newGetAssociatedRole pGroupId_ =
  GetAssociatedRole' {groupId = pGroupId_}

-- | The ID of the Greengrass group.
getAssociatedRole_groupId :: Lens.Lens' GetAssociatedRole Core.Text
getAssociatedRole_groupId = Lens.lens (\GetAssociatedRole' {groupId} -> groupId) (\s@GetAssociatedRole' {} a -> s {groupId = a} :: GetAssociatedRole)

instance Core.AWSRequest GetAssociatedRole where
  type
    AWSResponse GetAssociatedRole =
      GetAssociatedRoleResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAssociatedRoleResponse'
            Core.<$> (x Core..?> "RoleArn")
            Core.<*> (x Core..?> "AssociatedAt")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetAssociatedRole

instance Core.NFData GetAssociatedRole

instance Core.ToHeaders GetAssociatedRole where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetAssociatedRole where
  toPath GetAssociatedRole' {..} =
    Core.mconcat
      ["/greengrass/groups/", Core.toBS groupId, "/role"]

instance Core.ToQuery GetAssociatedRole where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetAssociatedRoleResponse' smart constructor.
data GetAssociatedRoleResponse = GetAssociatedRoleResponse'
  { -- | The ARN of the role that is associated with the group.
    roleArn :: Core.Maybe Core.Text,
    -- | The time when the role was associated with the group.
    associatedAt :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAssociatedRoleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'getAssociatedRoleResponse_roleArn' - The ARN of the role that is associated with the group.
--
-- 'associatedAt', 'getAssociatedRoleResponse_associatedAt' - The time when the role was associated with the group.
--
-- 'httpStatus', 'getAssociatedRoleResponse_httpStatus' - The response's http status code.
newGetAssociatedRoleResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetAssociatedRoleResponse
newGetAssociatedRoleResponse pHttpStatus_ =
  GetAssociatedRoleResponse'
    { roleArn = Core.Nothing,
      associatedAt = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the role that is associated with the group.
getAssociatedRoleResponse_roleArn :: Lens.Lens' GetAssociatedRoleResponse (Core.Maybe Core.Text)
getAssociatedRoleResponse_roleArn = Lens.lens (\GetAssociatedRoleResponse' {roleArn} -> roleArn) (\s@GetAssociatedRoleResponse' {} a -> s {roleArn = a} :: GetAssociatedRoleResponse)

-- | The time when the role was associated with the group.
getAssociatedRoleResponse_associatedAt :: Lens.Lens' GetAssociatedRoleResponse (Core.Maybe Core.Text)
getAssociatedRoleResponse_associatedAt = Lens.lens (\GetAssociatedRoleResponse' {associatedAt} -> associatedAt) (\s@GetAssociatedRoleResponse' {} a -> s {associatedAt = a} :: GetAssociatedRoleResponse)

-- | The response's http status code.
getAssociatedRoleResponse_httpStatus :: Lens.Lens' GetAssociatedRoleResponse Core.Int
getAssociatedRoleResponse_httpStatus = Lens.lens (\GetAssociatedRoleResponse' {httpStatus} -> httpStatus) (\s@GetAssociatedRoleResponse' {} a -> s {httpStatus = a} :: GetAssociatedRoleResponse)

instance Core.NFData GetAssociatedRoleResponse

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
-- Module      : Network.AWS.Greengrass.AssociateRoleToGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a role with a group. Your Greengrass core will use the role
-- to access AWS cloud services. The role\'s permissions should allow
-- Greengrass core Lambda functions to perform actions against the cloud.
module Network.AWS.Greengrass.AssociateRoleToGroup
  ( -- * Creating a Request
    AssociateRoleToGroup (..),
    newAssociateRoleToGroup,

    -- * Request Lenses
    associateRoleToGroup_groupId,
    associateRoleToGroup_roleArn,

    -- * Destructuring the Response
    AssociateRoleToGroupResponse (..),
    newAssociateRoleToGroupResponse,

    -- * Response Lenses
    associateRoleToGroupResponse_associatedAt,
    associateRoleToGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateRoleToGroup' smart constructor.
data AssociateRoleToGroup = AssociateRoleToGroup'
  { -- | The ID of the Greengrass group.
    groupId :: Core.Text,
    -- | The ARN of the role you wish to associate with this group. The existence
    -- of the role is not validated.
    roleArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateRoleToGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupId', 'associateRoleToGroup_groupId' - The ID of the Greengrass group.
--
-- 'roleArn', 'associateRoleToGroup_roleArn' - The ARN of the role you wish to associate with this group. The existence
-- of the role is not validated.
newAssociateRoleToGroup ::
  -- | 'groupId'
  Core.Text ->
  -- | 'roleArn'
  Core.Text ->
  AssociateRoleToGroup
newAssociateRoleToGroup pGroupId_ pRoleArn_ =
  AssociateRoleToGroup'
    { groupId = pGroupId_,
      roleArn = pRoleArn_
    }

-- | The ID of the Greengrass group.
associateRoleToGroup_groupId :: Lens.Lens' AssociateRoleToGroup Core.Text
associateRoleToGroup_groupId = Lens.lens (\AssociateRoleToGroup' {groupId} -> groupId) (\s@AssociateRoleToGroup' {} a -> s {groupId = a} :: AssociateRoleToGroup)

-- | The ARN of the role you wish to associate with this group. The existence
-- of the role is not validated.
associateRoleToGroup_roleArn :: Lens.Lens' AssociateRoleToGroup Core.Text
associateRoleToGroup_roleArn = Lens.lens (\AssociateRoleToGroup' {roleArn} -> roleArn) (\s@AssociateRoleToGroup' {} a -> s {roleArn = a} :: AssociateRoleToGroup)

instance Core.AWSRequest AssociateRoleToGroup where
  type
    AWSResponse AssociateRoleToGroup =
      AssociateRoleToGroupResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateRoleToGroupResponse'
            Core.<$> (x Core..?> "AssociatedAt")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AssociateRoleToGroup

instance Core.NFData AssociateRoleToGroup

instance Core.ToHeaders AssociateRoleToGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AssociateRoleToGroup where
  toJSON AssociateRoleToGroup' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("RoleArn" Core..= roleArn)]
      )

instance Core.ToPath AssociateRoleToGroup where
  toPath AssociateRoleToGroup' {..} =
    Core.mconcat
      ["/greengrass/groups/", Core.toBS groupId, "/role"]

instance Core.ToQuery AssociateRoleToGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAssociateRoleToGroupResponse' smart constructor.
data AssociateRoleToGroupResponse = AssociateRoleToGroupResponse'
  { -- | The time, in milliseconds since the epoch, when the role ARN was
    -- associated with the group.
    associatedAt :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateRoleToGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associatedAt', 'associateRoleToGroupResponse_associatedAt' - The time, in milliseconds since the epoch, when the role ARN was
-- associated with the group.
--
-- 'httpStatus', 'associateRoleToGroupResponse_httpStatus' - The response's http status code.
newAssociateRoleToGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AssociateRoleToGroupResponse
newAssociateRoleToGroupResponse pHttpStatus_ =
  AssociateRoleToGroupResponse'
    { associatedAt =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time, in milliseconds since the epoch, when the role ARN was
-- associated with the group.
associateRoleToGroupResponse_associatedAt :: Lens.Lens' AssociateRoleToGroupResponse (Core.Maybe Core.Text)
associateRoleToGroupResponse_associatedAt = Lens.lens (\AssociateRoleToGroupResponse' {associatedAt} -> associatedAt) (\s@AssociateRoleToGroupResponse' {} a -> s {associatedAt = a} :: AssociateRoleToGroupResponse)

-- | The response's http status code.
associateRoleToGroupResponse_httpStatus :: Lens.Lens' AssociateRoleToGroupResponse Core.Int
associateRoleToGroupResponse_httpStatus = Lens.lens (\AssociateRoleToGroupResponse' {httpStatus} -> httpStatus) (\s@AssociateRoleToGroupResponse' {} a -> s {httpStatus = a} :: AssociateRoleToGroupResponse)

instance Core.NFData AssociateRoleToGroupResponse

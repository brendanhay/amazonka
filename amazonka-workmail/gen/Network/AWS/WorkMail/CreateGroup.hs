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
-- Module      : Network.AWS.WorkMail.CreateGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a group that can be used in Amazon WorkMail by calling the
-- RegisterToWorkMail operation.
module Network.AWS.WorkMail.CreateGroup
  ( -- * Creating a Request
    CreateGroup (..),
    newCreateGroup,

    -- * Request Lenses
    createGroup_organizationId,
    createGroup_name,

    -- * Destructuring the Response
    CreateGroupResponse (..),
    newCreateGroupResponse,

    -- * Response Lenses
    createGroupResponse_groupId,
    createGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newCreateGroup' smart constructor.
data CreateGroup = CreateGroup'
  { -- | The organization under which the group is to be created.
    organizationId :: Core.Text,
    -- | The name of the group.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'createGroup_organizationId' - The organization under which the group is to be created.
--
-- 'name', 'createGroup_name' - The name of the group.
newCreateGroup ::
  -- | 'organizationId'
  Core.Text ->
  -- | 'name'
  Core.Text ->
  CreateGroup
newCreateGroup pOrganizationId_ pName_ =
  CreateGroup'
    { organizationId = pOrganizationId_,
      name = pName_
    }

-- | The organization under which the group is to be created.
createGroup_organizationId :: Lens.Lens' CreateGroup Core.Text
createGroup_organizationId = Lens.lens (\CreateGroup' {organizationId} -> organizationId) (\s@CreateGroup' {} a -> s {organizationId = a} :: CreateGroup)

-- | The name of the group.
createGroup_name :: Lens.Lens' CreateGroup Core.Text
createGroup_name = Lens.lens (\CreateGroup' {name} -> name) (\s@CreateGroup' {} a -> s {name = a} :: CreateGroup)

instance Core.AWSRequest CreateGroup where
  type AWSResponse CreateGroup = CreateGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGroupResponse'
            Core.<$> (x Core..?> "GroupId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateGroup

instance Core.NFData CreateGroup

instance Core.ToHeaders CreateGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("WorkMailService.CreateGroup" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateGroup where
  toJSON CreateGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("OrganizationId" Core..= organizationId),
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath CreateGroup where
  toPath = Core.const "/"

instance Core.ToQuery CreateGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateGroupResponse' smart constructor.
data CreateGroupResponse = CreateGroupResponse'
  { -- | The identifier of the group.
    groupId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupId', 'createGroupResponse_groupId' - The identifier of the group.
--
-- 'httpStatus', 'createGroupResponse_httpStatus' - The response's http status code.
newCreateGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateGroupResponse
newCreateGroupResponse pHttpStatus_ =
  CreateGroupResponse'
    { groupId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the group.
createGroupResponse_groupId :: Lens.Lens' CreateGroupResponse (Core.Maybe Core.Text)
createGroupResponse_groupId = Lens.lens (\CreateGroupResponse' {groupId} -> groupId) (\s@CreateGroupResponse' {} a -> s {groupId = a} :: CreateGroupResponse)

-- | The response's http status code.
createGroupResponse_httpStatus :: Lens.Lens' CreateGroupResponse Core.Int
createGroupResponse_httpStatus = Lens.lens (\CreateGroupResponse' {httpStatus} -> httpStatus) (\s@CreateGroupResponse' {} a -> s {httpStatus = a} :: CreateGroupResponse)

instance Core.NFData CreateGroupResponse

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
-- Module      : Network.AWS.WorkSpaces.CreateIpGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an IP access control group.
--
-- An IP access control group provides you with the ability to control the
-- IP addresses from which users are allowed to access their WorkSpaces. To
-- specify the CIDR address ranges, add rules to your IP access control
-- group and then associate the group with your directory. You can add
-- rules when you create the group or at any time using AuthorizeIpRules.
--
-- There is a default IP access control group associated with your
-- directory. If you don\'t associate an IP access control group with your
-- directory, the default group is used. The default group includes a
-- default rule that allows users to access their WorkSpaces from anywhere.
-- You cannot modify the default IP access control group for your
-- directory.
module Network.AWS.WorkSpaces.CreateIpGroup
  ( -- * Creating a Request
    CreateIpGroup (..),
    newCreateIpGroup,

    -- * Request Lenses
    createIpGroup_userRules,
    createIpGroup_groupDesc,
    createIpGroup_tags,
    createIpGroup_groupName,

    -- * Destructuring the Response
    CreateIpGroupResponse (..),
    newCreateIpGroupResponse,

    -- * Response Lenses
    createIpGroupResponse_groupId,
    createIpGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newCreateIpGroup' smart constructor.
data CreateIpGroup = CreateIpGroup'
  { -- | The rules to add to the group.
    userRules :: Core.Maybe [IpRuleItem],
    -- | The description of the group.
    groupDesc :: Core.Maybe Core.Text,
    -- | The tags. Each WorkSpaces resource can have a maximum of 50 tags.
    tags :: Core.Maybe [Tag],
    -- | The name of the group.
    groupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateIpGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userRules', 'createIpGroup_userRules' - The rules to add to the group.
--
-- 'groupDesc', 'createIpGroup_groupDesc' - The description of the group.
--
-- 'tags', 'createIpGroup_tags' - The tags. Each WorkSpaces resource can have a maximum of 50 tags.
--
-- 'groupName', 'createIpGroup_groupName' - The name of the group.
newCreateIpGroup ::
  -- | 'groupName'
  Core.Text ->
  CreateIpGroup
newCreateIpGroup pGroupName_ =
  CreateIpGroup'
    { userRules = Core.Nothing,
      groupDesc = Core.Nothing,
      tags = Core.Nothing,
      groupName = pGroupName_
    }

-- | The rules to add to the group.
createIpGroup_userRules :: Lens.Lens' CreateIpGroup (Core.Maybe [IpRuleItem])
createIpGroup_userRules = Lens.lens (\CreateIpGroup' {userRules} -> userRules) (\s@CreateIpGroup' {} a -> s {userRules = a} :: CreateIpGroup) Core.. Lens.mapping Lens._Coerce

-- | The description of the group.
createIpGroup_groupDesc :: Lens.Lens' CreateIpGroup (Core.Maybe Core.Text)
createIpGroup_groupDesc = Lens.lens (\CreateIpGroup' {groupDesc} -> groupDesc) (\s@CreateIpGroup' {} a -> s {groupDesc = a} :: CreateIpGroup)

-- | The tags. Each WorkSpaces resource can have a maximum of 50 tags.
createIpGroup_tags :: Lens.Lens' CreateIpGroup (Core.Maybe [Tag])
createIpGroup_tags = Lens.lens (\CreateIpGroup' {tags} -> tags) (\s@CreateIpGroup' {} a -> s {tags = a} :: CreateIpGroup) Core.. Lens.mapping Lens._Coerce

-- | The name of the group.
createIpGroup_groupName :: Lens.Lens' CreateIpGroup Core.Text
createIpGroup_groupName = Lens.lens (\CreateIpGroup' {groupName} -> groupName) (\s@CreateIpGroup' {} a -> s {groupName = a} :: CreateIpGroup)

instance Core.AWSRequest CreateIpGroup where
  type
    AWSResponse CreateIpGroup =
      CreateIpGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateIpGroupResponse'
            Core.<$> (x Core..?> "GroupId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateIpGroup

instance Core.NFData CreateIpGroup

instance Core.ToHeaders CreateIpGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.CreateIpGroup" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateIpGroup where
  toJSON CreateIpGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ ("UserRules" Core..=) Core.<$> userRules,
            ("GroupDesc" Core..=) Core.<$> groupDesc,
            ("Tags" Core..=) Core.<$> tags,
            Core.Just ("GroupName" Core..= groupName)
          ]
      )

instance Core.ToPath CreateIpGroup where
  toPath = Core.const "/"

instance Core.ToQuery CreateIpGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateIpGroupResponse' smart constructor.
data CreateIpGroupResponse = CreateIpGroupResponse'
  { -- | The identifier of the group.
    groupId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateIpGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupId', 'createIpGroupResponse_groupId' - The identifier of the group.
--
-- 'httpStatus', 'createIpGroupResponse_httpStatus' - The response's http status code.
newCreateIpGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateIpGroupResponse
newCreateIpGroupResponse pHttpStatus_ =
  CreateIpGroupResponse'
    { groupId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the group.
createIpGroupResponse_groupId :: Lens.Lens' CreateIpGroupResponse (Core.Maybe Core.Text)
createIpGroupResponse_groupId = Lens.lens (\CreateIpGroupResponse' {groupId} -> groupId) (\s@CreateIpGroupResponse' {} a -> s {groupId = a} :: CreateIpGroupResponse)

-- | The response's http status code.
createIpGroupResponse_httpStatus :: Lens.Lens' CreateIpGroupResponse Core.Int
createIpGroupResponse_httpStatus = Lens.lens (\CreateIpGroupResponse' {httpStatus} -> httpStatus) (\s@CreateIpGroupResponse' {} a -> s {httpStatus = a} :: CreateIpGroupResponse)

instance Core.NFData CreateIpGroupResponse

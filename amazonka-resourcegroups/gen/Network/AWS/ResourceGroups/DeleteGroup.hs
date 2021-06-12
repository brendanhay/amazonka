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
-- Module      : Network.AWS.ResourceGroups.DeleteGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified resource group. Deleting a resource group does not
-- delete any resources that are members of the group; it only deletes the
-- group structure.
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
-- -   @resource-groups:DeleteGroup@
module Network.AWS.ResourceGroups.DeleteGroup
  ( -- * Creating a Request
    DeleteGroup (..),
    newDeleteGroup,

    -- * Request Lenses
    deleteGroup_groupName,
    deleteGroup_group,

    -- * Destructuring the Response
    DeleteGroupResponse (..),
    newDeleteGroupResponse,

    -- * Response Lenses
    deleteGroupResponse_group,
    deleteGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import Network.AWS.ResourceGroups.Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteGroup' smart constructor.
data DeleteGroup = DeleteGroup'
  { -- | Deprecated - don\'t use this parameter. Use @Group@ instead.
    groupName :: Core.Maybe Core.Text,
    -- | The name or the ARN of the resource group to delete.
    group' :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupName', 'deleteGroup_groupName' - Deprecated - don\'t use this parameter. Use @Group@ instead.
--
-- 'group'', 'deleteGroup_group' - The name or the ARN of the resource group to delete.
newDeleteGroup ::
  DeleteGroup
newDeleteGroup =
  DeleteGroup'
    { groupName = Core.Nothing,
      group' = Core.Nothing
    }

-- | Deprecated - don\'t use this parameter. Use @Group@ instead.
deleteGroup_groupName :: Lens.Lens' DeleteGroup (Core.Maybe Core.Text)
deleteGroup_groupName = Lens.lens (\DeleteGroup' {groupName} -> groupName) (\s@DeleteGroup' {} a -> s {groupName = a} :: DeleteGroup)

-- | The name or the ARN of the resource group to delete.
deleteGroup_group :: Lens.Lens' DeleteGroup (Core.Maybe Core.Text)
deleteGroup_group = Lens.lens (\DeleteGroup' {group'} -> group') (\s@DeleteGroup' {} a -> s {group' = a} :: DeleteGroup)

instance Core.AWSRequest DeleteGroup where
  type AWSResponse DeleteGroup = DeleteGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteGroupResponse'
            Core.<$> (x Core..?> "Group")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteGroup

instance Core.NFData DeleteGroup

instance Core.ToHeaders DeleteGroup where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON DeleteGroup where
  toJSON DeleteGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ ("GroupName" Core..=) Core.<$> groupName,
            ("Group" Core..=) Core.<$> group'
          ]
      )

instance Core.ToPath DeleteGroup where
  toPath = Core.const "/delete-group"

instance Core.ToQuery DeleteGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteGroupResponse' smart constructor.
data DeleteGroupResponse = DeleteGroupResponse'
  { -- | A full description of the deleted resource group.
    group' :: Core.Maybe Group,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'group'', 'deleteGroupResponse_group' - A full description of the deleted resource group.
--
-- 'httpStatus', 'deleteGroupResponse_httpStatus' - The response's http status code.
newDeleteGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteGroupResponse
newDeleteGroupResponse pHttpStatus_ =
  DeleteGroupResponse'
    { group' = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A full description of the deleted resource group.
deleteGroupResponse_group :: Lens.Lens' DeleteGroupResponse (Core.Maybe Group)
deleteGroupResponse_group = Lens.lens (\DeleteGroupResponse' {group'} -> group') (\s@DeleteGroupResponse' {} a -> s {group' = a} :: DeleteGroupResponse)

-- | The response's http status code.
deleteGroupResponse_httpStatus :: Lens.Lens' DeleteGroupResponse Core.Int
deleteGroupResponse_httpStatus = Lens.lens (\DeleteGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteGroupResponse' {} a -> s {httpStatus = a} :: DeleteGroupResponse)

instance Core.NFData DeleteGroupResponse

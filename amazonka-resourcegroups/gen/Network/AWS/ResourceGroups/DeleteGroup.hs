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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import Network.AWS.ResourceGroups.Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteGroup' smart constructor.
data DeleteGroup = DeleteGroup'
  { -- | Deprecated - don\'t use this parameter. Use @Group@ instead.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The name or the ARN of the resource group to delete.
    group' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { groupName = Prelude.Nothing,
      group' = Prelude.Nothing
    }

-- | Deprecated - don\'t use this parameter. Use @Group@ instead.
deleteGroup_groupName :: Lens.Lens' DeleteGroup (Prelude.Maybe Prelude.Text)
deleteGroup_groupName = Lens.lens (\DeleteGroup' {groupName} -> groupName) (\s@DeleteGroup' {} a -> s {groupName = a} :: DeleteGroup)

-- | The name or the ARN of the resource group to delete.
deleteGroup_group :: Lens.Lens' DeleteGroup (Prelude.Maybe Prelude.Text)
deleteGroup_group = Lens.lens (\DeleteGroup' {group'} -> group') (\s@DeleteGroup' {} a -> s {group' = a} :: DeleteGroup)

instance Prelude.AWSRequest DeleteGroup where
  type Rs DeleteGroup = DeleteGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteGroupResponse'
            Prelude.<$> (x Prelude..?> "Group")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteGroup

instance Prelude.NFData DeleteGroup

instance Prelude.ToHeaders DeleteGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON DeleteGroup where
  toJSON DeleteGroup' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("GroupName" Prelude..=) Prelude.<$> groupName,
            ("Group" Prelude..=) Prelude.<$> group'
          ]
      )

instance Prelude.ToPath DeleteGroup where
  toPath = Prelude.const "/delete-group"

instance Prelude.ToQuery DeleteGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteGroupResponse' smart constructor.
data DeleteGroupResponse = DeleteGroupResponse'
  { -- | A full description of the deleted resource group.
    group' :: Prelude.Maybe Group,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteGroupResponse
newDeleteGroupResponse pHttpStatus_ =
  DeleteGroupResponse'
    { group' = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A full description of the deleted resource group.
deleteGroupResponse_group :: Lens.Lens' DeleteGroupResponse (Prelude.Maybe Group)
deleteGroupResponse_group = Lens.lens (\DeleteGroupResponse' {group'} -> group') (\s@DeleteGroupResponse' {} a -> s {group' = a} :: DeleteGroupResponse)

-- | The response's http status code.
deleteGroupResponse_httpStatus :: Lens.Lens' DeleteGroupResponse Prelude.Int
deleteGroupResponse_httpStatus = Lens.lens (\DeleteGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteGroupResponse' {} a -> s {httpStatus = a} :: DeleteGroupResponse)

instance Prelude.NFData DeleteGroupResponse

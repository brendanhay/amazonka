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
-- Module      : Network.AWS.WorkMail.DeleteGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a group from Amazon WorkMail.
module Network.AWS.WorkMail.DeleteGroup
  ( -- * Creating a Request
    DeleteGroup (..),
    newDeleteGroup,

    -- * Request Lenses
    deleteGroup_organizationId,
    deleteGroup_groupId,

    -- * Destructuring the Response
    DeleteGroupResponse (..),
    newDeleteGroupResponse,

    -- * Response Lenses
    deleteGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newDeleteGroup' smart constructor.
data DeleteGroup = DeleteGroup'
  { -- | The organization that contains the group.
    organizationId :: Core.Text,
    -- | The identifier of the group to be deleted.
    groupId :: Core.Text
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
-- 'organizationId', 'deleteGroup_organizationId' - The organization that contains the group.
--
-- 'groupId', 'deleteGroup_groupId' - The identifier of the group to be deleted.
newDeleteGroup ::
  -- | 'organizationId'
  Core.Text ->
  -- | 'groupId'
  Core.Text ->
  DeleteGroup
newDeleteGroup pOrganizationId_ pGroupId_ =
  DeleteGroup'
    { organizationId = pOrganizationId_,
      groupId = pGroupId_
    }

-- | The organization that contains the group.
deleteGroup_organizationId :: Lens.Lens' DeleteGroup Core.Text
deleteGroup_organizationId = Lens.lens (\DeleteGroup' {organizationId} -> organizationId) (\s@DeleteGroup' {} a -> s {organizationId = a} :: DeleteGroup)

-- | The identifier of the group to be deleted.
deleteGroup_groupId :: Lens.Lens' DeleteGroup Core.Text
deleteGroup_groupId = Lens.lens (\DeleteGroup' {groupId} -> groupId) (\s@DeleteGroup' {} a -> s {groupId = a} :: DeleteGroup)

instance Core.AWSRequest DeleteGroup where
  type AWSResponse DeleteGroup = DeleteGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteGroupResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteGroup

instance Core.NFData DeleteGroup

instance Core.ToHeaders DeleteGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("WorkMailService.DeleteGroup" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteGroup where
  toJSON DeleteGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("OrganizationId" Core..= organizationId),
            Core.Just ("GroupId" Core..= groupId)
          ]
      )

instance Core.ToPath DeleteGroup where
  toPath = Core.const "/"

instance Core.ToQuery DeleteGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteGroupResponse' smart constructor.
data DeleteGroupResponse = DeleteGroupResponse'
  { -- | The response's http status code.
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
-- 'httpStatus', 'deleteGroupResponse_httpStatus' - The response's http status code.
newDeleteGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteGroupResponse
newDeleteGroupResponse pHttpStatus_ =
  DeleteGroupResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteGroupResponse_httpStatus :: Lens.Lens' DeleteGroupResponse Core.Int
deleteGroupResponse_httpStatus = Lens.lens (\DeleteGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteGroupResponse' {} a -> s {httpStatus = a} :: DeleteGroupResponse)

instance Core.NFData DeleteGroupResponse

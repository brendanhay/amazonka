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
-- Module      : Network.AWS.Connect.DeleteUserHierarchyGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing user hierarchy group. It must not be associated with
-- any agents or have any active child groups.
module Network.AWS.Connect.DeleteUserHierarchyGroup
  ( -- * Creating a Request
    DeleteUserHierarchyGroup (..),
    newDeleteUserHierarchyGroup,

    -- * Request Lenses
    deleteUserHierarchyGroup_hierarchyGroupId,
    deleteUserHierarchyGroup_instanceId,

    -- * Destructuring the Response
    DeleteUserHierarchyGroupResponse (..),
    newDeleteUserHierarchyGroupResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteUserHierarchyGroup' smart constructor.
data DeleteUserHierarchyGroup = DeleteUserHierarchyGroup'
  { -- | The identifier of the hierarchy group.
    hierarchyGroupId :: Core.Text,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteUserHierarchyGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hierarchyGroupId', 'deleteUserHierarchyGroup_hierarchyGroupId' - The identifier of the hierarchy group.
--
-- 'instanceId', 'deleteUserHierarchyGroup_instanceId' - The identifier of the Amazon Connect instance.
newDeleteUserHierarchyGroup ::
  -- | 'hierarchyGroupId'
  Core.Text ->
  -- | 'instanceId'
  Core.Text ->
  DeleteUserHierarchyGroup
newDeleteUserHierarchyGroup
  pHierarchyGroupId_
  pInstanceId_ =
    DeleteUserHierarchyGroup'
      { hierarchyGroupId =
          pHierarchyGroupId_,
        instanceId = pInstanceId_
      }

-- | The identifier of the hierarchy group.
deleteUserHierarchyGroup_hierarchyGroupId :: Lens.Lens' DeleteUserHierarchyGroup Core.Text
deleteUserHierarchyGroup_hierarchyGroupId = Lens.lens (\DeleteUserHierarchyGroup' {hierarchyGroupId} -> hierarchyGroupId) (\s@DeleteUserHierarchyGroup' {} a -> s {hierarchyGroupId = a} :: DeleteUserHierarchyGroup)

-- | The identifier of the Amazon Connect instance.
deleteUserHierarchyGroup_instanceId :: Lens.Lens' DeleteUserHierarchyGroup Core.Text
deleteUserHierarchyGroup_instanceId = Lens.lens (\DeleteUserHierarchyGroup' {instanceId} -> instanceId) (\s@DeleteUserHierarchyGroup' {} a -> s {instanceId = a} :: DeleteUserHierarchyGroup)

instance Core.AWSRequest DeleteUserHierarchyGroup where
  type
    AWSResponse DeleteUserHierarchyGroup =
      DeleteUserHierarchyGroupResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull
      DeleteUserHierarchyGroupResponse'

instance Core.Hashable DeleteUserHierarchyGroup

instance Core.NFData DeleteUserHierarchyGroup

instance Core.ToHeaders DeleteUserHierarchyGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteUserHierarchyGroup where
  toPath DeleteUserHierarchyGroup' {..} =
    Core.mconcat
      [ "/user-hierarchy-groups/",
        Core.toBS instanceId,
        "/",
        Core.toBS hierarchyGroupId
      ]

instance Core.ToQuery DeleteUserHierarchyGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteUserHierarchyGroupResponse' smart constructor.
data DeleteUserHierarchyGroupResponse = DeleteUserHierarchyGroupResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteUserHierarchyGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteUserHierarchyGroupResponse ::
  DeleteUserHierarchyGroupResponse
newDeleteUserHierarchyGroupResponse =
  DeleteUserHierarchyGroupResponse'

instance Core.NFData DeleteUserHierarchyGroupResponse

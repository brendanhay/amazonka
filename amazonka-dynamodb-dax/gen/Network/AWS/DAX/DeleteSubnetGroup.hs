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
-- Module      : Network.AWS.DAX.DeleteSubnetGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a subnet group.
--
-- You cannot delete a subnet group if it is associated with any DAX
-- clusters.
module Network.AWS.DAX.DeleteSubnetGroup
  ( -- * Creating a Request
    DeleteSubnetGroup (..),
    newDeleteSubnetGroup,

    -- * Request Lenses
    deleteSubnetGroup_subnetGroupName,

    -- * Destructuring the Response
    DeleteSubnetGroupResponse (..),
    newDeleteSubnetGroupResponse,

    -- * Response Lenses
    deleteSubnetGroupResponse_deletionMessage,
    deleteSubnetGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DAX.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteSubnetGroup' smart constructor.
data DeleteSubnetGroup = DeleteSubnetGroup'
  { -- | The name of the subnet group to delete.
    subnetGroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteSubnetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subnetGroupName', 'deleteSubnetGroup_subnetGroupName' - The name of the subnet group to delete.
newDeleteSubnetGroup ::
  -- | 'subnetGroupName'
  Core.Text ->
  DeleteSubnetGroup
newDeleteSubnetGroup pSubnetGroupName_ =
  DeleteSubnetGroup'
    { subnetGroupName =
        pSubnetGroupName_
    }

-- | The name of the subnet group to delete.
deleteSubnetGroup_subnetGroupName :: Lens.Lens' DeleteSubnetGroup Core.Text
deleteSubnetGroup_subnetGroupName = Lens.lens (\DeleteSubnetGroup' {subnetGroupName} -> subnetGroupName) (\s@DeleteSubnetGroup' {} a -> s {subnetGroupName = a} :: DeleteSubnetGroup)

instance Core.AWSRequest DeleteSubnetGroup where
  type
    AWSResponse DeleteSubnetGroup =
      DeleteSubnetGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteSubnetGroupResponse'
            Core.<$> (x Core..?> "DeletionMessage")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteSubnetGroup

instance Core.NFData DeleteSubnetGroup

instance Core.ToHeaders DeleteSubnetGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonDAXV3.DeleteSubnetGroup" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteSubnetGroup where
  toJSON DeleteSubnetGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("SubnetGroupName" Core..= subnetGroupName)
          ]
      )

instance Core.ToPath DeleteSubnetGroup where
  toPath = Core.const "/"

instance Core.ToQuery DeleteSubnetGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteSubnetGroupResponse' smart constructor.
data DeleteSubnetGroupResponse = DeleteSubnetGroupResponse'
  { -- | A user-specified message for this action (i.e., a reason for deleting
    -- the subnet group).
    deletionMessage :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteSubnetGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deletionMessage', 'deleteSubnetGroupResponse_deletionMessage' - A user-specified message for this action (i.e., a reason for deleting
-- the subnet group).
--
-- 'httpStatus', 'deleteSubnetGroupResponse_httpStatus' - The response's http status code.
newDeleteSubnetGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteSubnetGroupResponse
newDeleteSubnetGroupResponse pHttpStatus_ =
  DeleteSubnetGroupResponse'
    { deletionMessage =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A user-specified message for this action (i.e., a reason for deleting
-- the subnet group).
deleteSubnetGroupResponse_deletionMessage :: Lens.Lens' DeleteSubnetGroupResponse (Core.Maybe Core.Text)
deleteSubnetGroupResponse_deletionMessage = Lens.lens (\DeleteSubnetGroupResponse' {deletionMessage} -> deletionMessage) (\s@DeleteSubnetGroupResponse' {} a -> s {deletionMessage = a} :: DeleteSubnetGroupResponse)

-- | The response's http status code.
deleteSubnetGroupResponse_httpStatus :: Lens.Lens' DeleteSubnetGroupResponse Core.Int
deleteSubnetGroupResponse_httpStatus = Lens.lens (\DeleteSubnetGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteSubnetGroupResponse' {} a -> s {httpStatus = a} :: DeleteSubnetGroupResponse)

instance Core.NFData DeleteSubnetGroupResponse

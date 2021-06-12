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
-- Module      : Network.AWS.DAX.DeleteParameterGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified parameter group. You cannot delete a parameter
-- group if it is associated with any DAX clusters.
module Network.AWS.DAX.DeleteParameterGroup
  ( -- * Creating a Request
    DeleteParameterGroup (..),
    newDeleteParameterGroup,

    -- * Request Lenses
    deleteParameterGroup_parameterGroupName,

    -- * Destructuring the Response
    DeleteParameterGroupResponse (..),
    newDeleteParameterGroupResponse,

    -- * Response Lenses
    deleteParameterGroupResponse_deletionMessage,
    deleteParameterGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DAX.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteParameterGroup' smart constructor.
data DeleteParameterGroup = DeleteParameterGroup'
  { -- | The name of the parameter group to delete.
    parameterGroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteParameterGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterGroupName', 'deleteParameterGroup_parameterGroupName' - The name of the parameter group to delete.
newDeleteParameterGroup ::
  -- | 'parameterGroupName'
  Core.Text ->
  DeleteParameterGroup
newDeleteParameterGroup pParameterGroupName_ =
  DeleteParameterGroup'
    { parameterGroupName =
        pParameterGroupName_
    }

-- | The name of the parameter group to delete.
deleteParameterGroup_parameterGroupName :: Lens.Lens' DeleteParameterGroup Core.Text
deleteParameterGroup_parameterGroupName = Lens.lens (\DeleteParameterGroup' {parameterGroupName} -> parameterGroupName) (\s@DeleteParameterGroup' {} a -> s {parameterGroupName = a} :: DeleteParameterGroup)

instance Core.AWSRequest DeleteParameterGroup where
  type
    AWSResponse DeleteParameterGroup =
      DeleteParameterGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteParameterGroupResponse'
            Core.<$> (x Core..?> "DeletionMessage")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteParameterGroup

instance Core.NFData DeleteParameterGroup

instance Core.ToHeaders DeleteParameterGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDAXV3.DeleteParameterGroup" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteParameterGroup where
  toJSON DeleteParameterGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ParameterGroupName" Core..= parameterGroupName)
          ]
      )

instance Core.ToPath DeleteParameterGroup where
  toPath = Core.const "/"

instance Core.ToQuery DeleteParameterGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteParameterGroupResponse' smart constructor.
data DeleteParameterGroupResponse = DeleteParameterGroupResponse'
  { -- | A user-specified message for this action (i.e., a reason for deleting
    -- the parameter group).
    deletionMessage :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteParameterGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deletionMessage', 'deleteParameterGroupResponse_deletionMessage' - A user-specified message for this action (i.e., a reason for deleting
-- the parameter group).
--
-- 'httpStatus', 'deleteParameterGroupResponse_httpStatus' - The response's http status code.
newDeleteParameterGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteParameterGroupResponse
newDeleteParameterGroupResponse pHttpStatus_ =
  DeleteParameterGroupResponse'
    { deletionMessage =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A user-specified message for this action (i.e., a reason for deleting
-- the parameter group).
deleteParameterGroupResponse_deletionMessage :: Lens.Lens' DeleteParameterGroupResponse (Core.Maybe Core.Text)
deleteParameterGroupResponse_deletionMessage = Lens.lens (\DeleteParameterGroupResponse' {deletionMessage} -> deletionMessage) (\s@DeleteParameterGroupResponse' {} a -> s {deletionMessage = a} :: DeleteParameterGroupResponse)

-- | The response's http status code.
deleteParameterGroupResponse_httpStatus :: Lens.Lens' DeleteParameterGroupResponse Core.Int
deleteParameterGroupResponse_httpStatus = Lens.lens (\DeleteParameterGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteParameterGroupResponse' {} a -> s {httpStatus = a} :: DeleteParameterGroupResponse)

instance Core.NFData DeleteParameterGroupResponse

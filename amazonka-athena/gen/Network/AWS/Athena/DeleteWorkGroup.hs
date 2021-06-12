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
-- Module      : Network.AWS.Athena.DeleteWorkGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the workgroup with the specified name. The primary workgroup
-- cannot be deleted.
module Network.AWS.Athena.DeleteWorkGroup
  ( -- * Creating a Request
    DeleteWorkGroup (..),
    newDeleteWorkGroup,

    -- * Request Lenses
    deleteWorkGroup_recursiveDeleteOption,
    deleteWorkGroup_workGroup,

    -- * Destructuring the Response
    DeleteWorkGroupResponse (..),
    newDeleteWorkGroupResponse,

    -- * Response Lenses
    deleteWorkGroupResponse_httpStatus,
  )
where

import Network.AWS.Athena.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteWorkGroup' smart constructor.
data DeleteWorkGroup = DeleteWorkGroup'
  { -- | The option to delete the workgroup and its contents even if the
    -- workgroup contains any named queries or query executions.
    recursiveDeleteOption :: Core.Maybe Core.Bool,
    -- | The unique name of the workgroup to delete.
    workGroup :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteWorkGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recursiveDeleteOption', 'deleteWorkGroup_recursiveDeleteOption' - The option to delete the workgroup and its contents even if the
-- workgroup contains any named queries or query executions.
--
-- 'workGroup', 'deleteWorkGroup_workGroup' - The unique name of the workgroup to delete.
newDeleteWorkGroup ::
  -- | 'workGroup'
  Core.Text ->
  DeleteWorkGroup
newDeleteWorkGroup pWorkGroup_ =
  DeleteWorkGroup'
    { recursiveDeleteOption =
        Core.Nothing,
      workGroup = pWorkGroup_
    }

-- | The option to delete the workgroup and its contents even if the
-- workgroup contains any named queries or query executions.
deleteWorkGroup_recursiveDeleteOption :: Lens.Lens' DeleteWorkGroup (Core.Maybe Core.Bool)
deleteWorkGroup_recursiveDeleteOption = Lens.lens (\DeleteWorkGroup' {recursiveDeleteOption} -> recursiveDeleteOption) (\s@DeleteWorkGroup' {} a -> s {recursiveDeleteOption = a} :: DeleteWorkGroup)

-- | The unique name of the workgroup to delete.
deleteWorkGroup_workGroup :: Lens.Lens' DeleteWorkGroup Core.Text
deleteWorkGroup_workGroup = Lens.lens (\DeleteWorkGroup' {workGroup} -> workGroup) (\s@DeleteWorkGroup' {} a -> s {workGroup = a} :: DeleteWorkGroup)

instance Core.AWSRequest DeleteWorkGroup where
  type
    AWSResponse DeleteWorkGroup =
      DeleteWorkGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteWorkGroupResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteWorkGroup

instance Core.NFData DeleteWorkGroup

instance Core.ToHeaders DeleteWorkGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonAthena.DeleteWorkGroup" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteWorkGroup where
  toJSON DeleteWorkGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RecursiveDeleteOption" Core..=)
              Core.<$> recursiveDeleteOption,
            Core.Just ("WorkGroup" Core..= workGroup)
          ]
      )

instance Core.ToPath DeleteWorkGroup where
  toPath = Core.const "/"

instance Core.ToQuery DeleteWorkGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteWorkGroupResponse' smart constructor.
data DeleteWorkGroupResponse = DeleteWorkGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteWorkGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteWorkGroupResponse_httpStatus' - The response's http status code.
newDeleteWorkGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteWorkGroupResponse
newDeleteWorkGroupResponse pHttpStatus_ =
  DeleteWorkGroupResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteWorkGroupResponse_httpStatus :: Lens.Lens' DeleteWorkGroupResponse Core.Int
deleteWorkGroupResponse_httpStatus = Lens.lens (\DeleteWorkGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteWorkGroupResponse' {} a -> s {httpStatus = a} :: DeleteWorkGroupResponse)

instance Core.NFData DeleteWorkGroupResponse

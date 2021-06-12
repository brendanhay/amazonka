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
-- Module      : Network.AWS.SageMaker.DeleteAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an action.
module Network.AWS.SageMaker.DeleteAction
  ( -- * Creating a Request
    DeleteAction (..),
    newDeleteAction,

    -- * Request Lenses
    deleteAction_actionName,

    -- * Destructuring the Response
    DeleteActionResponse (..),
    newDeleteActionResponse,

    -- * Response Lenses
    deleteActionResponse_actionArn,
    deleteActionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDeleteAction' smart constructor.
data DeleteAction = DeleteAction'
  { -- | The name of the action to delete.
    actionName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionName', 'deleteAction_actionName' - The name of the action to delete.
newDeleteAction ::
  -- | 'actionName'
  Core.Text ->
  DeleteAction
newDeleteAction pActionName_ =
  DeleteAction' {actionName = pActionName_}

-- | The name of the action to delete.
deleteAction_actionName :: Lens.Lens' DeleteAction Core.Text
deleteAction_actionName = Lens.lens (\DeleteAction' {actionName} -> actionName) (\s@DeleteAction' {} a -> s {actionName = a} :: DeleteAction)

instance Core.AWSRequest DeleteAction where
  type AWSResponse DeleteAction = DeleteActionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteActionResponse'
            Core.<$> (x Core..?> "ActionArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteAction

instance Core.NFData DeleteAction

instance Core.ToHeaders DeleteAction where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.DeleteAction" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteAction where
  toJSON DeleteAction' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ActionName" Core..= actionName)]
      )

instance Core.ToPath DeleteAction where
  toPath = Core.const "/"

instance Core.ToQuery DeleteAction where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteActionResponse' smart constructor.
data DeleteActionResponse = DeleteActionResponse'
  { -- | The Amazon Resource Name (ARN) of the action.
    actionArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionArn', 'deleteActionResponse_actionArn' - The Amazon Resource Name (ARN) of the action.
--
-- 'httpStatus', 'deleteActionResponse_httpStatus' - The response's http status code.
newDeleteActionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteActionResponse
newDeleteActionResponse pHttpStatus_ =
  DeleteActionResponse'
    { actionArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the action.
deleteActionResponse_actionArn :: Lens.Lens' DeleteActionResponse (Core.Maybe Core.Text)
deleteActionResponse_actionArn = Lens.lens (\DeleteActionResponse' {actionArn} -> actionArn) (\s@DeleteActionResponse' {} a -> s {actionArn = a} :: DeleteActionResponse)

-- | The response's http status code.
deleteActionResponse_httpStatus :: Lens.Lens' DeleteActionResponse Core.Int
deleteActionResponse_httpStatus = Lens.lens (\DeleteActionResponse' {httpStatus} -> httpStatus) (\s@DeleteActionResponse' {} a -> s {httpStatus = a} :: DeleteActionResponse)

instance Core.NFData DeleteActionResponse

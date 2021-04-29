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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDeleteAction' smart constructor.
data DeleteAction = DeleteAction'
  { -- | The name of the action to delete.
    actionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteAction
newDeleteAction pActionName_ =
  DeleteAction' {actionName = pActionName_}

-- | The name of the action to delete.
deleteAction_actionName :: Lens.Lens' DeleteAction Prelude.Text
deleteAction_actionName = Lens.lens (\DeleteAction' {actionName} -> actionName) (\s@DeleteAction' {} a -> s {actionName = a} :: DeleteAction)

instance Prelude.AWSRequest DeleteAction where
  type Rs DeleteAction = DeleteActionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteActionResponse'
            Prelude.<$> (x Prelude..?> "ActionArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAction

instance Prelude.NFData DeleteAction

instance Prelude.ToHeaders DeleteAction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("SageMaker.DeleteAction" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteAction where
  toJSON DeleteAction' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("ActionName" Prelude..= actionName)]
      )

instance Prelude.ToPath DeleteAction where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteAction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteActionResponse' smart constructor.
data DeleteActionResponse = DeleteActionResponse'
  { -- | The Amazon Resource Name (ARN) of the action.
    actionArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteActionResponse
newDeleteActionResponse pHttpStatus_ =
  DeleteActionResponse'
    { actionArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the action.
deleteActionResponse_actionArn :: Lens.Lens' DeleteActionResponse (Prelude.Maybe Prelude.Text)
deleteActionResponse_actionArn = Lens.lens (\DeleteActionResponse' {actionArn} -> actionArn) (\s@DeleteActionResponse' {} a -> s {actionArn = a} :: DeleteActionResponse)

-- | The response's http status code.
deleteActionResponse_httpStatus :: Lens.Lens' DeleteActionResponse Prelude.Int
deleteActionResponse_httpStatus = Lens.lens (\DeleteActionResponse' {httpStatus} -> httpStatus) (\s@DeleteActionResponse' {} a -> s {httpStatus = a} :: DeleteActionResponse)

instance Prelude.NFData DeleteActionResponse

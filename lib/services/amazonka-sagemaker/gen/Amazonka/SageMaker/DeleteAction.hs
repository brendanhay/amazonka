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
-- Module      : Amazonka.SageMaker.DeleteAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an action.
module Amazonka.SageMaker.DeleteAction
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDeleteAction' smart constructor.
data DeleteAction = DeleteAction'
  { -- | The name of the action to delete.
    actionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeleteAction where
  type AWSResponse DeleteAction = DeleteActionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteActionResponse'
            Prelude.<$> (x Data..?> "ActionArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAction where
  hashWithSalt _salt DeleteAction' {..} =
    _salt `Prelude.hashWithSalt` actionName

instance Prelude.NFData DeleteAction where
  rnf DeleteAction' {..} = Prelude.rnf actionName

instance Data.ToHeaders DeleteAction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.DeleteAction" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteAction where
  toJSON DeleteAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ActionName" Data..= actionName)]
      )

instance Data.ToPath DeleteAction where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteAction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteActionResponse' smart constructor.
data DeleteActionResponse = DeleteActionResponse'
  { -- | The Amazon Resource Name (ARN) of the action.
    actionArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData DeleteActionResponse where
  rnf DeleteActionResponse' {..} =
    Prelude.rnf actionArn
      `Prelude.seq` Prelude.rnf httpStatus

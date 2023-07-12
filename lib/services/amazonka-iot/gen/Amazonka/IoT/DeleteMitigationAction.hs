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
-- Module      : Amazonka.IoT.DeleteMitigationAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a defined mitigation action from your Amazon Web Services
-- accounts.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DeleteMitigationAction>
-- action.
module Amazonka.IoT.DeleteMitigationAction
  ( -- * Creating a Request
    DeleteMitigationAction (..),
    newDeleteMitigationAction,

    -- * Request Lenses
    deleteMitigationAction_actionName,

    -- * Destructuring the Response
    DeleteMitigationActionResponse (..),
    newDeleteMitigationActionResponse,

    -- * Response Lenses
    deleteMitigationActionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteMitigationAction' smart constructor.
data DeleteMitigationAction = DeleteMitigationAction'
  { -- | The name of the mitigation action that you want to delete.
    actionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMitigationAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionName', 'deleteMitigationAction_actionName' - The name of the mitigation action that you want to delete.
newDeleteMitigationAction ::
  -- | 'actionName'
  Prelude.Text ->
  DeleteMitigationAction
newDeleteMitigationAction pActionName_ =
  DeleteMitigationAction' {actionName = pActionName_}

-- | The name of the mitigation action that you want to delete.
deleteMitigationAction_actionName :: Lens.Lens' DeleteMitigationAction Prelude.Text
deleteMitigationAction_actionName = Lens.lens (\DeleteMitigationAction' {actionName} -> actionName) (\s@DeleteMitigationAction' {} a -> s {actionName = a} :: DeleteMitigationAction)

instance Core.AWSRequest DeleteMitigationAction where
  type
    AWSResponse DeleteMitigationAction =
      DeleteMitigationActionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteMitigationActionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteMitigationAction where
  hashWithSalt _salt DeleteMitigationAction' {..} =
    _salt `Prelude.hashWithSalt` actionName

instance Prelude.NFData DeleteMitigationAction where
  rnf DeleteMitigationAction' {..} =
    Prelude.rnf actionName

instance Data.ToHeaders DeleteMitigationAction where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteMitigationAction where
  toPath DeleteMitigationAction' {..} =
    Prelude.mconcat
      ["/mitigationactions/actions/", Data.toBS actionName]

instance Data.ToQuery DeleteMitigationAction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteMitigationActionResponse' smart constructor.
data DeleteMitigationActionResponse = DeleteMitigationActionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMitigationActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteMitigationActionResponse_httpStatus' - The response's http status code.
newDeleteMitigationActionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteMitigationActionResponse
newDeleteMitigationActionResponse pHttpStatus_ =
  DeleteMitigationActionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteMitigationActionResponse_httpStatus :: Lens.Lens' DeleteMitigationActionResponse Prelude.Int
deleteMitigationActionResponse_httpStatus = Lens.lens (\DeleteMitigationActionResponse' {httpStatus} -> httpStatus) (\s@DeleteMitigationActionResponse' {} a -> s {httpStatus = a} :: DeleteMitigationActionResponse)

instance
  Prelude.NFData
    DeleteMitigationActionResponse
  where
  rnf DeleteMitigationActionResponse' {..} =
    Prelude.rnf httpStatus

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
-- Module      : Amazonka.SecurityHub.DeleteActionTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a custom action target from Security Hub.
--
-- Deleting a custom action target does not affect any findings or insights
-- that were already sent to Amazon CloudWatch Events using the custom
-- action.
module Amazonka.SecurityHub.DeleteActionTarget
  ( -- * Creating a Request
    DeleteActionTarget (..),
    newDeleteActionTarget,

    -- * Request Lenses
    deleteActionTarget_actionTargetArn,

    -- * Destructuring the Response
    DeleteActionTargetResponse (..),
    newDeleteActionTargetResponse,

    -- * Response Lenses
    deleteActionTargetResponse_httpStatus,
    deleteActionTargetResponse_actionTargetArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newDeleteActionTarget' smart constructor.
data DeleteActionTarget = DeleteActionTarget'
  { -- | The Amazon Resource Name (ARN) of the custom action target to delete.
    actionTargetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteActionTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionTargetArn', 'deleteActionTarget_actionTargetArn' - The Amazon Resource Name (ARN) of the custom action target to delete.
newDeleteActionTarget ::
  -- | 'actionTargetArn'
  Prelude.Text ->
  DeleteActionTarget
newDeleteActionTarget pActionTargetArn_ =
  DeleteActionTarget'
    { actionTargetArn =
        pActionTargetArn_
    }

-- | The Amazon Resource Name (ARN) of the custom action target to delete.
deleteActionTarget_actionTargetArn :: Lens.Lens' DeleteActionTarget Prelude.Text
deleteActionTarget_actionTargetArn = Lens.lens (\DeleteActionTarget' {actionTargetArn} -> actionTargetArn) (\s@DeleteActionTarget' {} a -> s {actionTargetArn = a} :: DeleteActionTarget)

instance Core.AWSRequest DeleteActionTarget where
  type
    AWSResponse DeleteActionTarget =
      DeleteActionTargetResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteActionTargetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ActionTargetArn")
      )

instance Prelude.Hashable DeleteActionTarget where
  hashWithSalt _salt DeleteActionTarget' {..} =
    _salt `Prelude.hashWithSalt` actionTargetArn

instance Prelude.NFData DeleteActionTarget where
  rnf DeleteActionTarget' {..} =
    Prelude.rnf actionTargetArn

instance Data.ToHeaders DeleteActionTarget where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteActionTarget where
  toPath DeleteActionTarget' {..} =
    Prelude.mconcat
      ["/actionTargets/", Data.toBS actionTargetArn]

instance Data.ToQuery DeleteActionTarget where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteActionTargetResponse' smart constructor.
data DeleteActionTargetResponse = DeleteActionTargetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN of the custom action target that was deleted.
    actionTargetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteActionTargetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteActionTargetResponse_httpStatus' - The response's http status code.
--
-- 'actionTargetArn', 'deleteActionTargetResponse_actionTargetArn' - The ARN of the custom action target that was deleted.
newDeleteActionTargetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'actionTargetArn'
  Prelude.Text ->
  DeleteActionTargetResponse
newDeleteActionTargetResponse
  pHttpStatus_
  pActionTargetArn_ =
    DeleteActionTargetResponse'
      { httpStatus =
          pHttpStatus_,
        actionTargetArn = pActionTargetArn_
      }

-- | The response's http status code.
deleteActionTargetResponse_httpStatus :: Lens.Lens' DeleteActionTargetResponse Prelude.Int
deleteActionTargetResponse_httpStatus = Lens.lens (\DeleteActionTargetResponse' {httpStatus} -> httpStatus) (\s@DeleteActionTargetResponse' {} a -> s {httpStatus = a} :: DeleteActionTargetResponse)

-- | The ARN of the custom action target that was deleted.
deleteActionTargetResponse_actionTargetArn :: Lens.Lens' DeleteActionTargetResponse Prelude.Text
deleteActionTargetResponse_actionTargetArn = Lens.lens (\DeleteActionTargetResponse' {actionTargetArn} -> actionTargetArn) (\s@DeleteActionTargetResponse' {} a -> s {actionTargetArn = a} :: DeleteActionTargetResponse)

instance Prelude.NFData DeleteActionTargetResponse where
  rnf DeleteActionTargetResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf actionTargetArn

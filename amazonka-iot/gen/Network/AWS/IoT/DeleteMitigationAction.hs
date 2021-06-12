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
-- Module      : Network.AWS.IoT.DeleteMitigationAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a defined mitigation action from your AWS account.
module Network.AWS.IoT.DeleteMitigationAction
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

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteMitigationAction' smart constructor.
data DeleteMitigationAction = DeleteMitigationAction'
  { -- | The name of the mitigation action that you want to delete.
    actionName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DeleteMitigationAction
newDeleteMitigationAction pActionName_ =
  DeleteMitigationAction' {actionName = pActionName_}

-- | The name of the mitigation action that you want to delete.
deleteMitigationAction_actionName :: Lens.Lens' DeleteMitigationAction Core.Text
deleteMitigationAction_actionName = Lens.lens (\DeleteMitigationAction' {actionName} -> actionName) (\s@DeleteMitigationAction' {} a -> s {actionName = a} :: DeleteMitigationAction)

instance Core.AWSRequest DeleteMitigationAction where
  type
    AWSResponse DeleteMitigationAction =
      DeleteMitigationActionResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteMitigationActionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteMitigationAction

instance Core.NFData DeleteMitigationAction

instance Core.ToHeaders DeleteMitigationAction where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteMitigationAction where
  toPath DeleteMitigationAction' {..} =
    Core.mconcat
      ["/mitigationactions/actions/", Core.toBS actionName]

instance Core.ToQuery DeleteMitigationAction where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteMitigationActionResponse' smart constructor.
data DeleteMitigationActionResponse = DeleteMitigationActionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeleteMitigationActionResponse
newDeleteMitigationActionResponse pHttpStatus_ =
  DeleteMitigationActionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteMitigationActionResponse_httpStatus :: Lens.Lens' DeleteMitigationActionResponse Core.Int
deleteMitigationActionResponse_httpStatus = Lens.lens (\DeleteMitigationActionResponse' {httpStatus} -> httpStatus) (\s@DeleteMitigationActionResponse' {} a -> s {httpStatus = a} :: DeleteMitigationActionResponse)

instance Core.NFData DeleteMitigationActionResponse

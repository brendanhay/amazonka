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
-- Module      : Network.AWS.IoT.UpdateMitigationAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the definition for the specified mitigation action.
module Network.AWS.IoT.UpdateMitigationAction
  ( -- * Creating a Request
    UpdateMitigationAction (..),
    newUpdateMitigationAction,

    -- * Request Lenses
    updateMitigationAction_roleArn,
    updateMitigationAction_actionParams,
    updateMitigationAction_actionName,

    -- * Destructuring the Response
    UpdateMitigationActionResponse (..),
    newUpdateMitigationActionResponse,

    -- * Response Lenses
    updateMitigationActionResponse_actionArn,
    updateMitigationActionResponse_actionId,
    updateMitigationActionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateMitigationAction' smart constructor.
data UpdateMitigationAction = UpdateMitigationAction'
  { -- | The ARN of the IAM role that is used to apply the mitigation action.
    roleArn :: Core.Maybe Core.Text,
    -- | Defines the type of action and the parameters for that action.
    actionParams :: Core.Maybe MitigationActionParams,
    -- | The friendly name for the mitigation action. You cannot change the name
    -- by using @UpdateMitigationAction@. Instead, you must delete and recreate
    -- the mitigation action with the new name.
    actionName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateMitigationAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'updateMitigationAction_roleArn' - The ARN of the IAM role that is used to apply the mitigation action.
--
-- 'actionParams', 'updateMitigationAction_actionParams' - Defines the type of action and the parameters for that action.
--
-- 'actionName', 'updateMitigationAction_actionName' - The friendly name for the mitigation action. You cannot change the name
-- by using @UpdateMitigationAction@. Instead, you must delete and recreate
-- the mitigation action with the new name.
newUpdateMitigationAction ::
  -- | 'actionName'
  Core.Text ->
  UpdateMitigationAction
newUpdateMitigationAction pActionName_ =
  UpdateMitigationAction'
    { roleArn = Core.Nothing,
      actionParams = Core.Nothing,
      actionName = pActionName_
    }

-- | The ARN of the IAM role that is used to apply the mitigation action.
updateMitigationAction_roleArn :: Lens.Lens' UpdateMitigationAction (Core.Maybe Core.Text)
updateMitigationAction_roleArn = Lens.lens (\UpdateMitigationAction' {roleArn} -> roleArn) (\s@UpdateMitigationAction' {} a -> s {roleArn = a} :: UpdateMitigationAction)

-- | Defines the type of action and the parameters for that action.
updateMitigationAction_actionParams :: Lens.Lens' UpdateMitigationAction (Core.Maybe MitigationActionParams)
updateMitigationAction_actionParams = Lens.lens (\UpdateMitigationAction' {actionParams} -> actionParams) (\s@UpdateMitigationAction' {} a -> s {actionParams = a} :: UpdateMitigationAction)

-- | The friendly name for the mitigation action. You cannot change the name
-- by using @UpdateMitigationAction@. Instead, you must delete and recreate
-- the mitigation action with the new name.
updateMitigationAction_actionName :: Lens.Lens' UpdateMitigationAction Core.Text
updateMitigationAction_actionName = Lens.lens (\UpdateMitigationAction' {actionName} -> actionName) (\s@UpdateMitigationAction' {} a -> s {actionName = a} :: UpdateMitigationAction)

instance Core.AWSRequest UpdateMitigationAction where
  type
    AWSResponse UpdateMitigationAction =
      UpdateMitigationActionResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMitigationActionResponse'
            Core.<$> (x Core..?> "actionArn")
            Core.<*> (x Core..?> "actionId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateMitigationAction

instance Core.NFData UpdateMitigationAction

instance Core.ToHeaders UpdateMitigationAction where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON UpdateMitigationAction where
  toJSON UpdateMitigationAction' {..} =
    Core.object
      ( Core.catMaybes
          [ ("roleArn" Core..=) Core.<$> roleArn,
            ("actionParams" Core..=) Core.<$> actionParams
          ]
      )

instance Core.ToPath UpdateMitigationAction where
  toPath UpdateMitigationAction' {..} =
    Core.mconcat
      ["/mitigationactions/actions/", Core.toBS actionName]

instance Core.ToQuery UpdateMitigationAction where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateMitigationActionResponse' smart constructor.
data UpdateMitigationActionResponse = UpdateMitigationActionResponse'
  { -- | The ARN for the new mitigation action.
    actionArn :: Core.Maybe Core.Text,
    -- | A unique identifier for the mitigation action.
    actionId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateMitigationActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionArn', 'updateMitigationActionResponse_actionArn' - The ARN for the new mitigation action.
--
-- 'actionId', 'updateMitigationActionResponse_actionId' - A unique identifier for the mitigation action.
--
-- 'httpStatus', 'updateMitigationActionResponse_httpStatus' - The response's http status code.
newUpdateMitigationActionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateMitigationActionResponse
newUpdateMitigationActionResponse pHttpStatus_ =
  UpdateMitigationActionResponse'
    { actionArn =
        Core.Nothing,
      actionId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN for the new mitigation action.
updateMitigationActionResponse_actionArn :: Lens.Lens' UpdateMitigationActionResponse (Core.Maybe Core.Text)
updateMitigationActionResponse_actionArn = Lens.lens (\UpdateMitigationActionResponse' {actionArn} -> actionArn) (\s@UpdateMitigationActionResponse' {} a -> s {actionArn = a} :: UpdateMitigationActionResponse)

-- | A unique identifier for the mitigation action.
updateMitigationActionResponse_actionId :: Lens.Lens' UpdateMitigationActionResponse (Core.Maybe Core.Text)
updateMitigationActionResponse_actionId = Lens.lens (\UpdateMitigationActionResponse' {actionId} -> actionId) (\s@UpdateMitigationActionResponse' {} a -> s {actionId = a} :: UpdateMitigationActionResponse)

-- | The response's http status code.
updateMitigationActionResponse_httpStatus :: Lens.Lens' UpdateMitigationActionResponse Core.Int
updateMitigationActionResponse_httpStatus = Lens.lens (\UpdateMitigationActionResponse' {httpStatus} -> httpStatus) (\s@UpdateMitigationActionResponse' {} a -> s {httpStatus = a} :: UpdateMitigationActionResponse)

instance Core.NFData UpdateMitigationActionResponse

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
-- Module      : Network.AWS.SageMaker.UpdateAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an action.
module Network.AWS.SageMaker.UpdateAction
  ( -- * Creating a Request
    UpdateAction (..),
    newUpdateAction,

    -- * Request Lenses
    updateAction_status,
    updateAction_propertiesToRemove,
    updateAction_properties,
    updateAction_description,
    updateAction_actionName,

    -- * Destructuring the Response
    UpdateActionResponse (..),
    newUpdateActionResponse,

    -- * Response Lenses
    updateActionResponse_actionArn,
    updateActionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newUpdateAction' smart constructor.
data UpdateAction = UpdateAction'
  { -- | The new status for the action.
    status :: Core.Maybe ActionStatus,
    -- | A list of properties to remove.
    propertiesToRemove :: Core.Maybe [Core.Text],
    -- | The new list of properties. Overwrites the current property list.
    properties :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The new description for the action.
    description :: Core.Maybe Core.Text,
    -- | The name of the action to update.
    actionName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'updateAction_status' - The new status for the action.
--
-- 'propertiesToRemove', 'updateAction_propertiesToRemove' - A list of properties to remove.
--
-- 'properties', 'updateAction_properties' - The new list of properties. Overwrites the current property list.
--
-- 'description', 'updateAction_description' - The new description for the action.
--
-- 'actionName', 'updateAction_actionName' - The name of the action to update.
newUpdateAction ::
  -- | 'actionName'
  Core.Text ->
  UpdateAction
newUpdateAction pActionName_ =
  UpdateAction'
    { status = Core.Nothing,
      propertiesToRemove = Core.Nothing,
      properties = Core.Nothing,
      description = Core.Nothing,
      actionName = pActionName_
    }

-- | The new status for the action.
updateAction_status :: Lens.Lens' UpdateAction (Core.Maybe ActionStatus)
updateAction_status = Lens.lens (\UpdateAction' {status} -> status) (\s@UpdateAction' {} a -> s {status = a} :: UpdateAction)

-- | A list of properties to remove.
updateAction_propertiesToRemove :: Lens.Lens' UpdateAction (Core.Maybe [Core.Text])
updateAction_propertiesToRemove = Lens.lens (\UpdateAction' {propertiesToRemove} -> propertiesToRemove) (\s@UpdateAction' {} a -> s {propertiesToRemove = a} :: UpdateAction) Core.. Lens.mapping Lens._Coerce

-- | The new list of properties. Overwrites the current property list.
updateAction_properties :: Lens.Lens' UpdateAction (Core.Maybe (Core.HashMap Core.Text Core.Text))
updateAction_properties = Lens.lens (\UpdateAction' {properties} -> properties) (\s@UpdateAction' {} a -> s {properties = a} :: UpdateAction) Core.. Lens.mapping Lens._Coerce

-- | The new description for the action.
updateAction_description :: Lens.Lens' UpdateAction (Core.Maybe Core.Text)
updateAction_description = Lens.lens (\UpdateAction' {description} -> description) (\s@UpdateAction' {} a -> s {description = a} :: UpdateAction)

-- | The name of the action to update.
updateAction_actionName :: Lens.Lens' UpdateAction Core.Text
updateAction_actionName = Lens.lens (\UpdateAction' {actionName} -> actionName) (\s@UpdateAction' {} a -> s {actionName = a} :: UpdateAction)

instance Core.AWSRequest UpdateAction where
  type AWSResponse UpdateAction = UpdateActionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateActionResponse'
            Core.<$> (x Core..?> "ActionArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateAction

instance Core.NFData UpdateAction

instance Core.ToHeaders UpdateAction where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.UpdateAction" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateAction where
  toJSON UpdateAction' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Status" Core..=) Core.<$> status,
            ("PropertiesToRemove" Core..=)
              Core.<$> propertiesToRemove,
            ("Properties" Core..=) Core.<$> properties,
            ("Description" Core..=) Core.<$> description,
            Core.Just ("ActionName" Core..= actionName)
          ]
      )

instance Core.ToPath UpdateAction where
  toPath = Core.const "/"

instance Core.ToQuery UpdateAction where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateActionResponse' smart constructor.
data UpdateActionResponse = UpdateActionResponse'
  { -- | The Amazon Resource Name (ARN) of the action.
    actionArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionArn', 'updateActionResponse_actionArn' - The Amazon Resource Name (ARN) of the action.
--
-- 'httpStatus', 'updateActionResponse_httpStatus' - The response's http status code.
newUpdateActionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateActionResponse
newUpdateActionResponse pHttpStatus_ =
  UpdateActionResponse'
    { actionArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the action.
updateActionResponse_actionArn :: Lens.Lens' UpdateActionResponse (Core.Maybe Core.Text)
updateActionResponse_actionArn = Lens.lens (\UpdateActionResponse' {actionArn} -> actionArn) (\s@UpdateActionResponse' {} a -> s {actionArn = a} :: UpdateActionResponse)

-- | The response's http status code.
updateActionResponse_httpStatus :: Lens.Lens' UpdateActionResponse Core.Int
updateActionResponse_httpStatus = Lens.lens (\UpdateActionResponse' {httpStatus} -> httpStatus) (\s@UpdateActionResponse' {} a -> s {httpStatus = a} :: UpdateActionResponse)

instance Core.NFData UpdateActionResponse

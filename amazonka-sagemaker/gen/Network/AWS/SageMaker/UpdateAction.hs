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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newUpdateAction' smart constructor.
data UpdateAction = UpdateAction'
  { -- | The new status for the action.
    status :: Prelude.Maybe ActionStatus,
    -- | A list of properties to remove.
    propertiesToRemove :: Prelude.Maybe [Prelude.Text],
    -- | The new list of properties. Overwrites the current property list.
    properties :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The new description for the action.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the action to update.
    actionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  UpdateAction
newUpdateAction pActionName_ =
  UpdateAction'
    { status = Prelude.Nothing,
      propertiesToRemove = Prelude.Nothing,
      properties = Prelude.Nothing,
      description = Prelude.Nothing,
      actionName = pActionName_
    }

-- | The new status for the action.
updateAction_status :: Lens.Lens' UpdateAction (Prelude.Maybe ActionStatus)
updateAction_status = Lens.lens (\UpdateAction' {status} -> status) (\s@UpdateAction' {} a -> s {status = a} :: UpdateAction)

-- | A list of properties to remove.
updateAction_propertiesToRemove :: Lens.Lens' UpdateAction (Prelude.Maybe [Prelude.Text])
updateAction_propertiesToRemove = Lens.lens (\UpdateAction' {propertiesToRemove} -> propertiesToRemove) (\s@UpdateAction' {} a -> s {propertiesToRemove = a} :: UpdateAction) Prelude.. Lens.mapping Prelude._Coerce

-- | The new list of properties. Overwrites the current property list.
updateAction_properties :: Lens.Lens' UpdateAction (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateAction_properties = Lens.lens (\UpdateAction' {properties} -> properties) (\s@UpdateAction' {} a -> s {properties = a} :: UpdateAction) Prelude.. Lens.mapping Prelude._Coerce

-- | The new description for the action.
updateAction_description :: Lens.Lens' UpdateAction (Prelude.Maybe Prelude.Text)
updateAction_description = Lens.lens (\UpdateAction' {description} -> description) (\s@UpdateAction' {} a -> s {description = a} :: UpdateAction)

-- | The name of the action to update.
updateAction_actionName :: Lens.Lens' UpdateAction Prelude.Text
updateAction_actionName = Lens.lens (\UpdateAction' {actionName} -> actionName) (\s@UpdateAction' {} a -> s {actionName = a} :: UpdateAction)

instance Prelude.AWSRequest UpdateAction where
  type Rs UpdateAction = UpdateActionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateActionResponse'
            Prelude.<$> (x Prelude..?> "ActionArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateAction

instance Prelude.NFData UpdateAction

instance Prelude.ToHeaders UpdateAction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("SageMaker.UpdateAction" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateAction where
  toJSON UpdateAction' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Status" Prelude..=) Prelude.<$> status,
            ("PropertiesToRemove" Prelude..=)
              Prelude.<$> propertiesToRemove,
            ("Properties" Prelude..=) Prelude.<$> properties,
            ("Description" Prelude..=) Prelude.<$> description,
            Prelude.Just ("ActionName" Prelude..= actionName)
          ]
      )

instance Prelude.ToPath UpdateAction where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateAction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateActionResponse' smart constructor.
data UpdateActionResponse = UpdateActionResponse'
  { -- | The Amazon Resource Name (ARN) of the action.
    actionArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  UpdateActionResponse
newUpdateActionResponse pHttpStatus_ =
  UpdateActionResponse'
    { actionArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the action.
updateActionResponse_actionArn :: Lens.Lens' UpdateActionResponse (Prelude.Maybe Prelude.Text)
updateActionResponse_actionArn = Lens.lens (\UpdateActionResponse' {actionArn} -> actionArn) (\s@UpdateActionResponse' {} a -> s {actionArn = a} :: UpdateActionResponse)

-- | The response's http status code.
updateActionResponse_httpStatus :: Lens.Lens' UpdateActionResponse Prelude.Int
updateActionResponse_httpStatus = Lens.lens (\UpdateActionResponse' {httpStatus} -> httpStatus) (\s@UpdateActionResponse' {} a -> s {httpStatus = a} :: UpdateActionResponse)

instance Prelude.NFData UpdateActionResponse

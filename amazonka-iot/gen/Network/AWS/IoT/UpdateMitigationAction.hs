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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateMitigationAction' smart constructor.
data UpdateMitigationAction = UpdateMitigationAction'
  { -- | The ARN of the IAM role that is used to apply the mitigation action.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Defines the type of action and the parameters for that action.
    actionParams :: Prelude.Maybe MitigationActionParams,
    -- | The friendly name for the mitigation action. You cannot change the name
    -- by using @UpdateMitigationAction@. Instead, you must delete and recreate
    -- the mitigation action with the new name.
    actionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  UpdateMitigationAction
newUpdateMitigationAction pActionName_ =
  UpdateMitigationAction'
    { roleArn = Prelude.Nothing,
      actionParams = Prelude.Nothing,
      actionName = pActionName_
    }

-- | The ARN of the IAM role that is used to apply the mitigation action.
updateMitigationAction_roleArn :: Lens.Lens' UpdateMitigationAction (Prelude.Maybe Prelude.Text)
updateMitigationAction_roleArn = Lens.lens (\UpdateMitigationAction' {roleArn} -> roleArn) (\s@UpdateMitigationAction' {} a -> s {roleArn = a} :: UpdateMitigationAction)

-- | Defines the type of action and the parameters for that action.
updateMitigationAction_actionParams :: Lens.Lens' UpdateMitigationAction (Prelude.Maybe MitigationActionParams)
updateMitigationAction_actionParams = Lens.lens (\UpdateMitigationAction' {actionParams} -> actionParams) (\s@UpdateMitigationAction' {} a -> s {actionParams = a} :: UpdateMitigationAction)

-- | The friendly name for the mitigation action. You cannot change the name
-- by using @UpdateMitigationAction@. Instead, you must delete and recreate
-- the mitigation action with the new name.
updateMitigationAction_actionName :: Lens.Lens' UpdateMitigationAction Prelude.Text
updateMitigationAction_actionName = Lens.lens (\UpdateMitigationAction' {actionName} -> actionName) (\s@UpdateMitigationAction' {} a -> s {actionName = a} :: UpdateMitigationAction)

instance Prelude.AWSRequest UpdateMitigationAction where
  type
    Rs UpdateMitigationAction =
      UpdateMitigationActionResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMitigationActionResponse'
            Prelude.<$> (x Prelude..?> "actionArn")
            Prelude.<*> (x Prelude..?> "actionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateMitigationAction

instance Prelude.NFData UpdateMitigationAction

instance Prelude.ToHeaders UpdateMitigationAction where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON UpdateMitigationAction where
  toJSON UpdateMitigationAction' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("roleArn" Prelude..=) Prelude.<$> roleArn,
            ("actionParams" Prelude..=)
              Prelude.<$> actionParams
          ]
      )

instance Prelude.ToPath UpdateMitigationAction where
  toPath UpdateMitigationAction' {..} =
    Prelude.mconcat
      [ "/mitigationactions/actions/",
        Prelude.toBS actionName
      ]

instance Prelude.ToQuery UpdateMitigationAction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateMitigationActionResponse' smart constructor.
data UpdateMitigationActionResponse = UpdateMitigationActionResponse'
  { -- | The ARN for the new mitigation action.
    actionArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the mitigation action.
    actionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  UpdateMitigationActionResponse
newUpdateMitigationActionResponse pHttpStatus_ =
  UpdateMitigationActionResponse'
    { actionArn =
        Prelude.Nothing,
      actionId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN for the new mitigation action.
updateMitigationActionResponse_actionArn :: Lens.Lens' UpdateMitigationActionResponse (Prelude.Maybe Prelude.Text)
updateMitigationActionResponse_actionArn = Lens.lens (\UpdateMitigationActionResponse' {actionArn} -> actionArn) (\s@UpdateMitigationActionResponse' {} a -> s {actionArn = a} :: UpdateMitigationActionResponse)

-- | A unique identifier for the mitigation action.
updateMitigationActionResponse_actionId :: Lens.Lens' UpdateMitigationActionResponse (Prelude.Maybe Prelude.Text)
updateMitigationActionResponse_actionId = Lens.lens (\UpdateMitigationActionResponse' {actionId} -> actionId) (\s@UpdateMitigationActionResponse' {} a -> s {actionId = a} :: UpdateMitigationActionResponse)

-- | The response's http status code.
updateMitigationActionResponse_httpStatus :: Lens.Lens' UpdateMitigationActionResponse Prelude.Int
updateMitigationActionResponse_httpStatus = Lens.lens (\UpdateMitigationActionResponse' {httpStatus} -> httpStatus) (\s@UpdateMitigationActionResponse' {} a -> s {httpStatus = a} :: UpdateMitigationActionResponse)

instance
  Prelude.NFData
    UpdateMitigationActionResponse

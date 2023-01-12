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
-- Module      : Amazonka.IoT.UpdateMitigationAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the definition for the specified mitigation action.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions UpdateMitigationAction>
-- action.
module Amazonka.IoT.UpdateMitigationAction
  ( -- * Creating a Request
    UpdateMitigationAction (..),
    newUpdateMitigationAction,

    -- * Request Lenses
    updateMitigationAction_actionParams,
    updateMitigationAction_roleArn,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateMitigationAction' smart constructor.
data UpdateMitigationAction = UpdateMitigationAction'
  { -- | Defines the type of action and the parameters for that action.
    actionParams :: Prelude.Maybe MitigationActionParams,
    -- | The ARN of the IAM role that is used to apply the mitigation action.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The friendly name for the mitigation action. You cannot change the name
    -- by using @UpdateMitigationAction@. Instead, you must delete and recreate
    -- the mitigation action with the new name.
    actionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMitigationAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionParams', 'updateMitigationAction_actionParams' - Defines the type of action and the parameters for that action.
--
-- 'roleArn', 'updateMitigationAction_roleArn' - The ARN of the IAM role that is used to apply the mitigation action.
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
    { actionParams =
        Prelude.Nothing,
      roleArn = Prelude.Nothing,
      actionName = pActionName_
    }

-- | Defines the type of action and the parameters for that action.
updateMitigationAction_actionParams :: Lens.Lens' UpdateMitigationAction (Prelude.Maybe MitigationActionParams)
updateMitigationAction_actionParams = Lens.lens (\UpdateMitigationAction' {actionParams} -> actionParams) (\s@UpdateMitigationAction' {} a -> s {actionParams = a} :: UpdateMitigationAction)

-- | The ARN of the IAM role that is used to apply the mitigation action.
updateMitigationAction_roleArn :: Lens.Lens' UpdateMitigationAction (Prelude.Maybe Prelude.Text)
updateMitigationAction_roleArn = Lens.lens (\UpdateMitigationAction' {roleArn} -> roleArn) (\s@UpdateMitigationAction' {} a -> s {roleArn = a} :: UpdateMitigationAction)

-- | The friendly name for the mitigation action. You cannot change the name
-- by using @UpdateMitigationAction@. Instead, you must delete and recreate
-- the mitigation action with the new name.
updateMitigationAction_actionName :: Lens.Lens' UpdateMitigationAction Prelude.Text
updateMitigationAction_actionName = Lens.lens (\UpdateMitigationAction' {actionName} -> actionName) (\s@UpdateMitigationAction' {} a -> s {actionName = a} :: UpdateMitigationAction)

instance Core.AWSRequest UpdateMitigationAction where
  type
    AWSResponse UpdateMitigationAction =
      UpdateMitigationActionResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMitigationActionResponse'
            Prelude.<$> (x Data..?> "actionArn")
            Prelude.<*> (x Data..?> "actionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateMitigationAction where
  hashWithSalt _salt UpdateMitigationAction' {..} =
    _salt `Prelude.hashWithSalt` actionParams
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` actionName

instance Prelude.NFData UpdateMitigationAction where
  rnf UpdateMitigationAction' {..} =
    Prelude.rnf actionParams
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf actionName

instance Data.ToHeaders UpdateMitigationAction where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateMitigationAction where
  toJSON UpdateMitigationAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("actionParams" Data..=) Prelude.<$> actionParams,
            ("roleArn" Data..=) Prelude.<$> roleArn
          ]
      )

instance Data.ToPath UpdateMitigationAction where
  toPath UpdateMitigationAction' {..} =
    Prelude.mconcat
      ["/mitigationactions/actions/", Data.toBS actionName]

instance Data.ToQuery UpdateMitigationAction where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf UpdateMitigationActionResponse' {..} =
    Prelude.rnf actionArn
      `Prelude.seq` Prelude.rnf actionId
      `Prelude.seq` Prelude.rnf httpStatus

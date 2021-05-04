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
-- Module      : Network.AWS.IoT.CreateMitigationAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Defines an action that can be applied to audit findings by using
-- StartAuditMitigationActionsTask. Only certain types of mitigation
-- actions can be applied to specific check names. For more information,
-- see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/device-defender-mitigation-actions.html Mitigation actions>.
-- Each mitigation action can apply only one type of change.
module Network.AWS.IoT.CreateMitigationAction
  ( -- * Creating a Request
    CreateMitigationAction (..),
    newCreateMitigationAction,

    -- * Request Lenses
    createMitigationAction_tags,
    createMitigationAction_actionName,
    createMitigationAction_roleArn,
    createMitigationAction_actionParams,

    -- * Destructuring the Response
    CreateMitigationActionResponse (..),
    newCreateMitigationActionResponse,

    -- * Response Lenses
    createMitigationActionResponse_actionArn,
    createMitigationActionResponse_actionId,
    createMitigationActionResponse_httpStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateMitigationAction' smart constructor.
data CreateMitigationAction = CreateMitigationAction'
  { -- | Metadata that can be used to manage the mitigation action.
    tags :: Prelude.Maybe [Tag],
    -- | A friendly name for the action. Choose a friendly name that accurately
    -- describes the action (for example, @EnableLoggingAction@).
    actionName :: Prelude.Text,
    -- | The ARN of the IAM role that is used to apply the mitigation action.
    roleArn :: Prelude.Text,
    -- | Defines the type of action and the parameters for that action.
    actionParams :: MitigationActionParams
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateMitigationAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createMitigationAction_tags' - Metadata that can be used to manage the mitigation action.
--
-- 'actionName', 'createMitigationAction_actionName' - A friendly name for the action. Choose a friendly name that accurately
-- describes the action (for example, @EnableLoggingAction@).
--
-- 'roleArn', 'createMitigationAction_roleArn' - The ARN of the IAM role that is used to apply the mitigation action.
--
-- 'actionParams', 'createMitigationAction_actionParams' - Defines the type of action and the parameters for that action.
newCreateMitigationAction ::
  -- | 'actionName'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'actionParams'
  MitigationActionParams ->
  CreateMitigationAction
newCreateMitigationAction
  pActionName_
  pRoleArn_
  pActionParams_ =
    CreateMitigationAction'
      { tags = Prelude.Nothing,
        actionName = pActionName_,
        roleArn = pRoleArn_,
        actionParams = pActionParams_
      }

-- | Metadata that can be used to manage the mitigation action.
createMitigationAction_tags :: Lens.Lens' CreateMitigationAction (Prelude.Maybe [Tag])
createMitigationAction_tags = Lens.lens (\CreateMitigationAction' {tags} -> tags) (\s@CreateMitigationAction' {} a -> s {tags = a} :: CreateMitigationAction) Prelude.. Lens.mapping Prelude._Coerce

-- | A friendly name for the action. Choose a friendly name that accurately
-- describes the action (for example, @EnableLoggingAction@).
createMitigationAction_actionName :: Lens.Lens' CreateMitigationAction Prelude.Text
createMitigationAction_actionName = Lens.lens (\CreateMitigationAction' {actionName} -> actionName) (\s@CreateMitigationAction' {} a -> s {actionName = a} :: CreateMitigationAction)

-- | The ARN of the IAM role that is used to apply the mitigation action.
createMitigationAction_roleArn :: Lens.Lens' CreateMitigationAction Prelude.Text
createMitigationAction_roleArn = Lens.lens (\CreateMitigationAction' {roleArn} -> roleArn) (\s@CreateMitigationAction' {} a -> s {roleArn = a} :: CreateMitigationAction)

-- | Defines the type of action and the parameters for that action.
createMitigationAction_actionParams :: Lens.Lens' CreateMitigationAction MitigationActionParams
createMitigationAction_actionParams = Lens.lens (\CreateMitigationAction' {actionParams} -> actionParams) (\s@CreateMitigationAction' {} a -> s {actionParams = a} :: CreateMitigationAction)

instance Prelude.AWSRequest CreateMitigationAction where
  type
    Rs CreateMitigationAction =
      CreateMitigationActionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMitigationActionResponse'
            Prelude.<$> (x Prelude..?> "actionArn")
            Prelude.<*> (x Prelude..?> "actionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateMitigationAction

instance Prelude.NFData CreateMitigationAction

instance Prelude.ToHeaders CreateMitigationAction where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON CreateMitigationAction where
  toJSON CreateMitigationAction' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("tags" Prelude..=) Prelude.<$> tags,
            Prelude.Just ("roleArn" Prelude..= roleArn),
            Prelude.Just
              ("actionParams" Prelude..= actionParams)
          ]
      )

instance Prelude.ToPath CreateMitigationAction where
  toPath CreateMitigationAction' {..} =
    Prelude.mconcat
      [ "/mitigationactions/actions/",
        Prelude.toBS actionName
      ]

instance Prelude.ToQuery CreateMitigationAction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateMitigationActionResponse' smart constructor.
data CreateMitigationActionResponse = CreateMitigationActionResponse'
  { -- | The ARN for the new mitigation action.
    actionArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the new mitigation action.
    actionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateMitigationActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionArn', 'createMitigationActionResponse_actionArn' - The ARN for the new mitigation action.
--
-- 'actionId', 'createMitigationActionResponse_actionId' - A unique identifier for the new mitigation action.
--
-- 'httpStatus', 'createMitigationActionResponse_httpStatus' - The response's http status code.
newCreateMitigationActionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateMitigationActionResponse
newCreateMitigationActionResponse pHttpStatus_ =
  CreateMitigationActionResponse'
    { actionArn =
        Prelude.Nothing,
      actionId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN for the new mitigation action.
createMitigationActionResponse_actionArn :: Lens.Lens' CreateMitigationActionResponse (Prelude.Maybe Prelude.Text)
createMitigationActionResponse_actionArn = Lens.lens (\CreateMitigationActionResponse' {actionArn} -> actionArn) (\s@CreateMitigationActionResponse' {} a -> s {actionArn = a} :: CreateMitigationActionResponse)

-- | A unique identifier for the new mitigation action.
createMitigationActionResponse_actionId :: Lens.Lens' CreateMitigationActionResponse (Prelude.Maybe Prelude.Text)
createMitigationActionResponse_actionId = Lens.lens (\CreateMitigationActionResponse' {actionId} -> actionId) (\s@CreateMitigationActionResponse' {} a -> s {actionId = a} :: CreateMitigationActionResponse)

-- | The response's http status code.
createMitigationActionResponse_httpStatus :: Lens.Lens' CreateMitigationActionResponse Prelude.Int
createMitigationActionResponse_httpStatus = Lens.lens (\CreateMitigationActionResponse' {httpStatus} -> httpStatus) (\s@CreateMitigationActionResponse' {} a -> s {httpStatus = a} :: CreateMitigationActionResponse)

instance
  Prelude.NFData
    CreateMitigationActionResponse

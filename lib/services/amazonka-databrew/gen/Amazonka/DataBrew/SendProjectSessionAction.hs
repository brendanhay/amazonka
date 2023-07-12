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
-- Module      : Amazonka.DataBrew.SendProjectSessionAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Performs a recipe step within an interactive DataBrew session that\'s
-- currently open.
module Amazonka.DataBrew.SendProjectSessionAction
  ( -- * Creating a Request
    SendProjectSessionAction (..),
    newSendProjectSessionAction,

    -- * Request Lenses
    sendProjectSessionAction_clientSessionId,
    sendProjectSessionAction_preview,
    sendProjectSessionAction_recipeStep,
    sendProjectSessionAction_stepIndex,
    sendProjectSessionAction_viewFrame,
    sendProjectSessionAction_name,

    -- * Destructuring the Response
    SendProjectSessionActionResponse (..),
    newSendProjectSessionActionResponse,

    -- * Response Lenses
    sendProjectSessionActionResponse_actionId,
    sendProjectSessionActionResponse_result,
    sendProjectSessionActionResponse_httpStatus,
    sendProjectSessionActionResponse_name,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataBrew.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSendProjectSessionAction' smart constructor.
data SendProjectSessionAction = SendProjectSessionAction'
  { -- | A unique identifier for an interactive session that\'s currently open
    -- and ready for work. The action will be performed on this session.
    clientSessionId :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | If true, the result of the recipe step will be returned, but not
    -- applied.
    preview :: Prelude.Maybe Prelude.Bool,
    recipeStep :: Prelude.Maybe RecipeStep,
    -- | The index from which to preview a step. This index is used to preview
    -- the result of steps that have already been applied, so that the
    -- resulting view frame is from earlier in the view frame stack.
    stepIndex :: Prelude.Maybe Prelude.Natural,
    viewFrame :: Prelude.Maybe ViewFrame,
    -- | The name of the project to apply the action to.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendProjectSessionAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientSessionId', 'sendProjectSessionAction_clientSessionId' - A unique identifier for an interactive session that\'s currently open
-- and ready for work. The action will be performed on this session.
--
-- 'preview', 'sendProjectSessionAction_preview' - If true, the result of the recipe step will be returned, but not
-- applied.
--
-- 'recipeStep', 'sendProjectSessionAction_recipeStep' - Undocumented member.
--
-- 'stepIndex', 'sendProjectSessionAction_stepIndex' - The index from which to preview a step. This index is used to preview
-- the result of steps that have already been applied, so that the
-- resulting view frame is from earlier in the view frame stack.
--
-- 'viewFrame', 'sendProjectSessionAction_viewFrame' - Undocumented member.
--
-- 'name', 'sendProjectSessionAction_name' - The name of the project to apply the action to.
newSendProjectSessionAction ::
  -- | 'name'
  Prelude.Text ->
  SendProjectSessionAction
newSendProjectSessionAction pName_ =
  SendProjectSessionAction'
    { clientSessionId =
        Prelude.Nothing,
      preview = Prelude.Nothing,
      recipeStep = Prelude.Nothing,
      stepIndex = Prelude.Nothing,
      viewFrame = Prelude.Nothing,
      name = pName_
    }

-- | A unique identifier for an interactive session that\'s currently open
-- and ready for work. The action will be performed on this session.
sendProjectSessionAction_clientSessionId :: Lens.Lens' SendProjectSessionAction (Prelude.Maybe Prelude.Text)
sendProjectSessionAction_clientSessionId = Lens.lens (\SendProjectSessionAction' {clientSessionId} -> clientSessionId) (\s@SendProjectSessionAction' {} a -> s {clientSessionId = a} :: SendProjectSessionAction) Prelude.. Lens.mapping Data._Sensitive

-- | If true, the result of the recipe step will be returned, but not
-- applied.
sendProjectSessionAction_preview :: Lens.Lens' SendProjectSessionAction (Prelude.Maybe Prelude.Bool)
sendProjectSessionAction_preview = Lens.lens (\SendProjectSessionAction' {preview} -> preview) (\s@SendProjectSessionAction' {} a -> s {preview = a} :: SendProjectSessionAction)

-- | Undocumented member.
sendProjectSessionAction_recipeStep :: Lens.Lens' SendProjectSessionAction (Prelude.Maybe RecipeStep)
sendProjectSessionAction_recipeStep = Lens.lens (\SendProjectSessionAction' {recipeStep} -> recipeStep) (\s@SendProjectSessionAction' {} a -> s {recipeStep = a} :: SendProjectSessionAction)

-- | The index from which to preview a step. This index is used to preview
-- the result of steps that have already been applied, so that the
-- resulting view frame is from earlier in the view frame stack.
sendProjectSessionAction_stepIndex :: Lens.Lens' SendProjectSessionAction (Prelude.Maybe Prelude.Natural)
sendProjectSessionAction_stepIndex = Lens.lens (\SendProjectSessionAction' {stepIndex} -> stepIndex) (\s@SendProjectSessionAction' {} a -> s {stepIndex = a} :: SendProjectSessionAction)

-- | Undocumented member.
sendProjectSessionAction_viewFrame :: Lens.Lens' SendProjectSessionAction (Prelude.Maybe ViewFrame)
sendProjectSessionAction_viewFrame = Lens.lens (\SendProjectSessionAction' {viewFrame} -> viewFrame) (\s@SendProjectSessionAction' {} a -> s {viewFrame = a} :: SendProjectSessionAction)

-- | The name of the project to apply the action to.
sendProjectSessionAction_name :: Lens.Lens' SendProjectSessionAction Prelude.Text
sendProjectSessionAction_name = Lens.lens (\SendProjectSessionAction' {name} -> name) (\s@SendProjectSessionAction' {} a -> s {name = a} :: SendProjectSessionAction)

instance Core.AWSRequest SendProjectSessionAction where
  type
    AWSResponse SendProjectSessionAction =
      SendProjectSessionActionResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SendProjectSessionActionResponse'
            Prelude.<$> (x Data..?> "ActionId")
            Prelude.<*> (x Data..?> "Result")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Name")
      )

instance Prelude.Hashable SendProjectSessionAction where
  hashWithSalt _salt SendProjectSessionAction' {..} =
    _salt
      `Prelude.hashWithSalt` clientSessionId
      `Prelude.hashWithSalt` preview
      `Prelude.hashWithSalt` recipeStep
      `Prelude.hashWithSalt` stepIndex
      `Prelude.hashWithSalt` viewFrame
      `Prelude.hashWithSalt` name

instance Prelude.NFData SendProjectSessionAction where
  rnf SendProjectSessionAction' {..} =
    Prelude.rnf clientSessionId
      `Prelude.seq` Prelude.rnf preview
      `Prelude.seq` Prelude.rnf recipeStep
      `Prelude.seq` Prelude.rnf stepIndex
      `Prelude.seq` Prelude.rnf viewFrame
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders SendProjectSessionAction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SendProjectSessionAction where
  toJSON SendProjectSessionAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientSessionId" Data..=)
              Prelude.<$> clientSessionId,
            ("Preview" Data..=) Prelude.<$> preview,
            ("RecipeStep" Data..=) Prelude.<$> recipeStep,
            ("StepIndex" Data..=) Prelude.<$> stepIndex,
            ("ViewFrame" Data..=) Prelude.<$> viewFrame
          ]
      )

instance Data.ToPath SendProjectSessionAction where
  toPath SendProjectSessionAction' {..} =
    Prelude.mconcat
      [ "/projects/",
        Data.toBS name,
        "/sendProjectSessionAction"
      ]

instance Data.ToQuery SendProjectSessionAction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSendProjectSessionActionResponse' smart constructor.
data SendProjectSessionActionResponse = SendProjectSessionActionResponse'
  { -- | A unique identifier for the action that was performed.
    actionId :: Prelude.Maybe Prelude.Int,
    -- | A message indicating the result of performing the action.
    result :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the project that was affected by the action.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendProjectSessionActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionId', 'sendProjectSessionActionResponse_actionId' - A unique identifier for the action that was performed.
--
-- 'result', 'sendProjectSessionActionResponse_result' - A message indicating the result of performing the action.
--
-- 'httpStatus', 'sendProjectSessionActionResponse_httpStatus' - The response's http status code.
--
-- 'name', 'sendProjectSessionActionResponse_name' - The name of the project that was affected by the action.
newSendProjectSessionActionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  SendProjectSessionActionResponse
newSendProjectSessionActionResponse
  pHttpStatus_
  pName_ =
    SendProjectSessionActionResponse'
      { actionId =
          Prelude.Nothing,
        result = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        name = pName_
      }

-- | A unique identifier for the action that was performed.
sendProjectSessionActionResponse_actionId :: Lens.Lens' SendProjectSessionActionResponse (Prelude.Maybe Prelude.Int)
sendProjectSessionActionResponse_actionId = Lens.lens (\SendProjectSessionActionResponse' {actionId} -> actionId) (\s@SendProjectSessionActionResponse' {} a -> s {actionId = a} :: SendProjectSessionActionResponse)

-- | A message indicating the result of performing the action.
sendProjectSessionActionResponse_result :: Lens.Lens' SendProjectSessionActionResponse (Prelude.Maybe Prelude.Text)
sendProjectSessionActionResponse_result = Lens.lens (\SendProjectSessionActionResponse' {result} -> result) (\s@SendProjectSessionActionResponse' {} a -> s {result = a} :: SendProjectSessionActionResponse)

-- | The response's http status code.
sendProjectSessionActionResponse_httpStatus :: Lens.Lens' SendProjectSessionActionResponse Prelude.Int
sendProjectSessionActionResponse_httpStatus = Lens.lens (\SendProjectSessionActionResponse' {httpStatus} -> httpStatus) (\s@SendProjectSessionActionResponse' {} a -> s {httpStatus = a} :: SendProjectSessionActionResponse)

-- | The name of the project that was affected by the action.
sendProjectSessionActionResponse_name :: Lens.Lens' SendProjectSessionActionResponse Prelude.Text
sendProjectSessionActionResponse_name = Lens.lens (\SendProjectSessionActionResponse' {name} -> name) (\s@SendProjectSessionActionResponse' {} a -> s {name = a} :: SendProjectSessionActionResponse)

instance
  Prelude.NFData
    SendProjectSessionActionResponse
  where
  rnf SendProjectSessionActionResponse' {..} =
    Prelude.rnf actionId
      `Prelude.seq` Prelude.rnf result
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf name

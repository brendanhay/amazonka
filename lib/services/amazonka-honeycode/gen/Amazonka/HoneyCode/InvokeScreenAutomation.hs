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
-- Module      : Amazonka.HoneyCode.InvokeScreenAutomation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The InvokeScreenAutomation API allows invoking an action defined in a
-- screen in a Honeycode app. The API allows setting local variables, which
-- can then be used in the automation being invoked. This allows automating
-- the Honeycode app interactions to write, update or delete data in the
-- workbook.
module Amazonka.HoneyCode.InvokeScreenAutomation
  ( -- * Creating a Request
    InvokeScreenAutomation (..),
    newInvokeScreenAutomation,

    -- * Request Lenses
    invokeScreenAutomation_clientRequestToken,
    invokeScreenAutomation_rowId,
    invokeScreenAutomation_variables,
    invokeScreenAutomation_workbookId,
    invokeScreenAutomation_appId,
    invokeScreenAutomation_screenId,
    invokeScreenAutomation_screenAutomationId,

    -- * Destructuring the Response
    InvokeScreenAutomationResponse (..),
    newInvokeScreenAutomationResponse,

    -- * Response Lenses
    invokeScreenAutomationResponse_httpStatus,
    invokeScreenAutomationResponse_workbookCursor,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.HoneyCode.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newInvokeScreenAutomation' smart constructor.
data InvokeScreenAutomation = InvokeScreenAutomation'
  { -- | The request token for performing the automation action. Request tokens
    -- help to identify duplicate requests. If a call times out or fails due to
    -- a transient error like a failed network connection, you can retry the
    -- call with the same request token. The service ensures that if the first
    -- call using that request token is successfully performed, the second call
    -- will return the response of the previous call rather than performing the
    -- action again.
    --
    -- Note that request tokens are valid only for a few minutes. You cannot
    -- use request tokens to dedupe requests spanning hours or days.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The row ID for the automation if the automation is defined inside a
    -- block with source or list.
    rowId :: Prelude.Maybe Prelude.Text,
    -- | Variables are specified as a map where the key is the name of the
    -- variable as defined on the screen. The value is an object which
    -- currently has only one property, rawValue, which holds the value of the
    -- variable to be passed to the screen. Any variables defined in a screen
    -- are required to be passed in the call.
    variables :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text (Data.Sensitive VariableValue))),
    -- | The ID of the workbook that contains the screen automation.
    workbookId :: Prelude.Text,
    -- | The ID of the app that contains the screen automation.
    appId :: Prelude.Text,
    -- | The ID of the screen that contains the screen automation.
    screenId :: Prelude.Text,
    -- | The ID of the automation action to be performed.
    screenAutomationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InvokeScreenAutomation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'invokeScreenAutomation_clientRequestToken' - The request token for performing the automation action. Request tokens
-- help to identify duplicate requests. If a call times out or fails due to
-- a transient error like a failed network connection, you can retry the
-- call with the same request token. The service ensures that if the first
-- call using that request token is successfully performed, the second call
-- will return the response of the previous call rather than performing the
-- action again.
--
-- Note that request tokens are valid only for a few minutes. You cannot
-- use request tokens to dedupe requests spanning hours or days.
--
-- 'rowId', 'invokeScreenAutomation_rowId' - The row ID for the automation if the automation is defined inside a
-- block with source or list.
--
-- 'variables', 'invokeScreenAutomation_variables' - Variables are specified as a map where the key is the name of the
-- variable as defined on the screen. The value is an object which
-- currently has only one property, rawValue, which holds the value of the
-- variable to be passed to the screen. Any variables defined in a screen
-- are required to be passed in the call.
--
-- 'workbookId', 'invokeScreenAutomation_workbookId' - The ID of the workbook that contains the screen automation.
--
-- 'appId', 'invokeScreenAutomation_appId' - The ID of the app that contains the screen automation.
--
-- 'screenId', 'invokeScreenAutomation_screenId' - The ID of the screen that contains the screen automation.
--
-- 'screenAutomationId', 'invokeScreenAutomation_screenAutomationId' - The ID of the automation action to be performed.
newInvokeScreenAutomation ::
  -- | 'workbookId'
  Prelude.Text ->
  -- | 'appId'
  Prelude.Text ->
  -- | 'screenId'
  Prelude.Text ->
  -- | 'screenAutomationId'
  Prelude.Text ->
  InvokeScreenAutomation
newInvokeScreenAutomation
  pWorkbookId_
  pAppId_
  pScreenId_
  pScreenAutomationId_ =
    InvokeScreenAutomation'
      { clientRequestToken =
          Prelude.Nothing,
        rowId = Prelude.Nothing,
        variables = Prelude.Nothing,
        workbookId = pWorkbookId_,
        appId = pAppId_,
        screenId = pScreenId_,
        screenAutomationId = pScreenAutomationId_
      }

-- | The request token for performing the automation action. Request tokens
-- help to identify duplicate requests. If a call times out or fails due to
-- a transient error like a failed network connection, you can retry the
-- call with the same request token. The service ensures that if the first
-- call using that request token is successfully performed, the second call
-- will return the response of the previous call rather than performing the
-- action again.
--
-- Note that request tokens are valid only for a few minutes. You cannot
-- use request tokens to dedupe requests spanning hours or days.
invokeScreenAutomation_clientRequestToken :: Lens.Lens' InvokeScreenAutomation (Prelude.Maybe Prelude.Text)
invokeScreenAutomation_clientRequestToken = Lens.lens (\InvokeScreenAutomation' {clientRequestToken} -> clientRequestToken) (\s@InvokeScreenAutomation' {} a -> s {clientRequestToken = a} :: InvokeScreenAutomation)

-- | The row ID for the automation if the automation is defined inside a
-- block with source or list.
invokeScreenAutomation_rowId :: Lens.Lens' InvokeScreenAutomation (Prelude.Maybe Prelude.Text)
invokeScreenAutomation_rowId = Lens.lens (\InvokeScreenAutomation' {rowId} -> rowId) (\s@InvokeScreenAutomation' {} a -> s {rowId = a} :: InvokeScreenAutomation)

-- | Variables are specified as a map where the key is the name of the
-- variable as defined on the screen. The value is an object which
-- currently has only one property, rawValue, which holds the value of the
-- variable to be passed to the screen. Any variables defined in a screen
-- are required to be passed in the call.
invokeScreenAutomation_variables :: Lens.Lens' InvokeScreenAutomation (Prelude.Maybe (Prelude.HashMap Prelude.Text VariableValue))
invokeScreenAutomation_variables = Lens.lens (\InvokeScreenAutomation' {variables} -> variables) (\s@InvokeScreenAutomation' {} a -> s {variables = a} :: InvokeScreenAutomation) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The ID of the workbook that contains the screen automation.
invokeScreenAutomation_workbookId :: Lens.Lens' InvokeScreenAutomation Prelude.Text
invokeScreenAutomation_workbookId = Lens.lens (\InvokeScreenAutomation' {workbookId} -> workbookId) (\s@InvokeScreenAutomation' {} a -> s {workbookId = a} :: InvokeScreenAutomation)

-- | The ID of the app that contains the screen automation.
invokeScreenAutomation_appId :: Lens.Lens' InvokeScreenAutomation Prelude.Text
invokeScreenAutomation_appId = Lens.lens (\InvokeScreenAutomation' {appId} -> appId) (\s@InvokeScreenAutomation' {} a -> s {appId = a} :: InvokeScreenAutomation)

-- | The ID of the screen that contains the screen automation.
invokeScreenAutomation_screenId :: Lens.Lens' InvokeScreenAutomation Prelude.Text
invokeScreenAutomation_screenId = Lens.lens (\InvokeScreenAutomation' {screenId} -> screenId) (\s@InvokeScreenAutomation' {} a -> s {screenId = a} :: InvokeScreenAutomation)

-- | The ID of the automation action to be performed.
invokeScreenAutomation_screenAutomationId :: Lens.Lens' InvokeScreenAutomation Prelude.Text
invokeScreenAutomation_screenAutomationId = Lens.lens (\InvokeScreenAutomation' {screenAutomationId} -> screenAutomationId) (\s@InvokeScreenAutomation' {} a -> s {screenAutomationId = a} :: InvokeScreenAutomation)

instance Core.AWSRequest InvokeScreenAutomation where
  type
    AWSResponse InvokeScreenAutomation =
      InvokeScreenAutomationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          InvokeScreenAutomationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "workbookCursor")
      )

instance Prelude.Hashable InvokeScreenAutomation where
  hashWithSalt _salt InvokeScreenAutomation' {..} =
    _salt `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` rowId
      `Prelude.hashWithSalt` variables
      `Prelude.hashWithSalt` workbookId
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` screenId
      `Prelude.hashWithSalt` screenAutomationId

instance Prelude.NFData InvokeScreenAutomation where
  rnf InvokeScreenAutomation' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf rowId
      `Prelude.seq` Prelude.rnf variables
      `Prelude.seq` Prelude.rnf workbookId
      `Prelude.seq` Prelude.rnf appId
      `Prelude.seq` Prelude.rnf screenId
      `Prelude.seq` Prelude.rnf screenAutomationId

instance Data.ToHeaders InvokeScreenAutomation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON InvokeScreenAutomation where
  toJSON InvokeScreenAutomation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("rowId" Data..=) Prelude.<$> rowId,
            ("variables" Data..=) Prelude.<$> variables
          ]
      )

instance Data.ToPath InvokeScreenAutomation where
  toPath InvokeScreenAutomation' {..} =
    Prelude.mconcat
      [ "/workbooks/",
        Data.toBS workbookId,
        "/apps/",
        Data.toBS appId,
        "/screens/",
        Data.toBS screenId,
        "/automations/",
        Data.toBS screenAutomationId
      ]

instance Data.ToQuery InvokeScreenAutomation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newInvokeScreenAutomationResponse' smart constructor.
data InvokeScreenAutomationResponse = InvokeScreenAutomationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The updated workbook cursor after performing the automation action.
    workbookCursor :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InvokeScreenAutomationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'invokeScreenAutomationResponse_httpStatus' - The response's http status code.
--
-- 'workbookCursor', 'invokeScreenAutomationResponse_workbookCursor' - The updated workbook cursor after performing the automation action.
newInvokeScreenAutomationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'workbookCursor'
  Prelude.Integer ->
  InvokeScreenAutomationResponse
newInvokeScreenAutomationResponse
  pHttpStatus_
  pWorkbookCursor_ =
    InvokeScreenAutomationResponse'
      { httpStatus =
          pHttpStatus_,
        workbookCursor = pWorkbookCursor_
      }

-- | The response's http status code.
invokeScreenAutomationResponse_httpStatus :: Lens.Lens' InvokeScreenAutomationResponse Prelude.Int
invokeScreenAutomationResponse_httpStatus = Lens.lens (\InvokeScreenAutomationResponse' {httpStatus} -> httpStatus) (\s@InvokeScreenAutomationResponse' {} a -> s {httpStatus = a} :: InvokeScreenAutomationResponse)

-- | The updated workbook cursor after performing the automation action.
invokeScreenAutomationResponse_workbookCursor :: Lens.Lens' InvokeScreenAutomationResponse Prelude.Integer
invokeScreenAutomationResponse_workbookCursor = Lens.lens (\InvokeScreenAutomationResponse' {workbookCursor} -> workbookCursor) (\s@InvokeScreenAutomationResponse' {} a -> s {workbookCursor = a} :: InvokeScreenAutomationResponse)

instance
  Prelude.NFData
    InvokeScreenAutomationResponse
  where
  rnf InvokeScreenAutomationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf workbookCursor

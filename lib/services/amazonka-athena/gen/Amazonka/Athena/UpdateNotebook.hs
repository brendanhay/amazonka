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
-- Module      : Amazonka.Athena.UpdateNotebook
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the contents of a Spark notebook.
module Amazonka.Athena.UpdateNotebook
  ( -- * Creating a Request
    UpdateNotebook (..),
    newUpdateNotebook,

    -- * Request Lenses
    updateNotebook_clientRequestToken,
    updateNotebook_payload,
    updateNotebook_sessionId,
    updateNotebook_type,
    updateNotebook_notebookId,

    -- * Destructuring the Response
    UpdateNotebookResponse (..),
    newUpdateNotebookResponse,

    -- * Response Lenses
    updateNotebookResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateNotebook' smart constructor.
data UpdateNotebook = UpdateNotebook'
  { -- | A unique case-sensitive string used to ensure the request to create the
    -- notebook is idempotent (executes only once).
    --
    -- This token is listed as not required because Amazon Web Services SDKs
    -- (for example the Amazon Web Services SDK for Java) auto-generate the
    -- token for you. If you are not using the Amazon Web Services SDK or the
    -- Amazon Web Services CLI, you must provide this token or the action will
    -- fail.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The updated content for the notebook.
    payload :: Prelude.Maybe Prelude.Text,
    -- | The ID of the session in which the notebook will be updated.
    sessionId :: Prelude.Maybe Prelude.Text,
    -- | The notebook content type. Currently, the only valid type is @IPYNB@.
    type' :: Prelude.Maybe NotebookType,
    -- | The ID of the notebook to update.
    notebookId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateNotebook' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'updateNotebook_clientRequestToken' - A unique case-sensitive string used to ensure the request to create the
-- notebook is idempotent (executes only once).
--
-- This token is listed as not required because Amazon Web Services SDKs
-- (for example the Amazon Web Services SDK for Java) auto-generate the
-- token for you. If you are not using the Amazon Web Services SDK or the
-- Amazon Web Services CLI, you must provide this token or the action will
-- fail.
--
-- 'payload', 'updateNotebook_payload' - The updated content for the notebook.
--
-- 'sessionId', 'updateNotebook_sessionId' - The ID of the session in which the notebook will be updated.
--
-- 'type'', 'updateNotebook_type' - The notebook content type. Currently, the only valid type is @IPYNB@.
--
-- 'notebookId', 'updateNotebook_notebookId' - The ID of the notebook to update.
newUpdateNotebook ::
  -- | 'notebookId'
  Prelude.Text ->
  UpdateNotebook
newUpdateNotebook pNotebookId_ =
  UpdateNotebook'
    { clientRequestToken =
        Prelude.Nothing,
      payload = Prelude.Nothing,
      sessionId = Prelude.Nothing,
      type' = Prelude.Nothing,
      notebookId = pNotebookId_
    }

-- | A unique case-sensitive string used to ensure the request to create the
-- notebook is idempotent (executes only once).
--
-- This token is listed as not required because Amazon Web Services SDKs
-- (for example the Amazon Web Services SDK for Java) auto-generate the
-- token for you. If you are not using the Amazon Web Services SDK or the
-- Amazon Web Services CLI, you must provide this token or the action will
-- fail.
updateNotebook_clientRequestToken :: Lens.Lens' UpdateNotebook (Prelude.Maybe Prelude.Text)
updateNotebook_clientRequestToken = Lens.lens (\UpdateNotebook' {clientRequestToken} -> clientRequestToken) (\s@UpdateNotebook' {} a -> s {clientRequestToken = a} :: UpdateNotebook)

-- | The updated content for the notebook.
updateNotebook_payload :: Lens.Lens' UpdateNotebook (Prelude.Maybe Prelude.Text)
updateNotebook_payload = Lens.lens (\UpdateNotebook' {payload} -> payload) (\s@UpdateNotebook' {} a -> s {payload = a} :: UpdateNotebook)

-- | The ID of the session in which the notebook will be updated.
updateNotebook_sessionId :: Lens.Lens' UpdateNotebook (Prelude.Maybe Prelude.Text)
updateNotebook_sessionId = Lens.lens (\UpdateNotebook' {sessionId} -> sessionId) (\s@UpdateNotebook' {} a -> s {sessionId = a} :: UpdateNotebook)

-- | The notebook content type. Currently, the only valid type is @IPYNB@.
updateNotebook_type :: Lens.Lens' UpdateNotebook (Prelude.Maybe NotebookType)
updateNotebook_type = Lens.lens (\UpdateNotebook' {type'} -> type') (\s@UpdateNotebook' {} a -> s {type' = a} :: UpdateNotebook)

-- | The ID of the notebook to update.
updateNotebook_notebookId :: Lens.Lens' UpdateNotebook Prelude.Text
updateNotebook_notebookId = Lens.lens (\UpdateNotebook' {notebookId} -> notebookId) (\s@UpdateNotebook' {} a -> s {notebookId = a} :: UpdateNotebook)

instance Core.AWSRequest UpdateNotebook where
  type
    AWSResponse UpdateNotebook =
      UpdateNotebookResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateNotebookResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateNotebook where
  hashWithSalt _salt UpdateNotebook' {..} =
    _salt `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` payload
      `Prelude.hashWithSalt` sessionId
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` notebookId

instance Prelude.NFData UpdateNotebook where
  rnf UpdateNotebook' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf payload
      `Prelude.seq` Prelude.rnf sessionId
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf notebookId

instance Data.ToHeaders UpdateNotebook where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.UpdateNotebook" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateNotebook where
  toJSON UpdateNotebook' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("Payload" Data..=) Prelude.<$> payload,
            ("SessionId" Data..=) Prelude.<$> sessionId,
            ("Type" Data..=) Prelude.<$> type',
            Prelude.Just ("NotebookId" Data..= notebookId)
          ]
      )

instance Data.ToPath UpdateNotebook where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateNotebook where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateNotebookResponse' smart constructor.
data UpdateNotebookResponse = UpdateNotebookResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateNotebookResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateNotebookResponse_httpStatus' - The response's http status code.
newUpdateNotebookResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateNotebookResponse
newUpdateNotebookResponse pHttpStatus_ =
  UpdateNotebookResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateNotebookResponse_httpStatus :: Lens.Lens' UpdateNotebookResponse Prelude.Int
updateNotebookResponse_httpStatus = Lens.lens (\UpdateNotebookResponse' {httpStatus} -> httpStatus) (\s@UpdateNotebookResponse' {} a -> s {httpStatus = a} :: UpdateNotebookResponse)

instance Prelude.NFData UpdateNotebookResponse where
  rnf UpdateNotebookResponse' {..} =
    Prelude.rnf httpStatus

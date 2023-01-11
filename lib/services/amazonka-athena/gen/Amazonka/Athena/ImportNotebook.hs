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
-- Module      : Amazonka.Athena.ImportNotebook
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports a single @ipynb@ file to a Spark enabled workgroup. The maximum
-- file size that can be imported is 10 megabytes. If an @ipynb@ file with
-- the same name already exists in the workgroup, throws an error.
module Amazonka.Athena.ImportNotebook
  ( -- * Creating a Request
    ImportNotebook (..),
    newImportNotebook,

    -- * Request Lenses
    importNotebook_clientRequestToken,
    importNotebook_workGroup,
    importNotebook_name,
    importNotebook_payload,
    importNotebook_type,

    -- * Destructuring the Response
    ImportNotebookResponse (..),
    newImportNotebookResponse,

    -- * Response Lenses
    importNotebookResponse_notebookId,
    importNotebookResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newImportNotebook' smart constructor.
data ImportNotebook = ImportNotebook'
  { -- | A unique case-sensitive string used to ensure the request to import the
    -- notebook is idempotent (executes only once).
    --
    -- This token is listed as not required because Amazon Web Services SDKs
    -- (for example the Amazon Web Services SDK for Java) auto-generate the
    -- token for you. If you are not using the Amazon Web Services SDK or the
    -- Amazon Web Services CLI, you must provide this token or the action will
    -- fail.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the Spark enabled workgroup to import the notebook to.
    workGroup :: Prelude.Text,
    -- | The name of the notebook to import.
    name :: Prelude.Text,
    -- | The notebook content to be imported.
    payload :: Prelude.Text,
    -- | The notebook content type. Currently, the only valid type is @IPYNB@.
    type' :: NotebookType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportNotebook' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'importNotebook_clientRequestToken' - A unique case-sensitive string used to ensure the request to import the
-- notebook is idempotent (executes only once).
--
-- This token is listed as not required because Amazon Web Services SDKs
-- (for example the Amazon Web Services SDK for Java) auto-generate the
-- token for you. If you are not using the Amazon Web Services SDK or the
-- Amazon Web Services CLI, you must provide this token or the action will
-- fail.
--
-- 'workGroup', 'importNotebook_workGroup' - The name of the Spark enabled workgroup to import the notebook to.
--
-- 'name', 'importNotebook_name' - The name of the notebook to import.
--
-- 'payload', 'importNotebook_payload' - The notebook content to be imported.
--
-- 'type'', 'importNotebook_type' - The notebook content type. Currently, the only valid type is @IPYNB@.
newImportNotebook ::
  -- | 'workGroup'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'payload'
  Prelude.Text ->
  -- | 'type''
  NotebookType ->
  ImportNotebook
newImportNotebook pWorkGroup_ pName_ pPayload_ pType_ =
  ImportNotebook'
    { clientRequestToken =
        Prelude.Nothing,
      workGroup = pWorkGroup_,
      name = pName_,
      payload = pPayload_,
      type' = pType_
    }

-- | A unique case-sensitive string used to ensure the request to import the
-- notebook is idempotent (executes only once).
--
-- This token is listed as not required because Amazon Web Services SDKs
-- (for example the Amazon Web Services SDK for Java) auto-generate the
-- token for you. If you are not using the Amazon Web Services SDK or the
-- Amazon Web Services CLI, you must provide this token or the action will
-- fail.
importNotebook_clientRequestToken :: Lens.Lens' ImportNotebook (Prelude.Maybe Prelude.Text)
importNotebook_clientRequestToken = Lens.lens (\ImportNotebook' {clientRequestToken} -> clientRequestToken) (\s@ImportNotebook' {} a -> s {clientRequestToken = a} :: ImportNotebook)

-- | The name of the Spark enabled workgroup to import the notebook to.
importNotebook_workGroup :: Lens.Lens' ImportNotebook Prelude.Text
importNotebook_workGroup = Lens.lens (\ImportNotebook' {workGroup} -> workGroup) (\s@ImportNotebook' {} a -> s {workGroup = a} :: ImportNotebook)

-- | The name of the notebook to import.
importNotebook_name :: Lens.Lens' ImportNotebook Prelude.Text
importNotebook_name = Lens.lens (\ImportNotebook' {name} -> name) (\s@ImportNotebook' {} a -> s {name = a} :: ImportNotebook)

-- | The notebook content to be imported.
importNotebook_payload :: Lens.Lens' ImportNotebook Prelude.Text
importNotebook_payload = Lens.lens (\ImportNotebook' {payload} -> payload) (\s@ImportNotebook' {} a -> s {payload = a} :: ImportNotebook)

-- | The notebook content type. Currently, the only valid type is @IPYNB@.
importNotebook_type :: Lens.Lens' ImportNotebook NotebookType
importNotebook_type = Lens.lens (\ImportNotebook' {type'} -> type') (\s@ImportNotebook' {} a -> s {type' = a} :: ImportNotebook)

instance Core.AWSRequest ImportNotebook where
  type
    AWSResponse ImportNotebook =
      ImportNotebookResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ImportNotebookResponse'
            Prelude.<$> (x Data..?> "NotebookId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ImportNotebook where
  hashWithSalt _salt ImportNotebook' {..} =
    _salt `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` workGroup
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` payload
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ImportNotebook where
  rnf ImportNotebook' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf workGroup
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf payload
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders ImportNotebook where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.ImportNotebook" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ImportNotebook where
  toJSON ImportNotebook' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just ("WorkGroup" Data..= workGroup),
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Payload" Data..= payload),
            Prelude.Just ("Type" Data..= type')
          ]
      )

instance Data.ToPath ImportNotebook where
  toPath = Prelude.const "/"

instance Data.ToQuery ImportNotebook where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newImportNotebookResponse' smart constructor.
data ImportNotebookResponse = ImportNotebookResponse'
  { -- | The ID of the notebook to import.
    notebookId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportNotebookResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notebookId', 'importNotebookResponse_notebookId' - The ID of the notebook to import.
--
-- 'httpStatus', 'importNotebookResponse_httpStatus' - The response's http status code.
newImportNotebookResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ImportNotebookResponse
newImportNotebookResponse pHttpStatus_ =
  ImportNotebookResponse'
    { notebookId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the notebook to import.
importNotebookResponse_notebookId :: Lens.Lens' ImportNotebookResponse (Prelude.Maybe Prelude.Text)
importNotebookResponse_notebookId = Lens.lens (\ImportNotebookResponse' {notebookId} -> notebookId) (\s@ImportNotebookResponse' {} a -> s {notebookId = a} :: ImportNotebookResponse)

-- | The response's http status code.
importNotebookResponse_httpStatus :: Lens.Lens' ImportNotebookResponse Prelude.Int
importNotebookResponse_httpStatus = Lens.lens (\ImportNotebookResponse' {httpStatus} -> httpStatus) (\s@ImportNotebookResponse' {} a -> s {httpStatus = a} :: ImportNotebookResponse)

instance Prelude.NFData ImportNotebookResponse where
  rnf ImportNotebookResponse' {..} =
    Prelude.rnf notebookId
      `Prelude.seq` Prelude.rnf httpStatus

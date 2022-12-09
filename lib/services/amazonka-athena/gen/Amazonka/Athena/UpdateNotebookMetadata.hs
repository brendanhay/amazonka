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
-- Module      : Amazonka.Athena.UpdateNotebookMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the metadata for a notebook.
module Amazonka.Athena.UpdateNotebookMetadata
  ( -- * Creating a Request
    UpdateNotebookMetadata (..),
    newUpdateNotebookMetadata,

    -- * Request Lenses
    updateNotebookMetadata_clientRequestToken,
    updateNotebookMetadata_notebookId,
    updateNotebookMetadata_name,

    -- * Destructuring the Response
    UpdateNotebookMetadataResponse (..),
    newUpdateNotebookMetadataResponse,

    -- * Response Lenses
    updateNotebookMetadataResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateNotebookMetadata' smart constructor.
data UpdateNotebookMetadata = UpdateNotebookMetadata'
  { -- | A unique case-sensitive string used to ensure the request to create the
    -- notebook is idempotent (executes only once).
    --
    -- This token is listed as not required because Amazon Web Services SDKs
    -- (for example the Amazon Web Services SDK for Java) auto-generate the
    -- token for you. If you are not using the Amazon Web Services SDK or the
    -- Amazon Web Services CLI, you must provide this token or the action will
    -- fail.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the notebook to update the metadata for.
    notebookId :: Prelude.Text,
    -- | The name to update the notebook to.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateNotebookMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'updateNotebookMetadata_clientRequestToken' - A unique case-sensitive string used to ensure the request to create the
-- notebook is idempotent (executes only once).
--
-- This token is listed as not required because Amazon Web Services SDKs
-- (for example the Amazon Web Services SDK for Java) auto-generate the
-- token for you. If you are not using the Amazon Web Services SDK or the
-- Amazon Web Services CLI, you must provide this token or the action will
-- fail.
--
-- 'notebookId', 'updateNotebookMetadata_notebookId' - The ID of the notebook to update the metadata for.
--
-- 'name', 'updateNotebookMetadata_name' - The name to update the notebook to.
newUpdateNotebookMetadata ::
  -- | 'notebookId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  UpdateNotebookMetadata
newUpdateNotebookMetadata pNotebookId_ pName_ =
  UpdateNotebookMetadata'
    { clientRequestToken =
        Prelude.Nothing,
      notebookId = pNotebookId_,
      name = pName_
    }

-- | A unique case-sensitive string used to ensure the request to create the
-- notebook is idempotent (executes only once).
--
-- This token is listed as not required because Amazon Web Services SDKs
-- (for example the Amazon Web Services SDK for Java) auto-generate the
-- token for you. If you are not using the Amazon Web Services SDK or the
-- Amazon Web Services CLI, you must provide this token or the action will
-- fail.
updateNotebookMetadata_clientRequestToken :: Lens.Lens' UpdateNotebookMetadata (Prelude.Maybe Prelude.Text)
updateNotebookMetadata_clientRequestToken = Lens.lens (\UpdateNotebookMetadata' {clientRequestToken} -> clientRequestToken) (\s@UpdateNotebookMetadata' {} a -> s {clientRequestToken = a} :: UpdateNotebookMetadata)

-- | The ID of the notebook to update the metadata for.
updateNotebookMetadata_notebookId :: Lens.Lens' UpdateNotebookMetadata Prelude.Text
updateNotebookMetadata_notebookId = Lens.lens (\UpdateNotebookMetadata' {notebookId} -> notebookId) (\s@UpdateNotebookMetadata' {} a -> s {notebookId = a} :: UpdateNotebookMetadata)

-- | The name to update the notebook to.
updateNotebookMetadata_name :: Lens.Lens' UpdateNotebookMetadata Prelude.Text
updateNotebookMetadata_name = Lens.lens (\UpdateNotebookMetadata' {name} -> name) (\s@UpdateNotebookMetadata' {} a -> s {name = a} :: UpdateNotebookMetadata)

instance Core.AWSRequest UpdateNotebookMetadata where
  type
    AWSResponse UpdateNotebookMetadata =
      UpdateNotebookMetadataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateNotebookMetadataResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateNotebookMetadata where
  hashWithSalt _salt UpdateNotebookMetadata' {..} =
    _salt `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` notebookId
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateNotebookMetadata where
  rnf UpdateNotebookMetadata' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf notebookId
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders UpdateNotebookMetadata where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.UpdateNotebookMetadata" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateNotebookMetadata where
  toJSON UpdateNotebookMetadata' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just ("NotebookId" Data..= notebookId),
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath UpdateNotebookMetadata where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateNotebookMetadata where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateNotebookMetadataResponse' smart constructor.
data UpdateNotebookMetadataResponse = UpdateNotebookMetadataResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateNotebookMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateNotebookMetadataResponse_httpStatus' - The response's http status code.
newUpdateNotebookMetadataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateNotebookMetadataResponse
newUpdateNotebookMetadataResponse pHttpStatus_ =
  UpdateNotebookMetadataResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateNotebookMetadataResponse_httpStatus :: Lens.Lens' UpdateNotebookMetadataResponse Prelude.Int
updateNotebookMetadataResponse_httpStatus = Lens.lens (\UpdateNotebookMetadataResponse' {httpStatus} -> httpStatus) (\s@UpdateNotebookMetadataResponse' {} a -> s {httpStatus = a} :: UpdateNotebookMetadataResponse)

instance
  Prelude.NFData
    UpdateNotebookMetadataResponse
  where
  rnf UpdateNotebookMetadataResponse' {..} =
    Prelude.rnf httpStatus

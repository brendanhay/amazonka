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
-- Module      : Amazonka.Athena.ExportNotebook
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports the specified notebook and its metadata.
module Amazonka.Athena.ExportNotebook
  ( -- * Creating a Request
    ExportNotebook (..),
    newExportNotebook,

    -- * Request Lenses
    exportNotebook_notebookId,

    -- * Destructuring the Response
    ExportNotebookResponse (..),
    newExportNotebookResponse,

    -- * Response Lenses
    exportNotebookResponse_notebookMetadata,
    exportNotebookResponse_payload,
    exportNotebookResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newExportNotebook' smart constructor.
data ExportNotebook = ExportNotebook'
  { -- | The ID of the notebook to export.
    notebookId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportNotebook' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notebookId', 'exportNotebook_notebookId' - The ID of the notebook to export.
newExportNotebook ::
  -- | 'notebookId'
  Prelude.Text ->
  ExportNotebook
newExportNotebook pNotebookId_ =
  ExportNotebook' {notebookId = pNotebookId_}

-- | The ID of the notebook to export.
exportNotebook_notebookId :: Lens.Lens' ExportNotebook Prelude.Text
exportNotebook_notebookId = Lens.lens (\ExportNotebook' {notebookId} -> notebookId) (\s@ExportNotebook' {} a -> s {notebookId = a} :: ExportNotebook)

instance Core.AWSRequest ExportNotebook where
  type
    AWSResponse ExportNotebook =
      ExportNotebookResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ExportNotebookResponse'
            Prelude.<$> (x Data..?> "NotebookMetadata")
            Prelude.<*> (x Data..?> "Payload")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ExportNotebook where
  hashWithSalt _salt ExportNotebook' {..} =
    _salt `Prelude.hashWithSalt` notebookId

instance Prelude.NFData ExportNotebook where
  rnf ExportNotebook' {..} = Prelude.rnf notebookId

instance Data.ToHeaders ExportNotebook where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.ExportNotebook" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ExportNotebook where
  toJSON ExportNotebook' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("NotebookId" Data..= notebookId)]
      )

instance Data.ToPath ExportNotebook where
  toPath = Prelude.const "/"

instance Data.ToQuery ExportNotebook where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newExportNotebookResponse' smart constructor.
data ExportNotebookResponse = ExportNotebookResponse'
  { -- | The notebook metadata, including notebook ID, notebook name, and
    -- workgroup name.
    notebookMetadata :: Prelude.Maybe NotebookMetadata,
    -- | The content of the exported notebook.
    payload :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportNotebookResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notebookMetadata', 'exportNotebookResponse_notebookMetadata' - The notebook metadata, including notebook ID, notebook name, and
-- workgroup name.
--
-- 'payload', 'exportNotebookResponse_payload' - The content of the exported notebook.
--
-- 'httpStatus', 'exportNotebookResponse_httpStatus' - The response's http status code.
newExportNotebookResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ExportNotebookResponse
newExportNotebookResponse pHttpStatus_ =
  ExportNotebookResponse'
    { notebookMetadata =
        Prelude.Nothing,
      payload = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The notebook metadata, including notebook ID, notebook name, and
-- workgroup name.
exportNotebookResponse_notebookMetadata :: Lens.Lens' ExportNotebookResponse (Prelude.Maybe NotebookMetadata)
exportNotebookResponse_notebookMetadata = Lens.lens (\ExportNotebookResponse' {notebookMetadata} -> notebookMetadata) (\s@ExportNotebookResponse' {} a -> s {notebookMetadata = a} :: ExportNotebookResponse)

-- | The content of the exported notebook.
exportNotebookResponse_payload :: Lens.Lens' ExportNotebookResponse (Prelude.Maybe Prelude.Text)
exportNotebookResponse_payload = Lens.lens (\ExportNotebookResponse' {payload} -> payload) (\s@ExportNotebookResponse' {} a -> s {payload = a} :: ExportNotebookResponse)

-- | The response's http status code.
exportNotebookResponse_httpStatus :: Lens.Lens' ExportNotebookResponse Prelude.Int
exportNotebookResponse_httpStatus = Lens.lens (\ExportNotebookResponse' {httpStatus} -> httpStatus) (\s@ExportNotebookResponse' {} a -> s {httpStatus = a} :: ExportNotebookResponse)

instance Prelude.NFData ExportNotebookResponse where
  rnf ExportNotebookResponse' {..} =
    Prelude.rnf notebookMetadata `Prelude.seq`
      Prelude.rnf payload `Prelude.seq`
        Prelude.rnf httpStatus

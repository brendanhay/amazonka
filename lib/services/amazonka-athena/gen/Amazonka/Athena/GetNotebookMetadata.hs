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
-- Module      : Amazonka.Athena.GetNotebookMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves notebook metadata for the specified notebook ID.
module Amazonka.Athena.GetNotebookMetadata
  ( -- * Creating a Request
    GetNotebookMetadata (..),
    newGetNotebookMetadata,

    -- * Request Lenses
    getNotebookMetadata_notebookId,

    -- * Destructuring the Response
    GetNotebookMetadataResponse (..),
    newGetNotebookMetadataResponse,

    -- * Response Lenses
    getNotebookMetadataResponse_notebookMetadata,
    getNotebookMetadataResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetNotebookMetadata' smart constructor.
data GetNotebookMetadata = GetNotebookMetadata'
  { -- | The ID of the notebook whose metadata is to be retrieved.
    notebookId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetNotebookMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notebookId', 'getNotebookMetadata_notebookId' - The ID of the notebook whose metadata is to be retrieved.
newGetNotebookMetadata ::
  -- | 'notebookId'
  Prelude.Text ->
  GetNotebookMetadata
newGetNotebookMetadata pNotebookId_ =
  GetNotebookMetadata' {notebookId = pNotebookId_}

-- | The ID of the notebook whose metadata is to be retrieved.
getNotebookMetadata_notebookId :: Lens.Lens' GetNotebookMetadata Prelude.Text
getNotebookMetadata_notebookId = Lens.lens (\GetNotebookMetadata' {notebookId} -> notebookId) (\s@GetNotebookMetadata' {} a -> s {notebookId = a} :: GetNotebookMetadata)

instance Core.AWSRequest GetNotebookMetadata where
  type
    AWSResponse GetNotebookMetadata =
      GetNotebookMetadataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetNotebookMetadataResponse'
            Prelude.<$> (x Data..?> "NotebookMetadata")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetNotebookMetadata where
  hashWithSalt _salt GetNotebookMetadata' {..} =
    _salt `Prelude.hashWithSalt` notebookId

instance Prelude.NFData GetNotebookMetadata where
  rnf GetNotebookMetadata' {..} = Prelude.rnf notebookId

instance Data.ToHeaders GetNotebookMetadata where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.GetNotebookMetadata" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetNotebookMetadata where
  toJSON GetNotebookMetadata' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("NotebookId" Data..= notebookId)]
      )

instance Data.ToPath GetNotebookMetadata where
  toPath = Prelude.const "/"

instance Data.ToQuery GetNotebookMetadata where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetNotebookMetadataResponse' smart constructor.
data GetNotebookMetadataResponse = GetNotebookMetadataResponse'
  { -- | The metadata that is returned for the specified notebook ID.
    notebookMetadata :: Prelude.Maybe NotebookMetadata,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetNotebookMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notebookMetadata', 'getNotebookMetadataResponse_notebookMetadata' - The metadata that is returned for the specified notebook ID.
--
-- 'httpStatus', 'getNotebookMetadataResponse_httpStatus' - The response's http status code.
newGetNotebookMetadataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetNotebookMetadataResponse
newGetNotebookMetadataResponse pHttpStatus_ =
  GetNotebookMetadataResponse'
    { notebookMetadata =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The metadata that is returned for the specified notebook ID.
getNotebookMetadataResponse_notebookMetadata :: Lens.Lens' GetNotebookMetadataResponse (Prelude.Maybe NotebookMetadata)
getNotebookMetadataResponse_notebookMetadata = Lens.lens (\GetNotebookMetadataResponse' {notebookMetadata} -> notebookMetadata) (\s@GetNotebookMetadataResponse' {} a -> s {notebookMetadata = a} :: GetNotebookMetadataResponse)

-- | The response's http status code.
getNotebookMetadataResponse_httpStatus :: Lens.Lens' GetNotebookMetadataResponse Prelude.Int
getNotebookMetadataResponse_httpStatus = Lens.lens (\GetNotebookMetadataResponse' {httpStatus} -> httpStatus) (\s@GetNotebookMetadataResponse' {} a -> s {httpStatus = a} :: GetNotebookMetadataResponse)

instance Prelude.NFData GetNotebookMetadataResponse where
  rnf GetNotebookMetadataResponse' {..} =
    Prelude.rnf notebookMetadata `Prelude.seq`
      Prelude.rnf httpStatus

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
-- Module      : Amazonka.BackupStorage.ListChunks
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List chunks in a given Object
module Amazonka.BackupStorage.ListChunks
  ( -- * Creating a Request
    ListChunks (..),
    newListChunks,

    -- * Request Lenses
    listChunks_nextToken,
    listChunks_maxResults,
    listChunks_storageJobId,
    listChunks_objectToken,

    -- * Destructuring the Response
    ListChunksResponse (..),
    newListChunksResponse,

    -- * Response Lenses
    listChunksResponse_nextToken,
    listChunksResponse_httpStatus,
    listChunksResponse_chunkList,
  )
where

import Amazonka.BackupStorage.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListChunks' smart constructor.
data ListChunks = ListChunks'
  { -- | Pagination token
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Maximum number of chunks
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Storage job id
    storageJobId :: Prelude.Text,
    -- | Object token
    objectToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListChunks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listChunks_nextToken' - Pagination token
--
-- 'maxResults', 'listChunks_maxResults' - Maximum number of chunks
--
-- 'storageJobId', 'listChunks_storageJobId' - Storage job id
--
-- 'objectToken', 'listChunks_objectToken' - Object token
newListChunks ::
  -- | 'storageJobId'
  Prelude.Text ->
  -- | 'objectToken'
  Prelude.Text ->
  ListChunks
newListChunks pStorageJobId_ pObjectToken_ =
  ListChunks'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      storageJobId = pStorageJobId_,
      objectToken = pObjectToken_
    }

-- | Pagination token
listChunks_nextToken :: Lens.Lens' ListChunks (Prelude.Maybe Prelude.Text)
listChunks_nextToken = Lens.lens (\ListChunks' {nextToken} -> nextToken) (\s@ListChunks' {} a -> s {nextToken = a} :: ListChunks)

-- | Maximum number of chunks
listChunks_maxResults :: Lens.Lens' ListChunks (Prelude.Maybe Prelude.Natural)
listChunks_maxResults = Lens.lens (\ListChunks' {maxResults} -> maxResults) (\s@ListChunks' {} a -> s {maxResults = a} :: ListChunks)

-- | Storage job id
listChunks_storageJobId :: Lens.Lens' ListChunks Prelude.Text
listChunks_storageJobId = Lens.lens (\ListChunks' {storageJobId} -> storageJobId) (\s@ListChunks' {} a -> s {storageJobId = a} :: ListChunks)

-- | Object token
listChunks_objectToken :: Lens.Lens' ListChunks Prelude.Text
listChunks_objectToken = Lens.lens (\ListChunks' {objectToken} -> objectToken) (\s@ListChunks' {} a -> s {objectToken = a} :: ListChunks)

instance Core.AWSRequest ListChunks where
  type AWSResponse ListChunks = ListChunksResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListChunksResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "ChunkList" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListChunks where
  hashWithSalt _salt ListChunks' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` storageJobId
      `Prelude.hashWithSalt` objectToken

instance Prelude.NFData ListChunks where
  rnf ListChunks' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf storageJobId
      `Prelude.seq` Prelude.rnf objectToken

instance Data.ToHeaders ListChunks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListChunks where
  toPath ListChunks' {..} =
    Prelude.mconcat
      [ "/restore-jobs/",
        Data.toBS storageJobId,
        "/chunks/",
        Data.toBS objectToken,
        "/list"
      ]

instance Data.ToQuery ListChunks where
  toQuery ListChunks' {..} =
    Prelude.mconcat
      [ "next-token" Data.=: nextToken,
        "max-results" Data.=: maxResults
      ]

-- | /See:/ 'newListChunksResponse' smart constructor.
data ListChunksResponse = ListChunksResponse'
  { -- | Pagination token
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | List of chunks
    chunkList :: [Chunk]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListChunksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listChunksResponse_nextToken' - Pagination token
--
-- 'httpStatus', 'listChunksResponse_httpStatus' - The response's http status code.
--
-- 'chunkList', 'listChunksResponse_chunkList' - List of chunks
newListChunksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListChunksResponse
newListChunksResponse pHttpStatus_ =
  ListChunksResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      chunkList = Prelude.mempty
    }

-- | Pagination token
listChunksResponse_nextToken :: Lens.Lens' ListChunksResponse (Prelude.Maybe Prelude.Text)
listChunksResponse_nextToken = Lens.lens (\ListChunksResponse' {nextToken} -> nextToken) (\s@ListChunksResponse' {} a -> s {nextToken = a} :: ListChunksResponse)

-- | The response's http status code.
listChunksResponse_httpStatus :: Lens.Lens' ListChunksResponse Prelude.Int
listChunksResponse_httpStatus = Lens.lens (\ListChunksResponse' {httpStatus} -> httpStatus) (\s@ListChunksResponse' {} a -> s {httpStatus = a} :: ListChunksResponse)

-- | List of chunks
listChunksResponse_chunkList :: Lens.Lens' ListChunksResponse [Chunk]
listChunksResponse_chunkList = Lens.lens (\ListChunksResponse' {chunkList} -> chunkList) (\s@ListChunksResponse' {} a -> s {chunkList = a} :: ListChunksResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListChunksResponse where
  rnf ListChunksResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf chunkList

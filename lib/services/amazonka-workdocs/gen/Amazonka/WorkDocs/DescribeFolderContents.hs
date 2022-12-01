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
-- Module      : Amazonka.WorkDocs.DescribeFolderContents
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the contents of the specified folder, including its documents
-- and subfolders.
--
-- By default, Amazon WorkDocs returns the first 100 active document and
-- folder metadata items. If there are more results, the response includes
-- a marker that you can use to request the next set of results. You can
-- also request initialized documents.
--
-- This operation returns paginated results.
module Amazonka.WorkDocs.DescribeFolderContents
  ( -- * Creating a Request
    DescribeFolderContents (..),
    newDescribeFolderContents,

    -- * Request Lenses
    describeFolderContents_type,
    describeFolderContents_marker,
    describeFolderContents_authenticationToken,
    describeFolderContents_sort,
    describeFolderContents_limit,
    describeFolderContents_order,
    describeFolderContents_include,
    describeFolderContents_folderId,

    -- * Destructuring the Response
    DescribeFolderContentsResponse (..),
    newDescribeFolderContentsResponse,

    -- * Response Lenses
    describeFolderContentsResponse_marker,
    describeFolderContentsResponse_folders,
    describeFolderContentsResponse_documents,
    describeFolderContentsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkDocs.Types

-- | /See:/ 'newDescribeFolderContents' smart constructor.
data DescribeFolderContents = DescribeFolderContents'
  { -- | The type of items.
    type' :: Prelude.Maybe FolderContentType,
    -- | The marker for the next set of results. This marker was received from a
    -- previous call.
    marker :: Prelude.Maybe Prelude.Text,
    -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The sorting criteria.
    sort :: Prelude.Maybe ResourceSortType,
    -- | The maximum number of items to return with this call.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The order for the contents of the folder.
    order :: Prelude.Maybe OrderType,
    -- | The contents to include. Specify \"INITIALIZED\" to include initialized
    -- documents.
    include :: Prelude.Maybe Prelude.Text,
    -- | The ID of the folder.
    folderId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFolderContents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'describeFolderContents_type' - The type of items.
--
-- 'marker', 'describeFolderContents_marker' - The marker for the next set of results. This marker was received from a
-- previous call.
--
-- 'authenticationToken', 'describeFolderContents_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'sort', 'describeFolderContents_sort' - The sorting criteria.
--
-- 'limit', 'describeFolderContents_limit' - The maximum number of items to return with this call.
--
-- 'order', 'describeFolderContents_order' - The order for the contents of the folder.
--
-- 'include', 'describeFolderContents_include' - The contents to include. Specify \"INITIALIZED\" to include initialized
-- documents.
--
-- 'folderId', 'describeFolderContents_folderId' - The ID of the folder.
newDescribeFolderContents ::
  -- | 'folderId'
  Prelude.Text ->
  DescribeFolderContents
newDescribeFolderContents pFolderId_ =
  DescribeFolderContents'
    { type' = Prelude.Nothing,
      marker = Prelude.Nothing,
      authenticationToken = Prelude.Nothing,
      sort = Prelude.Nothing,
      limit = Prelude.Nothing,
      order = Prelude.Nothing,
      include = Prelude.Nothing,
      folderId = pFolderId_
    }

-- | The type of items.
describeFolderContents_type :: Lens.Lens' DescribeFolderContents (Prelude.Maybe FolderContentType)
describeFolderContents_type = Lens.lens (\DescribeFolderContents' {type'} -> type') (\s@DescribeFolderContents' {} a -> s {type' = a} :: DescribeFolderContents)

-- | The marker for the next set of results. This marker was received from a
-- previous call.
describeFolderContents_marker :: Lens.Lens' DescribeFolderContents (Prelude.Maybe Prelude.Text)
describeFolderContents_marker = Lens.lens (\DescribeFolderContents' {marker} -> marker) (\s@DescribeFolderContents' {} a -> s {marker = a} :: DescribeFolderContents)

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
describeFolderContents_authenticationToken :: Lens.Lens' DescribeFolderContents (Prelude.Maybe Prelude.Text)
describeFolderContents_authenticationToken = Lens.lens (\DescribeFolderContents' {authenticationToken} -> authenticationToken) (\s@DescribeFolderContents' {} a -> s {authenticationToken = a} :: DescribeFolderContents) Prelude.. Lens.mapping Core._Sensitive

-- | The sorting criteria.
describeFolderContents_sort :: Lens.Lens' DescribeFolderContents (Prelude.Maybe ResourceSortType)
describeFolderContents_sort = Lens.lens (\DescribeFolderContents' {sort} -> sort) (\s@DescribeFolderContents' {} a -> s {sort = a} :: DescribeFolderContents)

-- | The maximum number of items to return with this call.
describeFolderContents_limit :: Lens.Lens' DescribeFolderContents (Prelude.Maybe Prelude.Natural)
describeFolderContents_limit = Lens.lens (\DescribeFolderContents' {limit} -> limit) (\s@DescribeFolderContents' {} a -> s {limit = a} :: DescribeFolderContents)

-- | The order for the contents of the folder.
describeFolderContents_order :: Lens.Lens' DescribeFolderContents (Prelude.Maybe OrderType)
describeFolderContents_order = Lens.lens (\DescribeFolderContents' {order} -> order) (\s@DescribeFolderContents' {} a -> s {order = a} :: DescribeFolderContents)

-- | The contents to include. Specify \"INITIALIZED\" to include initialized
-- documents.
describeFolderContents_include :: Lens.Lens' DescribeFolderContents (Prelude.Maybe Prelude.Text)
describeFolderContents_include = Lens.lens (\DescribeFolderContents' {include} -> include) (\s@DescribeFolderContents' {} a -> s {include = a} :: DescribeFolderContents)

-- | The ID of the folder.
describeFolderContents_folderId :: Lens.Lens' DescribeFolderContents Prelude.Text
describeFolderContents_folderId = Lens.lens (\DescribeFolderContents' {folderId} -> folderId) (\s@DescribeFolderContents' {} a -> s {folderId = a} :: DescribeFolderContents)

instance Core.AWSPager DescribeFolderContents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeFolderContentsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeFolderContentsResponse_folders
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeFolderContentsResponse_documents
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeFolderContents_marker
          Lens..~ rs
          Lens.^? describeFolderContentsResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeFolderContents where
  type
    AWSResponse DescribeFolderContents =
      DescribeFolderContentsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFolderContentsResponse'
            Prelude.<$> (x Core..?> "Marker")
            Prelude.<*> (x Core..?> "Folders" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "Documents" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFolderContents where
  hashWithSalt _salt DescribeFolderContents' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` authenticationToken
      `Prelude.hashWithSalt` sort
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` order
      `Prelude.hashWithSalt` include
      `Prelude.hashWithSalt` folderId

instance Prelude.NFData DescribeFolderContents where
  rnf DescribeFolderContents' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf authenticationToken
      `Prelude.seq` Prelude.rnf sort
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf order
      `Prelude.seq` Prelude.rnf include
      `Prelude.seq` Prelude.rnf folderId

instance Core.ToHeaders DescribeFolderContents where
  toHeaders DescribeFolderContents' {..} =
    Prelude.mconcat
      [ "Authentication" Core.=# authenticationToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToPath DescribeFolderContents where
  toPath DescribeFolderContents' {..} =
    Prelude.mconcat
      ["/api/v1/folders/", Core.toBS folderId, "/contents"]

instance Core.ToQuery DescribeFolderContents where
  toQuery DescribeFolderContents' {..} =
    Prelude.mconcat
      [ "type" Core.=: type',
        "marker" Core.=: marker,
        "sort" Core.=: sort,
        "limit" Core.=: limit,
        "order" Core.=: order,
        "include" Core.=: include
      ]

-- | /See:/ 'newDescribeFolderContentsResponse' smart constructor.
data DescribeFolderContentsResponse = DescribeFolderContentsResponse'
  { -- | The marker to use when requesting the next set of results. If there are
    -- no additional results, the string is empty.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The subfolders in the specified folder.
    folders :: Prelude.Maybe [FolderMetadata],
    -- | The documents in the specified folder.
    documents :: Prelude.Maybe [DocumentMetadata],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFolderContentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeFolderContentsResponse_marker' - The marker to use when requesting the next set of results. If there are
-- no additional results, the string is empty.
--
-- 'folders', 'describeFolderContentsResponse_folders' - The subfolders in the specified folder.
--
-- 'documents', 'describeFolderContentsResponse_documents' - The documents in the specified folder.
--
-- 'httpStatus', 'describeFolderContentsResponse_httpStatus' - The response's http status code.
newDescribeFolderContentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFolderContentsResponse
newDescribeFolderContentsResponse pHttpStatus_ =
  DescribeFolderContentsResponse'
    { marker =
        Prelude.Nothing,
      folders = Prelude.Nothing,
      documents = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The marker to use when requesting the next set of results. If there are
-- no additional results, the string is empty.
describeFolderContentsResponse_marker :: Lens.Lens' DescribeFolderContentsResponse (Prelude.Maybe Prelude.Text)
describeFolderContentsResponse_marker = Lens.lens (\DescribeFolderContentsResponse' {marker} -> marker) (\s@DescribeFolderContentsResponse' {} a -> s {marker = a} :: DescribeFolderContentsResponse)

-- | The subfolders in the specified folder.
describeFolderContentsResponse_folders :: Lens.Lens' DescribeFolderContentsResponse (Prelude.Maybe [FolderMetadata])
describeFolderContentsResponse_folders = Lens.lens (\DescribeFolderContentsResponse' {folders} -> folders) (\s@DescribeFolderContentsResponse' {} a -> s {folders = a} :: DescribeFolderContentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The documents in the specified folder.
describeFolderContentsResponse_documents :: Lens.Lens' DescribeFolderContentsResponse (Prelude.Maybe [DocumentMetadata])
describeFolderContentsResponse_documents = Lens.lens (\DescribeFolderContentsResponse' {documents} -> documents) (\s@DescribeFolderContentsResponse' {} a -> s {documents = a} :: DescribeFolderContentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeFolderContentsResponse_httpStatus :: Lens.Lens' DescribeFolderContentsResponse Prelude.Int
describeFolderContentsResponse_httpStatus = Lens.lens (\DescribeFolderContentsResponse' {httpStatus} -> httpStatus) (\s@DescribeFolderContentsResponse' {} a -> s {httpStatus = a} :: DescribeFolderContentsResponse)

instance
  Prelude.NFData
    DescribeFolderContentsResponse
  where
  rnf DescribeFolderContentsResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf folders
      `Prelude.seq` Prelude.rnf documents
      `Prelude.seq` Prelude.rnf httpStatus

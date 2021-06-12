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
-- Module      : Network.AWS.WorkDocs.DescribeFolderContents
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.WorkDocs.DescribeFolderContents
  ( -- * Creating a Request
    DescribeFolderContents (..),
    newDescribeFolderContents,

    -- * Request Lenses
    describeFolderContents_include,
    describeFolderContents_order,
    describeFolderContents_authenticationToken,
    describeFolderContents_type,
    describeFolderContents_limit,
    describeFolderContents_sort,
    describeFolderContents_marker,
    describeFolderContents_folderId,

    -- * Destructuring the Response
    DescribeFolderContentsResponse (..),
    newDescribeFolderContentsResponse,

    -- * Response Lenses
    describeFolderContentsResponse_documents,
    describeFolderContentsResponse_folders,
    describeFolderContentsResponse_marker,
    describeFolderContentsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newDescribeFolderContents' smart constructor.
data DescribeFolderContents = DescribeFolderContents'
  { -- | The contents to include. Specify \"INITIALIZED\" to include initialized
    -- documents.
    include :: Core.Maybe Core.Text,
    -- | The order for the contents of the folder.
    order :: Core.Maybe OrderType,
    -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The type of items.
    type' :: Core.Maybe FolderContentType,
    -- | The maximum number of items to return with this call.
    limit :: Core.Maybe Core.Natural,
    -- | The sorting criteria.
    sort :: Core.Maybe ResourceSortType,
    -- | The marker for the next set of results. This marker was received from a
    -- previous call.
    marker :: Core.Maybe Core.Text,
    -- | The ID of the folder.
    folderId :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeFolderContents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'include', 'describeFolderContents_include' - The contents to include. Specify \"INITIALIZED\" to include initialized
-- documents.
--
-- 'order', 'describeFolderContents_order' - The order for the contents of the folder.
--
-- 'authenticationToken', 'describeFolderContents_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'type'', 'describeFolderContents_type' - The type of items.
--
-- 'limit', 'describeFolderContents_limit' - The maximum number of items to return with this call.
--
-- 'sort', 'describeFolderContents_sort' - The sorting criteria.
--
-- 'marker', 'describeFolderContents_marker' - The marker for the next set of results. This marker was received from a
-- previous call.
--
-- 'folderId', 'describeFolderContents_folderId' - The ID of the folder.
newDescribeFolderContents ::
  -- | 'folderId'
  Core.Text ->
  DescribeFolderContents
newDescribeFolderContents pFolderId_ =
  DescribeFolderContents'
    { include = Core.Nothing,
      order = Core.Nothing,
      authenticationToken = Core.Nothing,
      type' = Core.Nothing,
      limit = Core.Nothing,
      sort = Core.Nothing,
      marker = Core.Nothing,
      folderId = pFolderId_
    }

-- | The contents to include. Specify \"INITIALIZED\" to include initialized
-- documents.
describeFolderContents_include :: Lens.Lens' DescribeFolderContents (Core.Maybe Core.Text)
describeFolderContents_include = Lens.lens (\DescribeFolderContents' {include} -> include) (\s@DescribeFolderContents' {} a -> s {include = a} :: DescribeFolderContents)

-- | The order for the contents of the folder.
describeFolderContents_order :: Lens.Lens' DescribeFolderContents (Core.Maybe OrderType)
describeFolderContents_order = Lens.lens (\DescribeFolderContents' {order} -> order) (\s@DescribeFolderContents' {} a -> s {order = a} :: DescribeFolderContents)

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
describeFolderContents_authenticationToken :: Lens.Lens' DescribeFolderContents (Core.Maybe Core.Text)
describeFolderContents_authenticationToken = Lens.lens (\DescribeFolderContents' {authenticationToken} -> authenticationToken) (\s@DescribeFolderContents' {} a -> s {authenticationToken = a} :: DescribeFolderContents) Core.. Lens.mapping Core._Sensitive

-- | The type of items.
describeFolderContents_type :: Lens.Lens' DescribeFolderContents (Core.Maybe FolderContentType)
describeFolderContents_type = Lens.lens (\DescribeFolderContents' {type'} -> type') (\s@DescribeFolderContents' {} a -> s {type' = a} :: DescribeFolderContents)

-- | The maximum number of items to return with this call.
describeFolderContents_limit :: Lens.Lens' DescribeFolderContents (Core.Maybe Core.Natural)
describeFolderContents_limit = Lens.lens (\DescribeFolderContents' {limit} -> limit) (\s@DescribeFolderContents' {} a -> s {limit = a} :: DescribeFolderContents)

-- | The sorting criteria.
describeFolderContents_sort :: Lens.Lens' DescribeFolderContents (Core.Maybe ResourceSortType)
describeFolderContents_sort = Lens.lens (\DescribeFolderContents' {sort} -> sort) (\s@DescribeFolderContents' {} a -> s {sort = a} :: DescribeFolderContents)

-- | The marker for the next set of results. This marker was received from a
-- previous call.
describeFolderContents_marker :: Lens.Lens' DescribeFolderContents (Core.Maybe Core.Text)
describeFolderContents_marker = Lens.lens (\DescribeFolderContents' {marker} -> marker) (\s@DescribeFolderContents' {} a -> s {marker = a} :: DescribeFolderContents)

-- | The ID of the folder.
describeFolderContents_folderId :: Lens.Lens' DescribeFolderContents Core.Text
describeFolderContents_folderId = Lens.lens (\DescribeFolderContents' {folderId} -> folderId) (\s@DescribeFolderContents' {} a -> s {folderId = a} :: DescribeFolderContents)

instance Core.AWSPager DescribeFolderContents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeFolderContentsResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeFolderContentsResponse_folders
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeFolderContentsResponse_documents
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeFolderContents_marker
          Lens..~ rs
          Lens.^? describeFolderContentsResponse_marker
            Core.. Lens._Just

instance Core.AWSRequest DescribeFolderContents where
  type
    AWSResponse DescribeFolderContents =
      DescribeFolderContentsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFolderContentsResponse'
            Core.<$> (x Core..?> "Documents" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "Folders" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeFolderContents

instance Core.NFData DescribeFolderContents

instance Core.ToHeaders DescribeFolderContents where
  toHeaders DescribeFolderContents' {..} =
    Core.mconcat
      [ "Authentication" Core.=# authenticationToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
      ]

instance Core.ToPath DescribeFolderContents where
  toPath DescribeFolderContents' {..} =
    Core.mconcat
      ["/api/v1/folders/", Core.toBS folderId, "/contents"]

instance Core.ToQuery DescribeFolderContents where
  toQuery DescribeFolderContents' {..} =
    Core.mconcat
      [ "include" Core.=: include,
        "order" Core.=: order,
        "type" Core.=: type',
        "limit" Core.=: limit,
        "sort" Core.=: sort,
        "marker" Core.=: marker
      ]

-- | /See:/ 'newDescribeFolderContentsResponse' smart constructor.
data DescribeFolderContentsResponse = DescribeFolderContentsResponse'
  { -- | The documents in the specified folder.
    documents :: Core.Maybe [DocumentMetadata],
    -- | The subfolders in the specified folder.
    folders :: Core.Maybe [FolderMetadata],
    -- | The marker to use when requesting the next set of results. If there are
    -- no additional results, the string is empty.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeFolderContentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documents', 'describeFolderContentsResponse_documents' - The documents in the specified folder.
--
-- 'folders', 'describeFolderContentsResponse_folders' - The subfolders in the specified folder.
--
-- 'marker', 'describeFolderContentsResponse_marker' - The marker to use when requesting the next set of results. If there are
-- no additional results, the string is empty.
--
-- 'httpStatus', 'describeFolderContentsResponse_httpStatus' - The response's http status code.
newDescribeFolderContentsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeFolderContentsResponse
newDescribeFolderContentsResponse pHttpStatus_ =
  DescribeFolderContentsResponse'
    { documents =
        Core.Nothing,
      folders = Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The documents in the specified folder.
describeFolderContentsResponse_documents :: Lens.Lens' DescribeFolderContentsResponse (Core.Maybe [DocumentMetadata])
describeFolderContentsResponse_documents = Lens.lens (\DescribeFolderContentsResponse' {documents} -> documents) (\s@DescribeFolderContentsResponse' {} a -> s {documents = a} :: DescribeFolderContentsResponse) Core.. Lens.mapping Lens._Coerce

-- | The subfolders in the specified folder.
describeFolderContentsResponse_folders :: Lens.Lens' DescribeFolderContentsResponse (Core.Maybe [FolderMetadata])
describeFolderContentsResponse_folders = Lens.lens (\DescribeFolderContentsResponse' {folders} -> folders) (\s@DescribeFolderContentsResponse' {} a -> s {folders = a} :: DescribeFolderContentsResponse) Core.. Lens.mapping Lens._Coerce

-- | The marker to use when requesting the next set of results. If there are
-- no additional results, the string is empty.
describeFolderContentsResponse_marker :: Lens.Lens' DescribeFolderContentsResponse (Core.Maybe Core.Text)
describeFolderContentsResponse_marker = Lens.lens (\DescribeFolderContentsResponse' {marker} -> marker) (\s@DescribeFolderContentsResponse' {} a -> s {marker = a} :: DescribeFolderContentsResponse)

-- | The response's http status code.
describeFolderContentsResponse_httpStatus :: Lens.Lens' DescribeFolderContentsResponse Core.Int
describeFolderContentsResponse_httpStatus = Lens.lens (\DescribeFolderContentsResponse' {httpStatus} -> httpStatus) (\s@DescribeFolderContentsResponse' {} a -> s {httpStatus = a} :: DescribeFolderContentsResponse)

instance Core.NFData DescribeFolderContentsResponse

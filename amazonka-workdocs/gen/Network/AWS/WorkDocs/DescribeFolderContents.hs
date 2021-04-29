{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newDescribeFolderContents' smart constructor.
data DescribeFolderContents = DescribeFolderContents'
  { -- | The contents to include. Specify \"INITIALIZED\" to include initialized
    -- documents.
    include :: Prelude.Maybe Prelude.Text,
    -- | The order for the contents of the folder.
    order :: Prelude.Maybe OrderType,
    -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The type of items.
    type' :: Prelude.Maybe FolderContentType,
    -- | The maximum number of items to return with this call.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The sorting criteria.
    sort :: Prelude.Maybe ResourceSortType,
    -- | The marker for the next set of results. This marker was received from a
    -- previous call.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The ID of the folder.
    folderId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DescribeFolderContents
newDescribeFolderContents pFolderId_ =
  DescribeFolderContents'
    { include = Prelude.Nothing,
      order = Prelude.Nothing,
      authenticationToken = Prelude.Nothing,
      type' = Prelude.Nothing,
      limit = Prelude.Nothing,
      sort = Prelude.Nothing,
      marker = Prelude.Nothing,
      folderId = pFolderId_
    }

-- | The contents to include. Specify \"INITIALIZED\" to include initialized
-- documents.
describeFolderContents_include :: Lens.Lens' DescribeFolderContents (Prelude.Maybe Prelude.Text)
describeFolderContents_include = Lens.lens (\DescribeFolderContents' {include} -> include) (\s@DescribeFolderContents' {} a -> s {include = a} :: DescribeFolderContents)

-- | The order for the contents of the folder.
describeFolderContents_order :: Lens.Lens' DescribeFolderContents (Prelude.Maybe OrderType)
describeFolderContents_order = Lens.lens (\DescribeFolderContents' {order} -> order) (\s@DescribeFolderContents' {} a -> s {order = a} :: DescribeFolderContents)

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
describeFolderContents_authenticationToken :: Lens.Lens' DescribeFolderContents (Prelude.Maybe Prelude.Text)
describeFolderContents_authenticationToken = Lens.lens (\DescribeFolderContents' {authenticationToken} -> authenticationToken) (\s@DescribeFolderContents' {} a -> s {authenticationToken = a} :: DescribeFolderContents) Prelude.. Lens.mapping Prelude._Sensitive

-- | The type of items.
describeFolderContents_type :: Lens.Lens' DescribeFolderContents (Prelude.Maybe FolderContentType)
describeFolderContents_type = Lens.lens (\DescribeFolderContents' {type'} -> type') (\s@DescribeFolderContents' {} a -> s {type' = a} :: DescribeFolderContents)

-- | The maximum number of items to return with this call.
describeFolderContents_limit :: Lens.Lens' DescribeFolderContents (Prelude.Maybe Prelude.Natural)
describeFolderContents_limit = Lens.lens (\DescribeFolderContents' {limit} -> limit) (\s@DescribeFolderContents' {} a -> s {limit = a} :: DescribeFolderContents)

-- | The sorting criteria.
describeFolderContents_sort :: Lens.Lens' DescribeFolderContents (Prelude.Maybe ResourceSortType)
describeFolderContents_sort = Lens.lens (\DescribeFolderContents' {sort} -> sort) (\s@DescribeFolderContents' {} a -> s {sort = a} :: DescribeFolderContents)

-- | The marker for the next set of results. This marker was received from a
-- previous call.
describeFolderContents_marker :: Lens.Lens' DescribeFolderContents (Prelude.Maybe Prelude.Text)
describeFolderContents_marker = Lens.lens (\DescribeFolderContents' {marker} -> marker) (\s@DescribeFolderContents' {} a -> s {marker = a} :: DescribeFolderContents)

-- | The ID of the folder.
describeFolderContents_folderId :: Lens.Lens' DescribeFolderContents Prelude.Text
describeFolderContents_folderId = Lens.lens (\DescribeFolderContents' {folderId} -> folderId) (\s@DescribeFolderContents' {} a -> s {folderId = a} :: DescribeFolderContents)

instance Pager.AWSPager DescribeFolderContents where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeFolderContentsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeFolderContentsResponse_folders
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeFolderContentsResponse_documents
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeFolderContents_marker
          Lens..~ rs
          Lens.^? describeFolderContentsResponse_marker
            Prelude.. Lens._Just

instance Prelude.AWSRequest DescribeFolderContents where
  type
    Rs DescribeFolderContents =
      DescribeFolderContentsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFolderContentsResponse'
            Prelude.<$> ( x Prelude..?> "Documents"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "Folders" Prelude..!@ Prelude.mempty)
            Prelude.<*> (x Prelude..?> "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFolderContents

instance Prelude.NFData DescribeFolderContents

instance Prelude.ToHeaders DescribeFolderContents where
  toHeaders DescribeFolderContents' {..} =
    Prelude.mconcat
      [ "Authentication" Prelude.=# authenticationToken,
        "Content-Type"
          Prelude.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Prelude.ToPath DescribeFolderContents where
  toPath DescribeFolderContents' {..} =
    Prelude.mconcat
      [ "/api/v1/folders/",
        Prelude.toBS folderId,
        "/contents"
      ]

instance Prelude.ToQuery DescribeFolderContents where
  toQuery DescribeFolderContents' {..} =
    Prelude.mconcat
      [ "include" Prelude.=: include,
        "order" Prelude.=: order,
        "type" Prelude.=: type',
        "limit" Prelude.=: limit,
        "sort" Prelude.=: sort,
        "marker" Prelude.=: marker
      ]

-- | /See:/ 'newDescribeFolderContentsResponse' smart constructor.
data DescribeFolderContentsResponse = DescribeFolderContentsResponse'
  { -- | The documents in the specified folder.
    documents :: Prelude.Maybe [DocumentMetadata],
    -- | The subfolders in the specified folder.
    folders :: Prelude.Maybe [FolderMetadata],
    -- | The marker to use when requesting the next set of results. If there are
    -- no additional results, the string is empty.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeFolderContentsResponse
newDescribeFolderContentsResponse pHttpStatus_ =
  DescribeFolderContentsResponse'
    { documents =
        Prelude.Nothing,
      folders = Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The documents in the specified folder.
describeFolderContentsResponse_documents :: Lens.Lens' DescribeFolderContentsResponse (Prelude.Maybe [DocumentMetadata])
describeFolderContentsResponse_documents = Lens.lens (\DescribeFolderContentsResponse' {documents} -> documents) (\s@DescribeFolderContentsResponse' {} a -> s {documents = a} :: DescribeFolderContentsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The subfolders in the specified folder.
describeFolderContentsResponse_folders :: Lens.Lens' DescribeFolderContentsResponse (Prelude.Maybe [FolderMetadata])
describeFolderContentsResponse_folders = Lens.lens (\DescribeFolderContentsResponse' {folders} -> folders) (\s@DescribeFolderContentsResponse' {} a -> s {folders = a} :: DescribeFolderContentsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The marker to use when requesting the next set of results. If there are
-- no additional results, the string is empty.
describeFolderContentsResponse_marker :: Lens.Lens' DescribeFolderContentsResponse (Prelude.Maybe Prelude.Text)
describeFolderContentsResponse_marker = Lens.lens (\DescribeFolderContentsResponse' {marker} -> marker) (\s@DescribeFolderContentsResponse' {} a -> s {marker = a} :: DescribeFolderContentsResponse)

-- | The response's http status code.
describeFolderContentsResponse_httpStatus :: Lens.Lens' DescribeFolderContentsResponse Prelude.Int
describeFolderContentsResponse_httpStatus = Lens.lens (\DescribeFolderContentsResponse' {httpStatus} -> httpStatus) (\s@DescribeFolderContentsResponse' {} a -> s {httpStatus = a} :: DescribeFolderContentsResponse)

instance
  Prelude.NFData
    DescribeFolderContentsResponse

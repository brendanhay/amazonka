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
-- Module      : Amazonka.QuickSight.SearchFolders
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches the subfolders in a folder.
module Amazonka.QuickSight.SearchFolders
  ( -- * Creating a Request
    SearchFolders (..),
    newSearchFolders,

    -- * Request Lenses
    searchFolders_nextToken,
    searchFolders_maxResults,
    searchFolders_awsAccountId,
    searchFolders_filters,

    -- * Destructuring the Response
    SearchFoldersResponse (..),
    newSearchFoldersResponse,

    -- * Response Lenses
    searchFoldersResponse_nextToken,
    searchFoldersResponse_folderSummaryList,
    searchFoldersResponse_requestId,
    searchFoldersResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchFolders' smart constructor.
data SearchFolders = SearchFolders'
  { -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID for the Amazon Web Services account that contains the folder.
    awsAccountId :: Prelude.Text,
    -- | The filters to apply to the search. Currently, you can search only by
    -- the parent folder ARN. For example,
    -- @\"Filters\": [ { \"Name\": \"PARENT_FOLDER_ARN\", \"Operator\": \"StringEquals\", \"Value\": \"arn:aws:quicksight:us-east-1:1:folder\/folderId\" } ]@.
    filters :: [FolderSearchFilter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchFolders' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchFolders_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'maxResults', 'searchFolders_maxResults' - The maximum number of results to be returned per request.
--
-- 'awsAccountId', 'searchFolders_awsAccountId' - The ID for the Amazon Web Services account that contains the folder.
--
-- 'filters', 'searchFolders_filters' - The filters to apply to the search. Currently, you can search only by
-- the parent folder ARN. For example,
-- @\"Filters\": [ { \"Name\": \"PARENT_FOLDER_ARN\", \"Operator\": \"StringEquals\", \"Value\": \"arn:aws:quicksight:us-east-1:1:folder\/folderId\" } ]@.
newSearchFolders ::
  -- | 'awsAccountId'
  Prelude.Text ->
  SearchFolders
newSearchFolders pAwsAccountId_ =
  SearchFolders'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      awsAccountId = pAwsAccountId_,
      filters = Prelude.mempty
    }

-- | The token for the next set of results, or null if there are no more
-- results.
searchFolders_nextToken :: Lens.Lens' SearchFolders (Prelude.Maybe Prelude.Text)
searchFolders_nextToken = Lens.lens (\SearchFolders' {nextToken} -> nextToken) (\s@SearchFolders' {} a -> s {nextToken = a} :: SearchFolders)

-- | The maximum number of results to be returned per request.
searchFolders_maxResults :: Lens.Lens' SearchFolders (Prelude.Maybe Prelude.Natural)
searchFolders_maxResults = Lens.lens (\SearchFolders' {maxResults} -> maxResults) (\s@SearchFolders' {} a -> s {maxResults = a} :: SearchFolders)

-- | The ID for the Amazon Web Services account that contains the folder.
searchFolders_awsAccountId :: Lens.Lens' SearchFolders Prelude.Text
searchFolders_awsAccountId = Lens.lens (\SearchFolders' {awsAccountId} -> awsAccountId) (\s@SearchFolders' {} a -> s {awsAccountId = a} :: SearchFolders)

-- | The filters to apply to the search. Currently, you can search only by
-- the parent folder ARN. For example,
-- @\"Filters\": [ { \"Name\": \"PARENT_FOLDER_ARN\", \"Operator\": \"StringEquals\", \"Value\": \"arn:aws:quicksight:us-east-1:1:folder\/folderId\" } ]@.
searchFolders_filters :: Lens.Lens' SearchFolders [FolderSearchFilter]
searchFolders_filters = Lens.lens (\SearchFolders' {filters} -> filters) (\s@SearchFolders' {} a -> s {filters = a} :: SearchFolders) Prelude.. Lens.coerced

instance Core.AWSRequest SearchFolders where
  type
    AWSResponse SearchFolders =
      SearchFoldersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchFoldersResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "FolderSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchFolders where
  hashWithSalt _salt SearchFolders' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` filters

instance Prelude.NFData SearchFolders where
  rnf SearchFolders' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf filters

instance Core.ToHeaders SearchFolders where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SearchFolders where
  toJSON SearchFolders' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("Filters" Core..= filters)
          ]
      )

instance Core.ToPath SearchFolders where
  toPath SearchFolders' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Core.toBS awsAccountId,
        "/search/folders"
      ]

instance Core.ToQuery SearchFolders where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchFoldersResponse' smart constructor.
data SearchFoldersResponse = SearchFoldersResponse'
  { -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A structure that contains all of the folders in the Amazon Web Services
    -- account. This structure provides basic information about the folders.
    folderSummaryList :: Prelude.Maybe [FolderSummary],
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchFoldersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchFoldersResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'folderSummaryList', 'searchFoldersResponse_folderSummaryList' - A structure that contains all of the folders in the Amazon Web Services
-- account. This structure provides basic information about the folders.
--
-- 'requestId', 'searchFoldersResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'searchFoldersResponse_status' - The HTTP status of the request.
newSearchFoldersResponse ::
  -- | 'status'
  Prelude.Int ->
  SearchFoldersResponse
newSearchFoldersResponse pStatus_ =
  SearchFoldersResponse'
    { nextToken = Prelude.Nothing,
      folderSummaryList = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | The token for the next set of results, or null if there are no more
-- results.
searchFoldersResponse_nextToken :: Lens.Lens' SearchFoldersResponse (Prelude.Maybe Prelude.Text)
searchFoldersResponse_nextToken = Lens.lens (\SearchFoldersResponse' {nextToken} -> nextToken) (\s@SearchFoldersResponse' {} a -> s {nextToken = a} :: SearchFoldersResponse)

-- | A structure that contains all of the folders in the Amazon Web Services
-- account. This structure provides basic information about the folders.
searchFoldersResponse_folderSummaryList :: Lens.Lens' SearchFoldersResponse (Prelude.Maybe [FolderSummary])
searchFoldersResponse_folderSummaryList = Lens.lens (\SearchFoldersResponse' {folderSummaryList} -> folderSummaryList) (\s@SearchFoldersResponse' {} a -> s {folderSummaryList = a} :: SearchFoldersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services request ID for this operation.
searchFoldersResponse_requestId :: Lens.Lens' SearchFoldersResponse (Prelude.Maybe Prelude.Text)
searchFoldersResponse_requestId = Lens.lens (\SearchFoldersResponse' {requestId} -> requestId) (\s@SearchFoldersResponse' {} a -> s {requestId = a} :: SearchFoldersResponse)

-- | The HTTP status of the request.
searchFoldersResponse_status :: Lens.Lens' SearchFoldersResponse Prelude.Int
searchFoldersResponse_status = Lens.lens (\SearchFoldersResponse' {status} -> status) (\s@SearchFoldersResponse' {} a -> s {status = a} :: SearchFoldersResponse)

instance Prelude.NFData SearchFoldersResponse where
  rnf SearchFoldersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf folderSummaryList
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status

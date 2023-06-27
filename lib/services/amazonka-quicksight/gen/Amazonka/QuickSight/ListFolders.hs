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
-- Module      : Amazonka.QuickSight.ListFolders
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all folders in an account.
module Amazonka.QuickSight.ListFolders
  ( -- * Creating a Request
    ListFolders (..),
    newListFolders,

    -- * Request Lenses
    listFolders_maxResults,
    listFolders_nextToken,
    listFolders_awsAccountId,

    -- * Destructuring the Response
    ListFoldersResponse (..),
    newListFoldersResponse,

    -- * Response Lenses
    listFoldersResponse_folderSummaryList,
    listFoldersResponse_nextToken,
    listFoldersResponse_requestId,
    listFoldersResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFolders' smart constructor.
data ListFolders = ListFolders'
  { -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID for the Amazon Web Services account that contains the folder.
    awsAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFolders' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listFolders_maxResults' - The maximum number of results to be returned per request.
--
-- 'nextToken', 'listFolders_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'awsAccountId', 'listFolders_awsAccountId' - The ID for the Amazon Web Services account that contains the folder.
newListFolders ::
  -- | 'awsAccountId'
  Prelude.Text ->
  ListFolders
newListFolders pAwsAccountId_ =
  ListFolders'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      awsAccountId = pAwsAccountId_
    }

-- | The maximum number of results to be returned per request.
listFolders_maxResults :: Lens.Lens' ListFolders (Prelude.Maybe Prelude.Natural)
listFolders_maxResults = Lens.lens (\ListFolders' {maxResults} -> maxResults) (\s@ListFolders' {} a -> s {maxResults = a} :: ListFolders)

-- | The token for the next set of results, or null if there are no more
-- results.
listFolders_nextToken :: Lens.Lens' ListFolders (Prelude.Maybe Prelude.Text)
listFolders_nextToken = Lens.lens (\ListFolders' {nextToken} -> nextToken) (\s@ListFolders' {} a -> s {nextToken = a} :: ListFolders)

-- | The ID for the Amazon Web Services account that contains the folder.
listFolders_awsAccountId :: Lens.Lens' ListFolders Prelude.Text
listFolders_awsAccountId = Lens.lens (\ListFolders' {awsAccountId} -> awsAccountId) (\s@ListFolders' {} a -> s {awsAccountId = a} :: ListFolders)

instance Core.AWSRequest ListFolders where
  type AWSResponse ListFolders = ListFoldersResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFoldersResponse'
            Prelude.<$> ( x
                            Data..?> "FolderSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFolders where
  hashWithSalt _salt ListFolders' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` awsAccountId

instance Prelude.NFData ListFolders where
  rnf ListFolders' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf awsAccountId

instance Data.ToHeaders ListFolders where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListFolders where
  toPath ListFolders' {..} =
    Prelude.mconcat
      ["/accounts/", Data.toBS awsAccountId, "/folders"]

instance Data.ToQuery ListFolders where
  toQuery ListFolders' {..} =
    Prelude.mconcat
      [ "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken
      ]

-- | /See:/ 'newListFoldersResponse' smart constructor.
data ListFoldersResponse = ListFoldersResponse'
  { -- | A structure that contains all of the folders in the Amazon Web Services
    -- account. This structure provides basic information about the folders.
    folderSummaryList :: Prelude.Maybe [FolderSummary],
    -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFoldersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'folderSummaryList', 'listFoldersResponse_folderSummaryList' - A structure that contains all of the folders in the Amazon Web Services
-- account. This structure provides basic information about the folders.
--
-- 'nextToken', 'listFoldersResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'requestId', 'listFoldersResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'listFoldersResponse_status' - The HTTP status of the request.
newListFoldersResponse ::
  -- | 'status'
  Prelude.Int ->
  ListFoldersResponse
newListFoldersResponse pStatus_ =
  ListFoldersResponse'
    { folderSummaryList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | A structure that contains all of the folders in the Amazon Web Services
-- account. This structure provides basic information about the folders.
listFoldersResponse_folderSummaryList :: Lens.Lens' ListFoldersResponse (Prelude.Maybe [FolderSummary])
listFoldersResponse_folderSummaryList = Lens.lens (\ListFoldersResponse' {folderSummaryList} -> folderSummaryList) (\s@ListFoldersResponse' {} a -> s {folderSummaryList = a} :: ListFoldersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or null if there are no more
-- results.
listFoldersResponse_nextToken :: Lens.Lens' ListFoldersResponse (Prelude.Maybe Prelude.Text)
listFoldersResponse_nextToken = Lens.lens (\ListFoldersResponse' {nextToken} -> nextToken) (\s@ListFoldersResponse' {} a -> s {nextToken = a} :: ListFoldersResponse)

-- | The Amazon Web Services request ID for this operation.
listFoldersResponse_requestId :: Lens.Lens' ListFoldersResponse (Prelude.Maybe Prelude.Text)
listFoldersResponse_requestId = Lens.lens (\ListFoldersResponse' {requestId} -> requestId) (\s@ListFoldersResponse' {} a -> s {requestId = a} :: ListFoldersResponse)

-- | The HTTP status of the request.
listFoldersResponse_status :: Lens.Lens' ListFoldersResponse Prelude.Int
listFoldersResponse_status = Lens.lens (\ListFoldersResponse' {status} -> status) (\s@ListFoldersResponse' {} a -> s {status = a} :: ListFoldersResponse)

instance Prelude.NFData ListFoldersResponse where
  rnf ListFoldersResponse' {..} =
    Prelude.rnf folderSummaryList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status

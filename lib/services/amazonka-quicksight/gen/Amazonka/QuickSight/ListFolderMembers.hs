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
-- Module      : Amazonka.QuickSight.ListFolderMembers
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all assets (@DASHBOARD@, @ANALYSIS@, and @DATASET@) in a folder.
module Amazonka.QuickSight.ListFolderMembers
  ( -- * Creating a Request
    ListFolderMembers (..),
    newListFolderMembers,

    -- * Request Lenses
    listFolderMembers_nextToken,
    listFolderMembers_maxResults,
    listFolderMembers_awsAccountId,
    listFolderMembers_folderId,

    -- * Destructuring the Response
    ListFolderMembersResponse (..),
    newListFolderMembersResponse,

    -- * Response Lenses
    listFolderMembersResponse_nextToken,
    listFolderMembersResponse_requestId,
    listFolderMembersResponse_folderMemberList,
    listFolderMembersResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFolderMembers' smart constructor.
data ListFolderMembers = ListFolderMembers'
  { -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID for the Amazon Web Services account that contains the folder.
    awsAccountId :: Prelude.Text,
    -- | The ID of the folder.
    folderId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFolderMembers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFolderMembers_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'maxResults', 'listFolderMembers_maxResults' - The maximum number of results to be returned per request.
--
-- 'awsAccountId', 'listFolderMembers_awsAccountId' - The ID for the Amazon Web Services account that contains the folder.
--
-- 'folderId', 'listFolderMembers_folderId' - The ID of the folder.
newListFolderMembers ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'folderId'
  Prelude.Text ->
  ListFolderMembers
newListFolderMembers pAwsAccountId_ pFolderId_ =
  ListFolderMembers'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      awsAccountId = pAwsAccountId_,
      folderId = pFolderId_
    }

-- | The token for the next set of results, or null if there are no more
-- results.
listFolderMembers_nextToken :: Lens.Lens' ListFolderMembers (Prelude.Maybe Prelude.Text)
listFolderMembers_nextToken = Lens.lens (\ListFolderMembers' {nextToken} -> nextToken) (\s@ListFolderMembers' {} a -> s {nextToken = a} :: ListFolderMembers)

-- | The maximum number of results to be returned per request.
listFolderMembers_maxResults :: Lens.Lens' ListFolderMembers (Prelude.Maybe Prelude.Natural)
listFolderMembers_maxResults = Lens.lens (\ListFolderMembers' {maxResults} -> maxResults) (\s@ListFolderMembers' {} a -> s {maxResults = a} :: ListFolderMembers)

-- | The ID for the Amazon Web Services account that contains the folder.
listFolderMembers_awsAccountId :: Lens.Lens' ListFolderMembers Prelude.Text
listFolderMembers_awsAccountId = Lens.lens (\ListFolderMembers' {awsAccountId} -> awsAccountId) (\s@ListFolderMembers' {} a -> s {awsAccountId = a} :: ListFolderMembers)

-- | The ID of the folder.
listFolderMembers_folderId :: Lens.Lens' ListFolderMembers Prelude.Text
listFolderMembers_folderId = Lens.lens (\ListFolderMembers' {folderId} -> folderId) (\s@ListFolderMembers' {} a -> s {folderId = a} :: ListFolderMembers)

instance Core.AWSRequest ListFolderMembers where
  type
    AWSResponse ListFolderMembers =
      ListFolderMembersResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFolderMembersResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "RequestId")
            Prelude.<*> ( x Core..?> "FolderMemberList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFolderMembers where
  hashWithSalt _salt ListFolderMembers' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` folderId

instance Prelude.NFData ListFolderMembers where
  rnf ListFolderMembers' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf folderId

instance Core.ToHeaders ListFolderMembers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListFolderMembers where
  toPath ListFolderMembers' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Core.toBS awsAccountId,
        "/folders/",
        Core.toBS folderId,
        "/members"
      ]

instance Core.ToQuery ListFolderMembers where
  toQuery ListFolderMembers' {..} =
    Prelude.mconcat
      [ "next-token" Core.=: nextToken,
        "max-results" Core.=: maxResults
      ]

-- | /See:/ 'newListFolderMembersResponse' smart constructor.
data ListFolderMembersResponse = ListFolderMembersResponse'
  { -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | A structure that contains all of the folder members (dashboards,
    -- analyses, and datasets) in the folder.
    folderMemberList :: Prelude.Maybe [MemberIdArnPair],
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFolderMembersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFolderMembersResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'requestId', 'listFolderMembersResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'folderMemberList', 'listFolderMembersResponse_folderMemberList' - A structure that contains all of the folder members (dashboards,
-- analyses, and datasets) in the folder.
--
-- 'status', 'listFolderMembersResponse_status' - The HTTP status of the request.
newListFolderMembersResponse ::
  -- | 'status'
  Prelude.Int ->
  ListFolderMembersResponse
newListFolderMembersResponse pStatus_ =
  ListFolderMembersResponse'
    { nextToken =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      folderMemberList = Prelude.Nothing,
      status = pStatus_
    }

-- | The token for the next set of results, or null if there are no more
-- results.
listFolderMembersResponse_nextToken :: Lens.Lens' ListFolderMembersResponse (Prelude.Maybe Prelude.Text)
listFolderMembersResponse_nextToken = Lens.lens (\ListFolderMembersResponse' {nextToken} -> nextToken) (\s@ListFolderMembersResponse' {} a -> s {nextToken = a} :: ListFolderMembersResponse)

-- | The Amazon Web Services request ID for this operation.
listFolderMembersResponse_requestId :: Lens.Lens' ListFolderMembersResponse (Prelude.Maybe Prelude.Text)
listFolderMembersResponse_requestId = Lens.lens (\ListFolderMembersResponse' {requestId} -> requestId) (\s@ListFolderMembersResponse' {} a -> s {requestId = a} :: ListFolderMembersResponse)

-- | A structure that contains all of the folder members (dashboards,
-- analyses, and datasets) in the folder.
listFolderMembersResponse_folderMemberList :: Lens.Lens' ListFolderMembersResponse (Prelude.Maybe [MemberIdArnPair])
listFolderMembersResponse_folderMemberList = Lens.lens (\ListFolderMembersResponse' {folderMemberList} -> folderMemberList) (\s@ListFolderMembersResponse' {} a -> s {folderMemberList = a} :: ListFolderMembersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The HTTP status of the request.
listFolderMembersResponse_status :: Lens.Lens' ListFolderMembersResponse Prelude.Int
listFolderMembersResponse_status = Lens.lens (\ListFolderMembersResponse' {status} -> status) (\s@ListFolderMembersResponse' {} a -> s {status = a} :: ListFolderMembersResponse)

instance Prelude.NFData ListFolderMembersResponse where
  rnf ListFolderMembersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf folderMemberList
      `Prelude.seq` Prelude.rnf status

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
-- Module      : Amazonka.SSMContacts.ListPageReceipts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the engagements to contact channels that have been
-- acknowledged.
--
-- This operation returns paginated results.
module Amazonka.SSMContacts.ListPageReceipts
  ( -- * Creating a Request
    ListPageReceipts (..),
    newListPageReceipts,

    -- * Request Lenses
    listPageReceipts_maxResults,
    listPageReceipts_nextToken,
    listPageReceipts_pageId,

    -- * Destructuring the Response
    ListPageReceiptsResponse (..),
    newListPageReceiptsResponse,

    -- * Response Lenses
    listPageReceiptsResponse_nextToken,
    listPageReceiptsResponse_receipts,
    listPageReceiptsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMContacts.Types

-- | /See:/ 'newListPageReceipts' smart constructor.
data ListPageReceipts = ListPageReceipts'
  { -- | The maximum number of acknowledgements per page of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token to continue to the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the engagement to a specific contact
    -- channel.
    pageId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPageReceipts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listPageReceipts_maxResults' - The maximum number of acknowledgements per page of results.
--
-- 'nextToken', 'listPageReceipts_nextToken' - The pagination token to continue to the next page of results.
--
-- 'pageId', 'listPageReceipts_pageId' - The Amazon Resource Name (ARN) of the engagement to a specific contact
-- channel.
newListPageReceipts ::
  -- | 'pageId'
  Prelude.Text ->
  ListPageReceipts
newListPageReceipts pPageId_ =
  ListPageReceipts'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      pageId = pPageId_
    }

-- | The maximum number of acknowledgements per page of results.
listPageReceipts_maxResults :: Lens.Lens' ListPageReceipts (Prelude.Maybe Prelude.Natural)
listPageReceipts_maxResults = Lens.lens (\ListPageReceipts' {maxResults} -> maxResults) (\s@ListPageReceipts' {} a -> s {maxResults = a} :: ListPageReceipts)

-- | The pagination token to continue to the next page of results.
listPageReceipts_nextToken :: Lens.Lens' ListPageReceipts (Prelude.Maybe Prelude.Text)
listPageReceipts_nextToken = Lens.lens (\ListPageReceipts' {nextToken} -> nextToken) (\s@ListPageReceipts' {} a -> s {nextToken = a} :: ListPageReceipts)

-- | The Amazon Resource Name (ARN) of the engagement to a specific contact
-- channel.
listPageReceipts_pageId :: Lens.Lens' ListPageReceipts Prelude.Text
listPageReceipts_pageId = Lens.lens (\ListPageReceipts' {pageId} -> pageId) (\s@ListPageReceipts' {} a -> s {pageId = a} :: ListPageReceipts)

instance Core.AWSPager ListPageReceipts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPageReceiptsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPageReceiptsResponse_receipts
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listPageReceipts_nextToken
              Lens..~ rs
              Lens.^? listPageReceiptsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListPageReceipts where
  type
    AWSResponse ListPageReceipts =
      ListPageReceiptsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPageReceiptsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Receipts" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPageReceipts where
  hashWithSalt _salt ListPageReceipts' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` pageId

instance Prelude.NFData ListPageReceipts where
  rnf ListPageReceipts' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf pageId

instance Data.ToHeaders ListPageReceipts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SSMContacts.ListPageReceipts" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListPageReceipts where
  toJSON ListPageReceipts' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("PageId" Data..= pageId)
          ]
      )

instance Data.ToPath ListPageReceipts where
  toPath = Prelude.const "/"

instance Data.ToQuery ListPageReceipts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPageReceiptsResponse' smart constructor.
data ListPageReceiptsResponse = ListPageReceiptsResponse'
  { -- | The pagination token to continue to the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of each acknowledgement.
    receipts :: Prelude.Maybe [Receipt],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPageReceiptsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPageReceiptsResponse_nextToken' - The pagination token to continue to the next page of results.
--
-- 'receipts', 'listPageReceiptsResponse_receipts' - A list of each acknowledgement.
--
-- 'httpStatus', 'listPageReceiptsResponse_httpStatus' - The response's http status code.
newListPageReceiptsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPageReceiptsResponse
newListPageReceiptsResponse pHttpStatus_ =
  ListPageReceiptsResponse'
    { nextToken =
        Prelude.Nothing,
      receipts = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token to continue to the next page of results.
listPageReceiptsResponse_nextToken :: Lens.Lens' ListPageReceiptsResponse (Prelude.Maybe Prelude.Text)
listPageReceiptsResponse_nextToken = Lens.lens (\ListPageReceiptsResponse' {nextToken} -> nextToken) (\s@ListPageReceiptsResponse' {} a -> s {nextToken = a} :: ListPageReceiptsResponse)

-- | A list of each acknowledgement.
listPageReceiptsResponse_receipts :: Lens.Lens' ListPageReceiptsResponse (Prelude.Maybe [Receipt])
listPageReceiptsResponse_receipts = Lens.lens (\ListPageReceiptsResponse' {receipts} -> receipts) (\s@ListPageReceiptsResponse' {} a -> s {receipts = a} :: ListPageReceiptsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPageReceiptsResponse_httpStatus :: Lens.Lens' ListPageReceiptsResponse Prelude.Int
listPageReceiptsResponse_httpStatus = Lens.lens (\ListPageReceiptsResponse' {httpStatus} -> httpStatus) (\s@ListPageReceiptsResponse' {} a -> s {httpStatus = a} :: ListPageReceiptsResponse)

instance Prelude.NFData ListPageReceiptsResponse where
  rnf ListPageReceiptsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf receipts `Prelude.seq`
        Prelude.rnf httpStatus

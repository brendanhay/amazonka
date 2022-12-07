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
-- Module      : Amazonka.Kendra.ListQuerySuggestionsBlockLists
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the block lists used for query suggestions for an index.
--
-- For information on the current quota limits for block lists, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/quotas.html Quotas for Amazon Kendra>.
--
-- @ListQuerySuggestionsBlockLists@ is currently not supported in the
-- Amazon Web Services GovCloud (US-West) region.
module Amazonka.Kendra.ListQuerySuggestionsBlockLists
  ( -- * Creating a Request
    ListQuerySuggestionsBlockLists (..),
    newListQuerySuggestionsBlockLists,

    -- * Request Lenses
    listQuerySuggestionsBlockLists_nextToken,
    listQuerySuggestionsBlockLists_maxResults,
    listQuerySuggestionsBlockLists_indexId,

    -- * Destructuring the Response
    ListQuerySuggestionsBlockListsResponse (..),
    newListQuerySuggestionsBlockListsResponse,

    -- * Response Lenses
    listQuerySuggestionsBlockListsResponse_nextToken,
    listQuerySuggestionsBlockListsResponse_blockListSummaryItems,
    listQuerySuggestionsBlockListsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListQuerySuggestionsBlockLists' smart constructor.
data ListQuerySuggestionsBlockLists = ListQuerySuggestionsBlockLists'
  { -- | If the previous response was incomplete (because there is more data to
    -- retrieve), Amazon Kendra returns a pagination token in the response. You
    -- can use this pagination token to retrieve the next set of block lists
    -- (@BlockListSummaryItems@).
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of block lists to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The identifier of the index for a list of all block lists that exist for
    -- that index.
    --
    -- For information on the current quota limits for block lists, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/quotas.html Quotas for Amazon Kendra>.
    indexId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListQuerySuggestionsBlockLists' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listQuerySuggestionsBlockLists_nextToken' - If the previous response was incomplete (because there is more data to
-- retrieve), Amazon Kendra returns a pagination token in the response. You
-- can use this pagination token to retrieve the next set of block lists
-- (@BlockListSummaryItems@).
--
-- 'maxResults', 'listQuerySuggestionsBlockLists_maxResults' - The maximum number of block lists to return.
--
-- 'indexId', 'listQuerySuggestionsBlockLists_indexId' - The identifier of the index for a list of all block lists that exist for
-- that index.
--
-- For information on the current quota limits for block lists, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/quotas.html Quotas for Amazon Kendra>.
newListQuerySuggestionsBlockLists ::
  -- | 'indexId'
  Prelude.Text ->
  ListQuerySuggestionsBlockLists
newListQuerySuggestionsBlockLists pIndexId_ =
  ListQuerySuggestionsBlockLists'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      indexId = pIndexId_
    }

-- | If the previous response was incomplete (because there is more data to
-- retrieve), Amazon Kendra returns a pagination token in the response. You
-- can use this pagination token to retrieve the next set of block lists
-- (@BlockListSummaryItems@).
listQuerySuggestionsBlockLists_nextToken :: Lens.Lens' ListQuerySuggestionsBlockLists (Prelude.Maybe Prelude.Text)
listQuerySuggestionsBlockLists_nextToken = Lens.lens (\ListQuerySuggestionsBlockLists' {nextToken} -> nextToken) (\s@ListQuerySuggestionsBlockLists' {} a -> s {nextToken = a} :: ListQuerySuggestionsBlockLists)

-- | The maximum number of block lists to return.
listQuerySuggestionsBlockLists_maxResults :: Lens.Lens' ListQuerySuggestionsBlockLists (Prelude.Maybe Prelude.Natural)
listQuerySuggestionsBlockLists_maxResults = Lens.lens (\ListQuerySuggestionsBlockLists' {maxResults} -> maxResults) (\s@ListQuerySuggestionsBlockLists' {} a -> s {maxResults = a} :: ListQuerySuggestionsBlockLists)

-- | The identifier of the index for a list of all block lists that exist for
-- that index.
--
-- For information on the current quota limits for block lists, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/quotas.html Quotas for Amazon Kendra>.
listQuerySuggestionsBlockLists_indexId :: Lens.Lens' ListQuerySuggestionsBlockLists Prelude.Text
listQuerySuggestionsBlockLists_indexId = Lens.lens (\ListQuerySuggestionsBlockLists' {indexId} -> indexId) (\s@ListQuerySuggestionsBlockLists' {} a -> s {indexId = a} :: ListQuerySuggestionsBlockLists)

instance
  Core.AWSRequest
    ListQuerySuggestionsBlockLists
  where
  type
    AWSResponse ListQuerySuggestionsBlockLists =
      ListQuerySuggestionsBlockListsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListQuerySuggestionsBlockListsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "BlockListSummaryItems"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListQuerySuggestionsBlockLists
  where
  hashWithSalt
    _salt
    ListQuerySuggestionsBlockLists' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` indexId

instance
  Prelude.NFData
    ListQuerySuggestionsBlockLists
  where
  rnf ListQuerySuggestionsBlockLists' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf indexId

instance
  Data.ToHeaders
    ListQuerySuggestionsBlockLists
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSKendraFrontendService.ListQuerySuggestionsBlockLists" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListQuerySuggestionsBlockLists where
  toJSON ListQuerySuggestionsBlockLists' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just ("IndexId" Data..= indexId)
          ]
      )

instance Data.ToPath ListQuerySuggestionsBlockLists where
  toPath = Prelude.const "/"

instance Data.ToQuery ListQuerySuggestionsBlockLists where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListQuerySuggestionsBlockListsResponse' smart constructor.
data ListQuerySuggestionsBlockListsResponse = ListQuerySuggestionsBlockListsResponse'
  { -- | If the response is truncated, Amazon Kendra returns this token that you
    -- can use in the subsequent request to retrieve the next set of block
    -- lists.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Summary items for a block list.
    --
    -- This includes summary items on the block list ID, block list name, when
    -- the block list was created, when the block list was last updated, and
    -- the count of block words\/phrases in the block list.
    --
    -- For information on the current quota limits for block lists, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/quotas.html Quotas for Amazon Kendra>.
    blockListSummaryItems :: Prelude.Maybe [QuerySuggestionsBlockListSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListQuerySuggestionsBlockListsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listQuerySuggestionsBlockListsResponse_nextToken' - If the response is truncated, Amazon Kendra returns this token that you
-- can use in the subsequent request to retrieve the next set of block
-- lists.
--
-- 'blockListSummaryItems', 'listQuerySuggestionsBlockListsResponse_blockListSummaryItems' - Summary items for a block list.
--
-- This includes summary items on the block list ID, block list name, when
-- the block list was created, when the block list was last updated, and
-- the count of block words\/phrases in the block list.
--
-- For information on the current quota limits for block lists, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/quotas.html Quotas for Amazon Kendra>.
--
-- 'httpStatus', 'listQuerySuggestionsBlockListsResponse_httpStatus' - The response's http status code.
newListQuerySuggestionsBlockListsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListQuerySuggestionsBlockListsResponse
newListQuerySuggestionsBlockListsResponse
  pHttpStatus_ =
    ListQuerySuggestionsBlockListsResponse'
      { nextToken =
          Prelude.Nothing,
        blockListSummaryItems =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | If the response is truncated, Amazon Kendra returns this token that you
-- can use in the subsequent request to retrieve the next set of block
-- lists.
listQuerySuggestionsBlockListsResponse_nextToken :: Lens.Lens' ListQuerySuggestionsBlockListsResponse (Prelude.Maybe Prelude.Text)
listQuerySuggestionsBlockListsResponse_nextToken = Lens.lens (\ListQuerySuggestionsBlockListsResponse' {nextToken} -> nextToken) (\s@ListQuerySuggestionsBlockListsResponse' {} a -> s {nextToken = a} :: ListQuerySuggestionsBlockListsResponse)

-- | Summary items for a block list.
--
-- This includes summary items on the block list ID, block list name, when
-- the block list was created, when the block list was last updated, and
-- the count of block words\/phrases in the block list.
--
-- For information on the current quota limits for block lists, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/quotas.html Quotas for Amazon Kendra>.
listQuerySuggestionsBlockListsResponse_blockListSummaryItems :: Lens.Lens' ListQuerySuggestionsBlockListsResponse (Prelude.Maybe [QuerySuggestionsBlockListSummary])
listQuerySuggestionsBlockListsResponse_blockListSummaryItems = Lens.lens (\ListQuerySuggestionsBlockListsResponse' {blockListSummaryItems} -> blockListSummaryItems) (\s@ListQuerySuggestionsBlockListsResponse' {} a -> s {blockListSummaryItems = a} :: ListQuerySuggestionsBlockListsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listQuerySuggestionsBlockListsResponse_httpStatus :: Lens.Lens' ListQuerySuggestionsBlockListsResponse Prelude.Int
listQuerySuggestionsBlockListsResponse_httpStatus = Lens.lens (\ListQuerySuggestionsBlockListsResponse' {httpStatus} -> httpStatus) (\s@ListQuerySuggestionsBlockListsResponse' {} a -> s {httpStatus = a} :: ListQuerySuggestionsBlockListsResponse)

instance
  Prelude.NFData
    ListQuerySuggestionsBlockListsResponse
  where
  rnf ListQuerySuggestionsBlockListsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf blockListSummaryItems
      `Prelude.seq` Prelude.rnf httpStatus

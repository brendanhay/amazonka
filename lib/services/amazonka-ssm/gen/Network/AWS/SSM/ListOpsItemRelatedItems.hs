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
-- Module      : Network.AWS.SSM.ListOpsItemRelatedItems
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all related-item resources associated with a Systems Manager
-- OpsCenter OpsItem. OpsCenter is a capability of Amazon Web Services
-- Systems Manager.
--
-- This operation returns paginated results.
module Network.AWS.SSM.ListOpsItemRelatedItems
  ( -- * Creating a Request
    ListOpsItemRelatedItems (..),
    newListOpsItemRelatedItems,

    -- * Request Lenses
    listOpsItemRelatedItems_opsItemId,
    listOpsItemRelatedItems_filters,
    listOpsItemRelatedItems_nextToken,
    listOpsItemRelatedItems_maxResults,

    -- * Destructuring the Response
    ListOpsItemRelatedItemsResponse (..),
    newListOpsItemRelatedItemsResponse,

    -- * Response Lenses
    listOpsItemRelatedItemsResponse_nextToken,
    listOpsItemRelatedItemsResponse_summaries,
    listOpsItemRelatedItemsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newListOpsItemRelatedItems' smart constructor.
data ListOpsItemRelatedItems = ListOpsItemRelatedItems'
  { -- | The ID of the OpsItem for which you want to list all related-item
    -- resources.
    opsItemId :: Prelude.Maybe Prelude.Text,
    -- | One or more OpsItem filters. Use a filter to return a more specific list
    -- of results.
    filters :: Prelude.Maybe [OpsItemRelatedItemsFilter],
    -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOpsItemRelatedItems' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'opsItemId', 'listOpsItemRelatedItems_opsItemId' - The ID of the OpsItem for which you want to list all related-item
-- resources.
--
-- 'filters', 'listOpsItemRelatedItems_filters' - One or more OpsItem filters. Use a filter to return a more specific list
-- of results.
--
-- 'nextToken', 'listOpsItemRelatedItems_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'maxResults', 'listOpsItemRelatedItems_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
newListOpsItemRelatedItems ::
  ListOpsItemRelatedItems
newListOpsItemRelatedItems =
  ListOpsItemRelatedItems'
    { opsItemId =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The ID of the OpsItem for which you want to list all related-item
-- resources.
listOpsItemRelatedItems_opsItemId :: Lens.Lens' ListOpsItemRelatedItems (Prelude.Maybe Prelude.Text)
listOpsItemRelatedItems_opsItemId = Lens.lens (\ListOpsItemRelatedItems' {opsItemId} -> opsItemId) (\s@ListOpsItemRelatedItems' {} a -> s {opsItemId = a} :: ListOpsItemRelatedItems)

-- | One or more OpsItem filters. Use a filter to return a more specific list
-- of results.
listOpsItemRelatedItems_filters :: Lens.Lens' ListOpsItemRelatedItems (Prelude.Maybe [OpsItemRelatedItemsFilter])
listOpsItemRelatedItems_filters = Lens.lens (\ListOpsItemRelatedItems' {filters} -> filters) (\s@ListOpsItemRelatedItems' {} a -> s {filters = a} :: ListOpsItemRelatedItems) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
listOpsItemRelatedItems_nextToken :: Lens.Lens' ListOpsItemRelatedItems (Prelude.Maybe Prelude.Text)
listOpsItemRelatedItems_nextToken = Lens.lens (\ListOpsItemRelatedItems' {nextToken} -> nextToken) (\s@ListOpsItemRelatedItems' {} a -> s {nextToken = a} :: ListOpsItemRelatedItems)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
listOpsItemRelatedItems_maxResults :: Lens.Lens' ListOpsItemRelatedItems (Prelude.Maybe Prelude.Natural)
listOpsItemRelatedItems_maxResults = Lens.lens (\ListOpsItemRelatedItems' {maxResults} -> maxResults) (\s@ListOpsItemRelatedItems' {} a -> s {maxResults = a} :: ListOpsItemRelatedItems)

instance Core.AWSPager ListOpsItemRelatedItems where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listOpsItemRelatedItemsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listOpsItemRelatedItemsResponse_summaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listOpsItemRelatedItems_nextToken
          Lens..~ rs
          Lens.^? listOpsItemRelatedItemsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListOpsItemRelatedItems where
  type
    AWSResponse ListOpsItemRelatedItems =
      ListOpsItemRelatedItemsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOpsItemRelatedItemsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Summaries" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListOpsItemRelatedItems

instance Prelude.NFData ListOpsItemRelatedItems

instance Core.ToHeaders ListOpsItemRelatedItems where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.ListOpsItemRelatedItems" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListOpsItemRelatedItems where
  toJSON ListOpsItemRelatedItems' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("OpsItemId" Core..=) Prelude.<$> opsItemId,
            ("Filters" Core..=) Prelude.<$> filters,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListOpsItemRelatedItems where
  toPath = Prelude.const "/"

instance Core.ToQuery ListOpsItemRelatedItems where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListOpsItemRelatedItemsResponse' smart constructor.
data ListOpsItemRelatedItemsResponse = ListOpsItemRelatedItemsResponse'
  { -- | The token for the next set of items to return. Use this token to get the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of related-item resources for the specified OpsItem.
    summaries :: Prelude.Maybe [OpsItemRelatedItemSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOpsItemRelatedItemsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listOpsItemRelatedItemsResponse_nextToken' - The token for the next set of items to return. Use this token to get the
-- next set of results.
--
-- 'summaries', 'listOpsItemRelatedItemsResponse_summaries' - A list of related-item resources for the specified OpsItem.
--
-- 'httpStatus', 'listOpsItemRelatedItemsResponse_httpStatus' - The response's http status code.
newListOpsItemRelatedItemsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListOpsItemRelatedItemsResponse
newListOpsItemRelatedItemsResponse pHttpStatus_ =
  ListOpsItemRelatedItemsResponse'
    { nextToken =
        Prelude.Nothing,
      summaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of items to return. Use this token to get the
-- next set of results.
listOpsItemRelatedItemsResponse_nextToken :: Lens.Lens' ListOpsItemRelatedItemsResponse (Prelude.Maybe Prelude.Text)
listOpsItemRelatedItemsResponse_nextToken = Lens.lens (\ListOpsItemRelatedItemsResponse' {nextToken} -> nextToken) (\s@ListOpsItemRelatedItemsResponse' {} a -> s {nextToken = a} :: ListOpsItemRelatedItemsResponse)

-- | A list of related-item resources for the specified OpsItem.
listOpsItemRelatedItemsResponse_summaries :: Lens.Lens' ListOpsItemRelatedItemsResponse (Prelude.Maybe [OpsItemRelatedItemSummary])
listOpsItemRelatedItemsResponse_summaries = Lens.lens (\ListOpsItemRelatedItemsResponse' {summaries} -> summaries) (\s@ListOpsItemRelatedItemsResponse' {} a -> s {summaries = a} :: ListOpsItemRelatedItemsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listOpsItemRelatedItemsResponse_httpStatus :: Lens.Lens' ListOpsItemRelatedItemsResponse Prelude.Int
listOpsItemRelatedItemsResponse_httpStatus = Lens.lens (\ListOpsItemRelatedItemsResponse' {httpStatus} -> httpStatus) (\s@ListOpsItemRelatedItemsResponse' {} a -> s {httpStatus = a} :: ListOpsItemRelatedItemsResponse)

instance
  Prelude.NFData
    ListOpsItemRelatedItemsResponse

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
-- Module      : Amazonka.SageMaker.ListModelCards
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List existing model cards.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListModelCards
  ( -- * Creating a Request
    ListModelCards (..),
    newListModelCards,

    -- * Request Lenses
    listModelCards_creationTimeAfter,
    listModelCards_creationTimeBefore,
    listModelCards_maxResults,
    listModelCards_modelCardStatus,
    listModelCards_nameContains,
    listModelCards_nextToken,
    listModelCards_sortBy,
    listModelCards_sortOrder,

    -- * Destructuring the Response
    ListModelCardsResponse (..),
    newListModelCardsResponse,

    -- * Response Lenses
    listModelCardsResponse_nextToken,
    listModelCardsResponse_httpStatus,
    listModelCardsResponse_modelCardSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListModelCards' smart constructor.
data ListModelCards = ListModelCards'
  { -- | Only list model cards that were created after the time specified.
    creationTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | Only list model cards that were created before the time specified.
    creationTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | The maximum number of model cards to list.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Only list model cards with the specified approval status.
    modelCardStatus :: Prelude.Maybe ModelCardStatus,
    -- | Only list model cards with names that contain the specified string.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | If the response to a previous @ListModelCards@ request was truncated,
    -- the response includes a @NextToken@. To retrieve the next set of model
    -- cards, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Sort model cards by either name or creation time. Sorts by creation time
    -- by default.
    sortBy :: Prelude.Maybe ModelCardSortBy,
    -- | Sort model cards by ascending or descending order.
    sortOrder :: Prelude.Maybe ModelCardSortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListModelCards' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimeAfter', 'listModelCards_creationTimeAfter' - Only list model cards that were created after the time specified.
--
-- 'creationTimeBefore', 'listModelCards_creationTimeBefore' - Only list model cards that were created before the time specified.
--
-- 'maxResults', 'listModelCards_maxResults' - The maximum number of model cards to list.
--
-- 'modelCardStatus', 'listModelCards_modelCardStatus' - Only list model cards with the specified approval status.
--
-- 'nameContains', 'listModelCards_nameContains' - Only list model cards with names that contain the specified string.
--
-- 'nextToken', 'listModelCards_nextToken' - If the response to a previous @ListModelCards@ request was truncated,
-- the response includes a @NextToken@. To retrieve the next set of model
-- cards, use the token in the next request.
--
-- 'sortBy', 'listModelCards_sortBy' - Sort model cards by either name or creation time. Sorts by creation time
-- by default.
--
-- 'sortOrder', 'listModelCards_sortOrder' - Sort model cards by ascending or descending order.
newListModelCards ::
  ListModelCards
newListModelCards =
  ListModelCards'
    { creationTimeAfter =
        Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      modelCardStatus = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing
    }

-- | Only list model cards that were created after the time specified.
listModelCards_creationTimeAfter :: Lens.Lens' ListModelCards (Prelude.Maybe Prelude.UTCTime)
listModelCards_creationTimeAfter = Lens.lens (\ListModelCards' {creationTimeAfter} -> creationTimeAfter) (\s@ListModelCards' {} a -> s {creationTimeAfter = a} :: ListModelCards) Prelude.. Lens.mapping Data._Time

-- | Only list model cards that were created before the time specified.
listModelCards_creationTimeBefore :: Lens.Lens' ListModelCards (Prelude.Maybe Prelude.UTCTime)
listModelCards_creationTimeBefore = Lens.lens (\ListModelCards' {creationTimeBefore} -> creationTimeBefore) (\s@ListModelCards' {} a -> s {creationTimeBefore = a} :: ListModelCards) Prelude.. Lens.mapping Data._Time

-- | The maximum number of model cards to list.
listModelCards_maxResults :: Lens.Lens' ListModelCards (Prelude.Maybe Prelude.Natural)
listModelCards_maxResults = Lens.lens (\ListModelCards' {maxResults} -> maxResults) (\s@ListModelCards' {} a -> s {maxResults = a} :: ListModelCards)

-- | Only list model cards with the specified approval status.
listModelCards_modelCardStatus :: Lens.Lens' ListModelCards (Prelude.Maybe ModelCardStatus)
listModelCards_modelCardStatus = Lens.lens (\ListModelCards' {modelCardStatus} -> modelCardStatus) (\s@ListModelCards' {} a -> s {modelCardStatus = a} :: ListModelCards)

-- | Only list model cards with names that contain the specified string.
listModelCards_nameContains :: Lens.Lens' ListModelCards (Prelude.Maybe Prelude.Text)
listModelCards_nameContains = Lens.lens (\ListModelCards' {nameContains} -> nameContains) (\s@ListModelCards' {} a -> s {nameContains = a} :: ListModelCards)

-- | If the response to a previous @ListModelCards@ request was truncated,
-- the response includes a @NextToken@. To retrieve the next set of model
-- cards, use the token in the next request.
listModelCards_nextToken :: Lens.Lens' ListModelCards (Prelude.Maybe Prelude.Text)
listModelCards_nextToken = Lens.lens (\ListModelCards' {nextToken} -> nextToken) (\s@ListModelCards' {} a -> s {nextToken = a} :: ListModelCards)

-- | Sort model cards by either name or creation time. Sorts by creation time
-- by default.
listModelCards_sortBy :: Lens.Lens' ListModelCards (Prelude.Maybe ModelCardSortBy)
listModelCards_sortBy = Lens.lens (\ListModelCards' {sortBy} -> sortBy) (\s@ListModelCards' {} a -> s {sortBy = a} :: ListModelCards)

-- | Sort model cards by ascending or descending order.
listModelCards_sortOrder :: Lens.Lens' ListModelCards (Prelude.Maybe ModelCardSortOrder)
listModelCards_sortOrder = Lens.lens (\ListModelCards' {sortOrder} -> sortOrder) (\s@ListModelCards' {} a -> s {sortOrder = a} :: ListModelCards)

instance Core.AWSPager ListModelCards where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listModelCardsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listModelCardsResponse_modelCardSummaries
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listModelCards_nextToken
          Lens..~ rs
          Lens.^? listModelCardsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListModelCards where
  type
    AWSResponse ListModelCards =
      ListModelCardsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListModelCardsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "ModelCardSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListModelCards where
  hashWithSalt _salt ListModelCards' {..} =
    _salt
      `Prelude.hashWithSalt` creationTimeAfter
      `Prelude.hashWithSalt` creationTimeBefore
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` modelCardStatus
      `Prelude.hashWithSalt` nameContains
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder

instance Prelude.NFData ListModelCards where
  rnf ListModelCards' {..} =
    Prelude.rnf creationTimeAfter
      `Prelude.seq` Prelude.rnf creationTimeBefore
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf modelCardStatus
      `Prelude.seq` Prelude.rnf nameContains
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder

instance Data.ToHeaders ListModelCards where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.ListModelCards" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListModelCards where
  toJSON ListModelCards' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CreationTimeAfter" Data..=)
              Prelude.<$> creationTimeAfter,
            ("CreationTimeBefore" Data..=)
              Prelude.<$> creationTimeBefore,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("ModelCardStatus" Data..=)
              Prelude.<$> modelCardStatus,
            ("NameContains" Data..=) Prelude.<$> nameContains,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("SortOrder" Data..=) Prelude.<$> sortOrder
          ]
      )

instance Data.ToPath ListModelCards where
  toPath = Prelude.const "/"

instance Data.ToQuery ListModelCards where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListModelCardsResponse' smart constructor.
data ListModelCardsResponse = ListModelCardsResponse'
  { -- | If the response is truncated, SageMaker returns this token. To retrieve
    -- the next set of model cards, use it in the subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The summaries of the listed model cards.
    modelCardSummaries :: [ModelCardSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListModelCardsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listModelCardsResponse_nextToken' - If the response is truncated, SageMaker returns this token. To retrieve
-- the next set of model cards, use it in the subsequent request.
--
-- 'httpStatus', 'listModelCardsResponse_httpStatus' - The response's http status code.
--
-- 'modelCardSummaries', 'listModelCardsResponse_modelCardSummaries' - The summaries of the listed model cards.
newListModelCardsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListModelCardsResponse
newListModelCardsResponse pHttpStatus_ =
  ListModelCardsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      modelCardSummaries = Prelude.mempty
    }

-- | If the response is truncated, SageMaker returns this token. To retrieve
-- the next set of model cards, use it in the subsequent request.
listModelCardsResponse_nextToken :: Lens.Lens' ListModelCardsResponse (Prelude.Maybe Prelude.Text)
listModelCardsResponse_nextToken = Lens.lens (\ListModelCardsResponse' {nextToken} -> nextToken) (\s@ListModelCardsResponse' {} a -> s {nextToken = a} :: ListModelCardsResponse)

-- | The response's http status code.
listModelCardsResponse_httpStatus :: Lens.Lens' ListModelCardsResponse Prelude.Int
listModelCardsResponse_httpStatus = Lens.lens (\ListModelCardsResponse' {httpStatus} -> httpStatus) (\s@ListModelCardsResponse' {} a -> s {httpStatus = a} :: ListModelCardsResponse)

-- | The summaries of the listed model cards.
listModelCardsResponse_modelCardSummaries :: Lens.Lens' ListModelCardsResponse [ModelCardSummary]
listModelCardsResponse_modelCardSummaries = Lens.lens (\ListModelCardsResponse' {modelCardSummaries} -> modelCardSummaries) (\s@ListModelCardsResponse' {} a -> s {modelCardSummaries = a} :: ListModelCardsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListModelCardsResponse where
  rnf ListModelCardsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf modelCardSummaries

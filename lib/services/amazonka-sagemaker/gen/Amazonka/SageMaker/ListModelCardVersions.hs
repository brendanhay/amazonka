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
-- Module      : Amazonka.SageMaker.ListModelCardVersions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List existing versions of an Amazon SageMaker Model Card.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListModelCardVersions
  ( -- * Creating a Request
    ListModelCardVersions (..),
    newListModelCardVersions,

    -- * Request Lenses
    listModelCardVersions_creationTimeAfter,
    listModelCardVersions_creationTimeBefore,
    listModelCardVersions_maxResults,
    listModelCardVersions_modelCardStatus,
    listModelCardVersions_nextToken,
    listModelCardVersions_sortBy,
    listModelCardVersions_sortOrder,
    listModelCardVersions_modelCardName,

    -- * Destructuring the Response
    ListModelCardVersionsResponse (..),
    newListModelCardVersionsResponse,

    -- * Response Lenses
    listModelCardVersionsResponse_nextToken,
    listModelCardVersionsResponse_httpStatus,
    listModelCardVersionsResponse_modelCardVersionSummaryList,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListModelCardVersions' smart constructor.
data ListModelCardVersions = ListModelCardVersions'
  { -- | Only list model card versions that were created after the time
    -- specified.
    creationTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | Only list model card versions that were created before the time
    -- specified.
    creationTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | The maximum number of model card versions to list.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Only list model card versions with the specified approval status.
    modelCardStatus :: Prelude.Maybe ModelCardStatus,
    -- | If the response to a previous @ListModelCardVersions@ request was
    -- truncated, the response includes a @NextToken@. To retrieve the next set
    -- of model card versions, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Sort listed model card versions by version. Sorts by version by default.
    sortBy :: Prelude.Maybe ModelCardVersionSortBy,
    -- | Sort model card versions by ascending or descending order.
    sortOrder :: Prelude.Maybe ModelCardSortOrder,
    -- | List model card versions for the model card with the specified name.
    modelCardName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListModelCardVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimeAfter', 'listModelCardVersions_creationTimeAfter' - Only list model card versions that were created after the time
-- specified.
--
-- 'creationTimeBefore', 'listModelCardVersions_creationTimeBefore' - Only list model card versions that were created before the time
-- specified.
--
-- 'maxResults', 'listModelCardVersions_maxResults' - The maximum number of model card versions to list.
--
-- 'modelCardStatus', 'listModelCardVersions_modelCardStatus' - Only list model card versions with the specified approval status.
--
-- 'nextToken', 'listModelCardVersions_nextToken' - If the response to a previous @ListModelCardVersions@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of model card versions, use the token in the next request.
--
-- 'sortBy', 'listModelCardVersions_sortBy' - Sort listed model card versions by version. Sorts by version by default.
--
-- 'sortOrder', 'listModelCardVersions_sortOrder' - Sort model card versions by ascending or descending order.
--
-- 'modelCardName', 'listModelCardVersions_modelCardName' - List model card versions for the model card with the specified name.
newListModelCardVersions ::
  -- | 'modelCardName'
  Prelude.Text ->
  ListModelCardVersions
newListModelCardVersions pModelCardName_ =
  ListModelCardVersions'
    { creationTimeAfter =
        Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      modelCardStatus = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      modelCardName = pModelCardName_
    }

-- | Only list model card versions that were created after the time
-- specified.
listModelCardVersions_creationTimeAfter :: Lens.Lens' ListModelCardVersions (Prelude.Maybe Prelude.UTCTime)
listModelCardVersions_creationTimeAfter = Lens.lens (\ListModelCardVersions' {creationTimeAfter} -> creationTimeAfter) (\s@ListModelCardVersions' {} a -> s {creationTimeAfter = a} :: ListModelCardVersions) Prelude.. Lens.mapping Data._Time

-- | Only list model card versions that were created before the time
-- specified.
listModelCardVersions_creationTimeBefore :: Lens.Lens' ListModelCardVersions (Prelude.Maybe Prelude.UTCTime)
listModelCardVersions_creationTimeBefore = Lens.lens (\ListModelCardVersions' {creationTimeBefore} -> creationTimeBefore) (\s@ListModelCardVersions' {} a -> s {creationTimeBefore = a} :: ListModelCardVersions) Prelude.. Lens.mapping Data._Time

-- | The maximum number of model card versions to list.
listModelCardVersions_maxResults :: Lens.Lens' ListModelCardVersions (Prelude.Maybe Prelude.Natural)
listModelCardVersions_maxResults = Lens.lens (\ListModelCardVersions' {maxResults} -> maxResults) (\s@ListModelCardVersions' {} a -> s {maxResults = a} :: ListModelCardVersions)

-- | Only list model card versions with the specified approval status.
listModelCardVersions_modelCardStatus :: Lens.Lens' ListModelCardVersions (Prelude.Maybe ModelCardStatus)
listModelCardVersions_modelCardStatus = Lens.lens (\ListModelCardVersions' {modelCardStatus} -> modelCardStatus) (\s@ListModelCardVersions' {} a -> s {modelCardStatus = a} :: ListModelCardVersions)

-- | If the response to a previous @ListModelCardVersions@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of model card versions, use the token in the next request.
listModelCardVersions_nextToken :: Lens.Lens' ListModelCardVersions (Prelude.Maybe Prelude.Text)
listModelCardVersions_nextToken = Lens.lens (\ListModelCardVersions' {nextToken} -> nextToken) (\s@ListModelCardVersions' {} a -> s {nextToken = a} :: ListModelCardVersions)

-- | Sort listed model card versions by version. Sorts by version by default.
listModelCardVersions_sortBy :: Lens.Lens' ListModelCardVersions (Prelude.Maybe ModelCardVersionSortBy)
listModelCardVersions_sortBy = Lens.lens (\ListModelCardVersions' {sortBy} -> sortBy) (\s@ListModelCardVersions' {} a -> s {sortBy = a} :: ListModelCardVersions)

-- | Sort model card versions by ascending or descending order.
listModelCardVersions_sortOrder :: Lens.Lens' ListModelCardVersions (Prelude.Maybe ModelCardSortOrder)
listModelCardVersions_sortOrder = Lens.lens (\ListModelCardVersions' {sortOrder} -> sortOrder) (\s@ListModelCardVersions' {} a -> s {sortOrder = a} :: ListModelCardVersions)

-- | List model card versions for the model card with the specified name.
listModelCardVersions_modelCardName :: Lens.Lens' ListModelCardVersions Prelude.Text
listModelCardVersions_modelCardName = Lens.lens (\ListModelCardVersions' {modelCardName} -> modelCardName) (\s@ListModelCardVersions' {} a -> s {modelCardName = a} :: ListModelCardVersions)

instance Core.AWSPager ListModelCardVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listModelCardVersionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listModelCardVersionsResponse_modelCardVersionSummaryList
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listModelCardVersions_nextToken
          Lens..~ rs
          Lens.^? listModelCardVersionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListModelCardVersions where
  type
    AWSResponse ListModelCardVersions =
      ListModelCardVersionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListModelCardVersionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "ModelCardVersionSummaryList"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListModelCardVersions where
  hashWithSalt _salt ListModelCardVersions' {..} =
    _salt `Prelude.hashWithSalt` creationTimeAfter
      `Prelude.hashWithSalt` creationTimeBefore
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` modelCardStatus
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` modelCardName

instance Prelude.NFData ListModelCardVersions where
  rnf ListModelCardVersions' {..} =
    Prelude.rnf creationTimeAfter
      `Prelude.seq` Prelude.rnf creationTimeBefore
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf modelCardStatus
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf modelCardName

instance Data.ToHeaders ListModelCardVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.ListModelCardVersions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListModelCardVersions where
  toJSON ListModelCardVersions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CreationTimeAfter" Data..=)
              Prelude.<$> creationTimeAfter,
            ("CreationTimeBefore" Data..=)
              Prelude.<$> creationTimeBefore,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("ModelCardStatus" Data..=)
              Prelude.<$> modelCardStatus,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("SortOrder" Data..=) Prelude.<$> sortOrder,
            Prelude.Just
              ("ModelCardName" Data..= modelCardName)
          ]
      )

instance Data.ToPath ListModelCardVersions where
  toPath = Prelude.const "/"

instance Data.ToQuery ListModelCardVersions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListModelCardVersionsResponse' smart constructor.
data ListModelCardVersionsResponse = ListModelCardVersionsResponse'
  { -- | If the response is truncated, SageMaker returns this token. To retrieve
    -- the next set of model card versions, use it in the subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The summaries of the listed versions of the model card.
    modelCardVersionSummaryList :: [ModelCardVersionSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListModelCardVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listModelCardVersionsResponse_nextToken' - If the response is truncated, SageMaker returns this token. To retrieve
-- the next set of model card versions, use it in the subsequent request.
--
-- 'httpStatus', 'listModelCardVersionsResponse_httpStatus' - The response's http status code.
--
-- 'modelCardVersionSummaryList', 'listModelCardVersionsResponse_modelCardVersionSummaryList' - The summaries of the listed versions of the model card.
newListModelCardVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListModelCardVersionsResponse
newListModelCardVersionsResponse pHttpStatus_ =
  ListModelCardVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      modelCardVersionSummaryList = Prelude.mempty
    }

-- | If the response is truncated, SageMaker returns this token. To retrieve
-- the next set of model card versions, use it in the subsequent request.
listModelCardVersionsResponse_nextToken :: Lens.Lens' ListModelCardVersionsResponse (Prelude.Maybe Prelude.Text)
listModelCardVersionsResponse_nextToken = Lens.lens (\ListModelCardVersionsResponse' {nextToken} -> nextToken) (\s@ListModelCardVersionsResponse' {} a -> s {nextToken = a} :: ListModelCardVersionsResponse)

-- | The response's http status code.
listModelCardVersionsResponse_httpStatus :: Lens.Lens' ListModelCardVersionsResponse Prelude.Int
listModelCardVersionsResponse_httpStatus = Lens.lens (\ListModelCardVersionsResponse' {httpStatus} -> httpStatus) (\s@ListModelCardVersionsResponse' {} a -> s {httpStatus = a} :: ListModelCardVersionsResponse)

-- | The summaries of the listed versions of the model card.
listModelCardVersionsResponse_modelCardVersionSummaryList :: Lens.Lens' ListModelCardVersionsResponse [ModelCardVersionSummary]
listModelCardVersionsResponse_modelCardVersionSummaryList = Lens.lens (\ListModelCardVersionsResponse' {modelCardVersionSummaryList} -> modelCardVersionSummaryList) (\s@ListModelCardVersionsResponse' {} a -> s {modelCardVersionSummaryList = a} :: ListModelCardVersionsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListModelCardVersionsResponse where
  rnf ListModelCardVersionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf modelCardVersionSummaryList

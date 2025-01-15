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
-- Module      : Amazonka.SageMaker.ListModels
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists models created with the @CreateModel@ API.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListModels
  ( -- * Creating a Request
    ListModels (..),
    newListModels,

    -- * Request Lenses
    listModels_creationTimeAfter,
    listModels_creationTimeBefore,
    listModels_maxResults,
    listModels_nameContains,
    listModels_nextToken,
    listModels_sortBy,
    listModels_sortOrder,

    -- * Destructuring the Response
    ListModelsResponse (..),
    newListModelsResponse,

    -- * Response Lenses
    listModelsResponse_nextToken,
    listModelsResponse_httpStatus,
    listModelsResponse_models,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListModels' smart constructor.
data ListModels = ListModels'
  { -- | A filter that returns only models with a creation time greater than or
    -- equal to the specified time (timestamp).
    creationTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | A filter that returns only models created before the specified time
    -- (timestamp).
    creationTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | The maximum number of models to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A string in the model name. This filter returns only models whose name
    -- contains the specified string.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | If the response to a previous @ListModels@ request was truncated, the
    -- response includes a @NextToken@. To retrieve the next set of models, use
    -- the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Sorts the list of results. The default is @CreationTime@.
    sortBy :: Prelude.Maybe ModelSortKey,
    -- | The sort order for results. The default is @Descending@.
    sortOrder :: Prelude.Maybe OrderKey
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListModels' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimeAfter', 'listModels_creationTimeAfter' - A filter that returns only models with a creation time greater than or
-- equal to the specified time (timestamp).
--
-- 'creationTimeBefore', 'listModels_creationTimeBefore' - A filter that returns only models created before the specified time
-- (timestamp).
--
-- 'maxResults', 'listModels_maxResults' - The maximum number of models to return in the response.
--
-- 'nameContains', 'listModels_nameContains' - A string in the model name. This filter returns only models whose name
-- contains the specified string.
--
-- 'nextToken', 'listModels_nextToken' - If the response to a previous @ListModels@ request was truncated, the
-- response includes a @NextToken@. To retrieve the next set of models, use
-- the token in the next request.
--
-- 'sortBy', 'listModels_sortBy' - Sorts the list of results. The default is @CreationTime@.
--
-- 'sortOrder', 'listModels_sortOrder' - The sort order for results. The default is @Descending@.
newListModels ::
  ListModels
newListModels =
  ListModels'
    { creationTimeAfter = Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing
    }

-- | A filter that returns only models with a creation time greater than or
-- equal to the specified time (timestamp).
listModels_creationTimeAfter :: Lens.Lens' ListModels (Prelude.Maybe Prelude.UTCTime)
listModels_creationTimeAfter = Lens.lens (\ListModels' {creationTimeAfter} -> creationTimeAfter) (\s@ListModels' {} a -> s {creationTimeAfter = a} :: ListModels) Prelude.. Lens.mapping Data._Time

-- | A filter that returns only models created before the specified time
-- (timestamp).
listModels_creationTimeBefore :: Lens.Lens' ListModels (Prelude.Maybe Prelude.UTCTime)
listModels_creationTimeBefore = Lens.lens (\ListModels' {creationTimeBefore} -> creationTimeBefore) (\s@ListModels' {} a -> s {creationTimeBefore = a} :: ListModels) Prelude.. Lens.mapping Data._Time

-- | The maximum number of models to return in the response.
listModels_maxResults :: Lens.Lens' ListModels (Prelude.Maybe Prelude.Natural)
listModels_maxResults = Lens.lens (\ListModels' {maxResults} -> maxResults) (\s@ListModels' {} a -> s {maxResults = a} :: ListModels)

-- | A string in the model name. This filter returns only models whose name
-- contains the specified string.
listModels_nameContains :: Lens.Lens' ListModels (Prelude.Maybe Prelude.Text)
listModels_nameContains = Lens.lens (\ListModels' {nameContains} -> nameContains) (\s@ListModels' {} a -> s {nameContains = a} :: ListModels)

-- | If the response to a previous @ListModels@ request was truncated, the
-- response includes a @NextToken@. To retrieve the next set of models, use
-- the token in the next request.
listModels_nextToken :: Lens.Lens' ListModels (Prelude.Maybe Prelude.Text)
listModels_nextToken = Lens.lens (\ListModels' {nextToken} -> nextToken) (\s@ListModels' {} a -> s {nextToken = a} :: ListModels)

-- | Sorts the list of results. The default is @CreationTime@.
listModels_sortBy :: Lens.Lens' ListModels (Prelude.Maybe ModelSortKey)
listModels_sortBy = Lens.lens (\ListModels' {sortBy} -> sortBy) (\s@ListModels' {} a -> s {sortBy = a} :: ListModels)

-- | The sort order for results. The default is @Descending@.
listModels_sortOrder :: Lens.Lens' ListModels (Prelude.Maybe OrderKey)
listModels_sortOrder = Lens.lens (\ListModels' {sortOrder} -> sortOrder) (\s@ListModels' {} a -> s {sortOrder = a} :: ListModels)

instance Core.AWSPager ListModels where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listModelsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop (rs Lens.^. listModelsResponse_models) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listModels_nextToken
              Lens..~ rs
              Lens.^? listModelsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListModels where
  type AWSResponse ListModels = ListModelsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListModelsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Models" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListModels where
  hashWithSalt _salt ListModels' {..} =
    _salt
      `Prelude.hashWithSalt` creationTimeAfter
      `Prelude.hashWithSalt` creationTimeBefore
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nameContains
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder

instance Prelude.NFData ListModels where
  rnf ListModels' {..} =
    Prelude.rnf creationTimeAfter `Prelude.seq`
      Prelude.rnf creationTimeBefore `Prelude.seq`
        Prelude.rnf maxResults `Prelude.seq`
          Prelude.rnf nameContains `Prelude.seq`
            Prelude.rnf nextToken `Prelude.seq`
              Prelude.rnf sortBy `Prelude.seq`
                Prelude.rnf sortOrder

instance Data.ToHeaders ListModels where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.ListModels" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListModels where
  toJSON ListModels' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CreationTimeAfter" Data..=)
              Prelude.<$> creationTimeAfter,
            ("CreationTimeBefore" Data..=)
              Prelude.<$> creationTimeBefore,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NameContains" Data..=) Prelude.<$> nameContains,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("SortOrder" Data..=) Prelude.<$> sortOrder
          ]
      )

instance Data.ToPath ListModels where
  toPath = Prelude.const "/"

instance Data.ToQuery ListModels where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListModelsResponse' smart constructor.
data ListModelsResponse = ListModelsResponse'
  { -- | If the response is truncated, SageMaker returns this token. To retrieve
    -- the next set of models, use it in the subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of @ModelSummary@ objects, each of which lists a model.
    models :: [ModelSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListModelsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listModelsResponse_nextToken' - If the response is truncated, SageMaker returns this token. To retrieve
-- the next set of models, use it in the subsequent request.
--
-- 'httpStatus', 'listModelsResponse_httpStatus' - The response's http status code.
--
-- 'models', 'listModelsResponse_models' - An array of @ModelSummary@ objects, each of which lists a model.
newListModelsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListModelsResponse
newListModelsResponse pHttpStatus_ =
  ListModelsResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      models = Prelude.mempty
    }

-- | If the response is truncated, SageMaker returns this token. To retrieve
-- the next set of models, use it in the subsequent request.
listModelsResponse_nextToken :: Lens.Lens' ListModelsResponse (Prelude.Maybe Prelude.Text)
listModelsResponse_nextToken = Lens.lens (\ListModelsResponse' {nextToken} -> nextToken) (\s@ListModelsResponse' {} a -> s {nextToken = a} :: ListModelsResponse)

-- | The response's http status code.
listModelsResponse_httpStatus :: Lens.Lens' ListModelsResponse Prelude.Int
listModelsResponse_httpStatus = Lens.lens (\ListModelsResponse' {httpStatus} -> httpStatus) (\s@ListModelsResponse' {} a -> s {httpStatus = a} :: ListModelsResponse)

-- | An array of @ModelSummary@ objects, each of which lists a model.
listModelsResponse_models :: Lens.Lens' ListModelsResponse [ModelSummary]
listModelsResponse_models = Lens.lens (\ListModelsResponse' {models} -> models) (\s@ListModelsResponse' {} a -> s {models = a} :: ListModelsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListModelsResponse where
  rnf ListModelsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf models

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
-- Module      : Network.AWS.SageMaker.ListModels
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists models created with the CreateModel API.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListModels
  ( -- * Creating a Request
    ListModels (..),
    newListModels,

    -- * Request Lenses
    listModels_sortOrder,
    listModels_nextToken,
    listModels_nameContains,
    listModels_maxResults,
    listModels_creationTimeBefore,
    listModels_sortBy,
    listModels_creationTimeAfter,

    -- * Destructuring the Response
    ListModelsResponse (..),
    newListModelsResponse,

    -- * Response Lenses
    listModelsResponse_nextToken,
    listModelsResponse_httpStatus,
    listModelsResponse_models,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListModels' smart constructor.
data ListModels = ListModels'
  { -- | The sort order for results. The default is @Descending@.
    sortOrder :: Prelude.Maybe OrderKey,
    -- | If the response to a previous @ListModels@ request was truncated, the
    -- response includes a @NextToken@. To retrieve the next set of models, use
    -- the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A string in the training job name. This filter returns only models in
    -- the training job whose name contains the specified string.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of models to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A filter that returns only models created before the specified time
    -- (timestamp).
    creationTimeBefore :: Prelude.Maybe Prelude.POSIX,
    -- | Sorts the list of results. The default is @CreationTime@.
    sortBy :: Prelude.Maybe ModelSortKey,
    -- | A filter that returns only models with a creation time greater than or
    -- equal to the specified time (timestamp).
    creationTimeAfter :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListModels' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'listModels_sortOrder' - The sort order for results. The default is @Descending@.
--
-- 'nextToken', 'listModels_nextToken' - If the response to a previous @ListModels@ request was truncated, the
-- response includes a @NextToken@. To retrieve the next set of models, use
-- the token in the next request.
--
-- 'nameContains', 'listModels_nameContains' - A string in the training job name. This filter returns only models in
-- the training job whose name contains the specified string.
--
-- 'maxResults', 'listModels_maxResults' - The maximum number of models to return in the response.
--
-- 'creationTimeBefore', 'listModels_creationTimeBefore' - A filter that returns only models created before the specified time
-- (timestamp).
--
-- 'sortBy', 'listModels_sortBy' - Sorts the list of results. The default is @CreationTime@.
--
-- 'creationTimeAfter', 'listModels_creationTimeAfter' - A filter that returns only models with a creation time greater than or
-- equal to the specified time (timestamp).
newListModels ::
  ListModels
newListModels =
  ListModels'
    { sortOrder = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      creationTimeAfter = Prelude.Nothing
    }

-- | The sort order for results. The default is @Descending@.
listModels_sortOrder :: Lens.Lens' ListModels (Prelude.Maybe OrderKey)
listModels_sortOrder = Lens.lens (\ListModels' {sortOrder} -> sortOrder) (\s@ListModels' {} a -> s {sortOrder = a} :: ListModels)

-- | If the response to a previous @ListModels@ request was truncated, the
-- response includes a @NextToken@. To retrieve the next set of models, use
-- the token in the next request.
listModels_nextToken :: Lens.Lens' ListModels (Prelude.Maybe Prelude.Text)
listModels_nextToken = Lens.lens (\ListModels' {nextToken} -> nextToken) (\s@ListModels' {} a -> s {nextToken = a} :: ListModels)

-- | A string in the training job name. This filter returns only models in
-- the training job whose name contains the specified string.
listModels_nameContains :: Lens.Lens' ListModels (Prelude.Maybe Prelude.Text)
listModels_nameContains = Lens.lens (\ListModels' {nameContains} -> nameContains) (\s@ListModels' {} a -> s {nameContains = a} :: ListModels)

-- | The maximum number of models to return in the response.
listModels_maxResults :: Lens.Lens' ListModels (Prelude.Maybe Prelude.Natural)
listModels_maxResults = Lens.lens (\ListModels' {maxResults} -> maxResults) (\s@ListModels' {} a -> s {maxResults = a} :: ListModels)

-- | A filter that returns only models created before the specified time
-- (timestamp).
listModels_creationTimeBefore :: Lens.Lens' ListModels (Prelude.Maybe Prelude.UTCTime)
listModels_creationTimeBefore = Lens.lens (\ListModels' {creationTimeBefore} -> creationTimeBefore) (\s@ListModels' {} a -> s {creationTimeBefore = a} :: ListModels) Prelude.. Lens.mapping Prelude._Time

-- | Sorts the list of results. The default is @CreationTime@.
listModels_sortBy :: Lens.Lens' ListModels (Prelude.Maybe ModelSortKey)
listModels_sortBy = Lens.lens (\ListModels' {sortBy} -> sortBy) (\s@ListModels' {} a -> s {sortBy = a} :: ListModels)

-- | A filter that returns only models with a creation time greater than or
-- equal to the specified time (timestamp).
listModels_creationTimeAfter :: Lens.Lens' ListModels (Prelude.Maybe Prelude.UTCTime)
listModels_creationTimeAfter = Lens.lens (\ListModels' {creationTimeAfter} -> creationTimeAfter) (\s@ListModels' {} a -> s {creationTimeAfter = a} :: ListModels) Prelude.. Lens.mapping Prelude._Time

instance Pager.AWSPager ListModels where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listModelsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop (rs Lens.^. listModelsResponse_models) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listModels_nextToken
          Lens..~ rs
          Lens.^? listModelsResponse_nextToken Prelude.. Lens._Just

instance Prelude.AWSRequest ListModels where
  type Rs ListModels = ListModelsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListModelsResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..?> "Models" Prelude..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListModels

instance Prelude.NFData ListModels

instance Prelude.ToHeaders ListModels where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("SageMaker.ListModels" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListModels where
  toJSON ListModels' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SortOrder" Prelude..=) Prelude.<$> sortOrder,
            ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("NameContains" Prelude..=) Prelude.<$> nameContains,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            ("CreationTimeBefore" Prelude..=)
              Prelude.<$> creationTimeBefore,
            ("SortBy" Prelude..=) Prelude.<$> sortBy,
            ("CreationTimeAfter" Prelude..=)
              Prelude.<$> creationTimeAfter
          ]
      )

instance Prelude.ToPath ListModels where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListModels where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListModelsResponse' smart constructor.
data ListModelsResponse = ListModelsResponse'
  { -- | If the response is truncated, Amazon SageMaker returns this token. To
    -- retrieve the next set of models, use it in the subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of @ModelSummary@ objects, each of which lists a model.
    models :: [ModelSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListModelsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listModelsResponse_nextToken' - If the response is truncated, Amazon SageMaker returns this token. To
-- retrieve the next set of models, use it in the subsequent request.
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

-- | If the response is truncated, Amazon SageMaker returns this token. To
-- retrieve the next set of models, use it in the subsequent request.
listModelsResponse_nextToken :: Lens.Lens' ListModelsResponse (Prelude.Maybe Prelude.Text)
listModelsResponse_nextToken = Lens.lens (\ListModelsResponse' {nextToken} -> nextToken) (\s@ListModelsResponse' {} a -> s {nextToken = a} :: ListModelsResponse)

-- | The response's http status code.
listModelsResponse_httpStatus :: Lens.Lens' ListModelsResponse Prelude.Int
listModelsResponse_httpStatus = Lens.lens (\ListModelsResponse' {httpStatus} -> httpStatus) (\s@ListModelsResponse' {} a -> s {httpStatus = a} :: ListModelsResponse)

-- | An array of @ModelSummary@ objects, each of which lists a model.
listModelsResponse_models :: Lens.Lens' ListModelsResponse [ModelSummary]
listModelsResponse_models = Lens.lens (\ListModelsResponse' {models} -> models) (\s@ListModelsResponse' {} a -> s {models = a} :: ListModelsResponse) Prelude.. Prelude._Coerce

instance Prelude.NFData ListModelsResponse

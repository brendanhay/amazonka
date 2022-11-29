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
-- Module      : Amazonka.SageMaker.ListEndpointConfigs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists endpoint configurations.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListEndpointConfigs
  ( -- * Creating a Request
    ListEndpointConfigs (..),
    newListEndpointConfigs,

    -- * Request Lenses
    listEndpointConfigs_sortOrder,
    listEndpointConfigs_nextToken,
    listEndpointConfigs_nameContains,
    listEndpointConfigs_creationTimeBefore,
    listEndpointConfigs_sortBy,
    listEndpointConfigs_maxResults,
    listEndpointConfigs_creationTimeAfter,

    -- * Destructuring the Response
    ListEndpointConfigsResponse (..),
    newListEndpointConfigsResponse,

    -- * Response Lenses
    listEndpointConfigsResponse_nextToken,
    listEndpointConfigsResponse_httpStatus,
    listEndpointConfigsResponse_endpointConfigs,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListEndpointConfigs' smart constructor.
data ListEndpointConfigs = ListEndpointConfigs'
  { -- | The sort order for results. The default is @Descending@.
    sortOrder :: Prelude.Maybe OrderKey,
    -- | If the result of the previous @ListEndpointConfig@ request was
    -- truncated, the response includes a @NextToken@. To retrieve the next set
    -- of endpoint configurations, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A string in the endpoint configuration name. This filter returns only
    -- endpoint configurations whose name contains the specified string.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only endpoint configurations created before the
    -- specified time (timestamp).
    creationTimeBefore :: Prelude.Maybe Core.POSIX,
    -- | The field to sort results by. The default is @CreationTime@.
    sortBy :: Prelude.Maybe EndpointConfigSortKey,
    -- | The maximum number of training jobs to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A filter that returns only endpoint configurations with a creation time
    -- greater than or equal to the specified time (timestamp).
    creationTimeAfter :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEndpointConfigs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'listEndpointConfigs_sortOrder' - The sort order for results. The default is @Descending@.
--
-- 'nextToken', 'listEndpointConfigs_nextToken' - If the result of the previous @ListEndpointConfig@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of endpoint configurations, use the token in the next request.
--
-- 'nameContains', 'listEndpointConfigs_nameContains' - A string in the endpoint configuration name. This filter returns only
-- endpoint configurations whose name contains the specified string.
--
-- 'creationTimeBefore', 'listEndpointConfigs_creationTimeBefore' - A filter that returns only endpoint configurations created before the
-- specified time (timestamp).
--
-- 'sortBy', 'listEndpointConfigs_sortBy' - The field to sort results by. The default is @CreationTime@.
--
-- 'maxResults', 'listEndpointConfigs_maxResults' - The maximum number of training jobs to return in the response.
--
-- 'creationTimeAfter', 'listEndpointConfigs_creationTimeAfter' - A filter that returns only endpoint configurations with a creation time
-- greater than or equal to the specified time (timestamp).
newListEndpointConfigs ::
  ListEndpointConfigs
newListEndpointConfigs =
  ListEndpointConfigs'
    { sortOrder = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      creationTimeAfter = Prelude.Nothing
    }

-- | The sort order for results. The default is @Descending@.
listEndpointConfigs_sortOrder :: Lens.Lens' ListEndpointConfigs (Prelude.Maybe OrderKey)
listEndpointConfigs_sortOrder = Lens.lens (\ListEndpointConfigs' {sortOrder} -> sortOrder) (\s@ListEndpointConfigs' {} a -> s {sortOrder = a} :: ListEndpointConfigs)

-- | If the result of the previous @ListEndpointConfig@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of endpoint configurations, use the token in the next request.
listEndpointConfigs_nextToken :: Lens.Lens' ListEndpointConfigs (Prelude.Maybe Prelude.Text)
listEndpointConfigs_nextToken = Lens.lens (\ListEndpointConfigs' {nextToken} -> nextToken) (\s@ListEndpointConfigs' {} a -> s {nextToken = a} :: ListEndpointConfigs)

-- | A string in the endpoint configuration name. This filter returns only
-- endpoint configurations whose name contains the specified string.
listEndpointConfigs_nameContains :: Lens.Lens' ListEndpointConfigs (Prelude.Maybe Prelude.Text)
listEndpointConfigs_nameContains = Lens.lens (\ListEndpointConfigs' {nameContains} -> nameContains) (\s@ListEndpointConfigs' {} a -> s {nameContains = a} :: ListEndpointConfigs)

-- | A filter that returns only endpoint configurations created before the
-- specified time (timestamp).
listEndpointConfigs_creationTimeBefore :: Lens.Lens' ListEndpointConfigs (Prelude.Maybe Prelude.UTCTime)
listEndpointConfigs_creationTimeBefore = Lens.lens (\ListEndpointConfigs' {creationTimeBefore} -> creationTimeBefore) (\s@ListEndpointConfigs' {} a -> s {creationTimeBefore = a} :: ListEndpointConfigs) Prelude.. Lens.mapping Core._Time

-- | The field to sort results by. The default is @CreationTime@.
listEndpointConfigs_sortBy :: Lens.Lens' ListEndpointConfigs (Prelude.Maybe EndpointConfigSortKey)
listEndpointConfigs_sortBy = Lens.lens (\ListEndpointConfigs' {sortBy} -> sortBy) (\s@ListEndpointConfigs' {} a -> s {sortBy = a} :: ListEndpointConfigs)

-- | The maximum number of training jobs to return in the response.
listEndpointConfigs_maxResults :: Lens.Lens' ListEndpointConfigs (Prelude.Maybe Prelude.Natural)
listEndpointConfigs_maxResults = Lens.lens (\ListEndpointConfigs' {maxResults} -> maxResults) (\s@ListEndpointConfigs' {} a -> s {maxResults = a} :: ListEndpointConfigs)

-- | A filter that returns only endpoint configurations with a creation time
-- greater than or equal to the specified time (timestamp).
listEndpointConfigs_creationTimeAfter :: Lens.Lens' ListEndpointConfigs (Prelude.Maybe Prelude.UTCTime)
listEndpointConfigs_creationTimeAfter = Lens.lens (\ListEndpointConfigs' {creationTimeAfter} -> creationTimeAfter) (\s@ListEndpointConfigs' {} a -> s {creationTimeAfter = a} :: ListEndpointConfigs) Prelude.. Lens.mapping Core._Time

instance Core.AWSPager ListEndpointConfigs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEndpointConfigsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listEndpointConfigsResponse_endpointConfigs
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listEndpointConfigs_nextToken
          Lens..~ rs
          Lens.^? listEndpointConfigsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListEndpointConfigs where
  type
    AWSResponse ListEndpointConfigs =
      ListEndpointConfigsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEndpointConfigsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "EndpointConfigs"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListEndpointConfigs where
  hashWithSalt _salt ListEndpointConfigs' {..} =
    _salt `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` nameContains
      `Prelude.hashWithSalt` creationTimeBefore
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` creationTimeAfter

instance Prelude.NFData ListEndpointConfigs where
  rnf ListEndpointConfigs' {..} =
    Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf nameContains
      `Prelude.seq` Prelude.rnf creationTimeBefore
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf creationTimeAfter

instance Core.ToHeaders ListEndpointConfigs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.ListEndpointConfigs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListEndpointConfigs where
  toJSON ListEndpointConfigs' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SortOrder" Core..=) Prelude.<$> sortOrder,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("NameContains" Core..=) Prelude.<$> nameContains,
            ("CreationTimeBefore" Core..=)
              Prelude.<$> creationTimeBefore,
            ("SortBy" Core..=) Prelude.<$> sortBy,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("CreationTimeAfter" Core..=)
              Prelude.<$> creationTimeAfter
          ]
      )

instance Core.ToPath ListEndpointConfigs where
  toPath = Prelude.const "/"

instance Core.ToQuery ListEndpointConfigs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListEndpointConfigsResponse' smart constructor.
data ListEndpointConfigsResponse = ListEndpointConfigsResponse'
  { -- | If the response is truncated, SageMaker returns this token. To retrieve
    -- the next set of endpoint configurations, use it in the subsequent
    -- request
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of endpoint configurations.
    endpointConfigs :: [EndpointConfigSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEndpointConfigsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEndpointConfigsResponse_nextToken' - If the response is truncated, SageMaker returns this token. To retrieve
-- the next set of endpoint configurations, use it in the subsequent
-- request
--
-- 'httpStatus', 'listEndpointConfigsResponse_httpStatus' - The response's http status code.
--
-- 'endpointConfigs', 'listEndpointConfigsResponse_endpointConfigs' - An array of endpoint configurations.
newListEndpointConfigsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEndpointConfigsResponse
newListEndpointConfigsResponse pHttpStatus_ =
  ListEndpointConfigsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      endpointConfigs = Prelude.mempty
    }

-- | If the response is truncated, SageMaker returns this token. To retrieve
-- the next set of endpoint configurations, use it in the subsequent
-- request
listEndpointConfigsResponse_nextToken :: Lens.Lens' ListEndpointConfigsResponse (Prelude.Maybe Prelude.Text)
listEndpointConfigsResponse_nextToken = Lens.lens (\ListEndpointConfigsResponse' {nextToken} -> nextToken) (\s@ListEndpointConfigsResponse' {} a -> s {nextToken = a} :: ListEndpointConfigsResponse)

-- | The response's http status code.
listEndpointConfigsResponse_httpStatus :: Lens.Lens' ListEndpointConfigsResponse Prelude.Int
listEndpointConfigsResponse_httpStatus = Lens.lens (\ListEndpointConfigsResponse' {httpStatus} -> httpStatus) (\s@ListEndpointConfigsResponse' {} a -> s {httpStatus = a} :: ListEndpointConfigsResponse)

-- | An array of endpoint configurations.
listEndpointConfigsResponse_endpointConfigs :: Lens.Lens' ListEndpointConfigsResponse [EndpointConfigSummary]
listEndpointConfigsResponse_endpointConfigs = Lens.lens (\ListEndpointConfigsResponse' {endpointConfigs} -> endpointConfigs) (\s@ListEndpointConfigsResponse' {} a -> s {endpointConfigs = a} :: ListEndpointConfigsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListEndpointConfigsResponse where
  rnf ListEndpointConfigsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf endpointConfigs

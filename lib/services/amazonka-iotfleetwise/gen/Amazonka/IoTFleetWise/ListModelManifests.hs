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
-- Module      : Amazonka.IoTFleetWise.ListModelManifests
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of vehicle models (model manifests).
--
-- This API operation uses pagination. Specify the @nextToken@ parameter in
-- the request to return more results.
--
-- This operation returns paginated results.
module Amazonka.IoTFleetWise.ListModelManifests
  ( -- * Creating a Request
    ListModelManifests (..),
    newListModelManifests,

    -- * Request Lenses
    listModelManifests_maxResults,
    listModelManifests_nextToken,
    listModelManifests_signalCatalogArn,

    -- * Destructuring the Response
    ListModelManifestsResponse (..),
    newListModelManifestsResponse,

    -- * Response Lenses
    listModelManifestsResponse_nextToken,
    listModelManifestsResponse_summaries,
    listModelManifestsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListModelManifests' smart constructor.
data ListModelManifests = ListModelManifests'
  { -- | The maximum number of items to return, between 1 and 100, inclusive.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A pagination token for the next set of results.
    --
    -- If the results of a search are large, only a portion of the results are
    -- returned, and a @nextToken@ pagination token is returned in the
    -- response. To retrieve the next set of results, reissue the search
    -- request and include the returned token. When all results have been
    -- returned, the response does not contain a pagination token value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ARN of a signal catalog. If you specify a signal catalog, only the
    -- vehicle models associated with it are returned.
    signalCatalogArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListModelManifests' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listModelManifests_maxResults' - The maximum number of items to return, between 1 and 100, inclusive.
--
-- 'nextToken', 'listModelManifests_nextToken' - A pagination token for the next set of results.
--
-- If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next set of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
--
-- 'signalCatalogArn', 'listModelManifests_signalCatalogArn' - The ARN of a signal catalog. If you specify a signal catalog, only the
-- vehicle models associated with it are returned.
newListModelManifests ::
  ListModelManifests
newListModelManifests =
  ListModelManifests'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      signalCatalogArn = Prelude.Nothing
    }

-- | The maximum number of items to return, between 1 and 100, inclusive.
listModelManifests_maxResults :: Lens.Lens' ListModelManifests (Prelude.Maybe Prelude.Natural)
listModelManifests_maxResults = Lens.lens (\ListModelManifests' {maxResults} -> maxResults) (\s@ListModelManifests' {} a -> s {maxResults = a} :: ListModelManifests)

-- | A pagination token for the next set of results.
--
-- If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next set of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
listModelManifests_nextToken :: Lens.Lens' ListModelManifests (Prelude.Maybe Prelude.Text)
listModelManifests_nextToken = Lens.lens (\ListModelManifests' {nextToken} -> nextToken) (\s@ListModelManifests' {} a -> s {nextToken = a} :: ListModelManifests)

-- | The ARN of a signal catalog. If you specify a signal catalog, only the
-- vehicle models associated with it are returned.
listModelManifests_signalCatalogArn :: Lens.Lens' ListModelManifests (Prelude.Maybe Prelude.Text)
listModelManifests_signalCatalogArn = Lens.lens (\ListModelManifests' {signalCatalogArn} -> signalCatalogArn) (\s@ListModelManifests' {} a -> s {signalCatalogArn = a} :: ListModelManifests)

instance Core.AWSPager ListModelManifests where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listModelManifestsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listModelManifestsResponse_summaries
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listModelManifests_nextToken
          Lens..~ rs
          Lens.^? listModelManifestsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListModelManifests where
  type
    AWSResponse ListModelManifests =
      ListModelManifestsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListModelManifestsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "summaries" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListModelManifests where
  hashWithSalt _salt ListModelManifests' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` signalCatalogArn

instance Prelude.NFData ListModelManifests where
  rnf ListModelManifests' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf signalCatalogArn

instance Data.ToHeaders ListModelManifests where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "IoTAutobahnControlPlane.ListModelManifests" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListModelManifests where
  toJSON ListModelManifests' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("signalCatalogArn" Data..=)
              Prelude.<$> signalCatalogArn
          ]
      )

instance Data.ToPath ListModelManifests where
  toPath = Prelude.const "/"

instance Data.ToQuery ListModelManifests where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListModelManifestsResponse' smart constructor.
data ListModelManifestsResponse = ListModelManifestsResponse'
  { -- | The token to retrieve the next set of results, or @null@ if there are no
    -- more results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of information about vehicle models.
    summaries :: Prelude.Maybe [ModelManifestSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListModelManifestsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listModelManifestsResponse_nextToken' - The token to retrieve the next set of results, or @null@ if there are no
-- more results.
--
-- 'summaries', 'listModelManifestsResponse_summaries' - A list of information about vehicle models.
--
-- 'httpStatus', 'listModelManifestsResponse_httpStatus' - The response's http status code.
newListModelManifestsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListModelManifestsResponse
newListModelManifestsResponse pHttpStatus_ =
  ListModelManifestsResponse'
    { nextToken =
        Prelude.Nothing,
      summaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to retrieve the next set of results, or @null@ if there are no
-- more results.
listModelManifestsResponse_nextToken :: Lens.Lens' ListModelManifestsResponse (Prelude.Maybe Prelude.Text)
listModelManifestsResponse_nextToken = Lens.lens (\ListModelManifestsResponse' {nextToken} -> nextToken) (\s@ListModelManifestsResponse' {} a -> s {nextToken = a} :: ListModelManifestsResponse)

-- | A list of information about vehicle models.
listModelManifestsResponse_summaries :: Lens.Lens' ListModelManifestsResponse (Prelude.Maybe [ModelManifestSummary])
listModelManifestsResponse_summaries = Lens.lens (\ListModelManifestsResponse' {summaries} -> summaries) (\s@ListModelManifestsResponse' {} a -> s {summaries = a} :: ListModelManifestsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listModelManifestsResponse_httpStatus :: Lens.Lens' ListModelManifestsResponse Prelude.Int
listModelManifestsResponse_httpStatus = Lens.lens (\ListModelManifestsResponse' {httpStatus} -> httpStatus) (\s@ListModelManifestsResponse' {} a -> s {httpStatus = a} :: ListModelManifestsResponse)

instance Prelude.NFData ListModelManifestsResponse where
  rnf ListModelManifestsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf summaries
      `Prelude.seq` Prelude.rnf httpStatus

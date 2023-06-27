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
-- Module      : Amazonka.IoTFleetWise.ListDecoderManifests
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists decoder manifests.
--
-- This API operation uses pagination. Specify the @nextToken@ parameter in
-- the request to return more results.
--
-- This operation returns paginated results.
module Amazonka.IoTFleetWise.ListDecoderManifests
  ( -- * Creating a Request
    ListDecoderManifests (..),
    newListDecoderManifests,

    -- * Request Lenses
    listDecoderManifests_maxResults,
    listDecoderManifests_modelManifestArn,
    listDecoderManifests_nextToken,

    -- * Destructuring the Response
    ListDecoderManifestsResponse (..),
    newListDecoderManifestsResponse,

    -- * Response Lenses
    listDecoderManifestsResponse_nextToken,
    listDecoderManifestsResponse_summaries,
    listDecoderManifestsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDecoderManifests' smart constructor.
data ListDecoderManifests = ListDecoderManifests'
  { -- | The maximum number of items to return, between 1 and 100, inclusive.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of a vehicle model (model manifest)
    -- associated with the decoder manifest.
    modelManifestArn :: Prelude.Maybe Prelude.Text,
    -- | A pagination token for the next set of results.
    --
    -- If the results of a search are large, only a portion of the results are
    -- returned, and a @nextToken@ pagination token is returned in the
    -- response. To retrieve the next set of results, reissue the search
    -- request and include the returned token. When all results have been
    -- returned, the response does not contain a pagination token value.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDecoderManifests' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listDecoderManifests_maxResults' - The maximum number of items to return, between 1 and 100, inclusive.
--
-- 'modelManifestArn', 'listDecoderManifests_modelManifestArn' - The Amazon Resource Name (ARN) of a vehicle model (model manifest)
-- associated with the decoder manifest.
--
-- 'nextToken', 'listDecoderManifests_nextToken' - A pagination token for the next set of results.
--
-- If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next set of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
newListDecoderManifests ::
  ListDecoderManifests
newListDecoderManifests =
  ListDecoderManifests'
    { maxResults = Prelude.Nothing,
      modelManifestArn = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of items to return, between 1 and 100, inclusive.
listDecoderManifests_maxResults :: Lens.Lens' ListDecoderManifests (Prelude.Maybe Prelude.Natural)
listDecoderManifests_maxResults = Lens.lens (\ListDecoderManifests' {maxResults} -> maxResults) (\s@ListDecoderManifests' {} a -> s {maxResults = a} :: ListDecoderManifests)

-- | The Amazon Resource Name (ARN) of a vehicle model (model manifest)
-- associated with the decoder manifest.
listDecoderManifests_modelManifestArn :: Lens.Lens' ListDecoderManifests (Prelude.Maybe Prelude.Text)
listDecoderManifests_modelManifestArn = Lens.lens (\ListDecoderManifests' {modelManifestArn} -> modelManifestArn) (\s@ListDecoderManifests' {} a -> s {modelManifestArn = a} :: ListDecoderManifests)

-- | A pagination token for the next set of results.
--
-- If the results of a search are large, only a portion of the results are
-- returned, and a @nextToken@ pagination token is returned in the
-- response. To retrieve the next set of results, reissue the search
-- request and include the returned token. When all results have been
-- returned, the response does not contain a pagination token value.
listDecoderManifests_nextToken :: Lens.Lens' ListDecoderManifests (Prelude.Maybe Prelude.Text)
listDecoderManifests_nextToken = Lens.lens (\ListDecoderManifests' {nextToken} -> nextToken) (\s@ListDecoderManifests' {} a -> s {nextToken = a} :: ListDecoderManifests)

instance Core.AWSPager ListDecoderManifests where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDecoderManifestsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDecoderManifestsResponse_summaries
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listDecoderManifests_nextToken
          Lens..~ rs
          Lens.^? listDecoderManifestsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListDecoderManifests where
  type
    AWSResponse ListDecoderManifests =
      ListDecoderManifestsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDecoderManifestsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "summaries" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDecoderManifests where
  hashWithSalt _salt ListDecoderManifests' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` modelManifestArn
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListDecoderManifests where
  rnf ListDecoderManifests' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf modelManifestArn
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListDecoderManifests where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "IoTAutobahnControlPlane.ListDecoderManifests" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDecoderManifests where
  toJSON ListDecoderManifests' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("modelManifestArn" Data..=)
              Prelude.<$> modelManifestArn,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListDecoderManifests where
  toPath = Prelude.const "/"

instance Data.ToQuery ListDecoderManifests where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDecoderManifestsResponse' smart constructor.
data ListDecoderManifestsResponse = ListDecoderManifestsResponse'
  { -- | The token to retrieve the next set of results, or @null@ if there are no
    -- more results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of information about each decoder manifest.
    summaries :: Prelude.Maybe [DecoderManifestSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDecoderManifestsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDecoderManifestsResponse_nextToken' - The token to retrieve the next set of results, or @null@ if there are no
-- more results.
--
-- 'summaries', 'listDecoderManifestsResponse_summaries' - A list of information about each decoder manifest.
--
-- 'httpStatus', 'listDecoderManifestsResponse_httpStatus' - The response's http status code.
newListDecoderManifestsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDecoderManifestsResponse
newListDecoderManifestsResponse pHttpStatus_ =
  ListDecoderManifestsResponse'
    { nextToken =
        Prelude.Nothing,
      summaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to retrieve the next set of results, or @null@ if there are no
-- more results.
listDecoderManifestsResponse_nextToken :: Lens.Lens' ListDecoderManifestsResponse (Prelude.Maybe Prelude.Text)
listDecoderManifestsResponse_nextToken = Lens.lens (\ListDecoderManifestsResponse' {nextToken} -> nextToken) (\s@ListDecoderManifestsResponse' {} a -> s {nextToken = a} :: ListDecoderManifestsResponse)

-- | A list of information about each decoder manifest.
listDecoderManifestsResponse_summaries :: Lens.Lens' ListDecoderManifestsResponse (Prelude.Maybe [DecoderManifestSummary])
listDecoderManifestsResponse_summaries = Lens.lens (\ListDecoderManifestsResponse' {summaries} -> summaries) (\s@ListDecoderManifestsResponse' {} a -> s {summaries = a} :: ListDecoderManifestsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listDecoderManifestsResponse_httpStatus :: Lens.Lens' ListDecoderManifestsResponse Prelude.Int
listDecoderManifestsResponse_httpStatus = Lens.lens (\ListDecoderManifestsResponse' {httpStatus} -> httpStatus) (\s@ListDecoderManifestsResponse' {} a -> s {httpStatus = a} :: ListDecoderManifestsResponse)

instance Prelude.NFData ListDecoderManifestsResponse where
  rnf ListDecoderManifestsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf summaries
      `Prelude.seq` Prelude.rnf httpStatus

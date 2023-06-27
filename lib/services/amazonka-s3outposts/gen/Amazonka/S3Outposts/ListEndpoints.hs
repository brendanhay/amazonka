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
-- Module      : Amazonka.S3Outposts.ListEndpoints
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists endpoints associated with the specified Outpost.
--
-- Related actions include:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_s3outposts_CreateEndpoint.html CreateEndpoint>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_s3outposts_DeleteEndpoint.html DeleteEndpoint>
--
-- This operation returns paginated results.
module Amazonka.S3Outposts.ListEndpoints
  ( -- * Creating a Request
    ListEndpoints (..),
    newListEndpoints,

    -- * Request Lenses
    listEndpoints_maxResults,
    listEndpoints_nextToken,

    -- * Destructuring the Response
    ListEndpointsResponse (..),
    newListEndpointsResponse,

    -- * Response Lenses
    listEndpointsResponse_endpoints,
    listEndpointsResponse_nextToken,
    listEndpointsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3Outposts.Types

-- | /See:/ 'newListEndpoints' smart constructor.
data ListEndpoints = ListEndpoints'
  { -- | The maximum number of endpoints that will be returned in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If a previous response from this operation included a @NextToken@ value,
    -- provide that value here to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEndpoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listEndpoints_maxResults' - The maximum number of endpoints that will be returned in the response.
--
-- 'nextToken', 'listEndpoints_nextToken' - If a previous response from this operation included a @NextToken@ value,
-- provide that value here to retrieve the next page of results.
newListEndpoints ::
  ListEndpoints
newListEndpoints =
  ListEndpoints'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of endpoints that will be returned in the response.
listEndpoints_maxResults :: Lens.Lens' ListEndpoints (Prelude.Maybe Prelude.Natural)
listEndpoints_maxResults = Lens.lens (\ListEndpoints' {maxResults} -> maxResults) (\s@ListEndpoints' {} a -> s {maxResults = a} :: ListEndpoints)

-- | If a previous response from this operation included a @NextToken@ value,
-- provide that value here to retrieve the next page of results.
listEndpoints_nextToken :: Lens.Lens' ListEndpoints (Prelude.Maybe Prelude.Text)
listEndpoints_nextToken = Lens.lens (\ListEndpoints' {nextToken} -> nextToken) (\s@ListEndpoints' {} a -> s {nextToken = a} :: ListEndpoints)

instance Core.AWSPager ListEndpoints where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEndpointsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listEndpointsResponse_endpoints
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listEndpoints_nextToken
          Lens..~ rs
          Lens.^? listEndpointsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListEndpoints where
  type
    AWSResponse ListEndpoints =
      ListEndpointsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEndpointsResponse'
            Prelude.<$> (x Data..?> "Endpoints" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListEndpoints where
  hashWithSalt _salt ListEndpoints' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListEndpoints where
  rnf ListEndpoints' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListEndpoints where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListEndpoints where
  toPath = Prelude.const "/S3Outposts/ListEndpoints"

instance Data.ToQuery ListEndpoints where
  toQuery ListEndpoints' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListEndpointsResponse' smart constructor.
data ListEndpointsResponse = ListEndpointsResponse'
  { -- | The list of endpoints associated with the specified Outpost.
    endpoints :: Prelude.Maybe [Endpoint],
    -- | If the number of endpoints associated with the specified Outpost exceeds
    -- @MaxResults@, you can include this value in subsequent calls to this
    -- operation to retrieve more results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEndpointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpoints', 'listEndpointsResponse_endpoints' - The list of endpoints associated with the specified Outpost.
--
-- 'nextToken', 'listEndpointsResponse_nextToken' - If the number of endpoints associated with the specified Outpost exceeds
-- @MaxResults@, you can include this value in subsequent calls to this
-- operation to retrieve more results.
--
-- 'httpStatus', 'listEndpointsResponse_httpStatus' - The response's http status code.
newListEndpointsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEndpointsResponse
newListEndpointsResponse pHttpStatus_ =
  ListEndpointsResponse'
    { endpoints = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of endpoints associated with the specified Outpost.
listEndpointsResponse_endpoints :: Lens.Lens' ListEndpointsResponse (Prelude.Maybe [Endpoint])
listEndpointsResponse_endpoints = Lens.lens (\ListEndpointsResponse' {endpoints} -> endpoints) (\s@ListEndpointsResponse' {} a -> s {endpoints = a} :: ListEndpointsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the number of endpoints associated with the specified Outpost exceeds
-- @MaxResults@, you can include this value in subsequent calls to this
-- operation to retrieve more results.
listEndpointsResponse_nextToken :: Lens.Lens' ListEndpointsResponse (Prelude.Maybe Prelude.Text)
listEndpointsResponse_nextToken = Lens.lens (\ListEndpointsResponse' {nextToken} -> nextToken) (\s@ListEndpointsResponse' {} a -> s {nextToken = a} :: ListEndpointsResponse)

-- | The response's http status code.
listEndpointsResponse_httpStatus :: Lens.Lens' ListEndpointsResponse Prelude.Int
listEndpointsResponse_httpStatus = Lens.lens (\ListEndpointsResponse' {httpStatus} -> httpStatus) (\s@ListEndpointsResponse' {} a -> s {httpStatus = a} :: ListEndpointsResponse)

instance Prelude.NFData ListEndpointsResponse where
  rnf ListEndpointsResponse' {..} =
    Prelude.rnf endpoints
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Amazonka.S3Outposts.ListSharedEndpoints
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all endpoints associated with an Outpost that has been shared by
-- Amazon Web Services Resource Access Manager (RAM).
--
-- Related actions include:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_s3outposts_CreateEndpoint.html CreateEndpoint>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_s3outposts_DeleteEndpoint.html DeleteEndpoint>
--
-- This operation returns paginated results.
module Amazonka.S3Outposts.ListSharedEndpoints
  ( -- * Creating a Request
    ListSharedEndpoints (..),
    newListSharedEndpoints,

    -- * Request Lenses
    listSharedEndpoints_nextToken,
    listSharedEndpoints_maxResults,
    listSharedEndpoints_outpostId,

    -- * Destructuring the Response
    ListSharedEndpointsResponse (..),
    newListSharedEndpointsResponse,

    -- * Response Lenses
    listSharedEndpointsResponse_nextToken,
    listSharedEndpointsResponse_endpoints,
    listSharedEndpointsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3Outposts.Types

-- | /See:/ 'newListSharedEndpoints' smart constructor.
data ListSharedEndpoints = ListSharedEndpoints'
  { -- | If a previous response from this operation included a @NextToken@ value,
    -- you can provide that value here to retrieve the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of endpoints that will be returned in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the Amazon Web Services Outpost.
    outpostId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSharedEndpoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSharedEndpoints_nextToken' - If a previous response from this operation included a @NextToken@ value,
-- you can provide that value here to retrieve the next page of results.
--
-- 'maxResults', 'listSharedEndpoints_maxResults' - The maximum number of endpoints that will be returned in the response.
--
-- 'outpostId', 'listSharedEndpoints_outpostId' - The ID of the Amazon Web Services Outpost.
newListSharedEndpoints ::
  -- | 'outpostId'
  Prelude.Text ->
  ListSharedEndpoints
newListSharedEndpoints pOutpostId_ =
  ListSharedEndpoints'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      outpostId = pOutpostId_
    }

-- | If a previous response from this operation included a @NextToken@ value,
-- you can provide that value here to retrieve the next page of results.
listSharedEndpoints_nextToken :: Lens.Lens' ListSharedEndpoints (Prelude.Maybe Prelude.Text)
listSharedEndpoints_nextToken = Lens.lens (\ListSharedEndpoints' {nextToken} -> nextToken) (\s@ListSharedEndpoints' {} a -> s {nextToken = a} :: ListSharedEndpoints)

-- | The maximum number of endpoints that will be returned in the response.
listSharedEndpoints_maxResults :: Lens.Lens' ListSharedEndpoints (Prelude.Maybe Prelude.Natural)
listSharedEndpoints_maxResults = Lens.lens (\ListSharedEndpoints' {maxResults} -> maxResults) (\s@ListSharedEndpoints' {} a -> s {maxResults = a} :: ListSharedEndpoints)

-- | The ID of the Amazon Web Services Outpost.
listSharedEndpoints_outpostId :: Lens.Lens' ListSharedEndpoints Prelude.Text
listSharedEndpoints_outpostId = Lens.lens (\ListSharedEndpoints' {outpostId} -> outpostId) (\s@ListSharedEndpoints' {} a -> s {outpostId = a} :: ListSharedEndpoints)

instance Core.AWSPager ListSharedEndpoints where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSharedEndpointsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSharedEndpointsResponse_endpoints
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listSharedEndpoints_nextToken
          Lens..~ rs
          Lens.^? listSharedEndpointsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListSharedEndpoints where
  type
    AWSResponse ListSharedEndpoints =
      ListSharedEndpointsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSharedEndpointsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Endpoints" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSharedEndpoints where
  hashWithSalt _salt ListSharedEndpoints' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` outpostId

instance Prelude.NFData ListSharedEndpoints where
  rnf ListSharedEndpoints' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf outpostId

instance Core.ToHeaders ListSharedEndpoints where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListSharedEndpoints where
  toPath =
    Prelude.const "/S3Outposts/ListSharedEndpoints"

instance Core.ToQuery ListSharedEndpoints where
  toQuery ListSharedEndpoints' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults,
        "outpostId" Core.=: outpostId
      ]

-- | /See:/ 'newListSharedEndpointsResponse' smart constructor.
data ListSharedEndpointsResponse = ListSharedEndpointsResponse'
  { -- | If the number of endpoints associated with the specified Outpost exceeds
    -- @MaxResults@, you can include this value in subsequent calls to this
    -- operation to retrieve more results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of endpoints associated with the specified Outpost that have
    -- been shared by Amazon Web Services Resource Access Manager (RAM).
    endpoints :: Prelude.Maybe [Endpoint],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSharedEndpointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSharedEndpointsResponse_nextToken' - If the number of endpoints associated with the specified Outpost exceeds
-- @MaxResults@, you can include this value in subsequent calls to this
-- operation to retrieve more results.
--
-- 'endpoints', 'listSharedEndpointsResponse_endpoints' - The list of endpoints associated with the specified Outpost that have
-- been shared by Amazon Web Services Resource Access Manager (RAM).
--
-- 'httpStatus', 'listSharedEndpointsResponse_httpStatus' - The response's http status code.
newListSharedEndpointsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSharedEndpointsResponse
newListSharedEndpointsResponse pHttpStatus_ =
  ListSharedEndpointsResponse'
    { nextToken =
        Prelude.Nothing,
      endpoints = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the number of endpoints associated with the specified Outpost exceeds
-- @MaxResults@, you can include this value in subsequent calls to this
-- operation to retrieve more results.
listSharedEndpointsResponse_nextToken :: Lens.Lens' ListSharedEndpointsResponse (Prelude.Maybe Prelude.Text)
listSharedEndpointsResponse_nextToken = Lens.lens (\ListSharedEndpointsResponse' {nextToken} -> nextToken) (\s@ListSharedEndpointsResponse' {} a -> s {nextToken = a} :: ListSharedEndpointsResponse)

-- | The list of endpoints associated with the specified Outpost that have
-- been shared by Amazon Web Services Resource Access Manager (RAM).
listSharedEndpointsResponse_endpoints :: Lens.Lens' ListSharedEndpointsResponse (Prelude.Maybe [Endpoint])
listSharedEndpointsResponse_endpoints = Lens.lens (\ListSharedEndpointsResponse' {endpoints} -> endpoints) (\s@ListSharedEndpointsResponse' {} a -> s {endpoints = a} :: ListSharedEndpointsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSharedEndpointsResponse_httpStatus :: Lens.Lens' ListSharedEndpointsResponse Prelude.Int
listSharedEndpointsResponse_httpStatus = Lens.lens (\ListSharedEndpointsResponse' {httpStatus} -> httpStatus) (\s@ListSharedEndpointsResponse' {} a -> s {httpStatus = a} :: ListSharedEndpointsResponse)

instance Prelude.NFData ListSharedEndpointsResponse where
  rnf ListSharedEndpointsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf endpoints
      `Prelude.seq` Prelude.rnf httpStatus

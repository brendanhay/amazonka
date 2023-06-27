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
-- Module      : Amazonka.MediaConvert.DescribeEndpoints
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Send an request with an empty body to the regional API endpoint to get
-- your account API endpoint.
--
-- This operation returns paginated results.
module Amazonka.MediaConvert.DescribeEndpoints
  ( -- * Creating a Request
    DescribeEndpoints (..),
    newDescribeEndpoints,

    -- * Request Lenses
    describeEndpoints_maxResults,
    describeEndpoints_mode,
    describeEndpoints_nextToken,

    -- * Destructuring the Response
    DescribeEndpointsResponse (..),
    newDescribeEndpointsResponse,

    -- * Response Lenses
    describeEndpointsResponse_endpoints,
    describeEndpointsResponse_nextToken,
    describeEndpointsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | DescribeEndpointsRequest
--
-- /See:/ 'newDescribeEndpoints' smart constructor.
data DescribeEndpoints = DescribeEndpoints'
  { -- | Optional. Max number of endpoints, up to twenty, that will be returned
    -- at one time.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | Optional field, defaults to DEFAULT. Specify DEFAULT for this operation
    -- to return your endpoints if any exist, or to create an endpoint for you
    -- and return it if one doesn\'t already exist. Specify GET_ONLY to return
    -- your endpoints if any exist, or an empty list if none exist.
    mode :: Prelude.Maybe DescribeEndpointsMode,
    -- | Use this string, provided with the response to a previous request, to
    -- request the next batch of endpoints.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEndpoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'describeEndpoints_maxResults' - Optional. Max number of endpoints, up to twenty, that will be returned
-- at one time.
--
-- 'mode', 'describeEndpoints_mode' - Optional field, defaults to DEFAULT. Specify DEFAULT for this operation
-- to return your endpoints if any exist, or to create an endpoint for you
-- and return it if one doesn\'t already exist. Specify GET_ONLY to return
-- your endpoints if any exist, or an empty list if none exist.
--
-- 'nextToken', 'describeEndpoints_nextToken' - Use this string, provided with the response to a previous request, to
-- request the next batch of endpoints.
newDescribeEndpoints ::
  DescribeEndpoints
newDescribeEndpoints =
  DescribeEndpoints'
    { maxResults = Prelude.Nothing,
      mode = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Optional. Max number of endpoints, up to twenty, that will be returned
-- at one time.
describeEndpoints_maxResults :: Lens.Lens' DescribeEndpoints (Prelude.Maybe Prelude.Int)
describeEndpoints_maxResults = Lens.lens (\DescribeEndpoints' {maxResults} -> maxResults) (\s@DescribeEndpoints' {} a -> s {maxResults = a} :: DescribeEndpoints)

-- | Optional field, defaults to DEFAULT. Specify DEFAULT for this operation
-- to return your endpoints if any exist, or to create an endpoint for you
-- and return it if one doesn\'t already exist. Specify GET_ONLY to return
-- your endpoints if any exist, or an empty list if none exist.
describeEndpoints_mode :: Lens.Lens' DescribeEndpoints (Prelude.Maybe DescribeEndpointsMode)
describeEndpoints_mode = Lens.lens (\DescribeEndpoints' {mode} -> mode) (\s@DescribeEndpoints' {} a -> s {mode = a} :: DescribeEndpoints)

-- | Use this string, provided with the response to a previous request, to
-- request the next batch of endpoints.
describeEndpoints_nextToken :: Lens.Lens' DescribeEndpoints (Prelude.Maybe Prelude.Text)
describeEndpoints_nextToken = Lens.lens (\DescribeEndpoints' {nextToken} -> nextToken) (\s@DescribeEndpoints' {} a -> s {nextToken = a} :: DescribeEndpoints)

instance Core.AWSPager DescribeEndpoints where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeEndpointsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeEndpointsResponse_endpoints
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeEndpoints_nextToken
          Lens..~ rs
          Lens.^? describeEndpointsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeEndpoints where
  type
    AWSResponse DescribeEndpoints =
      DescribeEndpointsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEndpointsResponse'
            Prelude.<$> (x Data..?> "endpoints" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEndpoints where
  hashWithSalt _salt DescribeEndpoints' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` mode
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeEndpoints where
  rnf DescribeEndpoints' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf mode
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeEndpoints where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeEndpoints where
  toJSON DescribeEndpoints' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("mode" Data..=) Prelude.<$> mode,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath DescribeEndpoints where
  toPath = Prelude.const "/2017-08-29/endpoints"

instance Data.ToQuery DescribeEndpoints where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEndpointsResponse' smart constructor.
data DescribeEndpointsResponse = DescribeEndpointsResponse'
  { -- | List of endpoints
    endpoints :: Prelude.Maybe [Endpoint],
    -- | Use this string to request the next batch of endpoints.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEndpointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpoints', 'describeEndpointsResponse_endpoints' - List of endpoints
--
-- 'nextToken', 'describeEndpointsResponse_nextToken' - Use this string to request the next batch of endpoints.
--
-- 'httpStatus', 'describeEndpointsResponse_httpStatus' - The response's http status code.
newDescribeEndpointsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEndpointsResponse
newDescribeEndpointsResponse pHttpStatus_ =
  DescribeEndpointsResponse'
    { endpoints =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of endpoints
describeEndpointsResponse_endpoints :: Lens.Lens' DescribeEndpointsResponse (Prelude.Maybe [Endpoint])
describeEndpointsResponse_endpoints = Lens.lens (\DescribeEndpointsResponse' {endpoints} -> endpoints) (\s@DescribeEndpointsResponse' {} a -> s {endpoints = a} :: DescribeEndpointsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Use this string to request the next batch of endpoints.
describeEndpointsResponse_nextToken :: Lens.Lens' DescribeEndpointsResponse (Prelude.Maybe Prelude.Text)
describeEndpointsResponse_nextToken = Lens.lens (\DescribeEndpointsResponse' {nextToken} -> nextToken) (\s@DescribeEndpointsResponse' {} a -> s {nextToken = a} :: DescribeEndpointsResponse)

-- | The response's http status code.
describeEndpointsResponse_httpStatus :: Lens.Lens' DescribeEndpointsResponse Prelude.Int
describeEndpointsResponse_httpStatus = Lens.lens (\DescribeEndpointsResponse' {httpStatus} -> httpStatus) (\s@DescribeEndpointsResponse' {} a -> s {httpStatus = a} :: DescribeEndpointsResponse)

instance Prelude.NFData DescribeEndpointsResponse where
  rnf DescribeEndpointsResponse' {..} =
    Prelude.rnf endpoints
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

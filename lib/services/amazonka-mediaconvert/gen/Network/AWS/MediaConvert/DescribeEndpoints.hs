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
-- Module      : Network.AWS.MediaConvert.DescribeEndpoints
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Send an request with an empty body to the regional API endpoint to get
-- your account API endpoint.
--
-- This operation returns paginated results.
module Network.AWS.MediaConvert.DescribeEndpoints
  ( -- * Creating a Request
    DescribeEndpoints (..),
    newDescribeEndpoints,

    -- * Request Lenses
    describeEndpoints_mode,
    describeEndpoints_nextToken,
    describeEndpoints_maxResults,

    -- * Destructuring the Response
    DescribeEndpointsResponse (..),
    newDescribeEndpointsResponse,

    -- * Response Lenses
    describeEndpointsResponse_nextToken,
    describeEndpointsResponse_endpoints,
    describeEndpointsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | DescribeEndpointsRequest
--
-- /See:/ 'newDescribeEndpoints' smart constructor.
data DescribeEndpoints = DescribeEndpoints'
  { -- | Optional field, defaults to DEFAULT. Specify DEFAULT for this operation
    -- to return your endpoints if any exist, or to create an endpoint for you
    -- and return it if one doesn\'t already exist. Specify GET_ONLY to return
    -- your endpoints if any exist, or an empty list if none exist.
    mode :: Prelude.Maybe DescribeEndpointsMode,
    -- | Use this string, provided with the response to a previous request, to
    -- request the next batch of endpoints.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Optional. Max number of endpoints, up to twenty, that will be returned
    -- at one time.
    maxResults :: Prelude.Maybe Prelude.Int
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
-- 'mode', 'describeEndpoints_mode' - Optional field, defaults to DEFAULT. Specify DEFAULT for this operation
-- to return your endpoints if any exist, or to create an endpoint for you
-- and return it if one doesn\'t already exist. Specify GET_ONLY to return
-- your endpoints if any exist, or an empty list if none exist.
--
-- 'nextToken', 'describeEndpoints_nextToken' - Use this string, provided with the response to a previous request, to
-- request the next batch of endpoints.
--
-- 'maxResults', 'describeEndpoints_maxResults' - Optional. Max number of endpoints, up to twenty, that will be returned
-- at one time.
newDescribeEndpoints ::
  DescribeEndpoints
newDescribeEndpoints =
  DescribeEndpoints'
    { mode = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

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

-- | Optional. Max number of endpoints, up to twenty, that will be returned
-- at one time.
describeEndpoints_maxResults :: Lens.Lens' DescribeEndpoints (Prelude.Maybe Prelude.Int)
describeEndpoints_maxResults = Lens.lens (\DescribeEndpoints' {maxResults} -> maxResults) (\s@DescribeEndpoints' {} a -> s {maxResults = a} :: DescribeEndpoints)

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
      Prelude.Just Prelude.$
        rq
          Prelude.& describeEndpoints_nextToken
          Lens..~ rs
          Lens.^? describeEndpointsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeEndpoints where
  type
    AWSResponse DescribeEndpoints =
      DescribeEndpointsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEndpointsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "endpoints" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEndpoints

instance Prelude.NFData DescribeEndpoints

instance Core.ToHeaders DescribeEndpoints where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeEndpoints where
  toJSON DescribeEndpoints' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("mode" Core..=) Prelude.<$> mode,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath DescribeEndpoints where
  toPath = Prelude.const "/2017-08-29/endpoints"

instance Core.ToQuery DescribeEndpoints where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEndpointsResponse' smart constructor.
data DescribeEndpointsResponse = DescribeEndpointsResponse'
  { -- | Use this string to request the next batch of endpoints.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List of endpoints
    endpoints :: Prelude.Maybe [Endpoint],
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
-- 'nextToken', 'describeEndpointsResponse_nextToken' - Use this string to request the next batch of endpoints.
--
-- 'endpoints', 'describeEndpointsResponse_endpoints' - List of endpoints
--
-- 'httpStatus', 'describeEndpointsResponse_httpStatus' - The response's http status code.
newDescribeEndpointsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEndpointsResponse
newDescribeEndpointsResponse pHttpStatus_ =
  DescribeEndpointsResponse'
    { nextToken =
        Prelude.Nothing,
      endpoints = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Use this string to request the next batch of endpoints.
describeEndpointsResponse_nextToken :: Lens.Lens' DescribeEndpointsResponse (Prelude.Maybe Prelude.Text)
describeEndpointsResponse_nextToken = Lens.lens (\DescribeEndpointsResponse' {nextToken} -> nextToken) (\s@DescribeEndpointsResponse' {} a -> s {nextToken = a} :: DescribeEndpointsResponse)

-- | List of endpoints
describeEndpointsResponse_endpoints :: Lens.Lens' DescribeEndpointsResponse (Prelude.Maybe [Endpoint])
describeEndpointsResponse_endpoints = Lens.lens (\DescribeEndpointsResponse' {endpoints} -> endpoints) (\s@DescribeEndpointsResponse' {} a -> s {endpoints = a} :: DescribeEndpointsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeEndpointsResponse_httpStatus :: Lens.Lens' DescribeEndpointsResponse Prelude.Int
describeEndpointsResponse_httpStatus = Lens.lens (\DescribeEndpointsResponse' {httpStatus} -> httpStatus) (\s@DescribeEndpointsResponse' {} a -> s {httpStatus = a} :: DescribeEndpointsResponse)

instance Prelude.NFData DescribeEndpointsResponse

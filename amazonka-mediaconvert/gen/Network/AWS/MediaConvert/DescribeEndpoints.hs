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
    describeEndpoints_nextToken,
    describeEndpoints_maxResults,
    describeEndpoints_mode,

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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | DescribeEndpointsRequest
--
-- /See:/ 'newDescribeEndpoints' smart constructor.
data DescribeEndpoints = DescribeEndpoints'
  { -- | Use this string, provided with the response to a previous request, to
    -- request the next batch of endpoints.
    nextToken :: Core.Maybe Core.Text,
    -- | Optional. Max number of endpoints, up to twenty, that will be returned
    -- at one time.
    maxResults :: Core.Maybe Core.Int,
    -- | Optional field, defaults to DEFAULT. Specify DEFAULT for this operation
    -- to return your endpoints if any exist, or to create an endpoint for you
    -- and return it if one doesn\'t already exist. Specify GET_ONLY to return
    -- your endpoints if any exist, or an empty list if none exist.
    mode :: Core.Maybe DescribeEndpointsMode
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEndpoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeEndpoints_nextToken' - Use this string, provided with the response to a previous request, to
-- request the next batch of endpoints.
--
-- 'maxResults', 'describeEndpoints_maxResults' - Optional. Max number of endpoints, up to twenty, that will be returned
-- at one time.
--
-- 'mode', 'describeEndpoints_mode' - Optional field, defaults to DEFAULT. Specify DEFAULT for this operation
-- to return your endpoints if any exist, or to create an endpoint for you
-- and return it if one doesn\'t already exist. Specify GET_ONLY to return
-- your endpoints if any exist, or an empty list if none exist.
newDescribeEndpoints ::
  DescribeEndpoints
newDescribeEndpoints =
  DescribeEndpoints'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      mode = Core.Nothing
    }

-- | Use this string, provided with the response to a previous request, to
-- request the next batch of endpoints.
describeEndpoints_nextToken :: Lens.Lens' DescribeEndpoints (Core.Maybe Core.Text)
describeEndpoints_nextToken = Lens.lens (\DescribeEndpoints' {nextToken} -> nextToken) (\s@DescribeEndpoints' {} a -> s {nextToken = a} :: DescribeEndpoints)

-- | Optional. Max number of endpoints, up to twenty, that will be returned
-- at one time.
describeEndpoints_maxResults :: Lens.Lens' DescribeEndpoints (Core.Maybe Core.Int)
describeEndpoints_maxResults = Lens.lens (\DescribeEndpoints' {maxResults} -> maxResults) (\s@DescribeEndpoints' {} a -> s {maxResults = a} :: DescribeEndpoints)

-- | Optional field, defaults to DEFAULT. Specify DEFAULT for this operation
-- to return your endpoints if any exist, or to create an endpoint for you
-- and return it if one doesn\'t already exist. Specify GET_ONLY to return
-- your endpoints if any exist, or an empty list if none exist.
describeEndpoints_mode :: Lens.Lens' DescribeEndpoints (Core.Maybe DescribeEndpointsMode)
describeEndpoints_mode = Lens.lens (\DescribeEndpoints' {mode} -> mode) (\s@DescribeEndpoints' {} a -> s {mode = a} :: DescribeEndpoints)

instance Core.AWSPager DescribeEndpoints where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeEndpointsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeEndpointsResponse_endpoints
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeEndpoints_nextToken
          Lens..~ rs
          Lens.^? describeEndpointsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest DescribeEndpoints where
  type
    AWSResponse DescribeEndpoints =
      DescribeEndpointsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEndpointsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "endpoints" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeEndpoints

instance Core.NFData DescribeEndpoints

instance Core.ToHeaders DescribeEndpoints where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeEndpoints where
  toJSON DescribeEndpoints' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("mode" Core..=) Core.<$> mode
          ]
      )

instance Core.ToPath DescribeEndpoints where
  toPath = Core.const "/2017-08-29/endpoints"

instance Core.ToQuery DescribeEndpoints where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeEndpointsResponse' smart constructor.
data DescribeEndpointsResponse = DescribeEndpointsResponse'
  { -- | Use this string to request the next batch of endpoints.
    nextToken :: Core.Maybe Core.Text,
    -- | List of endpoints
    endpoints :: Core.Maybe [Endpoint],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeEndpointsResponse
newDescribeEndpointsResponse pHttpStatus_ =
  DescribeEndpointsResponse'
    { nextToken =
        Core.Nothing,
      endpoints = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Use this string to request the next batch of endpoints.
describeEndpointsResponse_nextToken :: Lens.Lens' DescribeEndpointsResponse (Core.Maybe Core.Text)
describeEndpointsResponse_nextToken = Lens.lens (\DescribeEndpointsResponse' {nextToken} -> nextToken) (\s@DescribeEndpointsResponse' {} a -> s {nextToken = a} :: DescribeEndpointsResponse)

-- | List of endpoints
describeEndpointsResponse_endpoints :: Lens.Lens' DescribeEndpointsResponse (Core.Maybe [Endpoint])
describeEndpointsResponse_endpoints = Lens.lens (\DescribeEndpointsResponse' {endpoints} -> endpoints) (\s@DescribeEndpointsResponse' {} a -> s {endpoints = a} :: DescribeEndpointsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeEndpointsResponse_httpStatus :: Lens.Lens' DescribeEndpointsResponse Core.Int
describeEndpointsResponse_httpStatus = Lens.lens (\DescribeEndpointsResponse' {httpStatus} -> httpStatus) (\s@DescribeEndpointsResponse' {} a -> s {httpStatus = a} :: DescribeEndpointsResponse)

instance Core.NFData DescribeEndpointsResponse

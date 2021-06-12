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
-- Module      : Network.AWS.Glue.GetDevEndpoints
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all the development endpoints in this AWS account.
--
-- When you create a development endpoint in a virtual private cloud (VPC),
-- AWS Glue returns only a private IP address and the public IP address
-- field is not populated. When you create a non-VPC development endpoint,
-- AWS Glue returns only a public IP address.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetDevEndpoints
  ( -- * Creating a Request
    GetDevEndpoints (..),
    newGetDevEndpoints,

    -- * Request Lenses
    getDevEndpoints_nextToken,
    getDevEndpoints_maxResults,

    -- * Destructuring the Response
    GetDevEndpointsResponse (..),
    newGetDevEndpointsResponse,

    -- * Response Lenses
    getDevEndpointsResponse_nextToken,
    getDevEndpointsResponse_devEndpoints,
    getDevEndpointsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetDevEndpoints' smart constructor.
data GetDevEndpoints = GetDevEndpoints'
  { -- | A continuation token, if this is a continuation call.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum size of information to return.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDevEndpoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getDevEndpoints_nextToken' - A continuation token, if this is a continuation call.
--
-- 'maxResults', 'getDevEndpoints_maxResults' - The maximum size of information to return.
newGetDevEndpoints ::
  GetDevEndpoints
newGetDevEndpoints =
  GetDevEndpoints'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | A continuation token, if this is a continuation call.
getDevEndpoints_nextToken :: Lens.Lens' GetDevEndpoints (Core.Maybe Core.Text)
getDevEndpoints_nextToken = Lens.lens (\GetDevEndpoints' {nextToken} -> nextToken) (\s@GetDevEndpoints' {} a -> s {nextToken = a} :: GetDevEndpoints)

-- | The maximum size of information to return.
getDevEndpoints_maxResults :: Lens.Lens' GetDevEndpoints (Core.Maybe Core.Natural)
getDevEndpoints_maxResults = Lens.lens (\GetDevEndpoints' {maxResults} -> maxResults) (\s@GetDevEndpoints' {} a -> s {maxResults = a} :: GetDevEndpoints)

instance Core.AWSPager GetDevEndpoints where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getDevEndpointsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getDevEndpointsResponse_devEndpoints
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getDevEndpoints_nextToken
          Lens..~ rs
          Lens.^? getDevEndpointsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest GetDevEndpoints where
  type
    AWSResponse GetDevEndpoints =
      GetDevEndpointsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDevEndpointsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "DevEndpoints" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetDevEndpoints

instance Core.NFData GetDevEndpoints

instance Core.ToHeaders GetDevEndpoints where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetDevEndpoints" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetDevEndpoints where
  toJSON GetDevEndpoints' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath GetDevEndpoints where
  toPath = Core.const "/"

instance Core.ToQuery GetDevEndpoints where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetDevEndpointsResponse' smart constructor.
data GetDevEndpointsResponse = GetDevEndpointsResponse'
  { -- | A continuation token, if not all @DevEndpoint@ definitions have yet been
    -- returned.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of @DevEndpoint@ definitions.
    devEndpoints :: Core.Maybe [DevEndpoint],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDevEndpointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getDevEndpointsResponse_nextToken' - A continuation token, if not all @DevEndpoint@ definitions have yet been
-- returned.
--
-- 'devEndpoints', 'getDevEndpointsResponse_devEndpoints' - A list of @DevEndpoint@ definitions.
--
-- 'httpStatus', 'getDevEndpointsResponse_httpStatus' - The response's http status code.
newGetDevEndpointsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetDevEndpointsResponse
newGetDevEndpointsResponse pHttpStatus_ =
  GetDevEndpointsResponse'
    { nextToken = Core.Nothing,
      devEndpoints = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token, if not all @DevEndpoint@ definitions have yet been
-- returned.
getDevEndpointsResponse_nextToken :: Lens.Lens' GetDevEndpointsResponse (Core.Maybe Core.Text)
getDevEndpointsResponse_nextToken = Lens.lens (\GetDevEndpointsResponse' {nextToken} -> nextToken) (\s@GetDevEndpointsResponse' {} a -> s {nextToken = a} :: GetDevEndpointsResponse)

-- | A list of @DevEndpoint@ definitions.
getDevEndpointsResponse_devEndpoints :: Lens.Lens' GetDevEndpointsResponse (Core.Maybe [DevEndpoint])
getDevEndpointsResponse_devEndpoints = Lens.lens (\GetDevEndpointsResponse' {devEndpoints} -> devEndpoints) (\s@GetDevEndpointsResponse' {} a -> s {devEndpoints = a} :: GetDevEndpointsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getDevEndpointsResponse_httpStatus :: Lens.Lens' GetDevEndpointsResponse Core.Int
getDevEndpointsResponse_httpStatus = Lens.lens (\GetDevEndpointsResponse' {httpStatus} -> httpStatus) (\s@GetDevEndpointsResponse' {} a -> s {httpStatus = a} :: GetDevEndpointsResponse)

instance Core.NFData GetDevEndpointsResponse

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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetDevEndpoints' smart constructor.
data GetDevEndpoints = GetDevEndpoints'
  { -- | A continuation token, if this is a continuation call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum size of information to return.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A continuation token, if this is a continuation call.
getDevEndpoints_nextToken :: Lens.Lens' GetDevEndpoints (Prelude.Maybe Prelude.Text)
getDevEndpoints_nextToken = Lens.lens (\GetDevEndpoints' {nextToken} -> nextToken) (\s@GetDevEndpoints' {} a -> s {nextToken = a} :: GetDevEndpoints)

-- | The maximum size of information to return.
getDevEndpoints_maxResults :: Lens.Lens' GetDevEndpoints (Prelude.Maybe Prelude.Natural)
getDevEndpoints_maxResults = Lens.lens (\GetDevEndpoints' {maxResults} -> maxResults) (\s@GetDevEndpoints' {} a -> s {maxResults = a} :: GetDevEndpoints)

instance Core.AWSPager GetDevEndpoints where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getDevEndpointsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getDevEndpointsResponse_devEndpoints
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getDevEndpoints_nextToken
          Lens..~ rs
          Lens.^? getDevEndpointsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetDevEndpoints where
  type
    AWSResponse GetDevEndpoints =
      GetDevEndpointsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDevEndpointsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "DevEndpoints" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDevEndpoints

instance Prelude.NFData GetDevEndpoints

instance Core.ToHeaders GetDevEndpoints where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetDevEndpoints" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetDevEndpoints where
  toJSON GetDevEndpoints' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath GetDevEndpoints where
  toPath = Prelude.const "/"

instance Core.ToQuery GetDevEndpoints where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDevEndpointsResponse' smart constructor.
data GetDevEndpointsResponse = GetDevEndpointsResponse'
  { -- | A continuation token, if not all @DevEndpoint@ definitions have yet been
    -- returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of @DevEndpoint@ definitions.
    devEndpoints :: Prelude.Maybe [DevEndpoint],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetDevEndpointsResponse
newGetDevEndpointsResponse pHttpStatus_ =
  GetDevEndpointsResponse'
    { nextToken =
        Prelude.Nothing,
      devEndpoints = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token, if not all @DevEndpoint@ definitions have yet been
-- returned.
getDevEndpointsResponse_nextToken :: Lens.Lens' GetDevEndpointsResponse (Prelude.Maybe Prelude.Text)
getDevEndpointsResponse_nextToken = Lens.lens (\GetDevEndpointsResponse' {nextToken} -> nextToken) (\s@GetDevEndpointsResponse' {} a -> s {nextToken = a} :: GetDevEndpointsResponse)

-- | A list of @DevEndpoint@ definitions.
getDevEndpointsResponse_devEndpoints :: Lens.Lens' GetDevEndpointsResponse (Prelude.Maybe [DevEndpoint])
getDevEndpointsResponse_devEndpoints = Lens.lens (\GetDevEndpointsResponse' {devEndpoints} -> devEndpoints) (\s@GetDevEndpointsResponse' {} a -> s {devEndpoints = a} :: GetDevEndpointsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getDevEndpointsResponse_httpStatus :: Lens.Lens' GetDevEndpointsResponse Prelude.Int
getDevEndpointsResponse_httpStatus = Lens.lens (\GetDevEndpointsResponse' {httpStatus} -> httpStatus) (\s@GetDevEndpointsResponse' {} a -> s {httpStatus = a} :: GetDevEndpointsResponse)

instance Prelude.NFData GetDevEndpointsResponse

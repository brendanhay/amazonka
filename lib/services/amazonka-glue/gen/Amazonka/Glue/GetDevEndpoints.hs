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
-- Module      : Amazonka.Glue.GetDevEndpoints
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all the development endpoints in this Amazon Web Services
-- account.
--
-- When you create a development endpoint in a virtual private cloud (VPC),
-- Glue returns only a private IP address and the public IP address field
-- is not populated. When you create a non-VPC development endpoint, Glue
-- returns only a public IP address.
--
-- This operation returns paginated results.
module Amazonka.Glue.GetDevEndpoints
  ( -- * Creating a Request
    GetDevEndpoints (..),
    newGetDevEndpoints,

    -- * Request Lenses
    getDevEndpoints_maxResults,
    getDevEndpoints_nextToken,

    -- * Destructuring the Response
    GetDevEndpointsResponse (..),
    newGetDevEndpointsResponse,

    -- * Response Lenses
    getDevEndpointsResponse_devEndpoints,
    getDevEndpointsResponse_nextToken,
    getDevEndpointsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDevEndpoints' smart constructor.
data GetDevEndpoints = GetDevEndpoints'
  { -- | The maximum size of information to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A continuation token, if this is a continuation call.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'maxResults', 'getDevEndpoints_maxResults' - The maximum size of information to return.
--
-- 'nextToken', 'getDevEndpoints_nextToken' - A continuation token, if this is a continuation call.
newGetDevEndpoints ::
  GetDevEndpoints
newGetDevEndpoints =
  GetDevEndpoints'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum size of information to return.
getDevEndpoints_maxResults :: Lens.Lens' GetDevEndpoints (Prelude.Maybe Prelude.Natural)
getDevEndpoints_maxResults = Lens.lens (\GetDevEndpoints' {maxResults} -> maxResults) (\s@GetDevEndpoints' {} a -> s {maxResults = a} :: GetDevEndpoints)

-- | A continuation token, if this is a continuation call.
getDevEndpoints_nextToken :: Lens.Lens' GetDevEndpoints (Prelude.Maybe Prelude.Text)
getDevEndpoints_nextToken = Lens.lens (\GetDevEndpoints' {nextToken} -> nextToken) (\s@GetDevEndpoints' {} a -> s {nextToken = a} :: GetDevEndpoints)

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
        Prelude.Just
          Prelude.$ rq
          Prelude.& getDevEndpoints_nextToken
          Lens..~ rs
          Lens.^? getDevEndpointsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest GetDevEndpoints where
  type
    AWSResponse GetDevEndpoints =
      GetDevEndpointsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDevEndpointsResponse'
            Prelude.<$> (x Data..?> "DevEndpoints" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDevEndpoints where
  hashWithSalt _salt GetDevEndpoints' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData GetDevEndpoints where
  rnf GetDevEndpoints' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders GetDevEndpoints where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.GetDevEndpoints" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDevEndpoints where
  toJSON GetDevEndpoints' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath GetDevEndpoints where
  toPath = Prelude.const "/"

instance Data.ToQuery GetDevEndpoints where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDevEndpointsResponse' smart constructor.
data GetDevEndpointsResponse = GetDevEndpointsResponse'
  { -- | A list of @DevEndpoint@ definitions.
    devEndpoints :: Prelude.Maybe [DevEndpoint],
    -- | A continuation token, if not all @DevEndpoint@ definitions have yet been
    -- returned.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'devEndpoints', 'getDevEndpointsResponse_devEndpoints' - A list of @DevEndpoint@ definitions.
--
-- 'nextToken', 'getDevEndpointsResponse_nextToken' - A continuation token, if not all @DevEndpoint@ definitions have yet been
-- returned.
--
-- 'httpStatus', 'getDevEndpointsResponse_httpStatus' - The response's http status code.
newGetDevEndpointsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDevEndpointsResponse
newGetDevEndpointsResponse pHttpStatus_ =
  GetDevEndpointsResponse'
    { devEndpoints =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of @DevEndpoint@ definitions.
getDevEndpointsResponse_devEndpoints :: Lens.Lens' GetDevEndpointsResponse (Prelude.Maybe [DevEndpoint])
getDevEndpointsResponse_devEndpoints = Lens.lens (\GetDevEndpointsResponse' {devEndpoints} -> devEndpoints) (\s@GetDevEndpointsResponse' {} a -> s {devEndpoints = a} :: GetDevEndpointsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A continuation token, if not all @DevEndpoint@ definitions have yet been
-- returned.
getDevEndpointsResponse_nextToken :: Lens.Lens' GetDevEndpointsResponse (Prelude.Maybe Prelude.Text)
getDevEndpointsResponse_nextToken = Lens.lens (\GetDevEndpointsResponse' {nextToken} -> nextToken) (\s@GetDevEndpointsResponse' {} a -> s {nextToken = a} :: GetDevEndpointsResponse)

-- | The response's http status code.
getDevEndpointsResponse_httpStatus :: Lens.Lens' GetDevEndpointsResponse Prelude.Int
getDevEndpointsResponse_httpStatus = Lens.lens (\GetDevEndpointsResponse' {httpStatus} -> httpStatus) (\s@GetDevEndpointsResponse' {} a -> s {httpStatus = a} :: GetDevEndpointsResponse)

instance Prelude.NFData GetDevEndpointsResponse where
  rnf GetDevEndpointsResponse' {..} =
    Prelude.rnf devEndpoints
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

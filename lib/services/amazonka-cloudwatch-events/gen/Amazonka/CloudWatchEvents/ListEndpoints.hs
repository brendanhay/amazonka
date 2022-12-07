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
-- Module      : Amazonka.CloudWatchEvents.ListEndpoints
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the global endpoints associated with this account. For more
-- information about global endpoints, see
-- <https://docs.aws.amazon.com/eventbridge/latest/userguide/eb-global-endpoints.html Making applications Regional-fault tolerant with global endpoints and event replication>
-- in the Amazon EventBridge User Guide..
module Amazonka.CloudWatchEvents.ListEndpoints
  ( -- * Creating a Request
    ListEndpoints (..),
    newListEndpoints,

    -- * Request Lenses
    listEndpoints_nextToken,
    listEndpoints_maxResults,
    listEndpoints_namePrefix,
    listEndpoints_homeRegion,

    -- * Destructuring the Response
    ListEndpointsResponse (..),
    newListEndpointsResponse,

    -- * Response Lenses
    listEndpointsResponse_nextToken,
    listEndpointsResponse_endpoints,
    listEndpointsResponse_httpStatus,
  )
where

import Amazonka.CloudWatchEvents.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEndpoints' smart constructor.
data ListEndpoints = ListEndpoints'
  { -- | If @nextToken@ is returned, there are more results available. The value
    -- of nextToken is a unique pagination token for each page. Make the call
    -- again using the returned token to retrieve the next page. Keep all other
    -- arguments unchanged. Each pagination token expires after 24 hours. Using
    -- an expired pagination token will return an HTTP 400 InvalidToken error.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results returned by the call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A value that will return a subset of the endpoints associated with this
    -- account. For example, @\"NamePrefix\": \"ABC\"@ will return all
    -- endpoints with \"ABC\" in the name.
    namePrefix :: Prelude.Maybe Prelude.Text,
    -- | The primary Region of the endpoints associated with this account. For
    -- example @\"HomeRegion\": \"us-east-1\"@.
    homeRegion :: Prelude.Maybe Prelude.Text
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
-- 'nextToken', 'listEndpoints_nextToken' - If @nextToken@ is returned, there are more results available. The value
-- of nextToken is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an HTTP 400 InvalidToken error.
--
-- 'maxResults', 'listEndpoints_maxResults' - The maximum number of results returned by the call.
--
-- 'namePrefix', 'listEndpoints_namePrefix' - A value that will return a subset of the endpoints associated with this
-- account. For example, @\"NamePrefix\": \"ABC\"@ will return all
-- endpoints with \"ABC\" in the name.
--
-- 'homeRegion', 'listEndpoints_homeRegion' - The primary Region of the endpoints associated with this account. For
-- example @\"HomeRegion\": \"us-east-1\"@.
newListEndpoints ::
  ListEndpoints
newListEndpoints =
  ListEndpoints'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      namePrefix = Prelude.Nothing,
      homeRegion = Prelude.Nothing
    }

-- | If @nextToken@ is returned, there are more results available. The value
-- of nextToken is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an HTTP 400 InvalidToken error.
listEndpoints_nextToken :: Lens.Lens' ListEndpoints (Prelude.Maybe Prelude.Text)
listEndpoints_nextToken = Lens.lens (\ListEndpoints' {nextToken} -> nextToken) (\s@ListEndpoints' {} a -> s {nextToken = a} :: ListEndpoints)

-- | The maximum number of results returned by the call.
listEndpoints_maxResults :: Lens.Lens' ListEndpoints (Prelude.Maybe Prelude.Natural)
listEndpoints_maxResults = Lens.lens (\ListEndpoints' {maxResults} -> maxResults) (\s@ListEndpoints' {} a -> s {maxResults = a} :: ListEndpoints)

-- | A value that will return a subset of the endpoints associated with this
-- account. For example, @\"NamePrefix\": \"ABC\"@ will return all
-- endpoints with \"ABC\" in the name.
listEndpoints_namePrefix :: Lens.Lens' ListEndpoints (Prelude.Maybe Prelude.Text)
listEndpoints_namePrefix = Lens.lens (\ListEndpoints' {namePrefix} -> namePrefix) (\s@ListEndpoints' {} a -> s {namePrefix = a} :: ListEndpoints)

-- | The primary Region of the endpoints associated with this account. For
-- example @\"HomeRegion\": \"us-east-1\"@.
listEndpoints_homeRegion :: Lens.Lens' ListEndpoints (Prelude.Maybe Prelude.Text)
listEndpoints_homeRegion = Lens.lens (\ListEndpoints' {homeRegion} -> homeRegion) (\s@ListEndpoints' {} a -> s {homeRegion = a} :: ListEndpoints)

instance Core.AWSRequest ListEndpoints where
  type
    AWSResponse ListEndpoints =
      ListEndpointsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEndpointsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Endpoints" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListEndpoints where
  hashWithSalt _salt ListEndpoints' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` namePrefix
      `Prelude.hashWithSalt` homeRegion

instance Prelude.NFData ListEndpoints where
  rnf ListEndpoints' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf namePrefix
      `Prelude.seq` Prelude.rnf homeRegion

instance Data.ToHeaders ListEndpoints where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSEvents.ListEndpoints" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListEndpoints where
  toJSON ListEndpoints' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NamePrefix" Data..=) Prelude.<$> namePrefix,
            ("HomeRegion" Data..=) Prelude.<$> homeRegion
          ]
      )

instance Data.ToPath ListEndpoints where
  toPath = Prelude.const "/"

instance Data.ToQuery ListEndpoints where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListEndpointsResponse' smart constructor.
data ListEndpointsResponse = ListEndpointsResponse'
  { -- | If @nextToken@ is returned, there are more results available. The value
    -- of nextToken is a unique pagination token for each page. Make the call
    -- again using the returned token to retrieve the next page. Keep all other
    -- arguments unchanged. Each pagination token expires after 24 hours. Using
    -- an expired pagination token will return an HTTP 400 InvalidToken error.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The endpoints returned by the call.
    endpoints :: Prelude.Maybe [Endpoint],
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
-- 'nextToken', 'listEndpointsResponse_nextToken' - If @nextToken@ is returned, there are more results available. The value
-- of nextToken is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an HTTP 400 InvalidToken error.
--
-- 'endpoints', 'listEndpointsResponse_endpoints' - The endpoints returned by the call.
--
-- 'httpStatus', 'listEndpointsResponse_httpStatus' - The response's http status code.
newListEndpointsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEndpointsResponse
newListEndpointsResponse pHttpStatus_ =
  ListEndpointsResponse'
    { nextToken = Prelude.Nothing,
      endpoints = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If @nextToken@ is returned, there are more results available. The value
-- of nextToken is a unique pagination token for each page. Make the call
-- again using the returned token to retrieve the next page. Keep all other
-- arguments unchanged. Each pagination token expires after 24 hours. Using
-- an expired pagination token will return an HTTP 400 InvalidToken error.
listEndpointsResponse_nextToken :: Lens.Lens' ListEndpointsResponse (Prelude.Maybe Prelude.Text)
listEndpointsResponse_nextToken = Lens.lens (\ListEndpointsResponse' {nextToken} -> nextToken) (\s@ListEndpointsResponse' {} a -> s {nextToken = a} :: ListEndpointsResponse)

-- | The endpoints returned by the call.
listEndpointsResponse_endpoints :: Lens.Lens' ListEndpointsResponse (Prelude.Maybe [Endpoint])
listEndpointsResponse_endpoints = Lens.lens (\ListEndpointsResponse' {endpoints} -> endpoints) (\s@ListEndpointsResponse' {} a -> s {endpoints = a} :: ListEndpointsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listEndpointsResponse_httpStatus :: Lens.Lens' ListEndpointsResponse Prelude.Int
listEndpointsResponse_httpStatus = Lens.lens (\ListEndpointsResponse' {httpStatus} -> httpStatus) (\s@ListEndpointsResponse' {} a -> s {httpStatus = a} :: ListEndpointsResponse)

instance Prelude.NFData ListEndpointsResponse where
  rnf ListEndpointsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf endpoints
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Amazonka.EMRContainers.ListManagedEndpoints
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists managed endpoints based on a set of parameters. A managed endpoint
-- is a gateway that connects Amazon EMR Studio to Amazon EMR on EKS so
-- that Amazon EMR Studio can communicate with your virtual cluster.
--
-- This operation returns paginated results.
module Amazonka.EMRContainers.ListManagedEndpoints
  ( -- * Creating a Request
    ListManagedEndpoints (..),
    newListManagedEndpoints,

    -- * Request Lenses
    listManagedEndpoints_createdAfter,
    listManagedEndpoints_createdBefore,
    listManagedEndpoints_maxResults,
    listManagedEndpoints_nextToken,
    listManagedEndpoints_states,
    listManagedEndpoints_types,
    listManagedEndpoints_virtualClusterId,

    -- * Destructuring the Response
    ListManagedEndpointsResponse (..),
    newListManagedEndpointsResponse,

    -- * Response Lenses
    listManagedEndpointsResponse_endpoints,
    listManagedEndpointsResponse_nextToken,
    listManagedEndpointsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMRContainers.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListManagedEndpoints' smart constructor.
data ListManagedEndpoints = ListManagedEndpoints'
  { -- | The date and time after which the endpoints are created.
    createdAfter :: Prelude.Maybe Data.ISO8601,
    -- | The date and time before which the endpoints are created.
    createdBefore :: Prelude.Maybe Data.ISO8601,
    -- | The maximum number of managed endpoints that can be listed.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The token for the next set of managed endpoints to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The states of the managed endpoints.
    states :: Prelude.Maybe [EndpointState],
    -- | The types of the managed endpoints.
    types :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the virtual cluster.
    virtualClusterId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListManagedEndpoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAfter', 'listManagedEndpoints_createdAfter' - The date and time after which the endpoints are created.
--
-- 'createdBefore', 'listManagedEndpoints_createdBefore' - The date and time before which the endpoints are created.
--
-- 'maxResults', 'listManagedEndpoints_maxResults' - The maximum number of managed endpoints that can be listed.
--
-- 'nextToken', 'listManagedEndpoints_nextToken' - The token for the next set of managed endpoints to return.
--
-- 'states', 'listManagedEndpoints_states' - The states of the managed endpoints.
--
-- 'types', 'listManagedEndpoints_types' - The types of the managed endpoints.
--
-- 'virtualClusterId', 'listManagedEndpoints_virtualClusterId' - The ID of the virtual cluster.
newListManagedEndpoints ::
  -- | 'virtualClusterId'
  Prelude.Text ->
  ListManagedEndpoints
newListManagedEndpoints pVirtualClusterId_ =
  ListManagedEndpoints'
    { createdAfter =
        Prelude.Nothing,
      createdBefore = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      states = Prelude.Nothing,
      types = Prelude.Nothing,
      virtualClusterId = pVirtualClusterId_
    }

-- | The date and time after which the endpoints are created.
listManagedEndpoints_createdAfter :: Lens.Lens' ListManagedEndpoints (Prelude.Maybe Prelude.UTCTime)
listManagedEndpoints_createdAfter = Lens.lens (\ListManagedEndpoints' {createdAfter} -> createdAfter) (\s@ListManagedEndpoints' {} a -> s {createdAfter = a} :: ListManagedEndpoints) Prelude.. Lens.mapping Data._Time

-- | The date and time before which the endpoints are created.
listManagedEndpoints_createdBefore :: Lens.Lens' ListManagedEndpoints (Prelude.Maybe Prelude.UTCTime)
listManagedEndpoints_createdBefore = Lens.lens (\ListManagedEndpoints' {createdBefore} -> createdBefore) (\s@ListManagedEndpoints' {} a -> s {createdBefore = a} :: ListManagedEndpoints) Prelude.. Lens.mapping Data._Time

-- | The maximum number of managed endpoints that can be listed.
listManagedEndpoints_maxResults :: Lens.Lens' ListManagedEndpoints (Prelude.Maybe Prelude.Int)
listManagedEndpoints_maxResults = Lens.lens (\ListManagedEndpoints' {maxResults} -> maxResults) (\s@ListManagedEndpoints' {} a -> s {maxResults = a} :: ListManagedEndpoints)

-- | The token for the next set of managed endpoints to return.
listManagedEndpoints_nextToken :: Lens.Lens' ListManagedEndpoints (Prelude.Maybe Prelude.Text)
listManagedEndpoints_nextToken = Lens.lens (\ListManagedEndpoints' {nextToken} -> nextToken) (\s@ListManagedEndpoints' {} a -> s {nextToken = a} :: ListManagedEndpoints)

-- | The states of the managed endpoints.
listManagedEndpoints_states :: Lens.Lens' ListManagedEndpoints (Prelude.Maybe [EndpointState])
listManagedEndpoints_states = Lens.lens (\ListManagedEndpoints' {states} -> states) (\s@ListManagedEndpoints' {} a -> s {states = a} :: ListManagedEndpoints) Prelude.. Lens.mapping Lens.coerced

-- | The types of the managed endpoints.
listManagedEndpoints_types :: Lens.Lens' ListManagedEndpoints (Prelude.Maybe [Prelude.Text])
listManagedEndpoints_types = Lens.lens (\ListManagedEndpoints' {types} -> types) (\s@ListManagedEndpoints' {} a -> s {types = a} :: ListManagedEndpoints) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the virtual cluster.
listManagedEndpoints_virtualClusterId :: Lens.Lens' ListManagedEndpoints Prelude.Text
listManagedEndpoints_virtualClusterId = Lens.lens (\ListManagedEndpoints' {virtualClusterId} -> virtualClusterId) (\s@ListManagedEndpoints' {} a -> s {virtualClusterId = a} :: ListManagedEndpoints)

instance Core.AWSPager ListManagedEndpoints where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listManagedEndpointsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listManagedEndpointsResponse_endpoints
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listManagedEndpoints_nextToken
          Lens..~ rs
          Lens.^? listManagedEndpointsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListManagedEndpoints where
  type
    AWSResponse ListManagedEndpoints =
      ListManagedEndpointsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListManagedEndpointsResponse'
            Prelude.<$> (x Data..?> "endpoints" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListManagedEndpoints where
  hashWithSalt _salt ListManagedEndpoints' {..} =
    _salt
      `Prelude.hashWithSalt` createdAfter
      `Prelude.hashWithSalt` createdBefore
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` states
      `Prelude.hashWithSalt` types
      `Prelude.hashWithSalt` virtualClusterId

instance Prelude.NFData ListManagedEndpoints where
  rnf ListManagedEndpoints' {..} =
    Prelude.rnf createdAfter
      `Prelude.seq` Prelude.rnf createdBefore
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf states
      `Prelude.seq` Prelude.rnf types
      `Prelude.seq` Prelude.rnf virtualClusterId

instance Data.ToHeaders ListManagedEndpoints where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListManagedEndpoints where
  toPath ListManagedEndpoints' {..} =
    Prelude.mconcat
      [ "/virtualclusters/",
        Data.toBS virtualClusterId,
        "/endpoints"
      ]

instance Data.ToQuery ListManagedEndpoints where
  toQuery ListManagedEndpoints' {..} =
    Prelude.mconcat
      [ "createdAfter" Data.=: createdAfter,
        "createdBefore" Data.=: createdBefore,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "states"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> states),
        "types"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> types)
      ]

-- | /See:/ 'newListManagedEndpointsResponse' smart constructor.
data ListManagedEndpointsResponse = ListManagedEndpointsResponse'
  { -- | The managed endpoints to be listed.
    endpoints :: Prelude.Maybe [Endpoint],
    -- | The token for the next set of endpoints to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListManagedEndpointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpoints', 'listManagedEndpointsResponse_endpoints' - The managed endpoints to be listed.
--
-- 'nextToken', 'listManagedEndpointsResponse_nextToken' - The token for the next set of endpoints to return.
--
-- 'httpStatus', 'listManagedEndpointsResponse_httpStatus' - The response's http status code.
newListManagedEndpointsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListManagedEndpointsResponse
newListManagedEndpointsResponse pHttpStatus_ =
  ListManagedEndpointsResponse'
    { endpoints =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The managed endpoints to be listed.
listManagedEndpointsResponse_endpoints :: Lens.Lens' ListManagedEndpointsResponse (Prelude.Maybe [Endpoint])
listManagedEndpointsResponse_endpoints = Lens.lens (\ListManagedEndpointsResponse' {endpoints} -> endpoints) (\s@ListManagedEndpointsResponse' {} a -> s {endpoints = a} :: ListManagedEndpointsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of endpoints to return.
listManagedEndpointsResponse_nextToken :: Lens.Lens' ListManagedEndpointsResponse (Prelude.Maybe Prelude.Text)
listManagedEndpointsResponse_nextToken = Lens.lens (\ListManagedEndpointsResponse' {nextToken} -> nextToken) (\s@ListManagedEndpointsResponse' {} a -> s {nextToken = a} :: ListManagedEndpointsResponse)

-- | The response's http status code.
listManagedEndpointsResponse_httpStatus :: Lens.Lens' ListManagedEndpointsResponse Prelude.Int
listManagedEndpointsResponse_httpStatus = Lens.lens (\ListManagedEndpointsResponse' {httpStatus} -> httpStatus) (\s@ListManagedEndpointsResponse' {} a -> s {httpStatus = a} :: ListManagedEndpointsResponse)

instance Prelude.NFData ListManagedEndpointsResponse where
  rnf ListManagedEndpointsResponse' {..} =
    Prelude.rnf endpoints
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

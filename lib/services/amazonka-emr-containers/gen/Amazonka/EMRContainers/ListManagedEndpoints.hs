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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists managed endpoints based on a set of parameters. A managed endpoint
-- is a gateway that connects EMR Studio to Amazon EMR on EKS so that EMR
-- Studio can communicate with your virtual cluster.
--
-- This operation returns paginated results.
module Amazonka.EMRContainers.ListManagedEndpoints
  ( -- * Creating a Request
    ListManagedEndpoints (..),
    newListManagedEndpoints,

    -- * Request Lenses
    listManagedEndpoints_states,
    listManagedEndpoints_createdAfter,
    listManagedEndpoints_types,
    listManagedEndpoints_nextToken,
    listManagedEndpoints_maxResults,
    listManagedEndpoints_createdBefore,
    listManagedEndpoints_virtualClusterId,

    -- * Destructuring the Response
    ListManagedEndpointsResponse (..),
    newListManagedEndpointsResponse,

    -- * Response Lenses
    listManagedEndpointsResponse_nextToken,
    listManagedEndpointsResponse_endpoints,
    listManagedEndpointsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EMRContainers.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListManagedEndpoints' smart constructor.
data ListManagedEndpoints = ListManagedEndpoints'
  { -- | The states of the managed endpoints.
    states :: Prelude.Maybe [EndpointState],
    -- | The date and time after which the endpoints are created.
    createdAfter :: Prelude.Maybe Core.POSIX,
    -- | The types of the managed endpoints.
    types :: Prelude.Maybe [Prelude.Text],
    -- | The token for the next set of managed endpoints to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of managed endpoints that can be listed.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The date and time before which the endpoints are created.
    createdBefore :: Prelude.Maybe Core.POSIX,
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
-- 'states', 'listManagedEndpoints_states' - The states of the managed endpoints.
--
-- 'createdAfter', 'listManagedEndpoints_createdAfter' - The date and time after which the endpoints are created.
--
-- 'types', 'listManagedEndpoints_types' - The types of the managed endpoints.
--
-- 'nextToken', 'listManagedEndpoints_nextToken' - The token for the next set of managed endpoints to return.
--
-- 'maxResults', 'listManagedEndpoints_maxResults' - The maximum number of managed endpoints that can be listed.
--
-- 'createdBefore', 'listManagedEndpoints_createdBefore' - The date and time before which the endpoints are created.
--
-- 'virtualClusterId', 'listManagedEndpoints_virtualClusterId' - The ID of the virtual cluster.
newListManagedEndpoints ::
  -- | 'virtualClusterId'
  Prelude.Text ->
  ListManagedEndpoints
newListManagedEndpoints pVirtualClusterId_ =
  ListManagedEndpoints'
    { states = Prelude.Nothing,
      createdAfter = Prelude.Nothing,
      types = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      createdBefore = Prelude.Nothing,
      virtualClusterId = pVirtualClusterId_
    }

-- | The states of the managed endpoints.
listManagedEndpoints_states :: Lens.Lens' ListManagedEndpoints (Prelude.Maybe [EndpointState])
listManagedEndpoints_states = Lens.lens (\ListManagedEndpoints' {states} -> states) (\s@ListManagedEndpoints' {} a -> s {states = a} :: ListManagedEndpoints) Prelude.. Lens.mapping Lens.coerced

-- | The date and time after which the endpoints are created.
listManagedEndpoints_createdAfter :: Lens.Lens' ListManagedEndpoints (Prelude.Maybe Prelude.UTCTime)
listManagedEndpoints_createdAfter = Lens.lens (\ListManagedEndpoints' {createdAfter} -> createdAfter) (\s@ListManagedEndpoints' {} a -> s {createdAfter = a} :: ListManagedEndpoints) Prelude.. Lens.mapping Core._Time

-- | The types of the managed endpoints.
listManagedEndpoints_types :: Lens.Lens' ListManagedEndpoints (Prelude.Maybe [Prelude.Text])
listManagedEndpoints_types = Lens.lens (\ListManagedEndpoints' {types} -> types) (\s@ListManagedEndpoints' {} a -> s {types = a} :: ListManagedEndpoints) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of managed endpoints to return.
listManagedEndpoints_nextToken :: Lens.Lens' ListManagedEndpoints (Prelude.Maybe Prelude.Text)
listManagedEndpoints_nextToken = Lens.lens (\ListManagedEndpoints' {nextToken} -> nextToken) (\s@ListManagedEndpoints' {} a -> s {nextToken = a} :: ListManagedEndpoints)

-- | The maximum number of managed endpoints that can be listed.
listManagedEndpoints_maxResults :: Lens.Lens' ListManagedEndpoints (Prelude.Maybe Prelude.Int)
listManagedEndpoints_maxResults = Lens.lens (\ListManagedEndpoints' {maxResults} -> maxResults) (\s@ListManagedEndpoints' {} a -> s {maxResults = a} :: ListManagedEndpoints)

-- | The date and time before which the endpoints are created.
listManagedEndpoints_createdBefore :: Lens.Lens' ListManagedEndpoints (Prelude.Maybe Prelude.UTCTime)
listManagedEndpoints_createdBefore = Lens.lens (\ListManagedEndpoints' {createdBefore} -> createdBefore) (\s@ListManagedEndpoints' {} a -> s {createdBefore = a} :: ListManagedEndpoints) Prelude.. Lens.mapping Core._Time

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
      Prelude.Just Prelude.$
        rq
          Prelude.& listManagedEndpoints_nextToken
          Lens..~ rs
          Lens.^? listManagedEndpointsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListManagedEndpoints where
  type
    AWSResponse ListManagedEndpoints =
      ListManagedEndpointsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListManagedEndpointsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "endpoints" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListManagedEndpoints

instance Prelude.NFData ListManagedEndpoints

instance Core.ToHeaders ListManagedEndpoints where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListManagedEndpoints where
  toPath ListManagedEndpoints' {..} =
    Prelude.mconcat
      [ "/virtualclusters/",
        Core.toBS virtualClusterId,
        "/endpoints"
      ]

instance Core.ToQuery ListManagedEndpoints where
  toQuery ListManagedEndpoints' {..} =
    Prelude.mconcat
      [ "states"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> states),
        "createdAfter" Core.=: createdAfter,
        "types"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> types),
        "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults,
        "createdBefore" Core.=: createdBefore
      ]

-- | /See:/ 'newListManagedEndpointsResponse' smart constructor.
data ListManagedEndpointsResponse = ListManagedEndpointsResponse'
  { -- | The token for the next set of endpoints to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The managed endpoints to be listed.
    endpoints :: Prelude.Maybe [Endpoint],
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
-- 'nextToken', 'listManagedEndpointsResponse_nextToken' - The token for the next set of endpoints to return.
--
-- 'endpoints', 'listManagedEndpointsResponse_endpoints' - The managed endpoints to be listed.
--
-- 'httpStatus', 'listManagedEndpointsResponse_httpStatus' - The response's http status code.
newListManagedEndpointsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListManagedEndpointsResponse
newListManagedEndpointsResponse pHttpStatus_ =
  ListManagedEndpointsResponse'
    { nextToken =
        Prelude.Nothing,
      endpoints = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of endpoints to return.
listManagedEndpointsResponse_nextToken :: Lens.Lens' ListManagedEndpointsResponse (Prelude.Maybe Prelude.Text)
listManagedEndpointsResponse_nextToken = Lens.lens (\ListManagedEndpointsResponse' {nextToken} -> nextToken) (\s@ListManagedEndpointsResponse' {} a -> s {nextToken = a} :: ListManagedEndpointsResponse)

-- | The managed endpoints to be listed.
listManagedEndpointsResponse_endpoints :: Lens.Lens' ListManagedEndpointsResponse (Prelude.Maybe [Endpoint])
listManagedEndpointsResponse_endpoints = Lens.lens (\ListManagedEndpointsResponse' {endpoints} -> endpoints) (\s@ListManagedEndpointsResponse' {} a -> s {endpoints = a} :: ListManagedEndpointsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listManagedEndpointsResponse_httpStatus :: Lens.Lens' ListManagedEndpointsResponse Prelude.Int
listManagedEndpointsResponse_httpStatus = Lens.lens (\ListManagedEndpointsResponse' {httpStatus} -> httpStatus) (\s@ListManagedEndpointsResponse' {} a -> s {httpStatus = a} :: ListManagedEndpointsResponse)

instance Prelude.NFData ListManagedEndpointsResponse

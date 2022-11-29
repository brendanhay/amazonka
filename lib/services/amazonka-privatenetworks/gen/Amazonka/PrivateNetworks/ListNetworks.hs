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
-- Module      : Amazonka.PrivateNetworks.ListNetworks
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists networks. Add filters to your request to return a more specific
-- list of results. Use filters to match the status of the network.
--
-- This operation returns paginated results.
module Amazonka.PrivateNetworks.ListNetworks
  ( -- * Creating a Request
    ListNetworks (..),
    newListNetworks,

    -- * Request Lenses
    listNetworks_filters,
    listNetworks_maxResults,
    listNetworks_startToken,

    -- * Destructuring the Response
    ListNetworksResponse (..),
    newListNetworksResponse,

    -- * Response Lenses
    listNetworksResponse_networks,
    listNetworksResponse_nextToken,
    listNetworksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.PrivateNetworks.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListNetworks' smart constructor.
data ListNetworks = ListNetworks'
  { -- | The filters.
    --
    -- -   @STATUS@ - The status (@AVAILABLE@ | @CREATED@ | @DELETED@ |
    --     @DEPROVISIONING@ | @PROVISIONING@).
    --
    -- Filter values are case sensitive. If you specify multiple values for a
    -- filter, the values are joined with an @OR@, and the request returns all
    -- results that match any of the specified values.
    filters :: Prelude.Maybe (Prelude.HashMap NetworkFilterKeys [Prelude.Text]),
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    startToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListNetworks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listNetworks_filters' - The filters.
--
-- -   @STATUS@ - The status (@AVAILABLE@ | @CREATED@ | @DELETED@ |
--     @DEPROVISIONING@ | @PROVISIONING@).
--
-- Filter values are case sensitive. If you specify multiple values for a
-- filter, the values are joined with an @OR@, and the request returns all
-- results that match any of the specified values.
--
-- 'maxResults', 'listNetworks_maxResults' - The maximum number of results to return.
--
-- 'startToken', 'listNetworks_startToken' - The token for the next page of results.
newListNetworks ::
  ListNetworks
newListNetworks =
  ListNetworks'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      startToken = Prelude.Nothing
    }

-- | The filters.
--
-- -   @STATUS@ - The status (@AVAILABLE@ | @CREATED@ | @DELETED@ |
--     @DEPROVISIONING@ | @PROVISIONING@).
--
-- Filter values are case sensitive. If you specify multiple values for a
-- filter, the values are joined with an @OR@, and the request returns all
-- results that match any of the specified values.
listNetworks_filters :: Lens.Lens' ListNetworks (Prelude.Maybe (Prelude.HashMap NetworkFilterKeys [Prelude.Text]))
listNetworks_filters = Lens.lens (\ListNetworks' {filters} -> filters) (\s@ListNetworks' {} a -> s {filters = a} :: ListNetworks) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return.
listNetworks_maxResults :: Lens.Lens' ListNetworks (Prelude.Maybe Prelude.Natural)
listNetworks_maxResults = Lens.lens (\ListNetworks' {maxResults} -> maxResults) (\s@ListNetworks' {} a -> s {maxResults = a} :: ListNetworks)

-- | The token for the next page of results.
listNetworks_startToken :: Lens.Lens' ListNetworks (Prelude.Maybe Prelude.Text)
listNetworks_startToken = Lens.lens (\ListNetworks' {startToken} -> startToken) (\s@ListNetworks' {} a -> s {startToken = a} :: ListNetworks)

instance Core.AWSPager ListNetworks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listNetworksResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listNetworksResponse_networks Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listNetworks_startToken
          Lens..~ rs
          Lens.^? listNetworksResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListNetworks where
  type AWSResponse ListNetworks = ListNetworksResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListNetworksResponse'
            Prelude.<$> (x Core..?> "networks" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListNetworks where
  hashWithSalt _salt ListNetworks' {..} =
    _salt `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` startToken

instance Prelude.NFData ListNetworks where
  rnf ListNetworks' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf startToken

instance Core.ToHeaders ListNetworks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListNetworks where
  toJSON ListNetworks' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("filters" Core..=) Prelude.<$> filters,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            ("startToken" Core..=) Prelude.<$> startToken
          ]
      )

instance Core.ToPath ListNetworks where
  toPath = Prelude.const "/v1/networks/list"

instance Core.ToQuery ListNetworks where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListNetworksResponse' smart constructor.
data ListNetworksResponse = ListNetworksResponse'
  { -- | The networks.
    networks :: Prelude.Maybe [Network],
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListNetworksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networks', 'listNetworksResponse_networks' - The networks.
--
-- 'nextToken', 'listNetworksResponse_nextToken' - The token for the next page of results.
--
-- 'httpStatus', 'listNetworksResponse_httpStatus' - The response's http status code.
newListNetworksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListNetworksResponse
newListNetworksResponse pHttpStatus_ =
  ListNetworksResponse'
    { networks = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The networks.
listNetworksResponse_networks :: Lens.Lens' ListNetworksResponse (Prelude.Maybe [Network])
listNetworksResponse_networks = Lens.lens (\ListNetworksResponse' {networks} -> networks) (\s@ListNetworksResponse' {} a -> s {networks = a} :: ListNetworksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next page of results.
listNetworksResponse_nextToken :: Lens.Lens' ListNetworksResponse (Prelude.Maybe Prelude.Text)
listNetworksResponse_nextToken = Lens.lens (\ListNetworksResponse' {nextToken} -> nextToken) (\s@ListNetworksResponse' {} a -> s {nextToken = a} :: ListNetworksResponse)

-- | The response's http status code.
listNetworksResponse_httpStatus :: Lens.Lens' ListNetworksResponse Prelude.Int
listNetworksResponse_httpStatus = Lens.lens (\ListNetworksResponse' {httpStatus} -> httpStatus) (\s@ListNetworksResponse' {} a -> s {httpStatus = a} :: ListNetworksResponse)

instance Prelude.NFData ListNetworksResponse where
  rnf ListNetworksResponse' {..} =
    Prelude.rnf networks
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Amazonka.PrivateNetworks.ListNetworkSites
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists network sites. Add filters to your request to return a more
-- specific list of results. Use filters to match the status of the network
-- site.
--
-- This operation returns paginated results.
module Amazonka.PrivateNetworks.ListNetworkSites
  ( -- * Creating a Request
    ListNetworkSites (..),
    newListNetworkSites,

    -- * Request Lenses
    listNetworkSites_filters,
    listNetworkSites_maxResults,
    listNetworkSites_startToken,
    listNetworkSites_networkArn,

    -- * Destructuring the Response
    ListNetworkSitesResponse (..),
    newListNetworkSitesResponse,

    -- * Response Lenses
    listNetworkSitesResponse_nextToken,
    listNetworkSitesResponse_networkSites,
    listNetworkSitesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.PrivateNetworks.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListNetworkSites' smart constructor.
data ListNetworkSites = ListNetworkSites'
  { -- | The filters. Add filters to your request to return a more specific list
    -- of results. Use filters to match the status of the network sites.
    --
    -- -   @STATUS@ - The status (@AVAILABLE@ | @CREATED@ | @DELETED@ |
    --     @DEPROVISIONING@ | @PROVISIONING@).
    --
    -- Filter values are case sensitive. If you specify multiple values for a
    -- filter, the values are joined with an @OR@, and the request returns all
    -- results that match any of the specified values.
    filters :: Prelude.Maybe (Prelude.HashMap NetworkSiteFilterKeys [Prelude.Text]),
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    startToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the network.
    networkArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListNetworkSites' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listNetworkSites_filters' - The filters. Add filters to your request to return a more specific list
-- of results. Use filters to match the status of the network sites.
--
-- -   @STATUS@ - The status (@AVAILABLE@ | @CREATED@ | @DELETED@ |
--     @DEPROVISIONING@ | @PROVISIONING@).
--
-- Filter values are case sensitive. If you specify multiple values for a
-- filter, the values are joined with an @OR@, and the request returns all
-- results that match any of the specified values.
--
-- 'maxResults', 'listNetworkSites_maxResults' - The maximum number of results to return.
--
-- 'startToken', 'listNetworkSites_startToken' - The token for the next page of results.
--
-- 'networkArn', 'listNetworkSites_networkArn' - The Amazon Resource Name (ARN) of the network.
newListNetworkSites ::
  -- | 'networkArn'
  Prelude.Text ->
  ListNetworkSites
newListNetworkSites pNetworkArn_ =
  ListNetworkSites'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      startToken = Prelude.Nothing,
      networkArn = pNetworkArn_
    }

-- | The filters. Add filters to your request to return a more specific list
-- of results. Use filters to match the status of the network sites.
--
-- -   @STATUS@ - The status (@AVAILABLE@ | @CREATED@ | @DELETED@ |
--     @DEPROVISIONING@ | @PROVISIONING@).
--
-- Filter values are case sensitive. If you specify multiple values for a
-- filter, the values are joined with an @OR@, and the request returns all
-- results that match any of the specified values.
listNetworkSites_filters :: Lens.Lens' ListNetworkSites (Prelude.Maybe (Prelude.HashMap NetworkSiteFilterKeys [Prelude.Text]))
listNetworkSites_filters = Lens.lens (\ListNetworkSites' {filters} -> filters) (\s@ListNetworkSites' {} a -> s {filters = a} :: ListNetworkSites) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return.
listNetworkSites_maxResults :: Lens.Lens' ListNetworkSites (Prelude.Maybe Prelude.Natural)
listNetworkSites_maxResults = Lens.lens (\ListNetworkSites' {maxResults} -> maxResults) (\s@ListNetworkSites' {} a -> s {maxResults = a} :: ListNetworkSites)

-- | The token for the next page of results.
listNetworkSites_startToken :: Lens.Lens' ListNetworkSites (Prelude.Maybe Prelude.Text)
listNetworkSites_startToken = Lens.lens (\ListNetworkSites' {startToken} -> startToken) (\s@ListNetworkSites' {} a -> s {startToken = a} :: ListNetworkSites)

-- | The Amazon Resource Name (ARN) of the network.
listNetworkSites_networkArn :: Lens.Lens' ListNetworkSites Prelude.Text
listNetworkSites_networkArn = Lens.lens (\ListNetworkSites' {networkArn} -> networkArn) (\s@ListNetworkSites' {} a -> s {networkArn = a} :: ListNetworkSites)

instance Core.AWSPager ListNetworkSites where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listNetworkSitesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listNetworkSitesResponse_networkSites
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listNetworkSites_startToken
          Lens..~ rs
          Lens.^? listNetworkSitesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListNetworkSites where
  type
    AWSResponse ListNetworkSites =
      ListNetworkSitesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListNetworkSitesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "networkSites" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListNetworkSites where
  hashWithSalt _salt ListNetworkSites' {..} =
    _salt `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` startToken
      `Prelude.hashWithSalt` networkArn

instance Prelude.NFData ListNetworkSites where
  rnf ListNetworkSites' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf startToken
      `Prelude.seq` Prelude.rnf networkArn

instance Core.ToHeaders ListNetworkSites where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListNetworkSites where
  toJSON ListNetworkSites' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("filters" Core..=) Prelude.<$> filters,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            ("startToken" Core..=) Prelude.<$> startToken,
            Prelude.Just ("networkArn" Core..= networkArn)
          ]
      )

instance Core.ToPath ListNetworkSites where
  toPath = Prelude.const "/v1/network-sites/list"

instance Core.ToQuery ListNetworkSites where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListNetworkSitesResponse' smart constructor.
data ListNetworkSitesResponse = ListNetworkSitesResponse'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the network sites.
    networkSites :: Prelude.Maybe [NetworkSite],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListNetworkSitesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listNetworkSitesResponse_nextToken' - The token for the next page of results.
--
-- 'networkSites', 'listNetworkSitesResponse_networkSites' - Information about the network sites.
--
-- 'httpStatus', 'listNetworkSitesResponse_httpStatus' - The response's http status code.
newListNetworkSitesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListNetworkSitesResponse
newListNetworkSitesResponse pHttpStatus_ =
  ListNetworkSitesResponse'
    { nextToken =
        Prelude.Nothing,
      networkSites = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next page of results.
listNetworkSitesResponse_nextToken :: Lens.Lens' ListNetworkSitesResponse (Prelude.Maybe Prelude.Text)
listNetworkSitesResponse_nextToken = Lens.lens (\ListNetworkSitesResponse' {nextToken} -> nextToken) (\s@ListNetworkSitesResponse' {} a -> s {nextToken = a} :: ListNetworkSitesResponse)

-- | Information about the network sites.
listNetworkSitesResponse_networkSites :: Lens.Lens' ListNetworkSitesResponse (Prelude.Maybe [NetworkSite])
listNetworkSitesResponse_networkSites = Lens.lens (\ListNetworkSitesResponse' {networkSites} -> networkSites) (\s@ListNetworkSitesResponse' {} a -> s {networkSites = a} :: ListNetworkSitesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listNetworkSitesResponse_httpStatus :: Lens.Lens' ListNetworkSitesResponse Prelude.Int
listNetworkSitesResponse_httpStatus = Lens.lens (\ListNetworkSitesResponse' {httpStatus} -> httpStatus) (\s@ListNetworkSitesResponse' {} a -> s {httpStatus = a} :: ListNetworkSitesResponse)

instance Prelude.NFData ListNetworkSitesResponse where
  rnf ListNetworkSitesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf networkSites
      `Prelude.seq` Prelude.rnf httpStatus
